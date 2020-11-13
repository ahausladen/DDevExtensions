unit ImportHooking;

interface

{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclPeImage.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) Petr Vones. All Rights Reserved.                                                   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Uwe Schuster (uschuster)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Hallvard Vassbotn                                                                              }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains various classes and support routines to read the contents of portable         }
{ executable (PE) files. You can use these classes to, for example examine the contents of the     }
{ imports section of an executable. In addition the unit contains support for Borland specific     }
{ structures and name unmangling.                                                                  }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Reduced to the minimum for import hooks and extended by Andreas Hausladen (ahuser)               }
{                                                                                                  }
{**************************************************************************************************}

{.$I jedi\jedi.inc}

uses
  Windows,
  {$IFDEF CMD_COMPILER}
  SimpleRtl.Containers,
  {$ELSE}
  Classes, Contnrs,
  IniFiles, // for TStringHash
  {$ENDIF CMD_COMPILER}
  SysUtils;

const
  CurProcess = Cardinal(-1);

// API hooking classes
type
  {$IF not declared(SIZE_T)}
  SIZE_T = DWORD;
  {$IFEND}
  PPointer = ^Pointer; // Delphi 5

  TJclPeMapImgHookItem = class(TObject)
  private
    FBaseAddress: Pointer;
    FFunctionName: string;
    FModuleName: string;
    FNewAddress: Pointer;
    FOriginalAddress: Pointer;
    FList: TObjectList;
  protected
    function InternalUnhook: Boolean;
  public
    destructor Destroy; override;
    function Unhook: Boolean;
    property BaseAddress: Pointer read FBaseAddress;
    property FunctionName: string read FFunctionName;
    property ModuleName: string read FModuleName;
    property NewAddress: Pointer read FNewAddress;
    property OriginalAddress: Pointer read FOriginalAddress;
  end;

  TReplaceImportEvent = function(ModuleName: PAnsiChar; FromProc: Pointer; var ToProc: Pointer): Boolean;

  TJclPeMapImgHooks = class(TObjectList)
  private
    function GetItems(Index: Integer): TJclPeMapImgHookItem;
    function GetItemFromOriginalAddress(BaseAddress, OriginalAddress: Pointer): TJclPeMapImgHookItem;
    function GetItemFromNewAddress(NewAddress: Pointer): TJclPeMapImgHookItem;
  public
    procedure DiscardUnhookInfo;
    function HookImport(Base: Pointer; const ModuleName, FunctionName: string;
      NewAddress: Pointer; var OriginalAddress: Pointer): Boolean; overload;
    function HookImport(Base: Pointer; const ModuleName, FunctionName: string;
      NewAddress: Pointer): Boolean; overload;
    function HookImport(Base: Pointer; ModuleHandle: THandle;
      const ModuleName, FunctionName: string; NewAddress: Pointer;
      var OriginalAddress: Pointer): Boolean; overload;
    function HookImport(Base: Pointer; ModuleHandle: THandle;
      const ModuleName, FunctionName: string; NewAddress: Pointer): Boolean; overload;
    //class function IsWin9xDebugThunk(P: Pointer): Boolean;
    class function ReplaceImport(Base: Pointer; const ModuleName: string; FromProc, ToProc: Pointer): Boolean;
    function EnumImports(Base: Pointer; EvReplaceImport: TReplaceImportEvent): Boolean;
    {class function SystemBase: Pointer;
    procedure UnhookAll;}
    class function GetImportEntryPtr(Base: Pointer; const ModuleName: string; FromProc: Pointer): PPointer;
    function UnhookByNewAddress(NewAddress: Pointer): Boolean;
    procedure UnhookByBaseAddress(BaseAddress: Pointer);
    property Items[Index: Integer]: TJclPeMapImgHookItem read GetItems; default;
    property ItemFromOriginalAddress[BaseAddress, OriginalAddress: Pointer]: TJclPeMapImgHookItem read GetItemFromOriginalAddress;
    property ItemFromNewAddress[NewAddress: Pointer]: TJclPeMapImgHookItem read GetItemFromNewAddress;
  end;

function IsWinNT: Boolean;
function PeMapImgNtHeaders(const BaseAddress: Pointer): PImageNtHeaders;
function PeMapImgSections(NtHeaders: PImageNtHeaders): PImageSectionHeader;
function PeMapImgFindSection(NtHeaders: PImageNtHeaders;
  const SectionName: string): PImageSectionHeader;
function CreateImportLibraryList(MappedAddress: PAnsiChar): TStrings;

implementation

const
  CSTR_EQUAL = 2;
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT      = 13;  { Delay Load Import Descriptors }

type
  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;

type
  ULONGLONG = Int64;

  TIIDUnion = record
    case Integer of
      0: (Characteristics: DWORD);         // 0 for terminating null import descriptor
      1: (OriginalFirstThunk: DWORD);      // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
  end;

  PIMAGE_IMPORT_DESCRIPTOR = ^IMAGE_IMPORT_DESCRIPTOR;
  _IMAGE_IMPORT_DESCRIPTOR = record
    Union: TIIDUnion;
    TimeDateStamp: DWORD;                  // 0 if not bound,
                                           // -1 if bound, and real date\time stamp
                                           //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                           // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: DWORD;                 // -1 if no forwarders
    Name: DWORD;
    FirstThunk: DWORD;                     // RVA to IAT (if bound this IAT has actual addresses)
  end;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  TImageImportDescriptor = IMAGE_IMPORT_DESCRIPTOR;
  PImageImportDescriptor = PIMAGE_IMPORT_DESCRIPTOR;

  PIMAGE_TLS_DIRECTORY32 = ^IMAGE_TLS_DIRECTORY32;
  _IMAGE_TLS_DIRECTORY32 = record
    StartAddressOfRawData: DWORD;
    EndAddressOfRawData: DWORD;
    AddressOfIndex: DWORD;             // PDWORD
    AddressOfCallBacks: DWORD;         // PIMAGE_TLS_CALLBACK *
    SizeOfZeroFill: DWORD;
    Characteristics: DWORD;
  end;
  IMAGE_TLS_DIRECTORY32 = _IMAGE_TLS_DIRECTORY32;
  TImageTlsDirectory32 = IMAGE_TLS_DIRECTORY32;
  PImageTlsDirectory32 = PIMAGE_TLS_DIRECTORY32;

  PIMAGE_THUNK_DATA32 = ^IMAGE_THUNK_DATA32;
  _IMAGE_THUNK_DATA32 = record
    case Integer of
      0: (ForwarderString: DWORD);   // PBYTE
      1: (Function_: DWORD);         // PDWORD
      2: (Ordinal: DWORD);
      3: (AddressOfData: DWORD);     // PIMAGE_IMPORT_BY_NAME
  end;
  IMAGE_THUNK_DATA32 = _IMAGE_THUNK_DATA32;
  TImageThunkData32 = IMAGE_THUNK_DATA32;
  PImageThunkData32 = PIMAGE_THUNK_DATA32;

  IMAGE_THUNK_DATA = IMAGE_THUNK_DATA32;
  {$EXTERNALSYM IMAGE_THUNK_DATA}
  PIMAGE_THUNK_DATA = PIMAGE_THUNK_DATA32;
  {$EXTERNALSYM PIMAGE_THUNK_DATA}
  TImageThunkData = TImageThunkData32;
  PImageThunkData = PImageThunkData32;

function IsWinNT: Boolean;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
    Result := OSVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT
  else
    Result := False;
end;

function PeMapImgNtHeaders(const BaseAddress: Pointer): PImageNtHeaders;
begin
  Result := nil;
  if IsBadReadPtr(BaseAddress, SizeOf(TImageDosHeader)) then
    Exit;
  if (PImageDosHeader(BaseAddress)^.e_magic <> IMAGE_DOS_SIGNATURE) or
    (PImageDosHeader(BaseAddress)^._lfanew = 0) then
    Exit;
  Result := PImageNtHeaders(PAnsiChar(BaseAddress) + DWORD(PImageDosHeader(BaseAddress)^._lfanew));
  if IsBadReadPtr(Result, SizeOf(TImageNtHeaders)) or
    (Result^.Signature <> IMAGE_NT_SIGNATURE) then
      Result := nil
end;

function PeMapImgSections(NtHeaders: PImageNtHeaders): PImageSectionHeader;
begin
  if NtHeaders = nil then
    Result := nil
  else
    Result := PImageSectionHeader(PAnsiChar(@NtHeaders^.OptionalHeader) +
      NtHeaders^.FileHeader.SizeOfOptionalHeader);
end;

function PeMapImgFindSection(NtHeaders: PImageNtHeaders;
  const SectionName: string): PImageSectionHeader;
var
  Header: PImageSectionHeader;
  I: Integer;
  P: PAnsiChar;
begin
  Result := nil;
  if NtHeaders <> nil then
  begin
    P := PAnsiChar({$IFDEF UNICODE}UTF8Encode{$ENDIF}(SectionName));
    Header := PeMapImgSections(NtHeaders);
    with NtHeaders^ do
      for I := 1 to FileHeader.NumberOfSections do
        {$WARNINGS OFF}
        if StrLComp(PAnsiChar(@Header^.Name), P, IMAGE_SIZEOF_SHORT_NAME) = 0 then
        {$WARNINGS ON}
        begin
          Result := Header;
          Break;
        end
        else
          Inc(Header);
  end;
end;

{ TJclPeMapImgHookItem }

destructor TJclPeMapImgHookItem.Destroy;
begin
  if FBaseAddress <> nil then
    InternalUnhook;
  inherited Destroy;
end;

function TJclPeMapImgHookItem.InternalUnhook: Boolean;
var
  Buf: TMemoryBasicInformation;
begin
  if (VirtualQuery(FBaseAddress, Buf, SizeOf(Buf)) = SizeOf(Buf)) and (Buf.State and MEM_FREE = 0) then
    Result := TJclPeMapImgHooks.ReplaceImport(FBaseAddress, ModuleName, NewAddress, OriginalAddress)
  else
    Result := True; // PE image is not available anymore (DLL got unloaded)
  if Result then
    FBaseAddress := nil;
end;

function TJclPeMapImgHookItem.Unhook: Boolean;
begin
  Result := InternalUnhook;
  if Result then
    FList.Remove(Self);
end;

{ TJclPeMapImgHooks }

function TJclPeMapImgHooks.GetItems(Index: Integer): TJclPeMapImgHookItem;
begin
  Result := TJclPeMapImgHookItem(inherited Items[Index]);
end;

function TJclPeMapImgHooks.GetItemFromNewAddress(NewAddress: Pointer): TJclPeMapImgHookItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].NewAddress = NewAddress then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeMapImgHooks.GetItemFromOriginalAddress(BaseAddress, OriginalAddress: Pointer): TJclPeMapImgHookItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (Items[I].BaseAddress = BaseAddress) and (Items[I].OriginalAddress = OriginalAddress) then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeMapImgHooks.HookImport(Base: Pointer; const ModuleName, FunctionName: string;
  NewAddress: Pointer): Boolean;
var
  P: Pointer;
begin
  Result := HookImport(Base, ModuleName, FunctionName, NewAddress, P);
end;

function TJclPeMapImgHooks.HookImport(Base: Pointer; const ModuleName, FunctionName: string;
  NewAddress: Pointer; var OriginalAddress: Pointer): Boolean;
begin
  Result := HookImport(Base, GetModuleHandle(PChar(ModuleName)), ModuleName, FunctionName,
    NewAddress, OriginalAddress);
end;

function TJclPeMapImgHooks.HookImport(Base: Pointer; ModuleHandle: THandle;
  const ModuleName, FunctionName: string; NewAddress: Pointer): Boolean;
var
  P: Pointer;
begin
  Result := HookImport(Base, ModuleHandle, ModuleName, FunctionName, NewAddress, P);
end;

function TJclPeMapImgHooks.HookImport(Base: Pointer; ModuleHandle: THandle;
  const ModuleName, FunctionName: string; NewAddress: Pointer; var OriginalAddress: Pointer): Boolean;
var
  Item: TJclPeMapImgHookItem;
begin
  Result := (ModuleHandle <> 0);
  if not Result then
  begin
    SetLastError(ERROR_MOD_NOT_FOUND);
    Exit;
  end;
  OriginalAddress := GetProcAddress(ModuleHandle, PAnsiChar(AnsiString(FunctionName)));
  Result := (OriginalAddress <> nil);
  if not Result then
  begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;
  Result := {(ItemFromOriginalAddress[Base, OriginalAddress] = nil) and} (NewAddress <> nil) and
    (OriginalAddress <> NewAddress);
  if not Result then
  begin
    SetLastError(ERROR_ALREADY_EXISTS);
    Exit;
  end;
  if Result then
    Result := ReplaceImport(Base, ModuleName, OriginalAddress, NewAddress);
  if Result then
  begin
    Item := TJclPeMapImgHookItem.Create;
    Item.FBaseAddress := Base;
    Item.FFunctionName := FunctionName;
    Item.FModuleName := ModuleName;
    Item.FOriginalAddress := OriginalAddress;
    Item.FNewAddress := NewAddress;
    Item.FList := Self;
    Add(Item);
  end
  else
    SetLastError(ERROR_INVALID_PARAMETER);
end;

{class function TJclPeMapImgHooks.IsWin9xDebugThunk(P: Pointer): Boolean;
begin
  with PWin9xDebugThunk(P)^ do
    Result := (PUSH = $68) and (JMP = $E9);
end;}

function IsWin9xDebugThunk(AnAddr: Pointer): Boolean;
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.PUSH, $68
  JNE @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.JMP, $E9
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@exit
@@NoThunk:
  XOR EAX, EAX
@@exit:
end;

class function TJclPeMapImgHooks.ReplaceImport(Base: Pointer; const ModuleName: string;
  FromProc, ToProc: Pointer): Boolean;
var
  FromProcDebugThunk, ImportThunk: PWin9xDebugThunk;
  IsThunked: Boolean;
  NtHeader: PImageNtHeaders;
  ImportDir: TImageDataDirectory;
  ImportDesc: PImageImportDescriptor;
  CurrName, RefName: PAnsiChar;
  ImportEntry: PImageThunkData;
  FoundProc: Boolean;
  LastProtect, Dummy: Cardinal;
begin
  Result := False;
  FromProcDebugThunk := PWin9xDebugThunk(FromProc);
  IsThunked := not IsWinNT and IsWin9xDebugThunk(FromProcDebugThunk);
  NtHeader := PeMapImgNtHeaders(Base);
  if (NtHeader = nil) or (FromProc = nil) then
    Exit;
  ImportDir := NtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImportDir.VirtualAddress = 0 then
    Exit;
  ImportDesc := PImageImportDescriptor(PAnsiChar(Base) + ImportDir.VirtualAddress);
  RefName := PAnsiChar({$IFDEF UNICODE}UTF8Encode{$ENDIF}(ModuleName));
  while ImportDesc^.Name <> 0 do
  begin
    CurrName := PAnsiChar(Base) + ImportDesc^.Name;
    {$WARNINGS OFF}
    if StrIComp(CurrName, RefName) = 0 then
    {$WARNINGS ON}
    begin
      ImportEntry := PImageThunkData(PAnsiChar(Base) + ImportDesc^.FirstThunk);
      while ImportEntry^.Function_ <> 0 do
      begin
        if IsThunked then
        begin
          ImportThunk := PWin9xDebugThunk(ImportEntry^.Function_);
          FoundProc := IsWin9xDebugThunk(ImportThunk) and (ImportThunk^.Addr = FromProcDebugThunk^.Addr);
        end
        else
          FoundProc := Pointer(ImportEntry^.Function_) = FromProc;
        if FoundProc then
        begin
          if VirtualProtectEx(CurProcess, @ImportEntry^.Function_, SizeOf(ToProc),
            PAGE_READWRITE, @LastProtect) then
          begin
            ImportEntry^.Function_ := Cardinal(ToProc);

            // According to Platform SDK documentation, the last parameter
            // has to be (point to) a valid variable
            VirtualProtectEx(CurProcess, @ImportEntry^.Function_, SizeOf(ToProc),
              LastProtect, Dummy);
            Result := True;
          end;
        end;
        Inc(ImportEntry);
      end;
    end;
    Inc(ImportDesc);
  end;
end;

function TJclPeMapImgHooks.EnumImports(Base: Pointer; EvReplaceImport: TReplaceImportEvent): Boolean;
var
  ImportThunk: PWin9xDebugThunk;
  NtHeader: PImageNtHeaders;
  ImportDir: TImageDataDirectory;
  ImportDesc: PImageImportDescriptor;
  CurrName: PAnsiChar;
  ImportEntry: PImageThunkData;
  FoundProc: Boolean;
  LastProtect, Dummy: Cardinal;
  FromProc, ToProc: Pointer;
  Item: TJclPeMapImgHookItem;
  WinNT: Boolean;
begin
  Result := False;
  NtHeader := PeMapImgNtHeaders(Base);
  if NtHeader = nil then
    Exit;
  ImportDir := NtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImportDir.VirtualAddress = 0 then
    Exit;
  WinNT := IsWinNT;
  ImportDesc := PImageImportDescriptor(PAnsiChar(Base) + ImportDir.VirtualAddress);
  while ImportDesc^.Name <> 0 do
  begin
    CurrName := PAnsiChar(Base) + ImportDesc^.Name;
    ToProc := nil;
    if EvReplaceImport(CurrName, nil, ToProc) then
    begin
      ImportEntry := PImageThunkData(PAnsiChar(Base) + ImportDesc^.FirstThunk);
      while ImportEntry^.Function_ <> 0 do
      begin
        FromProc := Pointer(ImportEntry^.Function_);
        if not WinNT then
        begin
          ImportThunk := PWin9xDebugThunk(FromProc);
          if IsWin9xDebugThunk(ImportThunk) then
            FromProc := ImportThunk^.Addr;
        end;
        ToProc := nil;
        FoundProc := EvReplaceImport(CurrName, FromProc, ToProc);
        if FoundProc and Assigned(ToProc) then
        begin
          if VirtualProtectEx(CurProcess, @ImportEntry^.Function_, SizeOf(ToProc),
            PAGE_READWRITE, @LastProtect) then
          begin
            ImportEntry^.Function_ := Cardinal(ToProc);
            // According to Platform SDK documentation, the last parameter
            // has to be (point to) a valid variable
            VirtualProtectEx(CurProcess, @ImportEntry^.Function_, SizeOf(ToProc),
              LastProtect, Dummy);

            Item := TJclPeMapImgHookItem.Create;
            Item.FBaseAddress := Base;
            //Item.FFunctionName := FunctionName;
            //Item.FModuleName := ModuleName;
            Item.FOriginalAddress := FromProc;
            Item.FNewAddress := ToProc;
            Item.FList := Self;
            Add(Item);
            Result := True;
          end;
        end;
        Inc(ImportEntry);
      end;
    end;
    Inc(ImportDesc);
  end;
end;

class function TJclPeMapImgHooks.GetImportEntryPtr(Base: Pointer; const ModuleName: string; FromProc: Pointer): PPointer;
var
  FromProcDebugThunk, ImportThunk: PWin9xDebugThunk;
  IsThunked: Boolean;
  NtHeader: PImageNtHeaders;
  ImportDir: TImageDataDirectory;
  ImportDesc: PImageImportDescriptor;
  CurrName, RefName: PAnsiChar;
  ImportEntry: PImageThunkData;
  FoundProc: Boolean;
begin
  Result := nil;
  FromProcDebugThunk := PWin9xDebugThunk(FromProc);
  IsThunked := not IsWinNT and IsWin9xDebugThunk(FromProcDebugThunk);
  NtHeader := PeMapImgNtHeaders(Base);
  if NtHeader = nil then
    Exit;
  ImportDir := NtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImportDir.VirtualAddress = 0 then
    Exit;
  ImportDesc := PImageImportDescriptor(PAnsiChar(Base) + ImportDir.VirtualAddress);
  RefName := PAnsiChar({$IFDEF UNICODE}UTF8Encode{$ENDIF}(ModuleName));
  while ImportDesc^.Name <> 0 do
  begin
    CurrName := PAnsiChar(Base) + ImportDesc^.Name;
    {$WARNINGS OFF}
    if StrIComp(CurrName, RefName) = 0 then
    {$WARNINGS ON}
    begin
      ImportEntry := PImageThunkData(PAnsiChar(Base) + ImportDesc^.FirstThunk);
      while ImportEntry^.Function_ <> 0 do
      begin
        if IsThunked then
        begin
          ImportThunk := PWin9xDebugThunk(ImportEntry^.Function_);
          FoundProc := IsWin9xDebugThunk(ImportThunk) and (ImportThunk^.Addr = FromProcDebugThunk^.Addr);
        end
        else
          FoundProc := Pointer(ImportEntry^.Function_) = FromProc;
        if FoundProc then
        begin
          Result := @ImportEntry^.Function_;
          Exit;
        end;
        Inc(ImportEntry);
      end;
    end;
    Inc(ImportDesc);
  end;
end;

function TJclPeMapImgHooks.UnhookByNewAddress(NewAddress: Pointer): Boolean;
var
  Item: TJclPeMapImgHookItem;
begin
  Item := ItemFromNewAddress[NewAddress];
  Result := (Item <> nil) and Item.Unhook;
end;

procedure TJclPeMapImgHooks.UnhookByBaseAddress(BaseAddress: Pointer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].BaseAddress = BaseAddress then
      Items[I].Unhook;
end;

procedure TJclPeMapImgHooks.DiscardUnhookInfo;
var
  I: Integer;
  {$IF (CompilerVersion >= 23.0) or defined(CMD_COMPILER)}  // XE2+ or Simple.Containers
  L: TPointerList;
  {$ELSE}
  L: PPointerList;
  {$IFEND}
begin
  L := List;
  for I := Count downto 1 do
    TJclPeMapImgHookItem(L[I - 1]).FBaseAddress := nil;
    //Items[I - 1].FBaseAddress := nil;
end;

const
  ImageHlpLib = 'imagehlp.dll';
type
  USHORT = Word;

  ImgDelayDescr = packed record
    grAttrs: DWORD;                 // attributes
    szName: DWORD;                  // pointer to dll name
    phmod: PDWORD;                  // address of module handle
    { TODO : probably wrong declaration }
    pIAT: TImageThunkData;          // address of the IAT
    { TODO : probably wrong declaration }
    pINT: TImageThunkData;          // address of the INT
    { TODO : probably wrong declaration }
    pBoundIAT: TImageThunkData;     // address of the optional bound IAT
    { TODO : probably wrong declaration }
    pUnloadIAT: TImageThunkData;    // address of optional copy of original IAT
    dwTimeStamp: DWORD;             // 0 if not bound,
                                    // O.W. date/time stamp of DLL bound to (Old BIND)
  end;
  TImgDelayDescr = ImgDelayDescr;
  PImgDelayDescr = ^ImgDelayDescr;

  PIMAGE_BOUND_IMPORT_DESCRIPTOR = ^IMAGE_BOUND_IMPORT_DESCRIPTOR;
  _IMAGE_BOUND_IMPORT_DESCRIPTOR = record
    TimeDateStamp: DWORD;
    OffsetModuleName: Word;
    NumberOfModuleForwarderRefs: Word;
    // Array of zero or more IMAGE_BOUND_FORWARDER_REF follows
  end;
  IMAGE_BOUND_IMPORT_DESCRIPTOR = _IMAGE_BOUND_IMPORT_DESCRIPTOR;
  TImageBoundImportDescriptor = IMAGE_BOUND_IMPORT_DESCRIPTOR;
  PImageBoundImportDescriptor = PIMAGE_BOUND_IMPORT_DESCRIPTOR;


function ImageDirectoryEntryToData(Base: Pointer; MappedAsImage: ByteBool;
  DirectoryEntry: USHORT; var Size: ULONG): Pointer; stdcall;
  external ImageHlpLib name 'ImageDirectoryEntryToData';

function DirectoryEntryToData(MappedAddress: PAnsiChar; Directory: Word): Pointer;
var
  Size: DWORD;
begin
  Result := ImageDirectoryEntryToData(MappedAddress, True, Directory, Size);
end;

function RvaToVa(MappedAddress: PAnsiChar; Rva: DWORD): Pointer;
begin
  Result := MappedAddress + Rva;
end;

function RvaToVaEx(MappedAddress: PAnsiChar; Rva: DWORD): Pointer;
var
  NtHeaders: PImageNtHeaders;
begin
  NtHeaders := PeMapImgNtHeaders(MappedAddress);
  if NtHeaders <> nil then
  begin
    if (Rva > NtHeaders^.OptionalHeader.SizeOfImage) and (Rva > NtHeaders^.OptionalHeader.ImageBase) then
      Dec(Rva, NtHeaders^.OptionalHeader.ImageBase);
    Result := RvaToVa(MappedAddress, Rva);
  end
  else
    Result := nil;
end;

function CreateImportLibraryList(MappedAddress: PAnsiChar): TStrings;
var
  ImportDesc: PImageImportDescriptor;
  DelayImportDesc: PImgDelayDescr;
  S: string;
  HashTable: TStringHash;
begin
  HashTable := TStringHash.Create;
  Result := TStringList.Create;
  try
    ImportDesc := DirectoryEntryToData(MappedAddress, IMAGE_DIRECTORY_ENTRY_IMPORT);
    if ImportDesc <> nil then
      while ImportDesc^.Name <> 0 do
      begin
        S := {$IFDEF UNICODE}UTF8ToString{$ENDIF}(PAnsiChar(RvaToVa(MappedAddress, ImportDesc^.Name)));
        if (S <> '') then
        begin
          if HashTable.ValueOf(S) = -1 then
          begin
            HashTable.Add(S, 1);
            Result.Add(S);
          end;
        end;
        Inc(ImportDesc);
      end;
    DelayImportDesc := DirectoryEntryToData(MappedAddress, IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT);
    if DelayImportDesc <> nil then
    begin
      while DelayImportDesc^.szName <> 0 do
      begin
        S := {$IFDEF UNICODE}UTF8ToString{$ENDIF}(PAnsiChar(RvaToVaEx(MappedAddress, DelayImportDesc^.szName)));
        if (S <> '') then
        begin
          if HashTable.ValueOf(S) = -1 then
          begin
            HashTable.Add(S, 1);
            Result.Add(S);
          end;
        end;
        Inc(DelayImportDesc);
      end;
    end;
  except
    HashTable.Free;
    Result.Free;
    raise;
  end;
end;

end.

