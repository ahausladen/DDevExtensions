{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit CompilerHooks;

{$I CompileInterceptor.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs, FileStreams, Variants;

var
  OptUseWriteCache: Boolean = True;
  //LogFile: TextFile;

type
  PUtf8Char = PAnsiChar;
  {$IFNDEF UNICODE}
  RawByteString = AnsiString;
  {$ENDIF UNICODE}

procedure InitCompileInterceptor;
procedure FiniCompileInterceptor;

{function StrIsAscii(P: PAnsiChar): Boolean;
function StrAnsiToUtf8(P: PAnsiChar): PUtf8Char;
function StrUtf8ToAnsi(P: PUtf8Char): PAnsiChar;}
function UTF8Encode(US: PWideChar): RawByteString; overload;

function ReadFileContent(const Filename: string): TMemoryStream;

implementation

uses
  InterceptImpl, InterceptIntf, IdeDllNames, ToolsAPIIntf;

type
  TIDENotifier = class(TComponent, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50)
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure BeforeCompile(const Project: IInterface; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IInterface; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
  end;

var
  NotifierId: Integer;
  IDENotifier: TIDENotifier;
  MyHandles: TBucketList;

type
  TOpenFile = class(TObject)
  private
    FOwnsStream: Boolean;
    FFilename: UTF8String;
    FVirtualStream: IVirtualStream;
    FStream: TStream;
    FHandle: THandle;
  public
    constructor Create(const AFilename: UTF8String; AHandle: THandle; AStream: TStream; AOwnsStream: Boolean = True); overload;
    constructor Create(const AFilename: UTF8String; AHandle: THandle; AVirtualStream: IVirtualStream); overload;
    //constructor Create(const AFilename: UTF8String; AHandle: THandle; AVirtualOutStream: IVirtualOutStream); overload;
    destructor Destroy; override;

    property Filename: UTF8String read FFilename;
    property Handle: THandle read FHandle;
    property Stream: TStream read FStream;
    property VirtualStream: IVirtualStream read FVirtualStream;
  end;

  TCachedOrgStream = class(TMemoryStream)
  public
    constructor Create(hFile: THandle; AOrgStreamData: POrgStreamData; ASize: Integer = -1);
  end;

  TCompilerFileInfo = class(TObject) // actually a TCppProjFile but it is user data
  end;

  TMessageLine = class(TObject) // actually a TCppCompilerMessageLine
  end;

  //TMsgKind = (mkHint, mkWarning, mkError, mkFatal, mkInfo);
  TProgKind = (pkUnknown, pkCompiler, pkLinker);

  {PComInOut = ^TComInOut;
  TComInOut = packed record
    Open: function(const Filename: PAnsiChar): THandle; pascal;
    Create: function(const Filename: PAnsiChar): THandle; pascal;
    Close: procedure(hFile: THandle); pascal; // WARNING: CppCompile calls this with handles <= 11 (stdin, stdout, stderr)
    Seek: function(hFile: THandle; Offset, Origin: Integer): Integer; pascal;
    Read: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    Write: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    FileNameDate: function(const Filename: PAnsiChar): Integer; pascal;
    FileStatus: function(hFile: THandle; out FileDate: Integer; out FileSize: Integer): Integer; pascal;

    LinkerSetErrorCount: procedure(Count: Cardinal); pascal;
    LinkerIdle: procedure; pascal;

    GetCurTime: function: Integer; pascal;
    BMessage: procedure(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar); pascal;
    Progress: function(Kind: TProgKind; const Filename: PAnsiChar; CurLines, TotalLines: Integer): Integer; pascal;
    PostDependentFilename: function(const Filename, DepFilename: PAnsiChar; Age: Integer; AFileInfo: TCompilerFileInfo): Boolean; pascal;
    IdeEnableBrowser: procedure; pascal;
    IdeEnableKibitzing: procedure(p1: PAnsiChar); pascal;
    KibizProgress: function: Boolean; pascal;
    MultiLineMessage: function(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar; ParentMessageLine: TMessageLine): TMessageLine; pascal;
    FileFlush: procedure(hFile: THandle); pascal; // C++Builder XE+
  end;}

  PPascalComInOut = ^TPascalComInOut;
  TPascalComInOut = packed record
    Open: function(const Filename: PAnsiChar): THandle; pascal;
    Create: function(const Filename: PAnsiChar): THandle; pascal;
    CreateV: function(const Filename: PAnsiChar): THandle; pascal;
    Close: procedure(hFile: THandle); pascal;
    Flush: procedure(hFile: THandle); pascal;
    Seek: function(hFile: THandle; Offset, Origin: Integer): Integer; pascal;
    Read: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    Write: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    FileNameDate: function(const Filename: PAnsiChar): Integer; pascal;
    FileStatus: function(hFile: THandle; out FileDate: Integer; out FileSize: Integer): Integer; pascal;

    GetCurTime: function: Integer; pascal;
    BMessage: procedure(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar); pascal;
    Progress: function(Kind: TProgKind; const Filename: PAnsiChar; CurLines, TotalLines: Integer): Integer; pascal;

    Empty1: function(p1, p2, p3: Integer): Integer; pascal;
    Empty2: function(p1, p2, p3: Integer): Integer; pascal;
    Empty3: function: Integer; pascal;
    Empty4: function: Integer; pascal;
    Empty5: procedure; pascal;
    Empty6: function(p1: Integer): Integer; pascal;
    Empty7: function(p1: Integer): Integer; pascal;
    Empty8: function(p1: Integer): Integer; pascal;
    Empty9: function(p1: Integer): Integer; pascal;
    Empty10: function(p1: Integer): Integer; pascal;
    Empty11: function(p1: Integer): Integer; pascal;
    Empty12: function(p1: Integer): Integer; pascal;

    MultiLineMessage: function(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar; ParentMessageLine: TMessageLine): TMessageLine; pascal;
  end;

{
  TPathTimestampCallback = procedure(UserData: Pointer; Path: PAnsiChar; Timestamp: Longint); pascal;

  PPascalComInOut160 = ^TPascalComInOut160;
  TPascalComInOut160 = packed record
    Open: function(const Filename: PAnsiChar): THandle; pascal;
    Create: function(const Filename: PAnsiChar): THandle; pascal;
    CreateV: function(const Filename: PAnsiChar): THandle; pascal;
    Close: procedure(hFile: THandle); pascal;
    Flush: procedure(hFile: THandle); pascal;
    Seek: function(hFile: THandle; Offset, Origin: Integer): Integer; pascal;
    Read: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    Write: function(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
    FileNameDate: function(const Filename: PAnsiChar): Integer; pascal;
    FileStatus: function(hFile: THandle; out FileDate: Integer; out FileSize: Integer): Integer; pascal;

    GetCurTime: function: Integer; pascal;
    BMessage: procedure(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar); pascal;
    Progress: function(Kind: TProgKind; const Filename: PAnsiChar; CurLines, TotalLines: Integer): Integer; pascal;

    Empty1: function(p1, p2, p3: Integer): Integer; pascal;
    Empty2: function(p1, p2, p3: Integer): Integer; pascal;
    Empty3: function: Integer; pascal;
    Empty4: function: Integer; pascal;
    Empty5: procedure; pascal;
    Empty6: function(p1: Integer): Integer; pascal;
    Empty7: function(p1: Integer): Integer; pascal;
    Empty8: function(p1: Integer): Integer; pascal;
    Empty9: function(p1: Integer): Integer; pascal;
    Empty10: function(p1: Integer): Integer; pascal;
    Empty11: function(p1: Integer): Integer; pascal;
    Empty12: function(p1: Integer): Integer; pascal;

    MultiLineMessage: function(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar; ParentMessageLine: TMessageLine): TMessageLine; pascal;
    EnumPathTimestamps: procedure(CallCount: Integer; UserData: Pointer; Path: PAnsiChar; Callback: TPathTimestampCallback); pascal;
  end;
}

{procedure CppInitialize(const ComInOut: TComInOut); stdcall;
  external 'bccide.dll' name 'CppInitialize';}

var
//  ComInOut: TComInOut;
//  OrgComInOut: PComInOut;
//  OrgCppStreamData: TOrgStreamData;
//  OrgRcComInOut: PComInOut;
  PascalComInOut: PPascalComInOut;
  OrgPascalComInOut: TPascalComInOut;
  OrgPascalStreamData: TOrgStreamData;

function StrFileExt(P: PAnsiChar): PAnsiChar;
begin
  Result := nil;
  if P <> nil then
    while (P^ <> #0) do
    begin
      if (P^ = '.') then
        Result := P
      else if (P^ = '\') or (P^ = '/') then
        Result := nil;
      Inc(P);
    end;
  if Result = nil then
    Result := P; // points to #0
end;
{
function StrIsAscii(P: PAnsiChar): Boolean;
begin
  if P <> nil then
  begin
    while (P^ <> #0) and (Byte(P^) and $80 = 0) do
      Inc(P);
    Result := P^ = #0;
  end
  else
    Result := False;
end;

function StrAnsiToUtf8(P: PAnsiChar): PUtf8Char;
var
  W: PWideChar;
  AnsiLen, WideLen, Utf8Len: Integer;
begin
  AnsiLen := StrLen(P);
  WideLen := MultiByteToWideChar(CP_ACP, 0, P, AnsiLen, nil, 0);
  GetMem(W, (WideLen + 1) * SizeOf(WideChar));
  W[WideLen] := #0;
  MultiByteToWideChar(CP_ACP, 0, P, AnsiLen, W, WideLen);

  Utf8Len := WideCharToMultiByte(CP_UTF8, 0, W, WideLen, nil, 0, nil, nil);
  GetMem(Result, (Utf8Len + 1) * SizeOf(AnsiChar));
  Result[Utf8Len] := #0;
  WideCharToMultiByte(CP_UTF8, 0, W, WideLen, Result, Utf8Len, nil, nil);
end;

function StrUtf8ToAnsi(P: PUtf8Char): PAnsiChar;
var
  W: PWideChar;
  AnsiLen, WideLen, Utf8Len: Integer;
begin
  Utf8Len := StrLen(P);
  WideLen := MultiByteToWideChar(CP_UTF8, 0, P, Utf8Len, nil, 0);
  GetMem(W, (WideLen + 1) * SizeOf(WideChar));
  W[WideLen] := #0;
  MultiByteToWideChar(CP_UTF8, 0, P, Utf8Len, W, WideLen);

  AnsiLen := WideCharToMultiByte(CP_ACP, 0, W, WideLen, nil, 0, nil, nil);
  GetMem(Result, (AnsiLen + 1) * SizeOf(AnsiChar));
  Result[AnsiLen] := #0;
  WideCharToMultiByte(CP_ACP, 0, W, WideLen, Result, AnsiLen, nil, nil);
end;
}

function UTF8Encode(US: PWideChar): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if US = '' then Exit;
  L := lstrlenW(US);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, US, L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    SetCodePage(Result, CP_UTF8, False);
end;

{ TOpenFile }

constructor TOpenFile.Create(const AFilename: UTF8String; AHandle: THandle;
  AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FFilename := AFilename;
  FHandle := AHandle;
  FOwnsStream := AOwnsStream;
  FStream := AStream;
end;

constructor TOpenFile.Create(const AFilename: UTF8String; AHandle: THandle;
  AVirtualStream: IVirtualStream);
begin
  inherited Create;
  FFilename := AFilename;
  FHandle := AHandle;
  FOwnsStream := False;
  FVirtualStream := AVirtualStream;
end;

destructor TOpenFile.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;

{ TCachedOrgStream }

constructor TCachedOrgStream.Create(hFile: THandle; AOrgStreamData: POrgStreamData; ASize: Integer);
var
  Buf: array[0..4096 - 1] of Byte; // Compiler reads with 4096 bytes and the org-callback returns corrupt data with other values
  BufRead: Integer;
  FileDate: Integer;
begin
  if ASize = -1 then
    AOrgStreamData.FileStatus(hFile, FileDate, ASize);
  if ASize > 0 then
    Capacity := ASize;
  repeat
    BufRead := AOrgStreamData.Read(hFile, @Buf[0], SizeOf(Buf));
    if BufRead > 0 then
      Write(Buf[0], BufRead);
  until BufRead <= 0;
  Position := 0;
end;

function ReadFileContent(const Filename: string): TMemoryStream;
var
  h: THandle;
  S: UTF8String;
begin
  S := UTF8Encode(Filename);

  h := OrgPascalComInOut.Open(PAnsiChar(S));
  if h <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := TCachedOrgStream.Create(h, @OrgPascalStreamData, 0);
    finally
      OrgPascalComInOut.Close(h);
    end;
  end
  else
    Result := nil;
end;

function OpenCompilerFile(const Filename: PAnsiChar; OrgStreamData: POrgStreamData): TOpenFile;
var
  Stream: TStream;
  hFile: THandle;
  VirtualStream: IVirtualStream;
begin
  hFile := INVALID_HANDLE_VALUE;
  Result := TOpenFile(INVALID_HANDLE_VALUE);
  Stream := nil;
  try
    if CompileInterceptorServices.HasInspectFilename then
      CompileInterceptorServices.InspectFilename(Filename, ifmOpen);
    if CompileInterceptorServices.HasVirtualFiles then
      if CompileInterceptorServices.OpenVirtualFile(Filename, VirtualStream) then
        if VirtualStream = nil then
          Exit;
    if VirtualStream = nil then
    begin
      hFile := OrgStreamData.Open(Filename);
      if hFile <> INVALID_HANDLE_VALUE then
      begin
        if CompileInterceptorServices.HasAlterFile then
        begin
          Stream := TCachedOrgStream.Create(hFile, OrgStreamData, 0);
          VirtualStream := CompileInterceptorServices.AlterFile(Filename, TCachedOrgStream(Stream).Memory, 0, Stream.Size);
          if VirtualStream <> nil then
            FreeAndNil(Stream);
        end
        else
        begin
          {if StrIComp(StrFileExt(Filename), '.inc') = 0 then
            Stream := TCachedOrgStream.Create(hFile, OrgStreamData)
          else}
            Stream := TOrgStream.Create(hFile, OrgStreamData);
        end;
      end
      else
      begin
        Result := TOpenFile(INVALID_HANDLE_VALUE);
        Exit;
      end;
    end
    else
      hFile := INVALID_HANDLE_VALUE;

    if VirtualStream <> nil then
      Result := TOpenFile.Create(Filename, hFile, VirtualStream)
    else
      Result := TOpenFile.Create(Filename, hFile, Stream, True);
  except
    on E: Exception do
    begin
      OrgPascalComInOut.BMessage(mkWarning, 0, Filename, 0, 0, 0, PAnsiChar(UTF8Encode('CompilerInterceptor: ' + E.Message)));

      if (Result <> nil) and (Result <> TOpenFile(INVALID_HANDLE_VALUE)) then
        Result.Free
      else
        Stream.Free;

      if hFile <> INVALID_HANDLE_VALUE then
        Result := TOpenFile.Create(Filename, hFile, TOrgStream.Create(hFile, OrgStreamData), True)
      else
        Result := TOpenFile(INVALID_HANDLE_VALUE);
    end;
  end;
end;

(*
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{ C++ Compiler Callbacks                                                       }
{------------------------------------------------------------------------------}
function Compiler_Open(const Filename: PAnsiChar): THandle; pascal;
begin
  Result := THandle(OpenCompilerFile(Filename, @OrgCppStreamData));
end;

function Compiler_Create(const Filename: PAnsiChar): THandle; pascal;

  procedure FileCreateError(Filename: PAnsiChar);
  begin
    { Show the user what file could not be created. }
//    MessagePane.CompilerMessage(CompileInfo, mkError, -1, -1, 1013,
//      CompileInfo.CurrentSourceFilename, 'F1013 ' + Format(RsCannotCreateFile, [string(Filename)]));
  end;

var
  hFile: THandle;
begin
  hFile := THandle(OrgComInOut.Create(Filename));
  if (hFile = 0) or (hFile = INVALID_HANDLE_VALUE) then
  begin
    FileCreateError(Filename);
    Result := hFile;
  end
  else
  begin
    if OptUseWriteCache then
      Result := THandle(TOpenFile.Create(Filename, hFile, TBufferedWriteStream.Create(hFile, @OrgCppStreamData), True))
    else
      Result := THandle(TOpenFile.Create(Filename, hFile, TOrgStream.Create(hFile, @OrgCppStreamData), True));
  end;
end;

procedure Compiler_Close(hFile: THandle); pascal;
var
  h: THandle;
begin
  if hFile > 11 then // do not close standard handles
  begin
    h := TOpenFile(hFile).Handle;
    TOpenFile(hFile).Free;
    if h <> INVALID_HANDLE_VALUE then
      OrgComInOut.Close(h);
  end;
end;

function Compiler_Seek(hFile: THandle; Offset, Origin: Integer): Integer; pascal;
begin
  Result := TOpenFile(hFile).Stream.Seek(Offset, Origin);
end;

function Compiler_Read(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
begin
  Result := TOpenFile(hFile).Stream.Read(Buffer^, Size);
end;

function Compiler_Write(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
begin
  Result := TOpenFile(hFile).Stream.Write(Buffer^, Size);
end;

function Compiler_FileNameDate(const Filename: PAnsiChar): Integer; pascal;
begin
  Result := OrgComInOut.FileNameDate(Filename);
end;

function Compiler_FileStatus(hFile: THandle; out FileDate: Integer; out FileSize: Integer): Integer; pascal;
var
  Size: Integer;
begin
  if TOpenFile(hFile).Handle <> INVALID_HANDLE_VALUE then
    Result := OrgComInOut.FileStatus(TOpenFile(hFile).Handle, FileDate, FileSize)
  else
  begin
    FileDate := Compiler_FileNameDate(PAnsiChar(TOpenFile(hFile).Filename));
    Result := 0;
  end;
  if TOpenFile(hFile).Stream.ClassType <> TOrgStream then
  begin
    Size := TOpenFile(hFile).Stream.Size;
    if Size > 1 then Result := 0 else Result := -1;
    FileSize := Size;
  end;
end;

procedure Compiler_BMessage(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar); pascal;
begin
  OrgComInOut.BMessage(MsgKind, Code, Filename, Line, StartColumn, EndColumn, Msg);
end;

function Compiler_MultiLineMessage(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar; Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar; ParentMessageLine: TMessageLine): TMessageLine; pascal;
begin
  Result := OrgComInOut.CompilerMessage(MsgKind, Code, Filename, Line, StartColumn, EndColumn, Msg, ParentMessageLine);
end;

function Compiler_PostDependentFilename(const Filename, DepFilename: PAnsiChar; Age: Integer; AFileInfo: TCompilerFileInfo): Boolean; pascal;
begin
  Result := OrgComInOut.PostDependentFilename(Filename, DepFilename, Age, AFileInfo);
end;

procedure Compiler_FileFlush(hFile: THandle); pascal;
var
  h: THandle;
begin
  if hFile > 11 then // do not close standard handles
  begin
    h := TOpenFile(hFile).Handle;
    if h <> INVALID_HANDLE_VALUE then
      OrgComInOut.FileFlush(h);
  end;
end;
*)
{------------------------------------------------------------------------------}
{ Pascal Compiler Callbacks                                                    }
{------------------------------------------------------------------------------}

function OpenPascalFile(const Filename: PAnsiChar): THandle; pascal;
begin
  Result := THandle(OpenCompilerFile(Filename, @OrgPascalStreamData));
  if Result <> INVALID_HANDLE_VALUE then
    MyHandles.Add(Pointer(Result), Pointer(1));
  //WriteLn(LogFile, 'OpenFile(', Filename, ') = ', IntToHex(Result, 8));
end;

function CreatePascalFile(const Filename: PAnsiChar): THandle; pascal;
var
  hFile: THandle;
  VirtualStream: IVirtualOutStream;
begin
  Result := INVALID_HANDLE_VALUE;
  { WriteCache is not possible because Compiler calls SeekPascalFile() }
  if CompileInterceptorServices.HasInspectFilename then
    CompileInterceptorServices.InspectFilename(Filename, ifmCreate);
  if CompileInterceptorServices.HasVirtualOutFiles then
    if CompileInterceptorServices.CreateVirtualFile(Filename, {out} VirtualStream) then
      if VirtualStream = nil then
        Exit;
  if VirtualStream <> nil then
  begin
    Result := THandle(TOpenFile.Create(Filename, INVALID_HANDLE_VALUE, VirtualStream));
    MyHandles.Add(Pointer(Result), Pointer(1));
  end
  else
  begin
    hFile := OrgPascalComInOut.Create(Filename);
    if hFile <> INVALID_HANDLE_VALUE then
    begin
      Result := THandle(TOpenFile.Create(Filename, hFile, TOrgStream.Create(hFile, @OrgPascalStreamData), True));
      MyHandles.Add(Pointer(Result), Pointer(1));
    end;
  end;
  //WriteLn(LogFile, 'CreateFile(', Filename, ') = ', IntToHex(Result, 8));
end;

function CreateVPascalFile(const Filename: PAnsiChar): THandle; pascal;
var
  hFile: THandle;
  VirtualStream: IVirtualOutStream;
begin
  Result := INVALID_HANDLE_VALUE;
  { WriteCache is not possible because Compiler calls SeekPascalFile() }
  if CompileInterceptorServices.HasInspectFilename then
    CompileInterceptorServices.InspectFilename(Filename, ifmCreate);
  if CompileInterceptorServices.HasVirtualOutFiles then
    if CompileInterceptorServices.CreateVirtualFile(Filename, {out} VirtualStream) then
      if VirtualStream = nil then
        Exit;
  if VirtualStream <> nil then
  begin
    Result := THandle(TOpenFile.Create(Filename, INVALID_HANDLE_VALUE, VirtualStream));
    MyHandles.Add(Pointer(Result), Pointer(1));
  end
  else
  begin
    hFile := OrgPascalComInOut.CreateV(Filename);
    if hFile <> INVALID_HANDLE_VALUE then
    begin
      Result := THandle(TOpenFile.Create(Filename, hFile, TOrgStream.Create(hFile, @OrgPascalStreamData), True));
      MyHandles.Add(Pointer(Result), Pointer(1));
    end;
  end;
  //WriteLn(LogFile, 'CreateFileV(', Filename, ') = ', IntToHex(Result, 8));
end;

procedure ClosePascalFile(hFile: THandle); pascal;
var
  h: THandle;
begin
  if (hFile > 11) and (hFile <> INVALID_HANDLE_VALUE) then // do not close standard handles
  begin
    if MyHandles.Remove(Pointer(hFile)) <> nil then
    begin
      h := TOpenFile(hFile).Handle;
      //WriteLn(LogFile, 'CloseFile(', TOpenFile(hFile).Filename, ')');
      TOpenFile(hFile).Free;
    end
    else
      h := hFile;
    if h <> INVALID_HANDLE_VALUE then
      OrgPascalComInOut.Close(h);
  end;
end;

procedure FlushPascalFile(hFile: THandle); pascal;
begin
  if (hFile > 11) and (hFile <> INVALID_HANDLE_VALUE) and MyHandles.Exists(Pointer(hFile)) then
  begin
    if TOpenFile(hFile).Handle <> INVALID_HANDLE_VALUE then
      OrgPascalComInOut.Flush(TOpenFile(hFile).Handle);
  end
  else
    OrgPascalComInOut.Flush(hFile);
end;

function SeekPascalFile(hFile: THandle; Offset, Origin: Integer): Integer; pascal;
begin
  //WriteLn(LogFile, 'SeekFile(', TOpenFile(hFile).Filename, ', ', Offset, ', ', Origin, ')');
  if (hFile <> INVALID_HANDLE_VALUE) and MyHandles.Exists(Pointer(hFile)) then
  begin
    if TOpenFile(hFile).Stream <> nil then
      Result := TOpenFile(hFile).Stream.Seek(Offset, Origin)
    else
      Result := TOpenFile(hFile).VirtualStream.Seek(Offset, Origin);
  end
  else
    Result := OrgPascalComInOut.Seek(hFile, Offset, Origin);
end;

function ReadPascalFile(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
begin
  //WriteLn(LogFile, 'ReadFile(', TOpenFile(hFile).Filename, ', ', Size, ')');
  if (hFile <> INVALID_HANDLE_VALUE) and MyHandles.Exists(Pointer(hFile)) then
  begin
    if TOpenFile(hFile).Stream <> nil then
      Result := TOpenFile(hFile).Stream.Read(Buffer^, Size)
    else
      Result := TOpenFile(hFile).VirtualStream.Read(Buffer^, Size);
  end
  else
    Result := OrgPascalComInOut.Read(hFile, Buffer, Size);
end;

function WritePascalFile(hFile: THandle; Buffer: PByte; Size: Cardinal): Integer; pascal;
begin
  //WriteLn(LogFile, 'WriteFile(', TOpenFile(hFile).Filename, ', ', Size, ')');
  if (hFile <> INVALID_HANDLE_VALUE) and MyHandles.Exists(Pointer(hFile)) then
  begin
    if TOpenFile(hFile).Stream <> nil then
      Result := TOpenFile(hFile).Stream.Write(Buffer^, Size)
    else
      Result := IVirtualOutStream(TOpenFile(hFile).VirtualStream).Write(Buffer^, Size);
  end
  else
    Result := OrgPascalComInOut.Write(hFile, Buffer, Size);
end;

function FileStatusPascalFile(hFile: THandle; out FileDate: Integer; out FileSize: Integer): Integer; pascal;
begin
  if (hFile <> INVALID_HANDLE_VALUE) and MyHandles.Exists(Pointer(hFile)) then
  begin
    if TOpenFile(hFile).Stream <> nil then
      Result := OrgPascalComInOut.FileStatus(TOpenFile(hFile).Handle, FileDate, FileSize)
    else
    begin
      TOpenFile(hFile).VirtualStream.FileStatus(FileDate, FileSize);
      if FileDate = 0 then
        OrgPascalComInOut.FileStatus(TOpenFile(hFile).Handle, FileDate, Result);
      if FileSize > 1 then Result := 0 else Result := -1;
    end;
  end
  else
    Result := OrgPascalComInOut.FileStatus(hFile, FileDate, FileSize);
  //WriteLn(LogFile, 'FileStatus(', TOpenFile(hFile).Filename, ', ', FileDate, ', ', FileSize, ')');
end;

function FileNameDate(const Filename: PAnsiChar): Integer; pascal;
begin
  if CompileInterceptorServices.HasFileNameDate then
    if CompileInterceptorServices.FileNameDate(FileName, {out} Result) then
      Exit;
  Result := OrgPascalComInOut.FileNameDate(FileName);
end;

procedure BMessagePascal(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar;
  Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar); pascal;

  procedure AlterMessage;
  var
    FilenameS, MsgS: UTF8String;
  begin
    FilenameS := Filename;
    MsgS := Msg;
    if CompileInterceptorServices.AlterMessage(False, MsgKind, Code, FilenameS, Line, StartColumn, MsgS) then
    begin
      if MsgS <> '' then
        OrgPascalComInOut.BMessage(MsgKind, Code, PAnsiChar(FilenameS), Line, StartColumn, EndColumn, PAnsiChar(MsgS));
    end
    else
      OrgPascalComInOut.BMessage(MsgKind, Code, Filename, Line, StartColumn, EndColumn, Msg);
  end;

begin
  if CompileInterceptorServices.HasAlterMessage then
    AlterMessage
  else
    OrgPascalComInOut.BMessage(MsgKind, Code, Filename, Line, StartColumn, EndColumn, Msg);
end;

function MultiLineMessagePascal(MsgKind: TMsgKind; Code: Integer; const Filename: PAnsiChar;
  Line, StartColumn, EndColumn: Integer; const Msg: PAnsiChar; ParentMessageLine: TMessageLine): TMessageLine; pascal;

  procedure AlterMessage;
  var
    FilenameS, MsgS: UTF8String;
  begin
    FilenameS := Filename;
    MsgS := Msg;
    if CompileInterceptorServices.AlterMessage(True, MsgKind, Code, FilenameS, Line, StartColumn, MsgS) then
    begin
      if MsgS <> '' then
        Result := OrgPascalComInOut.MultiLineMessage(MsgKind, Code, PAnsiChar(FilenameS), Line, StartColumn, EndColumn, PAnsiChar(MsgS), ParentMessageLine)
      else
        Result := nil;
    end
    else
      Result := OrgPascalComInOut.MultiLineMessage(MsgKind, Code, Filename, Line, StartColumn, EndColumn, Msg, ParentMessageLine);
  end;

begin
  if CompileInterceptorServices.HasAlterMessage then
    AlterMessage
  else
    Result := OrgPascalComInOut.MultiLineMessage(MsgKind, Code, Filename, Line, StartColumn, EndColumn, Msg, ParentMessageLine);
end;

{------------------------------------------------------------------------------}

procedure SetPascalComInOut;
begin
  PascalComInOut.Open := OpenPascalFile;
  PascalComInOut.Read := ReadPascalFile;
  PascalComInOut.CreateV := CreateVPascalFile;
  PascalComInOut.Create := CreatePascalFile;
  PascalComInOut.Flush := FlushPascalFile;
  PascalComInOut.Write := WritePascalFile;
  PascalComInOut.Seek := SeekPascalFile;
  PascalComInOut.FileStatus := FileStatusPascalFile;
  PascalComInOut.Close := ClosePascalFile;
  PascalComInOut.FileNameDate := FileNameDate;

  PascalComInOut.BMessage := BMessagePascal;
  PascalComInOut.MultiLineMessage := MultiLineMessagePascal;
end;

procedure UpdateOrgStreamFunctions;
begin
//  OrgCppStreamData.Open := OrgComInOut.Open;
//  OrgCppStreamData.Seek := OrgComInOut.Seek;
//  OrgCppStreamData.Read := OrgComInOut.Read;
//  OrgCppStreamData.Write := OrgComInOut.Write;
//  OrgCppStreamData.FileStatus := OrgCppStreamData.FileStatus;

  OrgPascalStreamData.Open := OrgPascalComInOut.Open;
  OrgPascalStreamData.Seek := OrgPascalComInOut.Seek;
  OrgPascalStreamData.Read := OrgPascalComInOut.Read;
  OrgPascalStreamData.Write := OrgPascalComInOut.Write;
  OrgPascalStreamData.FileStatus := OrgPascalComInOut.FileStatus;
end;

procedure InitCompileInterceptor;
const
  // = 9
//  CPPCompilerInitSymbol9 = '@Pascppcominout@CPPCompilerInit$qqrpqqsr28Pascppcominout@TCppCallbacks$v';
  PascalWin32CompilerInitSymbol9 = '@Pascppcominout@PascalCompilerInit$qqrpqqsr25Pascppcominout@TCallbacks$v';

  // >= 10
//  CPPCompilerInitSymbol10 = '@Pascppcominout@CPPCompilerInit$qqrpqqsr28Pascppcominout@TCppCallbacks$v';
  PascalWin32CompilerInitSymbol10 = '@Pascppcominout@PascalWin32CompilerInit$qqrpqqsr25Pascppcominout@TCallbacks$v';
  AnsiCompilerInitSymbol10 = '@Pascppcominout@AnsiCompilerInit$qqrpqqsr28Pascppcominout@TCppCallbacks$v'; {RcCompiler}

  // >= 15
//  sGetCppCallbacks = '@Pascppcominout@GetCppCallbacks$qqrv';
  sGetDccCallbacks = '@Pascppcominout@GetDccCallbacks$qqrv';

  // in [5, 6, 7]
//  CPPCompilerInitSymbol5 = '@Cominout@CPPCompilerInit$qqrpqqsr22Cominout@TCppCallbacks$v';
  PascalWin32CompilerInitSymbol5 = '@Cominout@PascalCompilerInit$qqrpqqsr19Cominout@TCallbacks$v';

var
  PascalWin32CompilerInitSymbol: PAnsiChar;
//  CPPCompilerInitSymbol: PAnsiChar;
//  AnsiCompilerInitSymbol: PAnsiChar;

//  GetCppCallbacks: function: PComInOut;
  GetDccCallbacks: function: PPascalComInOut;


//  Proc: ^PComInOut;
  PascalProc: ^PPascalComInOut;
  Services: IOTAServices50;
begin
  if Assigned(CompileInterceptorServices) then
    Exit;

  CompileInterceptorServices := TCompileInterceptorServices.Create(nil);

  if DelphiVer < 15 then
  begin
    if DelphiVer = 9 then
    begin
  //    CPPCompilerInitSymbol := CPPCompilerInitSymbol9;
      PascalWin32CompilerInitSymbol := PascalWin32CompilerInitSymbol9;
  //    AnsiCompilerInitSymbol := CPPCompilerInitSymbol9;
    end
    else
    if DelphiVer >= 10 then
    begin
  //    CPPCompilerInitSymbol := CPPCompilerInitSymbol10;
      PascalWin32CompilerInitSymbol := PascalWin32CompilerInitSymbol10;
  //    AnsiCompilerInitSymbol := AnsiCompilerInitSymbol10;
    end
    else
    begin
  //    CPPCompilerInitSymbol := CPPCompilerInitSymbol5;
      PascalWin32CompilerInitSymbol := PascalWin32CompilerInitSymbol5;
  //    AnsiCompilerInitSymbol := CPPCompilerInitSymbol5;
    end;

  //  Proc := GetProcAddress(GetModuleHandle(delphicoreide_bpl), CPPCompilerInitSymbol);
  //  if Proc <> nil then
  //  begin
  //    Proc := Pointer(Cardinal(Proc) + 1);
  //    OrgComInOut := Proc^;
  //    ComInOut := OrgComInOut^;
  //
  //{    ComInOut.Open := Compiler_Open;
  //    ComInOut.Create := Compiler_Create;
  //    ComInOut.Close := Compiler_Close;
  //    ComInOut.Seek := Compiler_Seek;
  //    ComInOut.Read := Compiler_Read;
  //    ComInOut.Write := Compiler_Write;
  //    ComInOut.FileNameDate := Compiler_FileNameDate;
  //    ComInOut.FileStatus := Compiler_FileStatus;
  //
  //    ComInOut.PostDependentFilename := Compiler_PostDependentFilename;
  //    ComInOut.BMessage := Compiler_BMessage;
  //    ComInOut.CompilerMessage := Compiler_MultiLineMessage;
  //    if DelphiVer >= 15 then
  //      ComInOut.FileFlush := Compiler_FileFlush;
  //}
  //
  //    //CppInitialize(ComInOut);
  //  end;


  //  if DelphiVer >= 10 then
  //  begin
  //    Proc := GetProcAddress(GetModuleHandle(delphicoreide_bpl), AnsiCompilerInitSymbol);
  //    if Proc <> nil then
  //    begin
  //      Proc := Pointer(Cardinal(Proc) + 1);
  //      OrgRcComInOut := Proc^;
  //    end;
  //  end
  //  else
  //    OrgRcComInOut := OrgComInOut;

    PascalProc := GetProcAddress(GetModuleHandle(delphicoreide_bpl), PascalWin32CompilerInitSymbol);
    if PascalProc <> nil then
    begin
      if DelphiVer >= 10 then
        PascalProc := Pointer(Cardinal(PascalProc) + 10)
      else
        PascalProc := Pointer(Cardinal(PascalProc) + 1);
      PascalComInOut := PascalProc^;
      OrgPascalComInOut := PascalComInOut^;

      SetPascalComInOut;
    end;

  end
  else
  begin
    // Delphi XE+
    GetDccCallbacks := GetProcAddress(GetModuleHandle(delphicoreide_bpl), sGetDccCallbacks);
    if Assigned(GetDccCallbacks) then
    begin
      PascalComInOut := GetDccCallbacks();
      OrgPascalComInOut := PascalComInOut^;

      SetPascalComInOut;
    end;
  end;

  UpdateOrgStreamFunctions;
  //AssignFile(LogFile, 'C:\Debug.txt');
  //Rewrite(LogFile);

  IDENotifier := TIDENotifier.Create(nil);
  if SupportsIDEServices(Services) then
    NotifierId := Services.AddNotifier(IDENotifier);
end;

procedure FiniCompileInterceptor;
var
  Services: IOTAServices50;
begin
  if not Assigned(CompileInterceptorServices) then
    Exit;
  //CloseFile(LogFile);
  CompileInterceptorServices.Free;
  CompileInterceptorServices := nil;
//  CppInitialize(OrgComInOut);

  if (NotifierId <> 0) and SupportsIDEServices(Services) then
    Services.RemoveNotifier(NotifierId);
  IDENotifier.Free;
end;

{ TIDENotifier }

procedure TIDENotifier.AfterSave;
begin
end;

procedure TIDENotifier.BeforeSave;
begin
end;

procedure TIDENotifier.Modified;
begin
end;

procedure TIDENotifier.Destroyed;
begin
end;

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
end;

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIDENotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
end;

procedure TIDENotifier.BeforeCompile(const Project: IInterface; var Cancel: Boolean);
begin
end;

function ReplaceMacros(const S: string; IgnoreList: TStrings = nil): string;
var
  I, K: Integer;
  Len: Integer;
  MacroName, MacroValue: string;
  MyIgnoreList: Boolean;
begin
  Result := S;
  Len := Length(S);
  I := Pos('$(', S);
  if I = 0 then
    Exit; // nothing to do

  MyIgnoreList := False;
  try
    while I <= Len do
    begin
      if (I < Len) and (Result[I] = '$') and (Result[I + 1] = '(') then
      begin
        K := I + 2;
        while (K <= Len) and (Result[K] <> ')') do
          Inc(K);
        MacroName := Copy(Result, I + 2, K - I - 2);
        if (IgnoreList = nil) or (IgnoreList.IndexOf(MacroName) = -1) then
        begin
          Delete(Result, I, K - I + 1);
          if MacroName <> '' then
          begin
            if IgnoreList = nil then
            begin
              IgnoreList := TStringList.Create;
              MyIgnoreList := True;
            end;
            K := IgnoreList.Add(MacroName);
            try
              { The IDE puts all macros to the Environment Variables list }
              MacroValue := ReplaceMacros(GetEnvironmentVariable(MacroName), IgnoreList);
            finally
              IgnoreList.Delete(K);
            end;
            Insert(MacroValue, Result, I);
            Inc(I, Length(MacroValue));
          end;
          Len := Length(Result);
          Dec(I);
        end;
      end;
      Inc(I);
    end;
  finally
    if MyIgnoreList then
      IgnoreList.Free;
  end;
end;

function GetGlobalLibraryPath: string;
var
  Services: IOTAServices50;
begin
  if SupportsIDEServices(Services) then
    { VarToStr makes the string unique in our memory manager }
    Result := VarToStr(Services.GetEnvironmentOptions.Values['LibraryPath'])
  else
    Result := '';
end;

procedure TIDENotifier.BeforeCompile(const Project: IInterface; IsCodeInsight: Boolean;
  var Cancel: Boolean);
var
{  Names: TOTAOptionNameArray;
  I: Integer;
  f: TextFile;}
  Options: IOTAOptions;
  ProjectFilename: string;
  SourcePaths, UnitPaths, DcuOutputDir: string;
  Prj: IOTAProject;
begin
  try
    ProjectFilename := '';
    if Project <> nil then
    begin
      { Project Options }
      Options := nil;
      case DelphiVer of
        1..4: raise Exception.Create('Delphi version is not supported');
        5: Options := IOTAProject_50(Project).ProjectOptions;
        6: Options := IOTAProject_60(Project).ProjectOptions;
        7: Options := IOTAProject_70(Project).ProjectOptions;
        9..14:
          begin
            if Supports(Project, IOTAProject90_140, Prj) then
            begin
              if (IOTAProject90_140(Prj).Personality = sDelphiPersonality) or
                 (IOTAProject90_140(Prj).Personality = sDelphiDotNetPersonality) then
                Options := IOTAProject90_140(Prj).ProjectOptions
              else
                Exit; // nothing to do
            end;
          end;
      else
        if Supports(Project, IOTAProject, Prj) then
        begin
          if (Prj.Personality = sDelphiPersonality) or
             (Prj.Personality = sDelphiDotNetPersonality) then
            Options := Prj.ProjectOptions
          else
            Exit; // nothing to do
        end;
      end;
      if Options <> nil then
      begin
        { VarToStr makes the string unique in our memory manager }
        DcuOutputDir := ReplaceMacros(VarToStr(Options.Values['UnitOutputDir']));
        UnitPaths := VarToStr(Options.Values['UnitDir']);
        Options := nil;
      end;
      ProjectFilename := IOTAModule40(Project).FileName;
    end;

    { Environment Options }

    UnitPaths := UnitPaths + ';' + GetGlobalLibraryPath();
    if UnitPaths[1] = ';' then
      Delete(UnitPaths, 1, 1)
    else if UnitPaths[Length(UnitPaths)] = ';' then
      Delete(UnitPaths, Length(UnitPaths) - 1, 1);
    UnitPaths := ReplaceMacros(UnitPaths);

    SourcePaths := UnitPaths;
    CompileInterceptorServices.CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir,
      IsCodeInsight, Cancel);


  {  Names := Options.GetOptionNames;
    AssignFile(f, 'C:\EnvNames.txt');
    Rewrite(f);
    for I := 0 to High(Names) do
      WriteLn(f, Names[I].Name, ' = ', VarToStr(Options.Values[Names[I].Name]));
    CloseFile(f);}
  except
    on E: Exception do
    begin
      if Assigned(OrgPascalComInOut.BMessage) then
        OrgPascalComInOut.BMessage(mkWarning, 0, '', 0, 0, 0, PAnsiChar(UTF8Encode('CompilerInterceptor: ' + E.Message)));
    end;
  end;
end;

initialization
  MyHandles := TBucketList.Create(bl64);

finalization
  FiniCompileInterceptor;
  FreeAndNil(MyHandles);

end.
