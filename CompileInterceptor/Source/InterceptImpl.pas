{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit InterceptImpl;

{$I CompileInterceptor.inc}

interface

uses
  SysUtils, Classes, Contnrs, FileStreams, InterceptIntf;

type
  TCompileInterceptorServices = class(TComponent, ICompileInterceptorServices)
  private
    FItems: TInterfaceList;
    FIds: TList;
    FOptions: TList;
    FHasAlterFile: Boolean;
    FHasVirtualFiles: Boolean;
    FHasInspectFilename: Boolean;
    FHasAlterMessage: Boolean;
    FHasCompileProjects: Boolean;
    FHasVirtualOutFiles: Boolean;
    FHasFileNameDates: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): ICompileInterceptor;
    function GetOption(Index: Integer): TCompileInterceptOptions;
    procedure UpdateOptionCache;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { ICompileInterceptorServices }
    function RegisterInterceptor(Interceptor: ICompileInterceptor): Integer; stdcall;
    procedure UnregisterInterceptor(Id: Integer); stdcall;
    function GetFileContent(Filename: PWideChar): IVirtualStream; stdcall;
  public
    function OpenVirtualFile(Filename: PAnsiChar; out Stream: IVirtualStream): Boolean;
    function CreateVirtualFile(Filename: PAnsiChar; out Stream: IVirtualOutStream): Boolean;
    function AlterFile(Filename: PAnsiChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream;
    procedure InspectFilename(Filename: PAnsiChar; FileMode: TInspectFileMode);
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind;
      var Code: Integer; var Filename: UTF8String; var Line, Column: Integer; var Msg: UTF8String): Boolean;
    procedure CompileProject(const ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: string;
      IsCodeInsight: Boolean; var Cancel: Boolean);
    function FileNameDate(Filename: PAnsiChar; out FileDate: Integer): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: ICompileInterceptor read GetItem; default;
    property Options[Index: Integer]: TCompileInterceptOptions read GetOption;

    property HasAlterFile: Boolean read FHasAlterFile;
    property HasVirtualFiles: Boolean read FHasVirtualFiles;
    property HasVirtualOutFiles: Boolean read FHasVirtualOutFiles;
    property HasInspectFilename: Boolean read FHasInspectFilename;
    property HasAlterMessage: Boolean read FHasAlterMessage;
    property HasCompileProjects: Boolean read FHasCompileProjects;
    property HasFileNameDate: Boolean read FHasFileNameDates;
  end;

  TVirtualStreamAdapter = class(TStream)
  private
    FVirtualStream: IVirtualStream;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF COMPILER6_UP}
  public
    constructor Create(AVirtualStream: IVirtualStream);

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    {$IFDEF COMPILER6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF COMPILER6_UP}
  end;

  TVirtualStream = class(TInterfacedObject, IVirtualStream)
  private
    FAutoDelete: Boolean;
    FStream: TStream;
    FFileDate: Integer;
  public
    constructor Create(AStream: TStream; AFileDate: Integer; AAutoDelete: Boolean = True);
    destructor Destroy; override;

    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

var
  CompileInterceptorServices: TCompileInterceptorServices;

implementation

uses
  CompilerHooks, IdeDllNames;

function GetCompileInterceptorServices: ICompileInterceptorServices; stdcall;
begin
  if not Assigned(CompileInterceptorServices) then
    InitCompileInterceptor;
  Result := CompileInterceptorServices;
end;

exports
  GetCompileInterceptorServices;


{ TVirtualStreamAdapter }

constructor TVirtualStreamAdapter.Create(AVirtualStream: IVirtualStream);
begin
  inherited Create;
  FVirtualStream := AVirtualStream;
end;

procedure TVirtualStreamAdapter.SetSize(NewSize: Longint);
begin
  {$IFDEF COMPILER6_UP}
  SetSize(Int64(NewSize));
  {$ELSE}
  Seek(NewSize, soFromBeginning);
  {$ENDIF COMPILER6_UP}
end;

{$IFDEF COMPILER6_UP}
procedure TVirtualStreamAdapter.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
end;
{$ENDIF COMPILER6_UP}

function TVirtualStreamAdapter.Read(var Buffer; Count: Integer): Integer;
begin
  Result := FVirtualStream.Read(Buffer, Count);
end;

function TVirtualStreamAdapter.Write(const Buffer; Count: Integer): Integer;
begin
//  Result := FVirtualStream.Write(Buffer, Count);
  Result := -1;
end;

{$IFDEF COMPILER6_UP}
function TVirtualStreamAdapter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TVirtualStreamAdapter.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF COMPILER6_UP}
begin
  Result := FVirtualStream.Seek(Integer(Offset), Integer(Origin));
end;


{ TVirtualStream }

constructor TVirtualStream.Create(AStream: TStream; AFileDate: Integer; AAutoDelete: Boolean);
begin
  inherited Create;
  FAutoDelete := AAutoDelete;
  FStream := AStream;
  FFileDate := AFileDate;
end;

destructor TVirtualStream.Destroy;
begin
  if FAutoDelete then
    FStream.Free;
  inherited Destroy;
end;

procedure TVirtualStream.FileStatus(out FileDate, FileSize: Integer);
begin
  FileSize := FStream.Size;
  FileDate := FFileDate;
end;

function TVirtualStream.Read(var Buffer; Size: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Size);
end;

function TVirtualStream.Seek(Offset, Origin: Integer): Integer;
begin
  Result := FStream.Seek(Offset, Origin);
end;


{ TCompileInterceptorServices }

var
  TCompileInterceptorServices_LastId: Integer = 0;

constructor TCompileInterceptorServices.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TInterfaceList.Create;
  FIds := TList.Create;
  FOptions := TList.Create;
end;

destructor TCompileInterceptorServices.Destroy;
begin
  FOptions.Free;
  FItems.Free;
  FIds.Free;
  inherited Destroy;
end;

function TCompileInterceptorServices.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCompileInterceptorServices.GetFileContent(Filename: PWideChar): IVirtualStream;
var
  Stream: TStream;
begin
  Stream := ReadFileContent(Filename);
  if Stream <> nil then
    Result := TVirtualStream.Create(Stream, 0);
end;

function TCompileInterceptorServices.GetItem(Index: Integer): ICompileInterceptor;
begin
  Result := ICompileInterceptor(FItems[Index]);
end;

function TCompileInterceptorServices.GetOption(Index: Integer): TCompileInterceptOptions;
begin
  Result := TCompileInterceptOptions(FOptions[Index]);
end;

function TCompileInterceptorServices.OpenVirtualFile(Filename: PAnsiChar; out Stream: IVirtualStream): Boolean;
var
  I: Integer;
  UniFilename: string;
  Intf: ICompileInterceptor20;
begin
  UniFilename := UTF8ToString(Filename);
  Result := False;
  for I := Count - 1 downto 0 do // last item is the first
  begin
    if Options[I] and CIO_VIRTUALFILES <> 0 then
    begin
      try
        if Supports(Items[I], ICompileInterceptor20, Intf) then
          Result := Intf.OpenVirtualFile(PWideChar(UniFilename), {out} Stream)
        else
        begin
          Stream := Items[I].GetVirtualFile(PWideChar(UniFilename));
          Result := Stream <> nil;
        end;
      except
        FItems.Delete(I); // remove broken handler
      end;
      if Result then
        Exit;
    end;
  end;
end;

function TCompileInterceptorServices.CreateVirtualFile(Filename: PAnsiChar; out Stream: IVirtualOutStream): Boolean;
var
  I: Integer;
  UniFilename: string;
  Intf: ICompileInterceptor20;
begin
  UniFilename := UTF8ToString(Filename);
  Result := False;
  for I := Count - 1 downto 0 do // last item is the first
  begin
    if (Options[I] and CIO_VIRTUALOUTFILES <> 0) and Supports(Items[I], ICompileInterceptor20, Intf) then
    begin
      try
        Result := Intf.CreateVirtualFile(PWideChar(UniFilename), {out} Stream);
      except
        FItems.Delete(I); // remove broken handler
      end;
      if Result then
        Exit;
    end;
  end;
end;

function TCompileInterceptorServices.AlterFile(Filename: PAnsiChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream;
var
  I: Integer;
  Stream: IVirtualStream;
  StreamDate, StreamSize: Integer;
  Buf: array of Byte;
  UniFilename: string;
begin
  UniFilename := UTF8ToString(Filename);
  Result := nil;
  Stream := nil;
  for I := Count - 1 downto 0 do // last item is the first
  begin
    if Options[I] and CIO_ALTERFILES <> 0 then
    begin
      if Stream <> nil then
      begin
        { The content is already altered. The next handler must use the new data }
        Stream.FileStatus(StreamDate, StreamSize);
        Stream.Seek(0, soFromBeginning);
        SetLength(Buf, FileSize);
        Content := nil;
        if StreamSize > 0 then
        begin
          FileSize := Stream.Read(Buf[0], StreamSize);
          if FileSize > 0 then
            Stream.Seek(0, soFromBeginning);
          Content := PByte(@Buf[0]);
        end;
      end;
      try
        Stream := Items[I].AlterFile(PWideChar(UniFilename), Content, FileDate, FileSize);
      except
        FItems.Delete(I); // remove broken handler
      end;
      if Stream <> nil then
        Result := Stream;
    end;
  end;
end;

procedure TCompileInterceptorServices.InspectFilename(Filename: PAnsiChar; FileMode: TInspectFileMode);
var
  I: Integer;
  UniFilename: string;
begin
  UniFilename := UTF8ToString(Filename);
  for I := Count - 1 downto 0 do // last item is the first
  begin
    if Options[I] and CIO_INSPECTFILENAMES <> 0 then
    begin
      try
        Items[I].InspectFilename(PWideChar(UniFilename), FileMode);
      except
        FItems.Delete(I); // remove broken handler
      end;
    end;
  end;
end;

function TCompileInterceptorServices.AlterMessage(IsCompilerMessage: Boolean;
  var MsgKind: TMsgKind; var Code: Integer; var Filename: UTF8String; var Line,
  Column: Integer; var Msg: UTF8String): Boolean;
var
  I: Integer;
  FilenameStr, MsgStr: IWideString;
begin
  Result := False;

  FilenameStr := TWideStringAdapter.Create(UTF8ToString(Filename));
  MsgStr := TWideStringAdapter.Create(UTF8ToString(Msg));

  for I := Count - 1 downto 0 do // last item is the first
  begin
    if Options[I] and CIO_ALTERMESSAGES <> 0 then
    begin
      try
        if Items[I].AlterMessage(IsCompilerMessage, MsgKind, Code, FilenameStr, Line, Column, MsgStr) then
          Result := True;
      except
        FItems.Delete(I); // remove broken handler
      end;
    end;
  end;

  if Result then
  begin
    Filename := UTF8Encode(FilenameStr.Value);
    Msg := UTF8Encode(FilenameStr.Value);
  end;
end;

procedure TCompileInterceptorServices.CompileProject(const ProjectFilename, UnitPaths, SourcePaths,
  DcuOutputDir: string; IsCodeInsight: Boolean; var Cancel: Boolean);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do // last item is the first
  begin
    if Options[I] and CIO_COMPILEPROJECTS <> 0 then
    begin
      try
        Items[I].CompileProject(PWideChar(ProjectFilename),
                                PWideChar(UnitPaths),
                                PWideChar(SourcePaths),
                                PWideChar(DcuOutputDir),
                                IsCodeInsight, Cancel);
      except
        FItems.Delete(I); // remove broken handler
      end;
    end;
  end;
end;

function TCompileInterceptorServices.FileNameDate(Filename: PAnsiChar; out FileDate: Integer): Boolean;
var
  I: Integer;
  UniFilename: string;
  Intf: ICompileInterceptor20;
begin
  UniFilename := UTF8ToString(Filename);
  Result := False;
  for I := Count - 1 downto 0 do // last item is the first
  begin
    if (Options[I] and CIO_FILENAMEDATES <> 0) and Supports(Items[I], ICompileInterceptor20, Intf) then
    begin
      try
        Result := Intf.FileNameDate(PWideChar(UniFilename), {out} FileDate);
      except
        FItems.Delete(I); // remove broken handler
      end;
      if Result then
      begin
        if FileDate = -1 then
          FileDate := 0;
        Exit;
      end;
    end;
  end;
end;

function TCompileInterceptorServices.RegisterInterceptor(Interceptor: ICompileInterceptor): Integer;
var
  Index: Integer;
begin
  if Interceptor <> nil then
  begin
    try
      Inc(TCompileInterceptorServices_LastId);
      FItems.Add(Interceptor);
      FIds.Add(Pointer(TCompileInterceptorServices_LastId));
      Result := TCompileInterceptorServices_LastId;
      Index := FOptions.Add(Pointer(Interceptor.GetOptions));

      { Update option cache }
      if Options[Index] and CIO_ALTERFILES <> 0 then
        FHasAlterFile := True;
      if Options[Index] and CIO_VIRTUALFILES <> 0 then
        FHasVirtualFiles := True;
      if Options[Index] and CIO_INSPECTFILENAMES <> 0 then
        FHasInspectFilename := True;
      if Options[Index] and CIO_ALTERMESSAGES <> 0 then
        FHasAlterMessage := True;
      if Options[Index] and CIO_COMPILEPROJECTS <> 0 then
        FHasCompileProjects := True;
      if Options[Index] and CIO_FILENAMEDATES <> 0 then
        FHasFileNameDates := True;

    except
      ApplicationHandleException(Self);
      Result := -1;
    end;
  end
  else
    Result := -1;
end;

procedure TCompileInterceptorServices.UnregisterInterceptor(Id: Integer);
var
  Index: Integer;
begin
  Index := FIds.IndexOf(Pointer(Id));
  if Index <> -1 then
  begin
    try
      FItems.Delete(Index);
      FIds.Delete(Index);
      FOptions.Delete(Index);
      UpdateOptionCache;
    except
      ApplicationHandleException(Self);
    end;
  end;
end;

procedure TCompileInterceptorServices.UpdateOptionCache;
var
  I: Integer;
begin
  FHasAlterFile := False;
  FHasVirtualFiles := False;
  FHasInspectFilename := False;
  FHasAlterMessage := False;
  FHasCompileProjects := False;
  for I := 0 to Count - 1 do
  begin
    if Options[I] and CIO_ALTERFILES <> 0 then
      FHasAlterFile := True;
    if Options[I] and CIO_VIRTUALFILES <> 0 then
      FHasVirtualFiles := True;
    if Options[I] and CIO_INSPECTFILENAMES <> 0 then
      FHasInspectFilename := True;
    if Options[I] and CIO_ALTERMESSAGES <> 0 then
      FHasAlterMessage := True;
    if Options[I] and CIO_COMPILEPROJECTS <> 0 then
      FHasCompileProjects := True;
  end;
end;

end.
