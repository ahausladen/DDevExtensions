library ExampleCompileInterceptor;

uses
  SysUtils,
  Classes,
  InterceptIntf in '..\Source\InterceptIntf.pas';

{$R *.res}

type
  TExampleInterceptor = class(TInterfacedObject, ICompileInterceptor, ICompileInterceptor20)
  public
    function GetOptions: TCompileInterceptOptions; stdcall;
    function GetVirtualFile(Filename: PWideChar): IVirtualStream; stdcall; // deprecated 'implement OpenVirtualFile'
    function AlterFile(Filename: PWideChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream; stdcall;
    procedure InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode); stdcall;
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind; var Code: Integer;
      const Filename: IUnicodeString; var Line, Column: Integer; const Msg: IUnicodeString): Boolean; stdcall;
    procedure CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
      IsCodeInsight: Boolean; var Cancel: Boolean); stdcall;
    function CreateVirtualFile(Filename: PWideChar; out Stream: IVirtualOutStream): Boolean; stdcall;
    function OpenVirtualFile(Filename: PWideChar; out Stream: IVirtualStream): Boolean; stdcall;
    function FileNameDate(Filename: PWideChar; out AFileDate: Integer): Boolean; stdcall;
  end;

  TDoneProc = procedure; stdcall;

procedure Done; stdcall;
begin
end;

procedure EntryPoint(const CompileInterceptorServices: ICompileInterceptorServices; var DoneProc: TDoneProc); stdcall;
begin
  DoneProc := Done;
  CompileInterceptorServices.RegisterInterceptor(TExampleInterceptor.Create);
end;

exports
  EntryPoint name 'CompileInterceptorEntryPoint';

{ TExampleInterceptor }

function TExampleInterceptor.AlterFile(Filename: PWideChar; Content: PByte;
  FileDate, FileSize: Integer): IVirtualStream;
begin
  Result := nil;
end;

function TExampleInterceptor.GetOptions: TCompileInterceptOptions;
begin
  Result := CIO_INSPECTFILENAMES;
end;

function TExampleInterceptor.GetVirtualFile(Filename: PWideChar): IVirtualStream;
begin
  Result := nil;
end;

function TExampleInterceptor.AlterMessage(IsCompilerMessage: Boolean;
  var MsgKind: TMsgKind; var Code: Integer; const Filename: IWideString;
  var Line, Column: Integer; const Msg: IWideString): Boolean;
begin
  Result := False;
end;

procedure TExampleInterceptor.InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode);
begin
  //WriteLn(Filename);
end;

procedure TExampleInterceptor.CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
end;

function TExampleInterceptor.CreateVirtualFile(Filename: PWideChar; out Stream: IVirtualOutStream): Boolean;
begin
  Result := False;
end;

function TExampleInterceptor.OpenVirtualFile(Filename: PWideChar; out Stream: IVirtualStream): Boolean;
begin
  Result := False;
end;

function TExampleInterceptor.FileNameDate(Filename: PWideChar; out AFileDate: Integer): Boolean;
begin
  Result := False;
end;

begin
end.
 