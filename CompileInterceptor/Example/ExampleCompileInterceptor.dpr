library ExampleCompileInterceptor;

uses
  SysUtils,
  Classes,
  InterceptIntf in '..\Source\InterceptIntf.pas';

{$R *.res}

type
  TExampleInterceptor = class(TInterfacedObject, ICompileInterceptor)
  public
    function GetOptions: TCompileInterceptOptions; stdcall;
    function GetVirtualFile(Filename: PAnsiChar): IVirtualStream; stdcall;
    function AlterFile(Filename: PAnsiChar; Content: PAnsiChar;
      FileDate: Integer; FileSize: Integer): IVirtualStream; stdcall;
  end;

procedure Done; stdcall;
begin
end;

procedure EntryPoint(const CompileInterceptorServices: ICompileInterceptorServices; var DoneProc: TDoneProc); stdcall;
begin
  DoneProc := Done;
  CompileInterceptorServices.RegisterInterceptor(TExampleInterceptor.Create);
end;

exports
  EntryPoint name CompileInterceptorEntryPoint;

{ TExampleInterceptor }

function TExampleInterceptor.AlterFile(Filename, Content: PAnsiChar;
  FileDate, FileSize: Integer): IVirtualStream;
begin
  Result := nil;
end;

function TExampleInterceptor.GetOptions: TCompileInterceptOptions;
begin
  Result := CIO_VIRTUALFILES;
end;

function TExampleInterceptor.GetVirtualFile(Filename: PAnsiChar): IVirtualStream;
begin
  if SameText(ExtractFileName(Filename), 'test.inc') then
    Result := T
  Result := nil;
end;

begin
end.
 