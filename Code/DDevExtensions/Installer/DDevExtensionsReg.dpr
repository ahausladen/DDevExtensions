{$SetPEFlags 1} // no reloc info in EXE

program DDevExtensionsReg;

{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  AppConsts in '..\Source\AppConsts.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Windows10');
  Application.Title := 'DDevExtensions Installer';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
