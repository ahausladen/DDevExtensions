{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmDDevExtOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmOptions, ExtCtrls, ComCtrls, StdCtrls;

type
  TFormDDevExtOptions = class(TFormOptions)
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

uses
  AppConsts;

{$R *.dfm}

procedure TFormDDevExtOptions.FormCreate(Sender: TObject);
begin
  inherited;
  lblVersion.Caption := 'Version ' + sPluginVersion;
  lblURL.Caption := sPluginSmallCopyright;
end;

end.
