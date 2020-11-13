unit FrmSwitchToModuleProject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmBase, StdCtrls, ExtCtrls, ToolsAPI;

type
  TFormSwitchToModuleProject = class(TFormBase)
    PanelBottom: TPanel;
    ButtonYes: TButton;
    ButtonNo: TButton;
    ButtonCancel: TButton;
    CheckBoxDontShowAgain: TCheckBox;
    BevelBottom: TBevel;
    ComboBoxProjects: TComboBox;
    LabelModuleCaption: TLabel;
    LabelFileName: TLabel;
    LabelActiveProjectCaption: TLabel;
    LabelActiveProject: TLabel;
    LabelQuestion: TLabel;
    LabelText: TLabel;
    CheckBoxTempSwitch: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private-Deklarationen }
    function GetProjectName(const AProject: IOTAProject): string;
    function InternShowDialog(AModule: IOTAModule; var AProject: IOTAProject;
      var ADontShowAgain: Boolean; SwitchTemporary: Boolean): TModalResult;
  public
    { Public-Deklarationen }
    class function ShowDialog(AModule: IOTAModule; var AProject: IOTAProject;
      var ADontShowAgain: Boolean; SwitchTemporary: Boolean): TModalResult;
  end;

implementation

uses
  Consts, AppConsts;

{$R *.dfm}

class function TFormSwitchToModuleProject.ShowDialog(AModule: IOTAModule;
  var AProject: IOTAProject; var ADontShowAgain: Boolean; SwitchTemporary: Boolean): TModalResult;
begin
  if AModule.OwnerCount > 0 then
  begin
    with TFormSwitchToModuleProject.Create(nil) do
    try
      Result := InternShowDialog(AModule, AProject, ADontShowAgain, SwitchTemporary)
    finally
      Free;
    end;
  end
  else
    Result := mrNo;
end;

function TFormSwitchToModuleProject.InternShowDialog(AModule: IOTAModule;
  var AProject: IOTAProject; var ADontShowAgain: Boolean; SwitchTemporary: Boolean): TModalResult;
var
  I: Integer;
begin
  for I := 0 to AModule.OwnerCount - 1 do
    ComboBoxProjects.Items.AddObject(GetProjectName(AModule.Owners[I]), TObject(I));
  ComboBoxProjects.ItemIndex := 0;

  CheckBoxTempSwitch.Checked := SwitchTemporary;
  AProject := GetActiveProject;
  LabelActiveProject.Caption := GetProjectName(AProject);
  if AProject <> nil then
    LabelActiveProject.Hint := AProject.FileName;
  LabelFileName.Caption := ExtractFileName(AModule.FileName);
  LabelFileName.Hint := AModule.FileName;

  Result := ShowModal;
  if Result = mrYes then
  begin
    AProject := AModule.Owners[Integer(ComboBoxProjects.Items.Objects[ComboBoxProjects.ItemIndex])];
    if CheckBoxTempSwitch.Checked then
      Result := mrRetry;
  end;
  if Result <> mrCancel then
    ADontShowAgain := CheckBoxDontShowAgain.Checked;
end;

procedure TFormSwitchToModuleProject.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ssShift in Shift then
    CheckBoxTempSwitch.Checked := not CheckBoxTempSwitch.Checked;
end;

function TFormSwitchToModuleProject.GetProjectName(const AProject: IOTAProject): string;
begin
  if AProject <> nil then
    Result := ChangeFileExt(ExtractFileName(AProject.FileName), '')
  else
    Result := '';
end;

procedure TFormSwitchToModuleProject.FormCreate(Sender: TObject);
begin
  inherited;
  { Localize dialog }
  ButtonYes.Caption := SYesButton;
  ButtonNo.Caption := SNoButton;
  ButtonCancel.Caption := SCancelButton;

  Caption := sCapSwitchToModuleProject;
  LabelActiveProjectCaption.Caption := sLblActiveProject;
  LabelModuleCaption.Caption := sLblActiveModule;
  LabelText.Caption := sLblSwitchCurrentModuleProject;
  LabelQuestion.Caption := sLblSwitchToModuleProjectQuestion;
  CheckBoxDontShowAgain.Caption := sLblDontShowAgain;
  CheckBoxTempSwitch.Caption := sLblTemporarySwitch;
end;

end.

