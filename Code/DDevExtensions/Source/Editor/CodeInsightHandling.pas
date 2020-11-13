unit CodeInsightHandling;

{
  IDE Version: 2009+

  Allows the TAB key to close the code insight window (like the ENTER key)
}

interface

procedure InitPlugin(Unload: Boolean);

implementation

uses
  Windows, Hooking, IDEHooks;

procedure TIDEPopupListBox_EditorKey(Instance: TObject; Sender: TObject; var Key: Char);
  external coreide_bpl name '@Idepopuplistbox@TIDEPopupListBox@EditorKey$qqrp14System@TObjectrb';

var
  OrgIDEPopupListBox_EditorKey: procedure(Instance: TObject; Sender: TObject; var Key: Char);
  IDEPopupListBox_EditorKeyHooked: Boolean = False;

procedure HookedIDEPopupListBox_EditorKey(Instance: TObject; Sender: TObject; var Key: Char);
begin
  if Key = #9 then
    Key := #13;
  OrgIDEPopupListBox_EditorKey(Instance, Sender, Key);
end;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
  begin
    if not Assigned(OrgIDEPopupListBox_EditorKey) then
      @OrgIDEPopupListBox_EditorKey := RedirectOrgCall(@TIDEPopupListBox_EditorKey, @HookedIDEPopupListBox_EditorKey)
    else
      RedirectOrg(@TIDEPopupListBox_EditorKey, @HookedIDEPopupListBox_EditorKey);
  end
  else
    RestoreOrgCall(@TIDEPopupListBox_EditorKey, @OrgIDEPopupListBox_EditorKey);
end;


end.
