{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit FrmOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, FrmTreePages, StdCtrls, ExtCtrls, ComCtrls, ShellAPI;

type
  TOptionPagesEvent = function: TTreePage of object;
  TOptionPagesProc = function: TTreePage;

  TFormOptions = class(TFormTreePages)
    lblURL: TLabel;
    Label1: TLabel;
    lblVersion: TLabel;
    procedure lblURLClick(Sender: TObject);
  private
    { Private-Deklarationen }
  protected
    class function GetGlobalOptionPages: TObjectList; virtual;
    class procedure SetGlobalOptionPages(Value: TObjectList); virtual;
    procedure PopulateRootPage(Root: TTreePage); override;
  public
    { Public-Deklarationen }
    class procedure RegisterPages(const OptionPages: TOptionPagesEvent);
    class procedure UnregisterPages(const OptionPages: TOptionPagesEvent);
    class procedure RegisterPagesEx(const OptionPages: TOptionPagesProc);
    class procedure UnregisterPagesEx(const OptionPages: TOptionPagesProc);
  end;

implementation

{$R *.dfm}

var
  GlobalOptionPages: TObjectList;

type
  TOptionPages = class(TObject)
    Event: TOptionPagesEvent;
    Proc: TOptionPagesProc;
  end;

{ TFormOptions }

class procedure TFormOptions.SetGlobalOptionPages(Value: TObjectList);
begin
  GlobalOptionPages := Value;
end;

class function TFormOptions.GetGlobalOptionPages: TObjectList;
begin
  Result := GlobalOptionPages;
end;

class procedure TFormOptions.RegisterPages(const OptionPages: TOptionPagesEvent);
var
  Item: TOptionPages;
begin
  if GetGlobalOptionPages = nil then
    SetGlobalOptionPages(TObjectList.Create);

  Item := TOptionPages.Create;
  Item.Event := OptionPages;
  GetGlobalOptionPages.Add(Item);
end;

class procedure TFormOptions.UnregisterPages(const OptionPages: TOptionPagesEvent);
var
  i: Integer;
begin
  if GetGlobalOptionPages <> nil then
  begin
    for i := GetGlobalOptionPages.Count - 1 downto 0 do
    begin
      if CompareMem(Addr(TMethod(TOptionPages(GetGlobalOptionPages[i]).Event)), Addr(TMethod(OptionPages)), SizeOf(TMethod)) then
      begin
        GetGlobalOptionPages.Delete(i);
        Break;
      end;
    end;
  end;
end;

class procedure TFormOptions.RegisterPagesEx(const OptionPages: TOptionPagesProc);
var
  Item: TOptionPages;
begin
  if GetGlobalOptionPages = nil then
    SetGlobalOptionPages(TObjectList.Create);

  Item := TOptionPages.Create;
  Item.Proc := OptionPages;
  GetGlobalOptionPages.Add(Item);
end;

class procedure TFormOptions.UnregisterPagesEx(const OptionPages: TOptionPagesProc);
var
  i: Integer;
begin
  if GetGlobalOptionPages <> nil then
  begin
    for i := GetGlobalOptionPages.Count - 1 downto 0 do
    begin
      if Addr(TOptionPages(GetGlobalOptionPages[i]).Proc) <> Addr(OptionPages) then
      begin
        GetGlobalOptionPages.Delete(i);
        Break;
      end;
    end;
  end;
end;

procedure TFormOptions.PopulateRootPage(Root: TTreePage);
var
  i: Integer;
  OptionPages: TOptionPages;
begin
  if GetGlobalOptionPages <> nil then
  begin
    for i := 0 to GetGlobalOptionPages.Count - 1 do
    begin
      OptionPages := TOptionPages(GetGlobalOptionPages[i]);
      if Assigned(OptionPages.Event) then
        Root.Add(OptionPages.Event())
      else
        Root.Add(OptionPages.Proc());
    end;
  end;
end;

procedure TFormOptions.lblURLClick(Sender: TObject);
begin
  with TLabel(Sender) do
  begin
    if ShellExecute(Handle, 'open', PChar(Hint), nil, nil, SW_SHOWMAXIMIZED) < 32 then
    begin
      Font.Color := clWindowText;
      Font.Style := [];
      OnClick := nil;
    end;
  end;
end;

initialization

finalization
  FreeAndNil(GlobalOptionPages);

end.
