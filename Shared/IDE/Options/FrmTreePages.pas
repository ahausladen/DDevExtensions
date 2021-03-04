{******************************************************************************}
{*                                                                            *}
{* (C) 2005-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit FrmTreePages;

{$I ..\jedi\jedi.inc}

interface

uses
  Windows, Messages, Contnrs, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, FrmBase;

type
  { ITreePageComponent is used by the environment option dialog to save and load
    the data from and into the personalitiy options. }
  ITreePageComponent = interface
    ['{353613EA-B5BF-43BC-A7D0-0AADD9388A03}']
      { LoadData loads the options into the dialog controls. }
    procedure LoadData;
      { SaveData stores the data from the dialog controls into the personality
        options. }
    procedure SaveData;

      { Selected is called when the page becomes selected. }
    procedure Selected;
      { Unselected is called when the page becomes unselected. }
    procedure Unselected;

      { SetUserData is called after the frame was created. The "UserData"
        parameter is the one that was specified when the tree page was
        registered. }
    procedure SetUserData(UserData: TObject);
  end;

  ITreePageComponentEx = interface
    ['{9260B5AB-8021-4208-9EEF-B3BD24C3AF9B}']
      { The implementor can use the caption to display it in the page }
    procedure SetTitle(const ACaption: string);
  end;

  TTreePage = class(TObject)
  private
    FItems: TObjectList;
    FParent: TTreePage;
    FName: string;
    FComponentClass: TComponentClass;
    FUserData: TObject;
    function GetCount: Integer;
    function GetItem(Index: Integer): TTreePage;
  public
    constructor Create(const AName: string; AComponentClass: TComponentClass; AUserData: TObject = nil);
    destructor Destroy; override;

    procedure Add(Page: TTreePage);
    procedure Delete(Index: Integer);
    procedure Clear;

    property Parent: TTreePage read FParent;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TTreePage read GetItem; default;

    property UserData: TObject read FUserData;
    property Name: string read FName;
    property ComponentClass: TComponentClass read FComponentClass;
  end;

  ETreePageError = class(Exception);

  TFormTreePages = class(TFormBase)
    PanelButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    bvlDivider: TBevel;
    PanelWorkingArea: TPanel;
    TreeView: TTreeView;
    PanelClient: TPanel;
    SplitterTree: TSplitter;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    FRootPage: TTreePage;
    FSelected: TComponent;
    function GetPageComponent(Index: Integer): ITreePageComponent;
    function GetPageComponentCount: Integer;
    procedure PopulateTreeNodes;
  protected
    function DoExecute: Boolean; virtual;
    procedure SelectionChanged(Node: TTreeNode); virtual;
    
    procedure PopulateRootPage(Root: TTreePage); virtual; abstract;
    procedure BeforeSaveData; virtual;
    procedure AfterSaveData; virtual;
    procedure AfterClose; virtual;

    property RootPage: TTreePage read FRootPage;
    property PageComponentCount: Integer read GetPageComponentCount;
    property PageComponents[Index: Integer]: ITreePageComponent read GetPageComponent;
  public
    { Public-Deklarationen }
    class function Execute: Boolean;
  end;

implementation

uses
  {$IFDEF COMPILER5}
  Consts,
  {$ELSE}
  RTLConsts,
  {$ENDIF COMPILER5}
  IDEUtils;

{$R *.dfm}

resourcestring
  RsTreePageError = 'TreePage does not support the ITreePageComponent interface.';

{ TTreePage }

constructor TTreePage.Create(const AName: string; AComponentClass: TComponentClass; AUserData: TObject);
begin
  inherited Create;
  if (AComponentClass <> nil) and not Supports(AComponentClass, ITreePageComponent) then
    raise ETreePageError.CreateFmt(RsTreePageError, [AComponentClass.ClassName]);
  FName := AName;
  FComponentClass := AComponentClass;
  FUserData := AUserData;
  FItems := TObjectList.Create;
end;

destructor TTreePage.Destroy;
begin
  if Parent <> nil then
    Parent.FItems.Extract(Self);
  FItems.Free;
  inherited Destroy;
end;

procedure TTreePage.Add(Page: TTreePage);
begin
  Assert(Page <> nil, 'TreePage must not be nil');
  Page.FParent := Self;
  FItems.Add(Page);
end;

function TTreePage.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTreePage.GetItem(Index: Integer): TTreePage;
begin
  Result := TTreePage(FItems[Index]);
end;

procedure TTreePage.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TTreePage.Clear;
begin
  FItems.Clear;
end;

{ TFormEnvironmentOptions }

procedure TFormTreePages.AfterClose;
begin
end;

procedure TFormTreePages.AfterSaveData;
begin
end;

procedure TFormTreePages.BeforeSaveData;
begin
end;

function TFormTreePages.GetPageComponent(Index: Integer): ITreePageComponent;
var
  i, Idx: Integer;
begin
  if Index >= 0 then
  begin
    Idx := Index;
    for i := 0 to PanelClient.ComponentCount - 1 do
    begin
      if SupportsEx(PanelClient.Components[i], ITreePageComponent, Result) then
      begin
        if Idx = 0 then
          Exit;
        Result := nil;
        Dec(Idx);
      end;
    end;
  end;
  raise EListError.CreateFmt(SListIndexError, [Index]);
end;

function TFormTreePages.GetPageComponentCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to PanelClient.ComponentCount - 1 do
    if Supports(PanelClient.Components[i], ITreePageComponent) then
      Inc(Result);
end;

class function TFormTreePages.Execute: Boolean;
begin
  with Self.Create(Application) do
  try
    Result := DoExecute;
  finally
    Free;
  end;
end;

function TFormTreePages.DoExecute: Boolean;
var
  i: Integer;
  Comp: ITreePageComponent;
begin
  TreeView.OnChange := nil;
  FRootPage.Clear;
  PopulateRootPage(FRootPage);

  PopulateTreeNodes;

  Result := ShowModal = mrOk;
  if Result then
  begin
    BeforeSaveData;
    TreeViewChange(TreeView, nil); // unselect
    try
      for i := 0 to PanelClient.ComponentCount - 1 do
        if SupportsEx(PanelClient.Components[i], ITreePageComponent, Comp) then
          Comp.SaveData;
    finally
      Comp := nil;
    end;
    AfterSaveData;
  end;
  AfterClose;
end;

procedure TFormTreePages.PopulateTreeNodes;

  procedure CreateTreeNode(ParentPage: TTreePage; Parent: TTreeNode);
  var
    i: Integer;
  begin
    for i := 0 to ParentPage.Count - 1 do
    begin
      CreateTreeNode(
        ParentPage[i],
        TreeView.Items.AddChildObject(Parent, ParentPage[i].Name, ParentPage[i])
      );
    end;
  end;

var
  i: Integer;
begin
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;
    CreateTreeNode(FRootPage, nil);
    for i := 0 to TreeView.Items.Count - 1 do
      TreeView.Items[i].Expand(True);
    if TreeView.Items.Count > 0 then
      TreeView.Items[0].Selected := True;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TFormTreePages.FormCreate(Sender: TObject);
begin
  btnOk.Anchors := [akBottom, akRight];
  btnCancel.Anchors := [akBottom, akRight];

  FRootPage := TTreePage.Create('', nil);
end;

procedure TFormTreePages.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRootPage);
end;

procedure TFormTreePages.FormShow(Sender: TObject);
begin
  TreeView.OnChange := TreeViewChange;
  TreeViewChange(TreeView, TreeView.Selected);
end;

procedure TFormTreePages.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  Comp: TComponent;
  LastSelected: TComponent;
  Intf: ITreePageComponent;
  IntfEx: ITreePageComponentEx;
begin
  if csDestroying in ComponentState then
    Exit;

  LastSelected := FSelected;
  if Node <> nil then
  begin
    { create if necessary and select }
    if TObject(Node.Data) is TTreePage then
    begin
      Comp := nil;
      if TTreePage(Node.Data).ComponentClass <> nil then
      begin
        Comp := TTreePage(Node.Data).ComponentClass.Create(PanelClient);
        if SupportsEx(Comp, ITreePageComponent, Intf) then
          Intf.SetUserData(TTreePage(Node.Data).UserData); // set before replacing Node.Data
        Comp.Name := '';
        Node.Data := Comp; // replace TTreePage by TComponent
        if Comp is TControl then
        begin
          TControl(Comp).Left := 0;
          TControl(Comp).Top := 0;
          TControl(Comp).Parent := PanelClient;
          if Supports(Comp, ITreePageComponentEx, IntfEx) then
            IntfEx.SetTitle(Node.Text);
        end;
        if Assigned(Intf) then
          Intf.LoadData;
      end
      else
      begin
        // select first child
        if Node.Count > 0 then
        begin
          if FSelected = Node.Item[0].Data then
            FSelected := nil;
          Node.Item[0].Selected := True;
          Exit;
        end;
      end;
    end
    else
      Comp := Node.Data;

    { set selected and show }
    FSelected := Comp;
    if Comp <> nil then
    begin
      if SupportsEx(Comp, ITreePageComponent, Intf) then
        Intf.Selected;
      if Comp is TControl then
        TControl(Comp).Show;
    end;
  end
  else
    FSelected := nil;

  if Assigned(LastSelected) and (LastSelected <> FSelected) then
  begin
    { unselect }
    if LastSelected is TControl then
      TControl(LastSelected).Hide;
    if SupportsEx(LastSelected, ITreePageComponent, Intf) then
      Intf.Unselected;
  end;
  SelectionChanged(Node);
end;

procedure TFormTreePages.SelectionChanged(Node: TTreeNode);
begin
end;

end.
