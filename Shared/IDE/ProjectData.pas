{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit ProjectData;

{$I ..\jedi\jedi.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, Contnrs, ToolsAPI, Forms, SimpleXmlImport, SimpleXmlIntf;

type
  TProjectDataList = class;

  TDataVariantItem = class(TObject)
  private
    FValue: Variant;
  public
    constructor Create(const AValue: Variant);
    property Value: Variant read FValue write FValue;
  end;

  TProjectData = class(TInterfacedObject, IOTANotifier, IOTAModuleNotifier)
  private
    FFilename: string;
    FOwner: TProjectDataList;
    FId: Integer;
    FItems: TStrings;
    FNonPersistents: TStrings;
    FProject: IOTAProject;
    FAllowSaveData: Boolean;
    FLoading: Boolean;
    function GetValue(const Name: string): Variant;
    procedure SetValue(const Name: string; const Value: Variant);
    function GetNonPersistent(const Name: string): TObject;
    procedure SetNonPersistent(const Name: string; const Value: TObject);
  protected
    procedure Clear;
  protected
    { IOTAModuleNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    function CheckOverwrite: Boolean;
    procedure Destroyed;
    procedure Modified;
    procedure ModuleRenamed(const NewName: String);

    property Loading: Boolean read FLoading;
  public
    constructor Create(AOwner: TProjectDataList; AProject: IOTAProject);
    destructor Destroy; override;
    procedure Reload;
    function HasValue(const Name: string): Boolean;

    property Project: IOTAProject read FProject;

    property AllowSaveData: Boolean read FAllowSaveData write FAllowSaveData;
    property NonPersistents[const Name: string]: TObject read GetNonPersistent write SetNonPersistent;
    property Values[const Name: string]: Variant read GetValue write SetValue;
  end;

  TProjectDataTransaction = class(TObject)
  private
    FOwner: TProjectDataList;
    FProjects: TList;
    FItems: TObjectList;
  public
    constructor Create(AOwner: TProjectDataList);
    destructor Destroy; override;

    procedure Rollback;
  end;


  TProjectDataEvent = procedure(Data: TProjectData) of object;
  TProjectDataSavingEvent = procedure(Data: TProjectData; Node: IXmlNode) of object;
  TProjectRenamedEvent = procedure(Data: TProjectData; const Filename, NewName: string) of object;

  TProjectDataNotifier = class(TObject)
  public
    Added: TProjectDataEvent;
    Destroying: TProjectDataEvent;
    Saving: TProjectDataSavingEvent;
    Loading: TProjectDataSavingEvent;
    Renamed: TProjectRenamedEvent;

    constructor Create;
    destructor Destroy; override;
  end;

  TProjectDataList = class(TObject)
  private
    FList: TObjectList;
    FTransactions: TObjectList;
    FNotifiers: TList;
    function GetProjectData(const Project: IOTAProject): TProjectData;
    function GetNotifier(Index: Integer): TProjectDataNotifier;
    function GetNotifierCount: Integer;
  protected
    procedure Saving(Data: TProjectData; Node: IXmlNode); virtual;
    procedure Loading(Data: TProjectData; Node: IXmlNode); virtual;
    procedure ProjectAdded(Data: TProjectData); virtual;
    procedure ProjectDestroying(Data: TProjectData); virtual;
    procedure ProjectRenamed(Data: TProjectData; const Filename, NewName: string); virtual;

    property NotifierCount: Integer read GetNotifierCount;
    property Notifiers[Index: Integer]: TProjectDataNotifier read GetNotifier;

    procedure AddNotifier(ANotifier: TProjectDataNotifier);
    procedure RemoveNotifier(ANotifier: TProjectDataNotifier);

    // not working yet
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  public
    constructor Create;
    destructor Destroy; override;

    property ProjectData[const Project: IOTAProject]: TProjectData read GetProjectData; default;
  end;

function ProjectDataList: TProjectDataList;

implementation

const
  sSuffix = '.projdata';

var
  GlobalProjectDataList: TProjectDataList;

function ProjectDataList: TProjectDataList;
begin
  if not Assigned(GlobalProjectDataList) then
    GlobalProjectDataList := TProjectDataList.Create;
  Result := GlobalProjectDataList;
end;

{ TDataVariantItem }

constructor TDataVariantItem.Create(const AValue: Variant);
begin
  inherited Create;
  FValue := AValue;
end;

{ TProjectDataList }

constructor TProjectDataList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FTransactions := TObjectList.Create;
  FNotifiers := TList.Create;
end;

destructor TProjectDataList.Destroy;
begin
  FNotifiers.Free;
  FTransactions.Free;
  FList.Free;
  inherited Destroy;
end;

function TProjectDataList.GetProjectData(const Project: IOTAProject): TProjectData;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := TProjectData(FList[I]);
    if Result.Project = Project then
      Exit;
  end;
  Result := TProjectData.Create(Self, Project);
  Result.Reload;
end;

procedure TProjectDataList.Rollback;
begin
  TProjectDataTransaction(FTransactions[FTransactions.Count - 1]).Rollback;
end;

procedure TProjectDataList.StartTransaction;
begin
  FTransactions.Add(TProjectDataTransaction.Create(Self));
end;

procedure TProjectDataList.Commit;
begin
  FTransactions.Delete(FTransactions.Count - 1);
end;

procedure TProjectDataList.Loading(Data: TProjectData; Node: IXmlNode);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[I].Loading) then
      Notifiers[I].Loading(Data, Node);
end;

procedure TProjectDataList.Saving(Data: TProjectData; Node: IXmlNode);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[I].Saving) then
      Notifiers[I].Saving(Data, Node);
end;

procedure TProjectDataList.ProjectAdded(Data: TProjectData);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[I].Added) then
      Notifiers[I].Added(Data);
end;

procedure TProjectDataList.ProjectDestroying(Data: TProjectData);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[I].Destroying) then
      Notifiers[I].Destroying(Data);
end;

procedure TProjectDataList.ProjectRenamed(Data: TProjectData;
  const Filename, NewName: string);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[I].Renamed) then
      Notifiers[I].Renamed(Data, Filename, NewName);
end;

procedure TProjectDataList.AddNotifier(ANotifier: TProjectDataNotifier);
begin
  FNotifiers.Add(ANotifier);
end;

procedure TProjectDataList.RemoveNotifier(ANotifier: TProjectDataNotifier);
begin
  FNotifiers.Extract(ANotifier);
end;

function TProjectDataList.GetNotifier(Index: Integer): TProjectDataNotifier;
begin
  Result := TProjectDataNotifier(FNotifiers[Index]);
end;

function TProjectDataList.GetNotifierCount: Integer;
begin
  Result := FNotifiers.Count;
end;

{ TProjectData }

constructor TProjectData.Create(AOwner: TProjectDataList; AProject: IOTAProject);
begin
  inherited Create;
  FOwner := AOwner;
  FOwner.FList.Add(Self);
  FProject := AProject;
  FFilename := FProject.FileName;
  FId := Project.AddNotifier(Self);
  FItems := TStringList.Create;
  FAllowSaveData := True;
  FNonPersistents := TStringList.Create;
  try
    FOwner.ProjectAdded(Self);
  except
    Application.HandleException(Self);
  end;
end;

destructor TProjectData.Destroy;
begin
  if FId <> -1 then
  begin
    try
      FOwner.ProjectDestroying(Self);
    except
      Application.HandleException(Self);
    end;
  end;
  FOwner.FList.Extract(Self);
  Clear;
  FItems.Free;
  FNonPersistents.Free;

  if FId <> -1 then
    Project.RemoveNotifier(FId);
  inherited Destroy;
end;

procedure TProjectData.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if FItems.Objects[I] is TDataVariantItem then
      FItems.Objects[I].Free;
  FItems.Clear;
end;

procedure TProjectData.BeforeSave;
begin
end;

procedure TProjectData.Modified;
begin
end;

procedure TProjectData.ModuleRenamed(const NewName: String);
var
  Filename: string;
begin
  Filename := ChangeFileExt(FFileName, sSuffix);
  if FileExists(Filename) then
    RenameFile(Filename, ChangeFileExt(NewName, sSuffix));
  try
    FOwner.ProjectRenamed(Self, FFilename, NewName);
  finally
    FFilename := NewName;
  end;
end;

function TProjectData.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TProjectData.Destroyed;
begin
  if FId <> -1 then
  begin
    try
      FOwner.ProjectDestroying(Self);
    except
      Application.HandleException(Self);
    end;
    Project.RemoveNotifier(FId);
  end;
  FProject := nil;
  FId := -1;
end;

procedure TProjectData.AfterSave;
var
  Doc: IXmlDocument;
  Filename: string;
  Node: IXmlNode;
  I: Integer;
begin
  if not AllowSaveData then
    Exit;

  Filename := ChangeFileExt(Project.FileName, sSuffix);

  {if IsReadOnly(Filename) then // we can't overwrite a read only file
    Exit;}
  try
    Doc := NewXMLDocument;
    Doc.DocumentElement := Doc.CreateElement('Project', '');
    Node := Doc.DocumentElement.AddChild('Options');
    for I := 0 to FItems.Count - 1 do
      Node.AddChild(FItems[I]).Attributes['Value'] := (FItems.Objects[I] as TDataVariantItem).Value;
    FOwner.Saving(Self, Doc.DocumentElement);

    if (Node.ChildNodes.Count > 0) or (Doc.DocumentElement.ChildNodes.Count > 1) then
      Doc.SaveToFile(Filename)
    else
      DeleteFile(Filename);
  except
    Application.HandleException(Self);
  end;
end;

procedure TProjectData.Reload;
var
  Doc: IXmlDocument;
  Filename: string;
  Node: IXmlNode;
  I: Integer;
begin
  if FLoading then
    Exit;

  Filename := ChangeFileExt(Project.FileName, sSuffix);
  if FileExists(Filename) then
  begin
    try
      FLoading := True;
      try
        Doc := LoadXmlDocument(Filename);
        Node := Doc.DocumentElement.ChildNodes.FindNode('Options');
        if Node <> nil then
        begin
          Clear;
          for I := 0 to Node.ChildNodes.Count - 1 do
            Values[Node.ChildNodes[I].NodeName] := Node.ChildNodes[I].Attributes['Value'];
        end;
        FOwner.Loading(Self, Doc.DocumentElement);
      finally
        FLoading := False;
      end;
    except
      Application.HandleException(Self);
      Exit;
    end;
  end;
end;

function TProjectData.GetValue(const Name: string): Variant;
var
  Index: Integer;
begin
  Index := FItems.IndexOf(Name);
  if Index = -1 then
    Result := Null
  else
  if FItems.Objects[Index] is TDataVariantItem then
    Result := TDataVariantItem(FItems.Objects[Index]).Value
  else
    Result := Null;
end;

function TProjectData.HasValue(const Name: string): Boolean;
begin
  Result := FItems.IndexOf(Name) <> -1;
end;

procedure TProjectData.SetValue(const Name: string; const Value: Variant);
var
  Index: Integer;
begin
  Index := FItems.IndexOf(Name);
  if Index = -1 then
  begin
    FItems.AddObject(Name, TDataVariantItem.Create(Value));
    if not Loading then
    begin
      {$IFDEF COMPILER6_UP}
      Project.MarkModified;
      {$ELSE}
      Project.ProjectOptions.ModifiedState := True;
      {$ENDIF COMPILER6_UP}
    end;
  end
  else
  if FItems.Objects[Index] is TDataVariantItem then
  begin
    if TDataVariantItem(FItems.Objects[Index]).Value <> Value then
    begin
      TDataVariantItem(FItems.Objects[Index]).Value := Value;
      if not Loading then
      begin
        {$IFDEF COMPILER6_UP}
        Project.MarkModified;
        {$ELSE}
        Project.ProjectOptions.ModifiedState := True;
        {$ENDIF COMPILER6_UP}
      end;
    end;
  end;
end;

function TProjectData.GetNonPersistent(const Name: string): TObject;
var
  Index: Integer;
begin
  Index := FNonPersistents.IndexOf(Name);
  if Index <> -1 then
    Result := FNonPersistents.Objects[Index]
  else
    Result := nil;
end;

procedure TProjectData.SetNonPersistent(const Name: string; const Value: TObject);
var
  Index: Integer;
begin
  Index := FNonPersistents.IndexOf(Name);
  if Index <> -1 then
    FNonPersistents.Objects[Index] := Value
  else
    FNonPersistents.AddObject(Name, Value);
end;

{ TProjectDataTransaction }

constructor TProjectDataTransaction.Create(AOwner: TProjectDataList);
var
  I: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  FProjects := TList.Create;
  FItems := TObjectList.Create;

  for I := 0 to AOwner.FList.Count - 1 do
  begin
    FProjects.Add(Pointer(TProjectData(AOwner.FList[I]).Project));
//    FItems.Add(
//    FProjects.
  end;
end;

destructor TProjectDataTransaction.Destroy;
begin
  FProjects.Free;
  inherited Destroy;
end;

procedure TProjectDataTransaction.Rollback;
begin

end;

{ TProjectDataNotifier }

constructor TProjectDataNotifier.Create;
begin
  inherited Create;
  ProjectDataList.AddNotifier(Self);
end;

destructor TProjectDataNotifier.Destroy;
begin
  ProjectDataList.RemoveNotifier(Self);
  inherited Destroy;
end;

initialization

finalization
  FreeAndNil(GlobalProjectDataList);

end.

