{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit ModuleData;

interface

uses
  SysUtils, Classes, Contnrs, ToolsAPI, Forms, IDENotifiers;

type
  TModuleDataList = class;

  TModuleData = class(TInterfacedObject, IOTANotifier, IOTAModuleNotifier)
  private
    FOwner: TModuleDataList;
    FId: Integer;
    FModule: IOTAModule;
    FBucket: TBucketList;
    FFilename: string;
    function GetBucket(Index: TObject): TObject;
    procedure SetBucket(Index: TObject; const Value: TObject);
  protected
    { IOTAModuleNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    function CheckOverwrite: Boolean;
    procedure Destroyed;
    procedure Modified;
    procedure ModuleRenamed(const NewName: String);
  public
    constructor Create(AOwner: TModuleDataList; AModule: IOTAModule);
    destructor Destroy; override;

    property Bucket[Index: TObject]: TObject read GetBucket write SetBucket;

    property Module: IOTAModule read FModule;
    property Filename: string read FFilename;
  end;


  TModuleDataEvent = procedure(Data: TModuleData) of object;
  TModuleDataRenamedEvent = procedure(Data: TModuleData; const NewName: string) of object;

  TModuleDataNotifier = class(TObject)
  public
    Added: TModuleDataEvent;
    Destroying: TModuleDataEvent;
    BeforeSave: TModuleDataEvent;
    AfterSave: TModuleDataEvent;
    Modified: TModuleDataEvent;
    Renamed: TModuleDataRenamedEvent;

    constructor Create;
    destructor Destroy; override;
  end;

  TModuleDataList = class(TObject)
  private
    FList: TObjectList;
    FNotifiers: TList;
    FIDENotifier: TIDENotifier;
    function GetModuleData(const Module: IOTAModule): TModuleData;
    function GetNotifier(Index: Integer): TModuleDataNotifier;
    function GetNotifierCount: Integer;
  protected
    procedure UpdateModules;

    procedure ModuleAdded(Data: TModuleData); virtual;
    procedure ModuleDestroying(Data: TModuleData); virtual;
    procedure ModuleBeforeSave(Data: TModuleData); virtual;
    procedure ModuleAfterSave(Data: TModuleData); virtual;
    procedure ModuleModified(Data: TModuleData); virtual;
    procedure ModuleRenamed(Data: TModuleData; const NewName: string); virtual;

    property NotifierCount: Integer read GetNotifierCount;
    property Notifiers[Index: Integer]: TModuleDataNotifier read GetNotifier;

    procedure AddNotifier(ANotifier: TModuleDataNotifier);
    procedure RemoveNotifier(ANotifier: TModuleDataNotifier);
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
      var Cancel: Boolean);
  public
    constructor Create; 
    destructor Destroy; override;

    property ModuleData[const Module: IOTAModule]: TModuleData read GetModuleData; default;
  end;

function ModuleDataList: TModuleDataList;

implementation

var
  GlobalModuleDataList: TModuleDataList;

function ModuleDataList: TModuleDataList;
begin
  if not Assigned(GlobalModuleDataList) then
    GlobalModuleDataList := TModuleDataList.Create;
  Result := GlobalModuleDataList;
end;

{ TModuleDataList }

constructor TModuleDataList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FNotifiers := TList.Create;
  UpdateModules;
  FIDENotifier := TIDENotifier.Create;
  FIDENotifier.OnFileNotification := FileNotification;
end;

destructor TModuleDataList.Destroy;
begin
  FIDENotifier.Free;
  FNotifiers.Free;
  FList.Free;
  inherited Destroy;
end;

function TModuleDataList.GetModuleData(const Module: IOTAModule): TModuleData;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Result := TModuleData(FList[i]);
    if Result.Module = Module then
      Exit;
  end;
  Result := TModuleData.Create(Self, Module);
end;

procedure TModuleDataList.ModuleAdded(Data: TModuleData);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[i].Added) then
      Notifiers[i].Added(Data);
end;

procedure TModuleDataList.ModuleDestroying(Data: TModuleData);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[i].Destroying) then
      Notifiers[i].Destroying(Data);
end;

procedure TModuleDataList.ModuleBeforeSave(Data: TModuleData);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[i].BeforeSave) then
      Notifiers[i].BeforeSave(Data);
end;

procedure TModuleDataList.ModuleAfterSave(Data: TModuleData);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[i].AfterSave) then
      Notifiers[i].AfterSave(Data);
end;

procedure TModuleDataList.ModuleModified(Data: TModuleData);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[i].Modified) then
      Notifiers[i].Modified(Data);
end;

procedure TModuleDataList.ModuleRenamed(Data: TModuleData; const NewName: string);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
    if Assigned(Notifiers[i].Renamed) then
      Notifiers[i].Renamed(Data, NewName);
end;

procedure TModuleDataList.AddNotifier(ANotifier: TModuleDataNotifier);
begin
  FNotifiers.Add(ANotifier);
end;

procedure TModuleDataList.RemoveNotifier(ANotifier: TModuleDataNotifier);
begin
  FNotifiers.Extract(ANotifier);
end;

function TModuleDataList.GetNotifier(Index: Integer): TModuleDataNotifier;
begin
  Result := TModuleDataNotifier(FNotifiers[Index]);
end;

function TModuleDataList.GetNotifierCount: Integer;
begin
  Result := FNotifiers.Count;
end;

procedure TModuleDataList.UpdateModules;
var
  Modules: IOTAModuleServices;
  i: Integer;
begin
  { Create TModuleData objects for all opened modules }
  if Supports(BorlandIDEServices, IOTAModuleServices, Modules) then
  begin
    try
      for i := 0 to Modules.ModuleCount - 1 do
        ModuleData[Modules.Modules[i]];
    except
      // catch Delphi 5 exceptions
    end;
  end;
end;

procedure TModuleDataList.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  case NotifyCode of
    ofnFileOpened:
      UpdateModules;
  end;
end;

{ TModuleData }

constructor TModuleData.Create(AOwner: TModuleDataList; AModule: IOTAModule);
begin
  inherited Create;
  FOwner := AOwner;
  FOwner.FList.Add(Self);
  FModule := AModule;
  FFilename := Module.FileName;
  FId := Module.AddNotifier(Self);
end;

destructor TModuleData.Destroy;
begin
  if FId <> -1 then
  begin
    try
      FOwner.ModuleDestroying(Self);
    except
      Application.HandleException(Self);
    end;
  end;
  FOwner.FList.Extract(Self);
  FBucket.Free;

  if FId <> -1 then
    Module.RemoveNotifier(FId);
  inherited Destroy;
end;

procedure TModuleData.Destroyed;
begin
  if FId <> -1 then
  begin
    try
      FOwner.ModuleDestroying(Self);
    except
      Application.HandleException(Self);
    end;
    Module.RemoveNotifier(FId);
  end;
  FModule := nil;
  FId := -1;
end;

function TModuleData.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TModuleData.BeforeSave;
begin
  FOwner.ModuleBeforeSave(Self);
end;

procedure TModuleData.AfterSave;
begin
  FOwner.ModuleAfterSave(Self);
end;

procedure TModuleData.Modified;
begin
  FOwner.ModuleModified(Self);
end;

procedure TModuleData.ModuleRenamed(const NewName: String);
begin
  FOwner.ModuleRenamed(Self, NewName);
  FFilename := NewName;
end;

function TModuleData.GetBucket(Index: TObject): TObject;
begin
  if not Assigned(FBucket) or not FBucket.Find(Index, Pointer(Result)) then
    Result := nil;
end;

procedure TModuleData.SetBucket(Index: TObject; const Value: TObject);
begin
  if not Assigned(FBucket) then
  begin
    if Value = nil then
      Exit;
    FBucket := TBucketList.Create(bl128);
  end;
  if FBucket.Exists(Index) then
  begin
    if Value = nil then
      FBucket.Remove(Index)
    else
      FBucket.Data[Index] := Value;
  end
  else
  if Value <> nil then
    FBucket.Add(Index, Value);
end;

{ TModuleDataNotifier }

constructor TModuleDataNotifier.Create;
begin
  inherited Create;
  ModuleDataList.AddNotifier(Self);
end;

destructor TModuleDataNotifier.Destroy;
begin
  ModuleDataList.RemoveNotifier(Self);
  inherited Destroy;
end;

initialization

finalization
  FreeAndNil(GlobalModuleDataList);

end.
