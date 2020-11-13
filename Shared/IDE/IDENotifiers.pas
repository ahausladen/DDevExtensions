{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit IDENotifiers;

interface

uses
  SysUtils, Classes, Contnrs, ToolsAPI;

type
  TBeforeCompileEvent = procedure(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean) of object;
  TAfterCompileEvent = procedure(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean) of object;
  TFileNotificationEvent = procedure(NotifyCode: TOTAFileNotification; const FileName: string;
      var Cancel: Boolean) of object;

  TIDENotifier = class(TObject)
  private
    FOnBeforeCompile: TBeforeCompileEvent;
    FOnFileNotification: TFileNotificationEvent;
    FOnAfterCompile: TAfterCompileEvent;
  protected
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); virtual;
    procedure AfterCompile(Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean); virtual;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property OnBeforeCompile: TBeforeCompileEvent read FOnBeforeCompile write FOnBeforeCompile;
    property OnAfterCompile: TAfterCompileEvent read FOnAfterCompile write FOnAfterCompile;
    property OnFileNotification: TFileNotificationEvent read FOnFileNotification write FOnFileNotification;
  end;

implementation

type
  TIDENotifierList = class(TComponent, IOTANotifier, IOTAIDENotifier80, IOTAIDENotifier, IOTAIDENotifier50)
  private
    FId: Integer;
    FNotifiers: TList;
    function GetNotifier(Index: Integer): TIDENotifier;
    function GetNotifierCount: Integer;
  protected
    property NotifierCount: Integer read GetNotifierCount;
    property Notifiers[Index: Integer]: TIDENotifier read GetNotifier;

    procedure AddNotifier(ANotifier: TIDENotifier);
    procedure RemoveNotifier(ANotifier: TIDENotifier);
  protected
    { IOTAIDENotifier }
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterSave;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeSave;
    procedure Destroyed;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
      var Cancel: Boolean);
    procedure Modified;

    { IOTAIDENotifier50 }
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;

    { IOTAIDENotifier80 }
    procedure AfterCompile(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

function IDENotifierList: TIDENotifierList; forward;

var
  GlobalIDENotifierList: TIDENotifierList;

function IDENotifierList: TIDENotifierList;
begin
  if not Assigned(GlobalIDENotifierList) then
    GlobalIDENotifierList := TIDENotifierList.Create;
  Result := GlobalIDENotifierList;
end;

{ TIDENotifierList }

constructor TIDENotifierList.Create;
begin
  inherited Create(nil);
  FNotifiers := TList.Create;
  FId := (BorlandIDEServices as IOTAServices).AddNotifier(Self);
end;

destructor TIDENotifierList.Destroy;
begin
  if FId <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FId);
  FNotifiers.Free;
  inherited Destroy;
end;

procedure TIDENotifierList.AddNotifier(ANotifier: TIDENotifier);
begin
  FNotifiers.Add(ANotifier);
end;

procedure TIDENotifierList.RemoveNotifier(ANotifier: TIDENotifier);
begin
  FNotifiers.Extract(ANotifier);
end;

function TIDENotifierList.GetNotifier(Index: Integer): TIDENotifier;
begin
  Result := TIDENotifier(FNotifiers[Index]);
end;

function TIDENotifierList.GetNotifierCount: Integer;
begin
  Result := FNotifiers.Count;
end;

procedure TIDENotifierList.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  i: Integer;
begin
  for i := 0 to NotifierCount - 1 do
  begin
    Notifiers[i].FileNotification(NotifyCode, FileName, Cancel);
    if Cancel then
      Break;
  end;
end;

procedure TIDENotifierList.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIDENotifierList.AfterSave;
begin
end;

procedure TIDENotifierList.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIDENotifierList.BeforeSave;
begin
end;

procedure TIDENotifierList.Destroyed;
begin
  FID := -1;
end;

procedure TIDENotifierList.Modified;
begin
end;

procedure TIDENotifierList.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
end;

procedure TIDENotifierList.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
  begin
    Notifiers[I].BeforeCompile(Project, IsCodeInsight, Cancel);
    if Cancel then
      Break;
  end;
end;

procedure TIDENotifierList.AfterCompile(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean);
var
  I: Integer;
begin
  for I := 0 to NotifierCount - 1 do
    Notifiers[I].AfterCompile(Project, Succeeded, IsCodeInsight);
end;

{ TIDENotifier }

constructor TIDENotifier.Create;
begin
  inherited Create;
  IDENotifierList.AddNotifier(Self);
end;

destructor TIDENotifier.Destroy;
begin
  IDENotifierList.RemoveNotifier(Self);
  inherited Destroy;
end;

procedure TIDENotifier.AfterCompile(Project: IOTAProject; Succeeded, IsCodeInsight: Boolean);
begin
  if Assigned(FOnAfterCompile) then
    FOnAfterCompile(project, Succeeded, IsCodeInsight);
end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if Assigned(FOnBeforeCompile) then
    FOnBeforeCompile(Project, IsCodeInsight, Cancel);
end;

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if Assigned(FOnFileNotification) then
    FOnFileNotification(NotifyCode, FileName, Cancel);
end;

initialization

finalization
  FreeAndNil(GlobalIDENotifierList);

end.

