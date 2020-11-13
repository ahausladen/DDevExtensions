{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit ComponentManager;

{$I DelphiExtension.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs, Controls, IDEHooks, ToolsAPI,
  {$IFDEF COMPILER5}
  Consts,
  {$ELSE}
  RTLConsts,
  {$ENDIF COMPILER5}
  Hooking;

type
  {$IFDEF COMPILER5}
  PBoolean = ^Boolean;
  {$ENDIF COMPILER5}

  IComponentChangeNotifier = interface
    ['{CFB82019-0197-4551-9E63-D5C9C5D62B19}']
    procedure ComponentsChanged;
  end;

  TRegisteredComponents = class(TObject)
  private
    FPalettes: TStrings;
    FComponentClasses: TClassList;
    FComponents: TStrings;
    FComponentNotifiers: TInterfaceList;
    FLastModifyCount: Cardinal;
    function GetPalette(Index: Integer): string;
    function GetPaletteCount: Integer;
    function GetComponentClass(Index: Integer): TComponentClass;
    function GetComponentCount: Integer;
    function GetComponentModule(Index: Integer): HMODULE;
  protected
    procedure ComponentsChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure RegisterComponents(const Palette: string; const AComponentClasses: array of TComponentClass);
    procedure DeletePackageComponents(Module: HMODULE);

    class function IsInActiveControlGroup(ComponentClass: TComponentClass): Boolean;

    function CreateComponentByNameList(PaletteIndex: Integer): TStrings;
    function PaletteOf(ComponentClass: TComponentClass): Integer; overload;
    function PaletteOf(const ComponentName: string): Integer; overload;
    function FindModule(ComponentClass: TComponentClass): HMODULE;
    function FindComponentClass(const ComponentName: string): TComponentClass;

    property PaletteCount: Integer read GetPaletteCount;
    property Palettes[Index: Integer]: string read GetPalette;

    property ComponentCount: Integer read GetComponentCount;
    property ComponentClasses[Index: Integer]: TComponentClass read GetComponentClass;
    property ComponentModules[Index: Integer]: HMODULE read GetComponentModule;

    procedure AddNotifier(Notifier: IComponentChangeNotifier);
    procedure RemoveNotifier(Notifier: IComponentChangeNotifier);

    property LastModifyCount: Cardinal read FLastModifyCount;
  end;

function RegisteredComponents: TRegisteredComponents;
function LoadComponentBitmap(ComponentClass: TComponentClass; IsDefault: PBoolean = nil): HBitmap;

procedure InitComponentManager;
procedure FiniComponentManager;

implementation

uses
  Main, IDEUtils;

var
  GlobalRegisteredComponents: TRegisteredComponents;

function RegisteredComponents: TRegisteredComponents;
begin
  if not Assigned(GlobalRegisteredComponents) then
    GlobalRegisteredComponents := TRegisteredComponents.Create;
  Result := GlobalRegisteredComponents;
end;


function LoadComponentBitmap(ComponentClass: TComponentClass; IsDefault: PBoolean): HBitmap;
var
  c: TClass;
  ClsName: string;
begin
  c := ComponentClass;
  Result := 0;
  while (Result = 0) and (c <> nil) and (c <> TComponent) and (c <> TControl) do
  begin
    ClsName := string(c.ClassName);
    Result := Windows.LoadBitmap(RegisteredComponents.FindModule(TComponentClass(c)), PChar(ClsName));
    if Result = 0 then
      Result := Windows.LoadBitmap(ModuleFromAddr(c.ClassInfo), PChar(ClsName));

    c := c.ClassParent;
  end;
  if Result = 0 then
  begin
    Result := Windows.LoadBitmap(GetModuleHandle(delphicoreide_bpl), 'DEFAULT');
    if IsDefault <> nil then
      IsDefault^ := True;
  end
  else
  if IsDefault <> nil then
    IsDefault^ := False;
end;

{ TRegisteredComponents }

constructor TRegisteredComponents.Create;
begin
  inherited Create;
  FPalettes := TStringList.Create;
  FComponents := TStringList.Create;
  FComponentClasses := TClassList.Create;
  FComponentNotifiers := TInterfaceList.Create;
end;

destructor TRegisteredComponents.Destroy;
begin
  FComponentNotifiers.Clear;
  Clear;
  FPalettes.Free;
  FComponents.Free;
  FComponentClasses.Free;
  FComponentNotifiers.Free;
  inherited Destroy;
end;

function TRegisteredComponents.GetPalette(Index: Integer): string;
begin
  Result := FPalettes[Index];
end;

function TRegisteredComponents.GetPaletteCount: Integer;
begin
  Result := FPalettes.Count;
end;

{$IFDEF COMPILER6_UP}
class function TRegisteredComponents.IsInActiveControlGroup(
  ComponentClass: TComponentClass): Boolean;
var
  Group: TPersistentClass;
begin
  if ComponentClass <> nil then
  begin
    Result := True;
    Group := ClassGroupOf(ComponentClass);
    if (Group <> nil) and Group.ClassNameIs('TControl'){Group.ClassType = TControl} then
    begin
      if (BorlandIDEServices as IOTAServices).GetActiveDesignerType = dVCL then
      begin
        if Group <> ClassGroupOf(TControl) then
          Result := False;
      end
      else
        if Group = ClassGroupOf(TControl) then
          Result := False;
    end;
  end
  else
    Result := False;
end;
{$ELSE}
class function TRegisteredComponents.IsInActiveControlGroup(
  ComponentClass: TComponentClass): Boolean;
begin
  Result := True;
end;
{$ENDIF COMPILER6_UP}

function TRegisteredComponents.GetComponentClass(Index: Integer): TComponentClass;
begin
  Result := TComponentClass(FComponentClasses[Index]);
end;

function TRegisteredComponents.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

function TRegisteredComponents.CreateComponentByNameList(PaletteIndex: Integer): TStrings;
begin
  Result := TStringList.Create;
  try
    Result.Assign(TStrings(FPalettes.Objects[PaletteIndex]));
  except
    Result.Free;
    raise;
  end;
end;

procedure TRegisteredComponents.DeletePackageComponents(Module: HMODULE);
var
  i, k: Integer;
  CompList: TStrings;
begin
  for i := FPalettes.Count - 1 downto 0 do
  begin
    CompList := TStrings(FPalettes.Objects[i]);
    for k := CompList.Count - 1 downto 0 do
      if CompList.Objects[k] = Pointer(Module) then
        CompList.Delete(k);
    if CompList.Count = 0 then
      FPalettes.Delete(i);
  end;
  for i := FComponents.Count - 1 downto 0 do
  begin
    if FComponents.Objects[i] = Pointer(Module) then
    begin
      FComponents.Delete(i);
      FComponentClasses[i] := nil;
    end;
  end;
  FComponentClasses.Pack;
  ComponentsChanged;
end;

procedure TRegisteredComponents.RegisterComponents(const Palette: string;
  const AComponentClasses: array of TComponentClass);
var
  PaletteIndex: Integer;
  CompList: TStrings;
  i: Integer;
  Module: HMODULE;
  ClsName: string;
begin
  if Length(AComponentClasses) > 0 then
  begin
    PaletteIndex := FPalettes.IndexOf(Palette);
    if PaletteIndex = -1 then
      PaletteIndex := FPalettes.AddObject(Palette, TStringList.Create);
    CompList := TStrings(FPalettes.Objects[PaletteIndex]);

    Module := ModuleFromAddr(Caller(2));
    for i := 0 to High(AComponentClasses) do
    begin
      if (AComponentClasses[i] <> nil) and (FComponentClasses.IndexOf(AComponentClasses[i]) = -1) then
      begin
        ClsName := AComponentClasses[i].ClassName;
        CompList.AddObject(ClsName, Pointer(Module));
        FComponents.AddObject(ClsName, Pointer(Module));
        FComponentClasses.Add(AComponentClasses[i]);
      end;
    end;
    ComponentsChanged;
  end;
end;

procedure TRegisteredComponents.AddNotifier(Notifier: IComponentChangeNotifier);
begin
  if FComponentNotifiers.IndexOf(Notifier) = -1 then
    FComponentNotifiers.Add(Notifier);
end;

procedure TRegisteredComponents.RemoveNotifier(Notifier: IComponentChangeNotifier);
begin
  FComponentNotifiers.Remove(Notifier);
end;

procedure TRegisteredComponents.Clear;
var
  i: Integer;
begin
  for i := 0 to FPalettes.Count - 1 do
    FPalettes.Objects[i].Free;
  FComponents.Clear;
  FComponentClasses.Clear;
  ComponentsChanged;
end;

procedure TRegisteredComponents.ComponentsChanged;
var
  i: Integer;
begin
  Inc(FLastModifyCount);
  for i := 0 to FComponentNotifiers.Count - 1 do
    IComponentChangeNotifier(FComponentNotifiers[i]).ComponentsChanged;
end;

function TRegisteredComponents.PaletteOf(ComponentClass: TComponentClass): Integer;
var
  Index, k: Integer;
  HInstance: HINST;
  S: string;
  CompList: TStrings;
begin
  Index := FComponentClasses.IndexOf(ComponentClass);
  if Index <> -1 then
  begin
    HInstance := HINST(FComponents.Objects[Index]);
    //S := ComponentClass.ClassName;
    S := FComponents[Index];

    for Result := 0 to FPalettes.Count - 1 do
    begin
      CompList := TStrings(FPalettes.Objects[Result]);
      for k := 0 to CompList.Count - 1 do
        if SameText(CompList[k], S) and (CompList.Objects[k] = Pointer(HInstance)) then
          Exit;
    end;
  end;
  Result := -1;
end;

function TRegisteredComponents.PaletteOf(const ComponentName: string): Integer;
begin
  for Result := 0 to FPalettes.Count - 1 do
    if TStrings(FPalettes.Objects[Result]).IndexOf(ComponentName) <> -1 then
      Exit;
  Result := -1;
end;

function TRegisteredComponents.GetComponentModule(Index: Integer): HMODULE;
begin
  Result := HMODULE(FComponents.Objects[Index]);
end;

function TRegisteredComponents.FindModule(ComponentClass: TComponentClass): HMODULE;
var
  Index: Integer;
begin
  Index := FComponentClasses.IndexOf(ComponentClass);
  if Index <> -1 then
    Result := ComponentModules[Index]
  else
    Result := 0;
end;

function TRegisteredComponents.FindComponentClass(const ComponentName: string): TComponentClass;
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if ComponentClasses[i].ClassNameIs(ComponentName) then
    begin
      Result := ComponentClasses[i];
      Exit;
    end;
  end;
  Result := nil;
end;

{------------------------------------------------------------------------------}

{$IFDEF NEED_COMPONENTMANAGER}
var
  HookRegisterComponents: TRedirectCode;
{$ENDIF NEED_COMPONENTMANAGER}

procedure HookedRegisterComponents(const Page: string;
  const ComponentClasses: array of TComponentClass);
begin
  try
    RegisteredComponents.RegisterComponents(Page, ComponentClasses);
  except
  end;
  if Assigned(RegisterComponentsProc) then
    RegisterComponentsProc(Page, ComponentClasses)
  else
    raise EComponentError.CreateRes(@SRegisterError);
end;

{------------------------------------------------------------------------------}

procedure InitComponentManager;
begin
  {$IFDEF NEED_COMPONENTMANAGER}
  CodeRedirect(@RegisterComponents, @HookedRegisterComponents, HookRegisterComponents);
  {$ENDIF NEED_COMPONENTMANAGER}
end;

procedure FiniComponentManager;
begin
  {$IFDEF NEED_COMPONENTMANAGER}
  CodeRestore(HookRegisterComponents);
  {$ENDIF NEED_COMPONENTMANAGER}
end;

end.
