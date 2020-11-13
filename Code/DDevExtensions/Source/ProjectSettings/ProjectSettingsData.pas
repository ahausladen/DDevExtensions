{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit ProjectSettingsData;

{$I ..\DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  Windows, SysUtils, Classes, Contnrs, ToolsAPI, SimpleXmlImport, SimpleXmlIntf,
  TypInfo, Utils;

type
  TSettingCopyMode = (scCopyDefault, scCopyAll, scAssign);

  TSettingsOption = class(TObject)
  private
    FName: string;
    FValue: Variant;
    FActive: Boolean;
  public
    constructor Create(const AName: string; const AValue: Variant);
    function Clone: TSettingsOption; virtual;

    property Name: string read FName;
    property Value: Variant read FValue write FValue;
    property Active: Boolean read FActive write FActive;
  end;

  TProjectSetting = class(TPersistent)
  private
    FOptions: TObjectList;
    FName: string;
    function GetOption(const Name: string): Variant;
    procedure SetOption(const Name: string; const Value: Variant);
    function GetCount: Integer;
    function GetItem(Index: Integer): TSettingsOption;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToXml(Xml: IXmlNode);
    procedure LoadFromXml(Xml: IXmlNode);

    procedure Assign(Source: TPersistent); override;
    function Compare(Setting: TProjectSetting): Boolean;

    procedure CopyFrom(Source: TProjectSetting); overload;
    procedure CopyFrom(Source: TProjectSetting; const ExceptList: array of string); overload; virtual;
    procedure CopyFrom(Project: IOTAProject; CopyMode: TSettingCopyMode = scCopyDefault); overload;
    procedure CopyFrom(Project: IOTAProject; const ExceptList: array of string; CopyMode: TSettingCopyMode = scCopyDefault); overload;
    procedure CopyTo(Project: IOTAProject; CopyMode: TSettingCopyMode = scCopyDefault); overload;
    procedure CopyTo(Project: IOTAProject; const ExceptList: array of string;
      CopyMode: TSettingCopyMode = scCopyDefault); overload;

    function IndexOf(const AName: string): Integer;

    property Name: string read FName write FName;
    property Options[const Name: string]: Variant read GetOption write SetOption; default;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSettingsOption read GetItem;
  end;

  TProjectSettingList = class(TPersistent)
  private
    FItems: TObjectList;
    FCompilerOptions: TStrings;
    FVersionInfoOptions: TStrings;
    FLinkerOptions: TStrings;

    function GetCount: Integer;
    function GetItem(Index: Integer): TProjectSetting;
  protected
    class procedure InitOptions; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Delete(Index: Integer);
    procedure Remove(Setting: TProjectSetting);
    function Add: TProjectSetting;
    procedure Clear;

    class procedure FillOptionNames(List: TStrings);

    function FindEqual(Setting: TProjectSetting): TProjectSetting;

    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToXml(Xml: IXmlNode);
    procedure LoadFromXml(Xml: IXmlNode);

    procedure Assign(Source: TPersistent); override;
    function IndexOf(Setting: TProjectSetting): Integer;
    function FindByName(const AName: string): TProjectSetting;

    procedure Sort;

    property CompilerOptions: TStrings read FCompilerOptions;
    property LinkerOptions: TStrings read FLinkerOptions;
    property VersionInfoOptions: TStrings read FVersionInfoOptions;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TProjectSetting read GetItem; default;
  end;

procedure FinializeCachedSettingData;

implementation

var
  GlobalCompilerOptions: TStrings;
  GlobalLinkerOptions: TStrings;

procedure FinializeCachedSettingData;
begin
  FreeAndNil(GlobalCompilerOptions);
  FreeAndNil(GlobalLinkerOptions);
end;

{ TSettingsOption }

constructor TSettingsOption.Create(const AName: string; const AValue: Variant);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
  FActive := True;
end;

function TSettingsOption.Clone: TSettingsOption;
begin
  Result := TSettingsOption.Create(Name, Value);
  Result.Active := FActive;
end;

{ TProjectSetting }

constructor TProjectSetting.Create;
begin
  inherited Create;
  FOptions := TObjectList.Create;
end;

destructor TProjectSetting.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TProjectSetting.Assign(Source: TPersistent);
begin
  if Source is TProjectSetting then
  begin
    FName := TProjectSetting(Source).Name;
    CopyFrom(TProjectSetting(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TProjectSetting.CopyFrom(Source: TProjectSetting; const ExceptList: array of string);
var
  i, k: Integer;
  Ignore: Boolean;
begin
  FOptions.Clear;
  if Source <> nil then
  begin
    for i := 0 to Source.Count - 1 do
    begin
      Ignore := False;
      for k := 0 to High(ExceptList) do
      begin
        if AnsiCompareText(Source.Items[i].Name, ExceptList[k]) = 0 then
        begin
          Ignore := True;
          Break;
        end;
      end;
      if not Ignore then
        FOptions.Add(Source.Items[i].Clone);
    end;
  end;
end;

procedure TProjectSetting.CopyFrom(Source: TProjectSetting);
begin
  CopyFrom(Source, []);
end;

procedure TProjectSetting.CopyFrom(Project: IOTAProject; CopyMode: TSettingCopyMode);
begin
  CopyFrom(Project, [], CopyMode);
end;

procedure TProjectSetting.CopyFrom(Project: IOTAProject; const ExceptList: array of string;
  CopyMode: TSettingCopyMode);
var
  i, k, ListIndex: Integer;
  Ignore: Boolean;
  Names: TOTAOptionNameArray;
  List: TStrings;
  OldOptions: TObjectList;
  Item: TSettingsOption;
begin
  Names := nil;
  OldOptions := TObjectList.Create;
  {$IFDEF COMPILER5}
  for i := 0 to FOptions.Count - 1 do
    OldOptions.Add(FOptions[i]);
  {$ELSE}
  OldOptions.Assign(FOptions);
  {$ENDIF COMPILER5}
  try
    FOptions.OwnsObjects := False;
    FOptions.Clear;
    FOptions.OwnsObjects := True;
    if Project <> nil then
    begin
      Names := Project.ProjectOptions.GetOptionNames;
      List := TStringList.Create;
      try
        TProjectSettingList.FillOptionNames(List);
        for i := 0 to High(Names) do
        begin
          if Names[i].Kind in [tkClass, tkInterface, tkArray, tkRecord, tkDynArray] then
            Continue;

          Ignore := False;
          for k := 0 to High(ExceptList) do
          begin
            if AnsiCompareText(Names[i].Name, ExceptList[k]) = 0 then
            begin
              Ignore := True;
              Break;
            end;
          end;
          ListIndex := List.IndexOf(Names[i].Name);
          if not Ignore and (CopyMode <> scCopyAll) then
            Ignore := (ListIndex <> -1) and (List.Objects[ListIndex] = Pointer(2));
          if not Ignore and
             // these are Pointers:
             (Names[i].Name <> 'SysVars') and
             (Names[i].Name <> 'EnvVars') and
             (Names[i].Name <> 'Keys') then
          begin
            Item := nil;
            try
              for k := 0 to OldOptions.Count - 1 do
              begin
                if AnsiCompareText(TSettingsOption(OldOptions[k]).Name, Names[i].Name) = 0 then
                begin
                  Item := TSettingsOption(OldOptions[k]);
                  OldOptions.Extract(Item);
                  Break;
                end;
              end;
              if Item = nil then
              begin
                Item := TSettingsOption.Create(Names[i].Name, Project.ProjectOptions.Values[Names[i].Name]);
                if ListIndex <> -1 then
                  Item.Active := List.Objects[ListIndex] = nil;
              end
              else
                Item.Value := Project.ProjectOptions.Values[Names[i].Name];
            except
              Item.Free;
              Item := nil;
            end;

            if Item <> nil then
              FOptions.Add(Item);
          end;
        end;
      finally
        List.Free;
      end;
    end;
  finally
    OldOptions.Free;
  end;
end;

procedure TProjectSetting.CopyTo(Project: IOTAProject; CopyMode: TSettingCopyMode);
begin
  CopyTo(Project, [], CopyMode);
end;

procedure TProjectSetting.CopyTo(Project: IOTAProject; const ExceptList: array of string;
  CopyMode: TSettingCopyMode);
var
  i, k: Integer;
  Ignore: Boolean;
  Names: TOTAOptionNameArray;
  Index: Integer;
  List: TStrings;
  ListIndex: Integer;
begin
  Names := nil;
  if Project <> nil then
  begin
    Names := Project.ProjectOptions.GetOptionNames;
    List := TStringList.Create;
    try
      TProjectSettingList.FillOptionNames(List);
      for i := 0 to High(Names) do
      begin
        if Names[i].Name = 'Defines' then
          Write;
        if Names[i].Kind in [tkClass, tkInterface, tkArray, tkRecord, tkDynArray] then
          Continue;

        Ignore := False;
        for k := 0 to High(ExceptList) do
        begin
          if AnsiCompareText(Names[i].Name, ExceptList[k]) = 0 then
          begin
            Ignore := True;
            Break;
          end;
        end;
        ListIndex := List.IndexOf(Names[i].Name);
        if not Ignore and (CopyMode <> scCopyAll) then
          Ignore := (ListIndex <> -1) and (List.Objects[ListIndex] = Pointer(2));
        if not Ignore and
           // these are Pointers:
           (Names[i].Name <> 'SysVars') and
           (Names[i].Name <> 'EnvVars') and
           (Names[i].Name <> 'Keys') then
        begin
          Index := IndexOf(Names[i].Name);
          if (Index <> -1) and ((CopyMode <> scAssign) or Items[Index].Active) then
            Project.ProjectOptions.Values[Names[i].Name] := Items[Index].Value;
        end;
      end;
    finally
      List.Free;
    end;
  end;
end;

function TProjectSetting.GetCount: Integer;
begin
  Result := FOptions.Count;
end;

function TProjectSetting.GetItem(Index: Integer): TSettingsOption;
begin
  Result := TSettingsOption(FOptions[Index]);
end;

function TProjectSetting.GetOption(const Name: string): Variant;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if AnsiCompareText(Items[i].Name, Name) = 0 then
    begin
      Result := Items[i].Value;
      Exit;
    end;
  end;
  Result := Null;
end;

procedure TProjectSetting.SetOption(const Name: string; const Value: Variant);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if AnsiCompareText(Items[i].Name, Name) = 0 then
    begin
      Items[i].Value := Value;
      Exit;
    end;
  end;
end;

function TProjectSetting.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, AName) = 0 then
      Exit;
  Result := -1;
end;

function TProjectSetting.Compare(Setting: TProjectSetting): Boolean;
var
  i: Integer;
begin
  Result := Setting <> nil;
  if Result then
  begin
    Result := False;
    for i := 0 to Count - 1 do
      if Items[i].Active and (Setting.Options[Items[i].Name] <> Items[i].Value) then
        Exit;
    Result := True;
  end;
end;

procedure TProjectSetting.LoadFromXml(Xml: IXmlNode);
var
  i: Integer;
  Item: TSettingsOption;
  Node: IXmlNode;
begin
  FName := Xml.Attributes['Name'];
  FOptions.Clear;
  for i := 0 to Xml.ChildNodes.Count - 1 do
  begin
    Node := Xml.ChildNodes[i];
    Item := TSettingsOption.Create(Node.NodeName, Node.Attributes['Value']);
    if (Node.Attributes['Active'] <> Null) and (Node.Attributes['Active'] <> '') then
      Item.Active := Node.Attributes['Active'];
    FOptions.Add(Item);
  end;
end;

procedure TProjectSetting.SaveToXml(Xml: IXmlNode);
var
  i: Integer;
  Node: IXmlNode;
begin
  Xml.Attributes['Name'] := FName;
  for i := 0 to Count - 1 do
  begin
    Node := Xml.AddChild(Items[i].Name);
    if Items[i].Value <> Null then
      Node.Attributes['Value'] := Items[i].Value;
    Node.Attributes['Active'] := Items[i].Active;
  end;
end;

{ TProjectSettingList }

constructor TProjectSettingList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TProjectSettingList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TProjectSettingList.Add: TProjectSetting;
begin
  Result := TProjectSetting.Create;
  FItems.Add(Result);
end;

procedure TProjectSettingList.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if (Source is TProjectSettingList) or (Source = nil) then
  begin
    FItems.Clear;
    if Source <> nil then
      for i := 0 to TProjectSettingList(Source).Count - 1 do
        Add.Assign(TProjectSettingList(Source).Items[i]);
  end;
end;

procedure TProjectSettingList.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

function TProjectSettingList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TProjectSettingList.GetItem(Index: Integer): TProjectSetting;
begin
  Result := TProjectSetting(FItems[Index]);
end;

procedure TProjectSettingList.Clear;
begin
  FItems.Clear;
end;

function TProjectSettingList.IndexOf(Setting: TProjectSetting): Integer;
begin
  Result := FItems.IndexOf(Setting);
end;

class procedure TProjectSettingList.InitOptions;
begin
  with GlobalCompilerOptions do
  begin
    // Delphi
{    Add('Align');
    Add('BoolEval');
    Add('Assertions');
    Add('UnitDebugInfo');
    Add('ImportedData');
    Add('LongStrings');
    Add('IOChecks');
    Add('WriteableConst');
    Add('LocalSymbols');
    Add('TypeInfo');
    Add('Optimization');
    Add('OpenStrings');
    Add('OverflowChecks');
    Add('RangeChecks');
    Add('StackChecks');
    Add('TypedAddress');
    Add('SafeDivide');
    Add('VarStringChecks');
    Add('StackFrames');
    Add('ExtendedSyntax');
    Add('ReferenceInfo');
    Add('MinEnumSize');
    Add('OutputObj');
    Add('HintFlag');
    Add('WarnFlag');
    Add('UnitAliases');
    Add('Defines');
    Add('SysDefines');
    Add('NamespacePrefix');

    Add('WarnSymbolDeprecated');
    Add('WarnSymbolLibrary');
    Add('WarnSymbolPlatform');
    Add('WarnUnitLibrary');
    Add('WarnUnitPlatform');
    Add('WarnUnitDeprecated');
    Add('WarnHresultCompat');
    Add('WarnHidingMember');
    Add('WarnHiddenVirtual');
    Add('WarnGarbage');
    Add('WarnBoundsError');
    Add('WarnZeroNilCompat');
    Add('WarnStringConstTrunced');
    Add('WarnForLoopVarVarpar');
    Add('WarnTypedConstVarpar');
    Add('WarnAsgToTypedConst');
    Add('WarnCaseLabelRange');
    Add('WarnForVariable');
    Add('WarnConstructingAbstract');
    Add('WarnComparisonFalse');
    Add('WarnComparisonTrue');
    Add('WarnComparingSignedUnsigned');
    Add('WarnCombiningSignedUnsigned');
    Add('WarnUnsupportedConstruct');
    Add('WarnFileOpen');
    Add('WarnFileOpenUnitsrc');
    Add('WarnBadGlobalSymbol');
    Add('WarnDuplicateCtorDtor');
    Add('WarnInvalidDirective');
    Add('WarnPackageNoLink');
    Add('WarnPackagedThreadvar');
    Add('WarnImplicitImport');
    Add('WarnHppemitIgnored');
    Add('WarnNoRetval');
    Add('WarnUseBeforeDef');
    Add('WarnForLoopVarUndef');
    Add('WarnUnitNameMismatch');
    Add('WarnNoCfgFileFound');
    Add('WarnMessageDirective');
    Add('WarnImplicitVariants');
    Add('WarnUnicodeToLocale');
    Add('WarnLocaleToUnicode');
    Add('WarnImagebaseMultiple');
    Add('WarnSuspiciousTypecast');
    Add('WarnPrivatePropaccessor');
    Add('WarnUnsafeType');
    Add('WarnUnsafeCode');
    Add('WarnUnsafeCast');}

    AddObject('OutputDir', Pointer(1));
    AddObject('UnitOutputDir', Pointer(1));
    AddObject('UnitDir', Pointer(1));
    AddObject('ObjDir', Pointer(1));
    AddObject('SrcDir', Pointer(1));
    AddObject('ResDir', Pointer(1));
    AddObject('PkgDllDir', Pointer(1));
    AddObject('PkgDcpDir', Pointer(1));

    // C++
{    Add('CppDebugInfo');
    Add('LineNumbers');
    Add('AutoRegVars');
    Add('MergeDupStrs');
    Add('EnableInLines');
    Add('ShowWarnings');
    Add('StdStackFrame');
    Add('TreatEnumsAsInts');
    Add('PCH');
    Add('ShowInfoMsgs');
    Add('ShowExtendedMsgs');
    Add('InstructionSet');     
    Add('Alignment');
    Add('CallingConvention');
    Add('RegisterVars');
    Add('Ansi');
    Add('AutoDep');
    Add('Underscore');
    Add('PICCodeGen');
    Add('FastFloat');
    Add('PentiumFloat');
    Add('NestedComments');
    Add('MFCCompat');
    Add('IdentLen');
    Add('MemberPrecision');
    Add('ForLoops');
    Add('TwoChar');
    Add('CodeModifiers');
    Add('EnableRTTI');
    Add('EnableExceptions');
    Add('EHLocalInfo');
    Add('EHDtor');
    Add('EHPrologs');
    Add('ZeroBaseClass');
    Add('ZeroClassFunction');
    Add('ForceCppCompile');
    Add('MemberPointer');
    Add('VTables');
    Add('Templates');
    Add('PchPath');
    Add('PchStopAfter');
    Add('ATLMultiUse');
    Add('ATLDebugQI');
    Add('ATLCoinitMultiThreaded');
    Add('ATLAutoRegisterInproc');
    Add('ATLDebugRefCount');
    Add('ATLDebug');
    Add('ATLThreading');
    Add('CodeOpt');
    Add('FloatSupport');
    Add('TasmViaCppOpts');
    Add('TasmCrossReference');
    Add('TasmSymbolTables');
    Add('TasmGenerateListing');
    Add('TasmIncludeConditionals');
    Add('TasmIncludeErrors');
    Add('TasmExpanded');
    Add('TasmCaseCheckingOn');
    Add('TasmAllCase');
    Add('TasmDebugOn');
    Add('TasmFullDebug');
    Add('TasmWarningsOn');
    Add('TasmWarningsLevel1');
    Add('TasmHashTable');
    Add('TasmPasses');
    Add('TasmSymbolLength');
    Add('TasmDirective');
    Add('CGGlobalStackAccesses');
    Add('CGThisPointer');
    Add('CGInlinePointer');
    Add('CGLinkCGLib');}
  end;

  with GlobalLinkerOptions do
  begin
    AddObject('HostApplication', Pointer(1));
    AddObject('RunParams', Pointer(1));
    AddObject('Launcher', Pointer(1));
    AddObject('UseLauncher', Pointer(1));
    AddObject('DebugCWD', Pointer(1));
    AddObject('RemoteHost', Pointer(1));
    AddObject('RemotePath', Pointer(1));
    AddObject('RemoteLauncher', Pointer(1));
    AddObject('RemoteCWD', Pointer(1));
    AddObject('RemoteDebug', Pointer(1));

    // Delphi
{    Add('StackSize');
    Add('MaxStackSize');
    Add('MapFile');
    Add('DebugInfo');
    Add('RemoteSymbols');
    Add('ImageDebugInfo');
    Add('GenDRC');
    Add('GenDUI');
    Add('HeapSize');
    Add('MaxHeapSize');}

    AddObject('SOName', Pointer(1));
    AddObject('SOPrefix', Pointer(1));
    AddObject('SOPrefixDefined', Pointer(1));
    AddObject('SOSuffix', Pointer(1));
    AddObject('SOVersion', Pointer(1));
    AddObject('UsePackages', Pointer(1));
    AddObject('Packages', Pointer(1));
    AddObject('ExeDescription', Pointer(1));
    AddObject('ImplicitBuild', Pointer(1));
    AddObject('RuntimeOnly', Pointer(1));
    AddObject('DesigntimeOnly', Pointer(1));
    AddObject('DebugSourcePath', Pointer(1));

    AddObject('AutoIncBuildNum', Pointer(2));
    AddObject('Build', Pointer(2));
    AddObject('CodePage', Pointer(2));
    AddObject('IncludeVersionInfo', Pointer(2));
    AddObject('Locale', Pointer(2));
    AddObject('MajorVersion', Pointer(2));
    AddObject('MinorVersion', Pointer(2));
    AddObject('ModuleAttribs', Pointer(2));
    AddObject('Release', Pointer(2));

    // C++ ilink32
{    Add('LinkMaxErrors');
    Add('LinkShowMangle');
    Add('LinkGenImportLib');
    Add('LinkGenLib');
    Add('LinkNoStateFiles');
    Add('LinkSubsysMajor');
    Add('LinkSubsysMinor');
    Add('LinkCaseSensitiveLink');
    Add('LinkCalculateChecksum');
    Add('LinkFastTLS');
    Add('LinkReplaceResources');
    Add('LinkUserMajor');
    Add('LinkUserMinor');
    Add('LinkImageComment');
    Add('LinkDelayLoad');
    Add('ShowLinkerWarnings');
    Add('LinkDebugVcl');
    Add('UseDynamicRtl');
    Add('MultiThreaded');}

    AddObject('IncludePath', Pointer(1));
    AddObject('LibPath', Pointer(1));
    AddObject('DebugPath', Pointer(1));
    AddObject('ReleasePath', Pointer(1));
    AddObject('LibraryList', Pointer(1));

    // C++ tlib
{    Add('CaseSensitive');
    Add('ExtendedDictionary');
    Add('PurgeComment');
    Add('PageSize');}
    AddObject('ListFile', Pointer(1));
  end;
end;

class procedure TProjectSettingList.FillOptionNames(List: TStrings);
begin
  if not Assigned(GlobalCompilerOptions) then
  begin
    GlobalCompilerOptions := TStringList.Create;
    GlobalLinkerOptions := TStringList.Create;
    InitOptions;
  end;
  List.Clear;
  List.AddStrings(GlobalCompilerOptions);
  List.AddStrings(GlobalLinkerOptions);
end;

function TProjectSettingList.FindByName(const AName: string): TProjectSetting;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if AnsiCompareText(Result.Name, AName) = 0 then
      Exit;
  end;
  Result := nil;
end;

procedure TProjectSettingList.Remove(Setting: TProjectSetting);
begin
  FItems.Remove(Setting);
end;

function TProjectSettingList.FindEqual(Setting: TProjectSetting): TProjectSetting;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if Result.Compare(Setting) then
      Exit;
  end;
  Result := nil;
end;

procedure TProjectSettingList.LoadFromFile(const Filename: string);
begin
  LoadFromXml(LoadXmlDocument(Filename).Root);
end;

procedure TProjectSettingList.SaveToFile(const Filename: string);
var
  Doc: IXmlDocument;
begin
  Doc := NewXMLDocument;
  Doc.DocumentElement := Doc.CreateElement('ProjectSettings', '');
  SaveToXml(Doc.DocumentElement);

  ForceDirectories(ExtractFileDir(Filename));
  Doc.SaveToFile(Filename);
end;

procedure TProjectSettingList.LoadFromXml(Xml: IXmlNode);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Xml.ChildNodes.Count - 1 do
    if AnsiCompareText(Xml.ChildNodes[i].NodeName, 'ProjectSetting') = 0 then
      Add.LoadFromXml(Xml.ChildNodes[i]);
end;

procedure TProjectSettingList.SaveToXml(Xml: IXmlNode);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].SaveToXml(Xml.AddChild('ProjectSetting'));
end;

function SortSettings(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TProjectSetting(Item1).Name, TProjectSetting(Item2).Name);
end;

procedure TProjectSettingList.Sort;
begin
  FItems.Sort(SortSettings);
end;

end.
