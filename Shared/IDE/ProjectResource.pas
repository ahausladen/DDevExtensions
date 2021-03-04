{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit ProjectResource;

{$I ..\jedi\jedi.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs, ToolsAPI, TypInfo;

{$IF CompilerVersion >= 23.0} // Delphi XE2+
{$ELSE}
type
  TResFile = class(TObject)
  public
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    procedure GetIco(Name: PChar; Stream: TStream);
    procedure AddIco(Name: PChar; Stream: TStream);
    procedure RemoveIco(Name: PChar);
    function Find(ResType, Name: PChar): Pointer;
  end;

function FindProjectResource(Project: IOTAProject): IOTAProjectResource;
function UpdateProjectVersionInfo(Project: IOTAProject): Boolean;
function GetResFileFromResource(Resource: IOTAProjectResource): TResFile;
{$IFEND}

type
  TIconResourceItem = record
    Width, Height: Integer;
    Colors: Integer;
    Planes: Integer;
    BitCount: Integer;
    IsPng: Boolean;
    ImageDataSize: LongInt;
    ImageData: Pointer;
  end;

  TIconResource = class(TObject)
  private
    FStream: TStream;
    FPaintIcon: HICON;
    FPaintIconIndex: Integer;
    FFileName: string;
    function GetCount: Integer;
    function GetData: Pointer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function GetImage(Index: Integer): TIconResourceItem;
    procedure DestroyPaintIcon;
    {$IF CompilerVersion >= 23.0} // Delphi XE2+
    {$ELSE}
    procedure LoadFromResFile(Name: PChar; ResFile: TResFile);
    {$IFEND}
  protected
    property Data: Pointer read GetData;
  public
    constructor Create;
    destructor Destroy; override;

    function GetPaintIcon(Index: Integer): HICON;

    //procedure LoadFromMemory(P: PByte; Size: Integer);
    procedure LoadFromIconFile(const FileName: string);
    procedure LoadFromProjectResource(AProject: IOTAProject);
    procedure SaveToProjectResource(AProject: IOTAProject);
    procedure Clear;

    property Count: Integer read GetCount;
    property Images[Index: Integer]: TIconResourceItem read GetImage;
    property Stream: TStream read FStream;
  end;

type
  TVersion = record
    Major, Minor, Release, Build: Word;
  end;

  TProjectVersion = record
    Valid: Boolean;

    //ProductVersion: TVersion;
    FileVersion: TVersion;
    FileVersionStr: string;
    ProductVersionStr: string;
    ProductName: string;
    CompanyName: string;
    FileDescription: string;
    LegalCopyright: string;
    LegalTrademarks: string;
    InternalName: string;
    OriginalFilename: string;
    Comments: string;
  end;

function GetProjectVersion(Project: IOTAProject): TProjectVersion;
function SetProjectVersion(Project: IOTAProject; const Version: TProjectVersion; UpdateAllPlatforms: Boolean): Boolean;
procedure IncrementBuildVersion(Project: IOTAProject);
function SetVersionInfoExtraKey(AProject: IOTAProject; const AKey, AValue: string): Boolean;

implementation

{$IF CompilerVersion >= 23.0} // Delphi XE2+
uses
  Variants, CommonOptionStrs, Consts, RTLConsts, IDEUtils, IDEHooks;
{$ELSE}
uses
  IDEHooks, UnitVersionInfo,
  {$IFDEF COMPILER14_UP}
  Rtti,
  {$ENDIF COMPILER14_UP}
  IDEUtils, Consts, RTLConsts;
{$IFEND}

procedure IncrementBuildVersion(Project: IOTAProject);
var
  Version: TProjectVersion;
begin
  if Project <> nil then
  begin
    Version := GetProjectVersion(Project);
    if Version.Valid then
    begin
      Version.FileVersion.Build := Version.FileVersion.Build + 1;
      with Version.FileVersion do
        Version.FileVersionStr := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
      SetProjectVersion(Project, Version, True);
    end;
  end;
end;

{$IF CompilerVersion >= 23.0} // Delphi XE2+

function GetAllConfigurations(Project: IOTAProject): TArray<IOTABuildConfiguration>;

  procedure AddChildren(var A: TArray<IOTABuildConfiguration>; var Index: Integer);
  var
    I: Integer;
    Config: IOTABuildConfiguration;
    Platforms: TArray<string>;
  begin
    Config := A[Index - 1];
    Platforms := Config.Platforms;

    SetLength(A, Length(A) + Config.ChildCount + Length(Platforms));
    for I := 0 to Config.ChildCount - 1 do
    begin
      A[Index] := Config.Children[I];
      Inc(Index);
      AddChildren(A, Index);
    end;

    for I := 0 to High(Platforms) do
    begin
      A[Index] := Config.PlatformConfiguration[Platforms[I]];
      Inc(Index);
      AddChildren(A, Index);
    end;
  end;

var
  Configurations: IOTAProjectOptionsConfigurations;
  I, Index: Integer;
begin
  Result := nil;
  if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
  begin
    SetLength(Result, Configurations.ConfigurationCount);
    Index := 0;
    for I := 0 to Configurations.ConfigurationCount - 1 do
    begin
      Result[Index] := Configurations.Configurations[I];
      Inc(Index);
      AddChildren(Result, Index);
    end;
  end;
end;

function GetProjectVersion(Project: IOTAProject): TProjectVersion;
var
  Configurations: IOTAProjectOptionsConfigurations;
  BuildConfig: IOTABuildConfiguration;
  VerInfoKeys: TStrings;
begin
  Result.Valid := False;

  if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
  begin
    if Configurations.BaseConfiguration.AsBoolean[sVerInfo_IncludeVerInfo] and (Length(Configurations.BaseConfiguration.Value[sVerInfo_Keys]) > Length(Configurations.ActiveConfiguration.Value[sVerInfo_Keys])) then
      BuildConfig := Configurations.BaseConfiguration // this happens when a project was migrated
    else
      BuildConfig := Configurations.ActiveConfiguration;
    if (BuildConfig <> nil) and BuildConfig.AsBoolean[sVerInfo_IncludeVerInfo] then
    begin
      Result.Valid := True;
      Result.FileVersion.Major := BuildConfig.AsInteger[sVerInfo_MajorVer];
      Result.FileVersion.Minor := BuildConfig.AsInteger[sVerInfo_MinorVer];
      Result.FileVersion.Release := BuildConfig.AsInteger[sVerInfo_Release];
      Result.FileVersion.Build := BuildConfig.AsInteger[sVerInfo_Build];

      VerInfoKeys := TStringList.Create;
      try
        ExtractStrings([';'], [], PChar(BuildConfig.Value[sVerInfo_Keys]), VerInfoKeys);
        Result.FileVersionStr := VerInfoKeys.Values['FileVersion'];
        Result.ProductVersionStr := VerInfoKeys.Values['ProductVersion'];
        Result.ProductName := VerInfoKeys.Values['ProductName'];
        Result.CompanyName := VerInfoKeys.Values['CompanyName'];
        Result.FileDescription := VerInfoKeys.Values['FileDescription'];
        Result.LegalCopyright := VerInfoKeys.Values['LegalCopyright'];
        Result.LegalTrademarks := VerInfoKeys.Values['LegalTrademarks'];
        Result.InternalName := VerInfoKeys.Values['InternalName'];
        Result.OriginalFilename := VerInfoKeys.Values['OriginalFilename'];
        Result.Comments := VerInfoKeys.Values['Comments'];
      finally
        VerInfoKeys.Free;
      end;
    end;
  end;
end;

function SetProjectVersion(Project: IOTAProject; const Version: TProjectVersion; UpdateAllPlatforms: Boolean): Boolean;
var
  Configurations: IOTAProjectOptionsConfigurations;
  BuildConfig: IOTABuildConfiguration;
  KeysIndex: Integer;
  VerInfoKeys: TStrings;
  OrgKeys, Keys: string;
  Modified: Boolean;

  procedure SetBuildConfigInt(const Option: string; Value: Integer);
  begin
    if BuildConfig.AsInteger[Option] <> Value then
    begin
      BuildConfig.AsInteger[Option] := Value;
      Modified := True;
    end;
  end;

begin
  Result := False;
  if Version.Valid and Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
  begin
    // Write it to all BuildConfigurations that have VersionInfos
    Modified := False;
    for BuildConfig in GetAllConfigurations(Project) do
    begin
      if BuildConfig.AsBoolean[sVerInfo_IncludeVerInfo]
        {and BuildConfig.PropertyExists(sVerInfo_Keys)} then // XE3: Exists(sVerInfo_Keys)=False but, Value[sVerInfo_Keys] returns value
      begin
        if UpdateAllPlatforms or SameText(BuildConfig.Platform, Configurations.ActivePlatformName) then
        begin
          SetBuildConfigInt(sVerInfo_MajorVer, Version.FileVersion.Major);
          SetBuildConfigInt(sVerInfo_MinorVer, Version.FileVersion.Minor);
          SetBuildConfigInt(sVerInfo_Release, Version.FileVersion.Release);
          SetBuildConfigInt(sVerInfo_Build, Version.FileVersion.Build);

          VerInfoKeys := TStringList.Create;
          try
            OrgKeys := BuildConfig.Value[sVerInfo_Keys];
            ExtractStrings([';'], [], PChar(OrgKeys), VerInfoKeys);

            VerInfoKeys.Values['FileVersion'] := Version.FileVersionStr;
            VerInfoKeys.Values['ProductVersion'] := Version.ProductVersionStr;
            VerInfoKeys.Values['ProductName'] := Version.ProductName;
            VerInfoKeys.Values['CompanyName'] := Version.CompanyName;
            VerInfoKeys.Values['FileDescription'] := Version.FileDescription;
            VerInfoKeys.Values['LegalCopyright'] := Version.LegalCopyright;
            VerInfoKeys.Values['LegalTrademarks'] := Version.LegalTrademarks;
            VerInfoKeys.Values['InternalName'] := Version.InternalName;
            VerInfoKeys.Values['OriginalFilename'] := Version.OriginalFilename;
            VerInfoKeys.Values['Comments'] := Version.Comments;

            Keys := '';
            for KeysIndex := 0 to VerInfoKeys.Count - 1 do
            begin
              if Keys = '' then
                Keys := VerInfoKeys[KeysIndex]
              else
                Keys := Keys + ';' + VerInfoKeys[KeysIndex];
            end;

            if Keys <> OrgKeys then
            begin
              BuildConfig.Value[sVerInfo_Keys] := Keys;
              Modified := True;
            end;
          finally
            VerInfoKeys.Free;
          end;
        end;
        Result := True;
      end;
    end;
    if Modified then
      Project.MarkModified;
  end;
end;

function SetVersionInfoExtraKey(AProject: IOTAProject; const AKey, AValue: string): Boolean;
var
  BuildConfig: IOTABuildConfiguration;
  KeysIndex: Integer;
  VerInfoKeys: TStrings;
  OrgKeys, Keys: string;
  Modified: Boolean;
begin
  Result := False;
  Modified := False;
  for BuildConfig in GetAllConfigurations(AProject) do
  begin
    if BuildConfig.AsBoolean[sVerInfo_IncludeVerInfo]
      {and BuildConfig.PropertyExists(sVerInfo_Keys)} then // XE3: Exists(sVerInfo_Keys)=False but, Value[sVerInfo_Keys] returns value
    begin
      VerInfoKeys := TStringList.Create;
      try
        OrgKeys := BuildConfig.Value[sVerInfo_Keys];
        ExtractStrings([';'], [], PChar(OrgKeys), VerInfoKeys);

        VerInfoKeys.Values[AKey] := AValue;

        Keys := '';
        for KeysIndex := 0 to VerInfoKeys.Count - 1 do
        begin
          if Keys = '' then
            Keys := VerInfoKeys[KeysIndex]
          else
            Keys := Keys + ';' + VerInfoKeys[KeysIndex];
        end;
        if Keys <> OrgKeys then
        begin
          BuildConfig.Value[sVerInfo_Keys] := Keys;
          Modified := True;
        end;
      finally
        VerInfoKeys.Free;
      end;
      Result := True;
    end;
  end;
  if Modified then
    AProject.MarkModified;
end;

{$ELSE}

type
  TProjectOptions = class(TInterfacedObject);
  TProjOptsManager = class(TObject);

  TProjectVersionInfo = class(TObject)
    procedure LoadFromExistingProject(ResFile: TResFile);
  end;

  TResEntry = class(TObject)
  public
    ResFile: TOTAHandle;
  end;

procedure TProjectVersionInfo.LoadFromExistingProject(ResFile: TResFile);
  external coreide_bpl name '@Verinf@TVersionInfo@LoadFromExistingProject$qqrp17Resutils@TResFile';

procedure TResFile.SaveToStream(Stream: TStream);
  external coreide_bpl name '@Resutils@TResFile@SaveToStream$qqrp' + System_Classes_TStream;
procedure TResFile.LoadFromStream(Stream: TStream);
  external coreide_bpl name '@Resutils@TResFile@LoadFromStream$qqrp' + System_Classes_TStream;

procedure TResFile.GetIco(Name: PChar; Stream: TStream);
  external coreide_bpl name
  {$IFDEF UNICODE}'@Resutils@TResFile@GetIco$qqrpbp' + System_Classes_TStream;{$ELSE}'@Resutils@TResFile@GetIco$qqrpcp15Classes@TStream';{$ENDIF}
procedure TResFile.AddIco(Name: PChar; Stream: TStream);
  external coreide_bpl name
  {$IFDEF UNICODE}'@Resutils@TResFile@AddIco$qqrpbp' + System_Classes_TStream;{$ELSE}'@Resutils@TResFile@AddIco$qqrpcp15Classes@TStream';{$ENDIF}
procedure TResFile.RemoveIco(Name: PChar);
  external coreide_bpl name
  {$IFDEF UNICODE}'@Resutils@TResFile@RemoveIco$qqrpb';{$ELSE}'@Resutils@TResFile@RemoveIco$qqrpc';{$ENDIF}
function TResFile.Find(ResType, Name: PChar): Pointer;
  external coreide_bpl name
  {$IFDEF UNICODE}'@Resutils@TResFile@Find$qqrpbt1';{$ELSE}'@Resutils@TResFile@Find$qqrpct1';{$ENDIF}

function GetResFileFromResEntry(ResEntry: TOTAHandle): TResFile;
begin
  if ResEntry <> nil then
    Result := TResEntry(ResEntry).ResFile
  else
    Result := nil;
end;

function GetResFileFromResource(Resource: IOTAProjectResource): TResFile;
begin
  if (Resource <> nil) and (Resource.GetEntryCount > 0) then
    Result := GetResFileFromResEntry(Resource.GetEntry(0).GetEntryHandle)
  else
    Result := nil;
end;

{$IFDEF COMPILER9_UP}
type
  IProject = interface
  end;

{$IFDEF COMPILER14_UP}
type
  IProjectResource = interface
    ['{12E37955-201A-11D3-AC21-00C04FB16FB3}']
    {$IF CompilerVersion >= 23.0} // Delphi XE2+
    function GetHasOTAResource: Boolean;
    {$IFEND}
    function GetHasResourceRef: Boolean;
    function GetResourceFile: TResFile;
    function GetResourceFileName: string;
    procedure SetResourceFile(AResFile: TResFile);
    procedure UpdateResourceFile(AHasRes: Boolean);
  end;
{$ELSE}
  IDelphiProjectModuleHandler = interface
    ['{12E37955-201A-11D3-AC21-00C04FB16FB3}']
  end;

  TDelphiProjectModuleHandler = class(TInterfacedObject)
  public
    function GetResourceFile: TResFile;
  end;

function TDelphiProjectModuleHandler.GetResourceFile: TResFile;
  external delphicoreide_bpl name '@Basedelphiproject@TDelphiProjectModuleHandler@GetResourceFile$qqrv';
{$ENDIF COMPILER14_UP}

const
  {$IF CompilerVersion >= 28.0} // XE7+
  _IProject_ = '43System@%DelphiInterface$16Codemgr@IProject%';
  {$ELSE}
  _IProject_ = '44System@%DelphiInterface$t16Codemgr@IProject%';
  {$IFEND}

function TOTAProjectResource_Create(ResFile: TResFile; Project: IProject): TInterfacedObject;
  external coreide_bpl name '@Ideservices@TOTAProjectResource@$bctr$qqrp17Resutils@TResFile' + _IProject_;

{$ENDIF COMPILER9_UP}

function FindProjectResource(Project: IOTAProject): IOTAProjectResource;
var
  i: Integer;
  FileEditor: IOTAEditor;
  {$IFDEF COMPILER9_UP}
  ResFile: TResFile;
  VT: Pointer;
  OTAProjectResource: TInterfacedObject;
  {$IFDEF COMPILER14_UP}
  ModuleHandler: IProjectResource;
  {$ELSE}
  ModuleHandler: TDelphiProjectModuleHandler;
  Intf: IDelphiProjectModuleHandler;
  {$ENDIF COMPILER14_UP}
  {$ENDIF COMPILER9_UP}
begin
  Result := nil;
  if Project = nil then
    Exit;

  for i := 0 to Project.GetModuleFileCount - 1 do
  begin
    try
      FileEditor := Project.GetModuleFileEditor(i);
      if Supports(FileEditor, IOTAProjectResource, Result) then
        Exit;
    except
      // ignore BCB 5 bug: GetModuleFileEditor() meight raise an AV
    end;
  end;

  {$IFDEF COMPILER9_UP}
  ModuleHandler := nil;
  {$IFDEF COMPILER14_UP}
  Supports(Project, IProjectResource, ModuleHandler);
  {$ELSE}
  if Supports(Project, IDelphiProjectModuleHandler, Intf) then
    ModuleHandler := TDelphiProjectModuleHandler(DelphiInterfaceToObject(Intf));
  {$ENDIF COMPILER14_UP}

  if ModuleHandler <> nil then
  begin
    ResFile := ModuleHandler.GetResourceFile;
    if ResFile <> nil then
    begin
      // create a coreide.TOTAProjectResource object
      VT := GetProcAddress(GetModuleHandle(coreide_bpl), '@Ideservices@TOTAProjectResource@');
      if VT <> nil then
      begin
        asm
          xor eax, eax
          push eax
          mov eax, [VT]
          mov ecx, [ResFile]
          mov dl, 1
          call TOTAProjectResource_Create
          mov [OTAProjectResource], eax
        end;
        Supports(OTAProjectResource, IOTAProjectResource, Result);
        Result._Release; // decrease RefCount from 2 to 1
      end;
    end;
  end;
  {$ENDIF COMPILER9_UP}
end;

function GetProjectVersionInfo(Project: IOTAProject): TProjectVersionInfo;
{$IFDEF COMPILER14_UP}
var
  Ctx: TRttiContext;
{$ENDIF COMPILER14_UP}

  function GetProjOptsManager(ProjectOptions: TProjectOptions): TProjOptsManager;
  begin
    {$IFDEF COMPILER14_UP}
    Result := TProjOptsManager(Ctx.GetType(ProjectOptions.ClassInfo).GetProperty('Options').GetValue(ProjectOptions).AsObject)
    {$ELSE}
    Result := TProjOptsManager(FindObjectField(ProjectOptions, 'TProjOptsManager'));
    {$ENDIF COMPILER14_UP}
  end;

  function GetVersionInfo(ProjOptsManager: TProjOptsManager): TProjectVersionInfo;
  {$IFDEF COMPILER14_UP}
  var
    Field: TRttiField;
  {$ENDIF COMPILER14_UP}
  begin
    {$IFDEF COMPILER14_UP}
    Field := Ctx.GetType(ProjOptsManager.ClassInfo).GetField('VerInfOptions');
    if Field = nil then
      Field := Ctx.GetType(ProjOptsManager.ClassInfo).GetField('FVerInfOptions');
    Result := TProjectVersionInfo(Field.GetValue(ProjOptsManager).AsObject);
    {$ELSE}
    Result := TProjectVersionInfo(FindObjectField(ProjOptsManager, 'TVersionInfo'));
    {$ENDIF COMPILER14_UP}
  end;

var
  ProjectOptions: TProjectOptions;
begin
  if Project <> nil then
  begin
    //Project.ProjectOptions.Values['AutoIncBuildNum'] := True; // for debugging into the CPU view
    {$IFDEF COMPILER14_UP}
    ProjectOptions := TProjectOptions(Project.ProjectOptions as TObject);
    {$ELSE}
    ProjectOptions := TProjectOptions(DelphiInterfaceToObject(Project.ProjectOptions));
    {$ENDIF COMPILER14_UP}

    {$IFDEF COMPILER9_UP}
    if Project.Personality = sCBuilderPersonality then
      Result := GetVersionInfo(TProjOptsManager(ProjectOptions))
    else
    {$ENDIF COMPILER9_UP}
      Result := GetVersionInfo(GetProjOptsManager(ProjectOptions));
  end
  else
    Result := nil;
end;

function UpdateProjectVersionInfo(Project: IOTAProject): Boolean;
var
  Resource: IOTAProjectResource;
  ResourceEntry: IOTAResourceEntry;
  ResFile: TResFile;
begin
  ResFile := nil;
  Resource := FindProjectResource(Project);
  if Assigned(Resource) then
  begin
    ResourceEntry := Resource.FindEntry(RT_VERSION, PChar(1));
    if Assigned(ResourceEntry) then
      ResFile := GetResFileFromResEntry(ResourceEntry.GetEntryHandle);
    ResourceEntry := nil;
    Resource := nil;
    if ResFile <> nil then
      GetProjectVersionInfo(Project).LoadFromExistingProject(ResFile);
  end;
  Result := ResFile <> nil;
end;

function GetProjectVersion(Project: IOTAProject): TProjectVersion;
var
  k: Integer;
  Resource: IOTAProjectResource;
  ResourceEntry: IOTAResourceEntry;
  VerInfo: TVersionInfo;
  HasFileVersion: Boolean;
  HasProductVersion: Boolean;
  HasProductName: Boolean;
  HasCompanyName: Boolean;
  HasFileDescription: Boolean;
  HasLegalCopyright: Boolean;
  HasLegalTrademarks: Boolean;
  HasInternalName: Boolean;
  HasOriginalFilename: Boolean;
  HasComments: Boolean;
begin
  Result.Valid := False;
  Resource := FindProjectResource(Project);
  if Resource <> nil then
  begin
    ResourceEntry := Resource.FindEntry(RT_VERSION, PChar(1));
    if Assigned(ResourceEntry) then
    begin
      VerInfo := TVersionInfo.Create(PAnsiChar(ResourceEntry.GetData));
      try
        HasLegalCopyright := False;
        HasLegalTrademarks := False;
        HasCompanyName := False;
        HasFileDescription := False;
        HasFileVersion := False;
        HasProductVersion := False;
        HasProductName := False;
        HasInternalName := False;
        HasOriginalFilename := False;
        HasComments := False;
        for k := 0 to VerInfo.KeyCount - 1 do
        begin
          if VerInfo.KeyName[k] = 'LegalCopyright' then
            HasLegalCopyright := True
          else if VerInfo.KeyName[k] = 'LegalTrademarks' then
            HasLegalTrademarks := True
          else if VerInfo.KeyName[k] = 'CompanyName' then
            HasCompanyName := True
          else if VerInfo.KeyName[k] = 'FileDescription' then
            HasFileDescription := True
          else if VerInfo.KeyName[k] = 'FileVersion' then
            HasFileVersion := True
          else if VerInfo.KeyName[k] = 'ProductVersion' then
            HasProductVersion := True
          else if VerInfo.KeyName[k] = 'ProductName' then
            HasProductName := True
          else if VerInfo.KeyName[k] = 'InternalName' then
            HasInternalName := True
          else if VerInfo.KeyName[k] = 'OriginalFilename' then
            HasOriginalFilename := True
          else if VerInfo.KeyName[k] = 'Comments' then
            HasComments := True;
        end;

        Result.Valid := True;
{        Result.ProductVersion.Major := (VerInfo.FixedFileInfo.dwProductVersionMS shr 16) and $FFFF;
        Result.ProductVersion.Minor := VerInfo.FixedFileInfo.dwProductVersionMS and $FFFF;
        Result.ProductVersion.Release := (VerInfo.FixedFileInfo.dwProductVersionLS shr 16) and $FFFF;
        Result.ProductVersion.Build := VerInfo.FixedFileInfo.dwProductVersionLS and $FFFF;}

        Result.FileVersion.Major := (VerInfo.FixedFileInfo.dwFileVersionMS shr 16) and $FFFF;
        Result.FileVersion.Minor := VerInfo.FixedFileInfo.dwFileVersionMS and $FFFF;
        Result.FileVersion.Release := (VerInfo.FixedFileInfo.dwFileVersionLS shr 16) and $FFFF;
        Result.FileVersion.Build := VerInfo.FixedFileInfo.dwFileVersionLS and $FFFF;


        if not HasLegalCopyright then Result.LegalCopyright := ''
        else Result.LegalCopyright := VerInfo.KeyValue['LegalCopyright'];

        if not HasLegalTrademarks then Result.LegalTrademarks := ''
        else Result.LegalTrademarks := VerInfo.KeyValue['LegalTrademarks'];

        if not HasFileDescription then Result.FileDescription := ''
        else Result.FileDescription := VerInfo.KeyValue['FileDescription'];

        if not HasCompanyName then Result.CompanyName := ''
        else Result.CompanyName := VerInfo.KeyValue['CompanyName'];

        if not HasFileVersion then Result.FileVersionStr := ''
        else Result.FileVersionStr := VerInfo.KeyValue['FileVersion'];

        if not HasProductVersion then Result.ProductVersionStr := ''
        else Result.ProductVersionStr := VerInfo.KeyValue['ProductVersion'];

        if not HasProductName then Result.ProductName := ''
        else Result.ProductName := VerInfo.KeyValue['ProductName'];

        if not HasInternalName then Result.InternalName := ''
        else Result.InternalName := VerInfo.KeyValue['InternalName'];

        if not HasOriginalFilename then Result.OriginalFilename := ''
        else Result.OriginalFilename := VerInfo.KeyValue['OriginalFilename'];

        if not HasComments then Result.Comments := ''
        else Result.Comments := VerInfo.KeyValue['Comments'];

      finally
        VerInfo.Free;
      end;
    end;
  end;
end;

function SetProjectVersion(Project: IOTAProject; const Version: TProjectVersion; UpdateAllPlatforms: Boolean): Boolean;
var
  Resource: IOTAProjectResource;
  ResourceEntry: IOTAResourceEntry;
  VerInfo: TVersionInfo;
  Stream: TMemoryStream;
begin
  Result := False;
  Resource := FindProjectResource(Project);
  if Resource <> nil then
  begin
    ResourceEntry := Resource.FindEntry(RT_VERSION, PChar(1));
    if Assigned(ResourceEntry) then
    begin
      VerInfo := TVersionInfo.Create(PAnsiChar(ResourceEntry.GetData));
      try
        {$IFDEF COMPILER10_UP}
          {$IFNDEF COMPILER12_UP}
        // Delphi 2006 and 2007 have problem with their Build Configurations.
        // We need to "reflect" changes from a "Build Configuration switch". Otherwise the
        // ResourceEntry.SetDataSize() will apply the previous selected Build Configration's
        // project options.
        // This changes the content of ResourceEntry.GetData. So it must be done before altering
        // the resource data.
        Project.ProjectOptions.Values['Defines'] := Project.ProjectOptions.Values['Defines'];
          {$ENDIF ~COMPILER12_UP}
        {$ENDIF COMPILER10_UP}

        // update FixedFileInfo's product version
{            VerInfo.FixedFileInfo.dwProductVersionMS :=
          LongWord(Version.ProductVersion.Major) shl 16 or
          Version.ProductVersion.Minor;
        VerInfo.FixedFileInfo.dwProductVersionLS :=
          LongWord(Version.ProductVersion.Release) shl 16 or
          Version.ProductVersion.Build;}

        // update FixedFileInfo's file version
        VerInfo.FixedFileInfo.dwFileVersionMS :=
          LongWord(Version.FileVersion.Major) shl 16 or
          Version.FileVersion.Minor;
        VerInfo.FixedFileInfo.dwFileVersionLS :=
          LongWord(Version.FileVersion.Release) shl 16 or
          Version.FileVersion.Build;

        VerInfo.KeyValue['LegalCopyright'] := Version.LegalCopyright;
        VerInfo.KeyValue['LegalTrademarks'] := Version.LegalTrademarks;
        VerInfo.KeyValue['FileDescription'] := Version.FileDescription;
        VerInfo.KeyValue['CompanyName'] := Version.CompanyName;
        VerInfo.KeyValue['FileVersion'] := Version.FileVersionStr;
        VerInfo.KeyValue['ProductVersion'] := Version.ProductVersionStr;
        VerInfo.KeyValue['ProductName'] := Version.ProductName;
        VerInfo.KeyValue['InternalName'] := Version.InternalName;
        VerInfo.KeyValue['OriginalFilename'] := Version.OriginalFilename;
        VerInfo.KeyValue['Comments'] := Version.Comments;

        // write modified version information
        Stream := TMemoryStream.Create;
        try
          VerInfo.SaveToStream(Stream);
          ResourceEntry.DataSize := Stream.Size;
          Move(Stream.Memory^, ResourceEntry.GetData^, Stream.Size);
        finally
          Stream.Free;
        end;
      finally
        VerInfo.Free;
      end;
      ResourceEntry := nil;
      Resource := nil;
      Result := True;
      UpdateProjectVersionInfo(Project); // commit the changes to the TVersionInfo data structur of the IDE
    end;
  end;
end;

function SetVersionInfoExtraKey(AProject: IOTAProject; const AKey, AValue: string): Boolean;
var
  Resource: IOTAProjectResource;
  ResourceEntry: IOTAResourceEntry;
  VerInfo: TVersionInfo;
  Stream: TMemoryStream;
begin
  Result := False;
  Resource := FindProjectResource(AProject);
  if Resource <> nil then
  begin
    ResourceEntry := Resource.FindEntry(RT_VERSION, PChar(1));
    if ResourceEntry <> nil then
    begin
      VerInfo := TVersionInfo.Create(PAnsiChar(ResourceEntry.GetData));
      try
        VerInfo.KeyValue[AKey] := AValue;
        // write modified resource
        Stream := TMemoryStream.Create;
        try
          VerInfo.SaveToStream(Stream);
          ResourceEntry.DataSize := Stream.Size;
          Move(Stream.Memory^, ResourceEntry.GetData^, Stream.Size);
          Result := True;
        finally
          Stream.Free;
        end;
      finally
        VerInfo.Free;
      end;
      UpdateProjectVersionInfo(AProject);
    end;
  end;
end;
{$IFEND}

type
{$ALIGN 2}
  PIconHeader = ^TIconHeader;
  TIconHeader = record
    wReserved: Word;
    wType: Word;
    cwCount: Word;
  end;

  PIconResDirectory = ^TIconResDirectory;
  TIconResDirectory = packed record
    bWidth: Byte;
    bHeight: Byte;
    bColorCount: Byte;
    bReserved: Byte;
    wPlanes: Word;
    wBitCount: Word;
    lBytesInRes: LongInt;
    wNameOrdinal: Word;
  end;

  PIconDirectory = ^TIconDirectory;
  TIconDirectory = record
    bWidth: Byte;
    bHeight: Byte;
    wColors: Word;
    lReserved: LongInt;
    lDIBSize: LongInt;
    lDIBOffset: LongInt;
  end;
{$ALIGN 8}

{ TIconResource }

constructor TIconResource.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
end;

destructor TIconResource.Destroy;
begin
  DestroyPaintIcon;
  FStream.Free;
  inherited Destroy;
end;

procedure TIconResource.DestroyPaintIcon;
begin
  if FPaintIcon <> 0 then
    DestroyIcon(FPaintIcon);
  FPaintIconIndex := -1;
  FPaintIcon := 0;
end;

procedure TIconResource.Clear;
begin
  DestroyPaintIcon;
  Stream.Size := 0;
  FFileName := '';
end;

function TIconResource.GetCount: Integer;
begin
  if (Data = nil) or (Stream.Size < SizeOf(TIconHeader)) then
    Result := 0
  else
    Result := PIconHeader(Data).cwCount;
end;

function TIconResource.GetData: Pointer;
begin
  if Stream.Size = 0 then
    Result := nil
  else
    Result := TMemoryStream(Stream).Memory;
end;

function TIconResource.GetImage(Index: Integer): TIconResourceItem;
type
  PInt64 = ^Int64;
  TPngHeader = array[0..7] of AnsiChar;
const
  PNGHeader: TPngHeader = #137'PNG'#13#10#26#10;
  MNGHeader: TPngHeader = #138'MNG'#13#10#26#10;
var
  Directory: PIconHeader;
  P: PIconDirectory;
begin
  if (Data <> nil) and (Index >= 0) and (Index < Count) then
  begin
    Directory := Data;
    Inc(Directory);
    P := PIconDirectory(Directory);
    Inc(P, Index);

    Result.Width := P.bWidth;
    if Result.Width = 0 then
      Result.Width := 256;
    Result.Height := P.bHeight;
    if Result.Height = 0 then
      Result.Height := 256;

    Result.Colors := P.wColors;
    Result.ImageDataSize := P.lDIBSize;
    Result.ImageData := PAnsiChar(Data) + P.lDIBOffset;

    { Handle BMP and PNG icons }
    Result.IsPng := (Result.ImageDataSize > SizeOf(Int64)) and
                    ((PInt64(Result.ImageData)^ = PInt64(@PngHeader)^) or
                     (PInt64(Result.ImageData)^ = PInt64(@MngHeader)^));
    if Result.IsPng then
    begin
      Result.Planes := 1;
      Result.BitCount := 32;
    end
    else
    begin
      Result.Planes := PBitmapInfoHeader(Result.ImageData).biPlanes;
      Result.BitCount := PBitmapInfoHeader(Result.ImageData).biBitCount;
    end;
  end
  else
    raise EListError.CreateFmt(SListIndexError, [Index]);
end;

function TIconResource.GetPaintIcon(Index: Integer): HICON;
var
  Item: TIconResourceItem;
begin
  if FPaintIconIndex <> Index then
  begin
    DestroyPaintIcon;
    Item := Images[Index];
    FPaintIcon := CreateIconFromResourceEx(Item.ImageData, Item.ImageDataSize, True, $00030000,
      Item.Width, Item.Height, 0);
    FPaintIconIndex := Index;
  end;
  Result := FPaintIcon;
end;

procedure TIconResource.LoadFromIconFile(const FileName: string);
begin
  Clear;
  FFileName := FileName;
  TMemoryStream(Stream).LoadFromFile(FileName);
end;

{procedure TIconResource.LoadFromMemory(P: PByte; Size: Integer);
begin
  Clear;
  if (P <> nil) and (Size > 0) then
  begin
    Stream.Write(P^, Size);
    Stream.Position := 0;
  end;
end;}

{$IF CompilerVersion >= 23.0} // Delphi XE2+

function ExpandRootMacro(const InString: string; const AdditionalVars: TObject = nil): string;
  external coreide_bpl name '@Uiutils@ExpandRootMacro$qqrx20System@UnicodeString' + _xp_ + '22Codemgr@TNameValueHash';

procedure TIconResource.LoadFromProjectResource(AProject: IOTAProject);
var
  Configurations: IOTAProjectOptionsConfigurations;
  BuildConfig: IOTABuildConfiguration;
  IconFileName, Dir: string;
begin
  Clear;
  if Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
  begin
    IconFileName := '';
    BuildConfig := Configurations.BaseConfiguration;
    if (BuildConfig <> nil) and (BuildConfig.Value[sIcon_MainIcon] <> '') then
      IconFileName := BuildConfig.Value[sIcon_MainIcon]
    else
    begin
      BuildConfig := Configurations.ActiveConfiguration;
      if (BuildConfig <> nil) and (BuildConfig.Value[sIcon_MainIcon] <> '') then
        IconFileName := BuildConfig.Value[sIcon_MainIcon]
    end;
    if IconFileName <> '' then
    begin
      IconFileName := ExpandRootMacro(IconFileName);
      Dir := GetCurrentDir;
      try
        SetCurrentDir(ExtractFilePath(AProject.FileName));
        LoadFromIconFile(IconFileName);
      finally
        SetCurrentDir(Dir);
      end;
    end;
  end;
end;

procedure TIconResource.SaveToProjectResource(AProject: IOTAProject);
var
  Configurations: IOTAProjectOptionsConfigurations;
  BuildConfig: IOTABuildConfiguration;
  IconFileName: string;
  Modified: Boolean;
begin
  if Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
  begin
    Modified := False;
    BuildConfig := Configurations.BaseConfiguration;
    if (BuildConfig <> nil) {and (BuildConfig.Value[sIcon_MainIcon] <> '')} then
    begin
      if FFileName = '' then
        IconFileName := ''
      else if AnsiSameText(ExtractFilePath(FFileName), (BorlandIDEServices as IOTAServices).GetBinDirectory) then
        IconFileName := '$(BDS)\bin\' + ExtractFileName(FFileName)
      else
        IconFileName := ExtractRelativePath(ExtractFileDir(AProject.FileName), FFileName);

      if (Configurations.ActiveConfiguration <> BuildConfig) and (Configurations.ActiveConfiguration.Value[sIcon_MainIcon] <> '') then
      begin
        Configurations.ActiveConfiguration.Value[sIcon_MainIcon] := '';
        Modified := True;
      end;
      if BuildConfig.Value[sIcon_MainIcon] <> IconFileName then
      begin
        BuildConfig.Value[sIcon_MainIcon] := IconFileName;
        Modified := True;
      end;
      if Modified then
        AProject.MarkModified;
    end;
  end;
end;

{$ELSE}
procedure TIconResource.LoadFromProjectResource(AProject: IOTAProject);
begin
  LoadFromResFile('MAINICON', GetResFileFromResource(FindProjectResource(AProject)));
end;

procedure TIconResource.SaveToProjectResource(AProject: IOTAProject);
var
  ResFile: TResFile;
begin
  ResFile := GetResFileFromResource(FindProjectResource(AProject));
  if ResFile <> nil then
  begin
    ResFile.RemoveIco('MAINICON');
    if Count > 0 then
    begin
      Stream.Position := 0;
      ResFile.AddIco('MAINICON', Stream);
      Stream.Position := 0;
    end;
  end;
end;

procedure TIconResource.LoadFromResFile(Name: PChar; ResFile: TResFile);
begin
  Clear;
  if (ResFile <> nil) and (ResFile.Find(RT_GROUP_ICON, Name) <> nil) then
  begin
    ResFile.GetIco(Name, Stream);
    Stream.Position := 0;
  end;
end;
{$IFEND}

end.

