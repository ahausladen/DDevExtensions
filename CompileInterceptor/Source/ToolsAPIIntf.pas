{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2013 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

// WARNING: IOTAModule must be adjusted as well as IOTAProject because IOTAProject40 derives from it.
//          So much for the "backward compatible" OpenTools API.

unit ToolsAPIIntf;

interface

uses
  Windows, SysUtils, TypInfo, Classes, ActiveX;

{ Possible values for TOTAModuleType }
const
  omtForm          = 0;
  omtDataModule    = 1;
  omtProjUnit      = 2;
  omtUnit          = 3;
  omtRc            = 4;
  omtAsm           = 5;
  omtDef           = 6;
  omtObj           = 7;
  omtRes           = 8;
  omtLib           = 9;
  omtTypeLib       = 10;
  omtPackageImport = 11;
  omtFormResource  = 12;
  omtCustom        = 13;
  omtIDL           = 14;

  { This is the default personality that is used to register default file
    personality traits. }
  sDefaultPersonality = 'Default.Personality';
  { The following are Borland created personalities }
  sDelphiPersonality = 'Delphi.Personality';
  sDelphiDotNetPersonality = 'DelphiDotNet.Personality';
  sCBuilderPersonality = 'CPlusPlusBuilder.Personality';
  sCSharpPersonality = 'CSharp.Personality';
  sVBPersonality = 'VB.Personality';
  sDesignPersonality = 'Design.Personality';
  sGenericPersonality = 'Generic.Personality';

type
  IOTAModule = interface;
  IOTAProject = interface;

  {$IF CompilerVersion >= 21.0}
  TArrayOfString = TArray<string>;
  {$ELSE}
  TArrayOfString = array of string;
  {$IFEND}

  IOTANotifier = interface(IUnknown)
    ['{F17A7BCF-E07D-11D1-AB0B-00C04FB16FB3}']
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

  TOTAOptionName = record
    Name: string;
    Kind: TTypeKind;
  end;

  TOTAOptionNameArray = array of TOTAOptionName;
  TOTACompileMode = (cmOTAMake, cmOTABuild, cmOTACheck, cmOTAMakeUnit);
  TOTAModuleType = type Integer;

  IOTAOptions = interface(IUnknown)
    ['{9C0E91FC-FA5A-11D1-AB28-00C04FB16FB3}']
    procedure EditOptions;
    function GetOptionValue(const ValueName: string): Variant;
    procedure SetOptionValue(const ValueName: string; const Value: Variant);
    function GetOptionNames: TOTAOptionNameArray;
    property Values[const ValueName: string]: Variant read GetOptionValue write SetOptionValue;
  end;

  IOTAEnvironmentOptions = interface(IOTAOptions)                 
    ['{9C0E91FB-FA5A-11D1-AB28-00C04FB16FB3}']
  end;
  
  IOTAProjectOptions40 = interface(IOTAOptions)
    ['{F17A7BD4-E07D-11D1-AB0B-00C04FB16FB3}']
  end;

  IOTAProjectOptions70 = interface(IOTAProjectOptions40)
    ['{F899EBC6-E6E2-11D2-AA90-00C04FA370E9}']
    procedure SetModifiedState(State: Boolean);
    function GetModifiedState: Boolean;

    property ModifiedState: Boolean read GetModifiedState write SetModifiedState;
  end;

  IOTAProjectOptions = interface(IOTAProjectOptions70)
    ['{2888E741-E7FB-4BBC-A093-4B0903D9D990}']
    function GetTargetName: string;

    property TargetName: string read GetTargetName;
  end;

  IOTAProjectBuilder40 = interface(IUnknown)
    ['{F17A7BD5-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetShouldBuild: Boolean;
    function BuildProject(CompileMode: TOTACompileMode; Wait: Boolean): Boolean;

    property ShouldBuild: Boolean read GetShouldBuild;
  end;

  IOTAProjectBuilder = interface(IOTAProjectBuilder40)
    ['{08A5B1F5-FCDA-11D2-AC82-00C04FB173DC}']
    function BuildProject(CompileMode: TOTACompileMode; Wait, ClearMessages: Boolean): Boolean; overload;
  end;

  IOTAModuleNotifier = interface(IOTANotifier)
    ['{F17A7BCE-E07D-11D1-AB0B-00C04FB16FB3}']
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
  end;

  IOTAEditor = interface(IUnknown)
    ['{F17A7BD0-E07D-11D1-AB0B-00C04FB16FB3}']
    function AddNotifier(const ANotifier: IOTANotifier): Integer;
    function GetFileName: string;
    function GetModified: Boolean;
    function GetModule: IOTAModule;
    function MarkModified: Boolean;
    procedure RemoveNotifier(Index: Integer);
    procedure Show;

    property FileName: string read GetFileName;
    property Modified: Boolean read GetModified;
    property Module: IOTAModule read GetModule;
  end;

  IOTAModule40 = interface(IUnknown)
    ['{F17A7BCC-E07D-11D1-AB0B-00C04FB16FB3}']
    function AddNotifier(const ANotifier: IOTAModuleNotifier): Integer;
    procedure AddToInterface;
    function Close: Boolean;
    function GetFileName: string;
    function GetFileSystem: string;
    function GetModuleFileCount: Integer;
    function GetModuleFileEditor(Index: Integer): IOTAEditor;
    function GetOwnerCount: Integer; //deprecated;
    function GetOwner(Index: Integer): IOTAProject; //deprecated;
    function HasCoClasses: Boolean;
    procedure RemoveNotifier(Index: Integer);
    function Save(ChangeName, ForceSave: Boolean): Boolean;
    procedure SetFileName(const AFileName: string);
    procedure SetFileSystem(const AFileSystem: string);

    property OwnerCount: Integer read GetOwnerCount;
    property Owners[Index: Integer]: IOTAProject read GetOwner;
    property FileName: string read GetFileName write SetFileName;
    property FileSystem: string read GetFileSystem write SetFileSystem;
  end;

  IOTAModule50 = interface(IOTAModule40)
    ['{15D3FB81-EF27-488E-B2B4-26B59CA89D9D}']
    function CloseModule(ForceClosed: Boolean): Boolean;
    property ModuleFileCount: Integer read GetModuleFileCount;
    property ModuleFileEditors[Index: Integer]: IOTAEditor read GetModuleFileEditor;
  end;

  IOTAModule70 = interface(IOTAModule50)
    ['{2438BFB8-C742-48CD-8F50-DE6C7F764A55}']
    function GetCurrentEditor: IOTAEditor;
    function GetOwnerModuleCount: Integer;
    function GetOwnerModule(Index: Integer): IOTAModule;
    procedure MarkModified;

    property CurrentEditor: IOTAEditor read GetCurrentEditor;
    property OwnerModuleCount: Integer read GetOwnerModuleCount;
    property OwnerModules[Index: Integer]: IOTAModule read GetOwnerModule;
  end;

  IOTAModule140 = interface(IOTAModule70)
    ['{7FF96161-E610-4414-B8B1-D1ECA76FEAFB}']
    procedure Show;
    procedure ShowFilename(const FileName: string);
  end;

  IOTAModule = interface(IOTAModule140)
    ['{C0D4CBA8-54A3-48EA-BE63-98CE3D9F0F43}']
    procedure Refresh(ForceRefresh: Boolean);
    procedure GetAssociatedFilesFromModule(FileList: TStrings);
  end;

  IOTAModuleInfo50 = interface(IUnknown)
    ['{F17A7BD6-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetModuleType: TOTAModuleType;
    function GetName: string;
    function GetFileName: string;
    function GetFormName: string;
    function GetDesignClass: string;
    procedure GetCoClasses(CoClasses: TStrings);
    function OpenModule: IOTAModule;

    property ModuleType: TOTAModuleType read GetModuleType;
    property Name: string read GetName;
    property FileName: string read GetFileName;
    property FormName: string read GetFormName;
    property DesignClass: string read GetDesignClass;
  end;

  IOTAModuleInfo160 = interface(IOTAModuleInfo50)
    ['{B3EEB4D2-ECDD-4CDC-B96E-B5C8F6D050A8}']
    function GetCustomId: string;
    procedure GetAdditionalFiles(Files: TStrings);

    property CustomId: string read GetCustomId;
  end;

  IOTAModuleInfo = interface(IOTAModuleInfo160)
    ['{006DD7BE-55FD-4707-8C7E-3602C9721810}']
    procedure SetDeviceName(const Value: string);
    function GetDevicename: string;
    procedure SetBuildAction(const Value: string);
    function GetBuildAction: string;

    property DeviceName: string read GetDeviceName write SetDevicename;
    property BuildAction: string read GetBuildAction write SetBuildAction;
  end;

  IOTAProject40 = interface(IOTAModule)
    ['{F17A7BCA-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IOTAModuleInfo;
    function GetProjectOptions: IOTAProjectOptions;
    function GetProjectBuilder: IOTAProjectBuilder;

    property ProjectOptions: IOTAProjectOptions read GetProjectOptions;
    property ProjectBuilder: IOTAProjectBuilder read GetProjectBuilder;
  end;

  IOTAProject70 = interface(IOTAProject40)
    ['{06C88136-F367-4D47-B8B4-CCACB3D7439A}']
    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    procedure RemoveFile(const AFileName: string);
  end;

  IOTAProject90 = interface(IOTAProject70)
    ['{BBBE4CC6-36DE-4986-BD9E-9DF0F06FC8F1}']
    procedure AddFileWithParent(const AFileName: string; IsUnitOrForm: Boolean;
      const Parent: string);
    function GetProjectGUID: TGUID;
    function GetPersonality: string;
    function FindModuleInfo(const FileName: string): IOTAModuleInfo;

    property ProjectGUID: TGUID read GetProjectGUID;
    property Personality: string read GetPersonality;
  end;

  IOTAProject100 = interface(IOTAProject90)
    ['{D0090018-D879-41FC-8F83-AA4F40098ACF}']
    function Rename(const OldFileName, NewFileName: string): Boolean;
  end;

  IOTAProject120 = interface(IOTAProject100)
    ['{3D7E07CB-392D-4EFB-841D-A6C6E338CF13}']
    function GetProjectType: string;

    property ProjectType: string read GetProjectType;
  end;

  IOTAProject140 = interface(IOTAProject120)
    ['{6B1A57F9-34A3-4824-96F0-750A63328C4E}']
    procedure GetCompleteFileList(FileList: TStrings);
    procedure GetAssociatedFiles(const FileName: string; FileList: TStrings);
    function GetFileTransaction(const FileName: string; var InitialName,
      CurrentName: string): Boolean;
  end;

  // WARNING: IOTAModule must be adjusted as well because IOTAProject40 derives from it
  IOTAProject150 = interface(IOTAProject140)
    ['{A6287B50-DA09-44EF-AA80-9D1CAFDE7857}']
    procedure BeginFileTransactionUpdate;
    procedure EndFileTransactionUpdate(CommitUpdate: Boolean);
    procedure GetAddedDeletedFiles(const FileList: IInterfaceList);
    function GetFileTransactionList(const FileName: string; FileList: IInterfaceList): Boolean;
  end;

  IOTAProject160 = interface(IOTAProject150)
    ['{F5EA2A72-485D-49E8-B60A-B0E7C7B80A27}']
    function GetConfiguration: string;
    function GetFrameworkType: string;
    function GetPlatform: string;
    procedure SetConfiguration(const Value: string);
    procedure SetPlatform(const Value: string);
    function GetSupportedPlatforms: TArrayOfString;

    property CurrentConfiguration: string read GetConfiguration write SetConfiguration;
    property CurrentPlatform: string read GetPlatform write SetPlatform;
    property FrameworkType: string read GetFrameworkType;
    property SupportedPlatforms: TArrayOfString read GetSupportedPlatforms;
  end;

  IOTAProject = interface(IOTAProject160)
    ['{0E4BFB1D-2F3B-4CD6-A9A2-4903713B59E0}']
    function GetApplicationType: string;

    property ApplicationType: string read GetApplicationType;
  end;

  TOTAFileNotification = (ofnFileOpening, ofnFileOpened, ofnFileClosing,
    ofnDefaultDesktopLoad, ofnDefaultDesktopSave, ofnProjectDesktopLoad,
    ofnProjectDesktopSave, ofnPackageInstalled, ofnPackageUninstalled,
    ofnActiveProjectChanged
    {XE8:, ofnProjectOpenedFromTemplate});

  IOTAIDENotifier = interface(IOTANotifier)
    ['{E052204F-ECE9-11D1-AB19-00C04FB16FB3}']
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IInterface; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

  IOTAIDENotifier50 = interface(IOTAIDENotifier)
    ['{AC7D29F1-D9A9-11D2-A8C1-00C04FA32F53}']
    procedure BeforeCompile(const Project: IInterface; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  end;

  IOTAServices50 = interface(IUnknown)
    ['{7FD1CE91-E053-11D1-AB0B-00C04FB16FB3}']
    function AddNotifier(const Notifier: IOTAIDENotifier): Integer;
    procedure RemoveNotifier(Index: Integer);
    function GetBaseRegistryKey: string;
    function GetProductIdentifier: string;
    function GetParentHandle: HWND;
    function GetEnvironmentOptions: IOTAEnvironmentOptions;
  end;

  IOTAServices60 = interface(IOTAServices50)
    ['{577ECE00-59EE-4F21-8190-9FD8A45FE550}']
    function GetActiveDesignerType: string;
  end;

  IOTAServices70 = interface(IOTAServices60)
    ['{0044BB24-425D-D611-9CF1-00C04FA06AFC}']
    function GetRootDirectory: string;
    function GetBinDirectory: string;
    function GetTemplateDirectory: string;
  end;

  IOTAServices100 = interface(IOTAServices70)
    ['{33B33186-3CEC-4624-970E-417A8FE14089}']
    function GetApplicationDataDirectory: string;
  end;

  IOTAServices110 = interface(IOTAServices100)
    ['{17A48937-2C9C-4543-AB6D-2CF13BAE544B}']
    function GetLocalApplicationDataDirectory: string;
  end;

  IOTAServices140 = interface(IOTAServices110)
    ['{80E56DFA-82B2-425A-921E-8E5ED6164A11}']
    function GetIDEPreferredUILanguages: string;
  end;

  IOTAServices160 = interface(IOTAServices140)
    ['{86602DE0-50BF-4AE5-BAF4-D9438BD33218}']
    function GetStartupDirectory: string;
    function IsProject(const FileName: string): Boolean;
    function IsProjectGroup(const FileName: string): Boolean;
    function SaveStream(const Stream: IStream): string;
  end;

  IOTAServices = interface(IOTAServices160)
    ['{D1358CFB-9B5C-4E6C-BC4B-C6D06C6689C1}']
    function ExpandRootMacro(const S: string): string;
  end;

  IBorlandIDEServices = interface(IInterface)
    ['{7FD1CE92-E053-11D1-AB0B-00C04FB16FB3}']
  end;

  {------------------------------------------------------------------------}

  { Corrected Interfaces for Delphi 9 to 2010 }

  IOTAProject40_140 = interface(IOTAModule140)
    ['{F17A7BCA-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IOTAModuleInfo;
    function GetProjectOptions: IOTAProjectOptions;
    function GetProjectBuilder: IOTAProjectBuilder;

    property ProjectOptions: IOTAProjectOptions read GetProjectOptions;
    property ProjectBuilder: IOTAProjectBuilder read GetProjectBuilder;
  end;

  IOTAProject70_140 = interface(IOTAProject40_140)
    ['{06C88136-F367-4D47-B8B4-CCACB3D7439A}']
    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    procedure RemoveFile(const AFileName: string);
  end;

  IOTAProject90_140 = interface(IOTAProject70_140)
    ['{BBBE4CC6-36DE-4986-BD9E-9DF0F06FC8F1}']
    procedure AddFileWithParent(const AFileName: string; IsUnitOrForm: Boolean;
      const Parent: string);
    function GetProjectGUID: TGUID;
    function GetPersonality: string;
    function FindModuleInfo(const FileName: string): IOTAModuleInfo;

    property ProjectGUID: TGUID read GetProjectGUID;
    property Personality: string read GetPersonality;
  end;

  {------------------------------------------------------------------------}

  { Correct Interfaces for Delphi 6 and 7 }

  IOTAProject40_70 = interface(IOTAModule70)
    ['{F17A7BCA-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IOTAModuleInfo;
    function GetProjectOptions: IOTAProjectOptions;
    function GetProjectBuilder: IOTAProjectBuilder;

    property ProjectOptions: IOTAProjectOptions read GetProjectOptions;
    property ProjectBuilder: IOTAProjectBuilder read GetProjectBuilder;
  end;

  IOTAProject_70 = interface(IOTAProject40_70)
    ['{06C88136-F367-4D47-B8B4-CCACB3D7439A}']
    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    procedure RemoveFile(const AFileName: string);
  end;

  IOTAProject40_60 = IOTAProject40_70;
  IOTAProject_60 = IOTAProject_70;

  {------------------------------------------------------------------------}

  { Correct Interfaces for Delphi 5 }

  IOTAProject40_50 = interface(IOTAModule50)
    ['{F17A7BCA-E07D-11D1-AB0B-00C04FB16FB3}']
    function GetModuleCount: Integer;
    function GetModule(Index: Integer): IOTAModuleInfo;
    function GetProjectOptions: IOTAProjectOptions;
    function GetProjectBuilder: IOTAProjectBuilder;

    property ProjectOptions: IOTAProjectOptions read GetProjectOptions;
    property ProjectBuilder: IOTAProjectBuilder read GetProjectBuilder;
  end;

  IOTAProject_50 = interface(IOTAProject40_70)
    ['{06C88136-F367-4D47-B8B4-CCACB3D7439A}']
    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    procedure RemoveFile(const AFileName: string);
  end;

//function BorlandIDEServices: IBorlandIDEServices;
function SupportsIDEServices(out Intf: IOTAServices50): Boolean;

implementation

uses
  IdeDllNames;

var
  _BorlandIDEServices: PPointer;

procedure InitializeToolsAPI;
var
  h: THandle;
begin
  h := GetModuleHandle(designide_bpl);
  _BorlandIDEServices := GetProcAddress(h, '@Toolsapi@BorlandIDEServices');
end;

function BorlandIDEServices: IBorlandIDEServices;
begin
  if _BorlandIDEServices = nil then
    InitializeToolsAPI;
  if _BorlandIDEServices <> nil then
    Result := IBorlandIDEServices(_BorlandIDEServices^)
  else
    Result := nil;
end;

function SupportsIDEServices(out Intf: IOTAServices50): Boolean;
begin
  Intf := nil;
  if not Supports(BorlandIDEServices, IOTAServices, Intf) then
    if not Supports(BorlandIDEServices, IOTAServices160, Intf) then
      if not Supports(BorlandIDEServices, IOTAServices140, Intf) then
        if not Supports(BorlandIDEServices, IOTAServices110, Intf) then
          if not Supports(BorlandIDEServices, IOTAServices100, Intf) then
            if not Supports(BorlandIDEServices, IOTAServices70, Intf) then
              if not Supports(BorlandIDEServices, IOTAServices60, Intf) then
                if not Supports(BorlandIDEServices, IOTAServices50, Intf) then
                  Intf := nil;
  Result := Intf <> nil;
end;

end.
