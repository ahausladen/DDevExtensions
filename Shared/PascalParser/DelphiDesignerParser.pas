{******************************************************************************}
{*                                                                            *}
{* Delphi Designer Parser                                                     *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

//TODO: Generics
//TODO: verify reference to procedure/function

unit DelphiDesignerParser;

{$I DelphiParser.inc}

interface

uses
  SysUtils, Classes, Contnrs, DelphiPreproc, DelphiLexer;

type
  TCodeClassList = class;
  
  TModuleType = (
    mtUnit, mtProgram, mtLibrary, mtPackage
  );

  TAccessKind = (
    akPrivate, akStrictPrivate, akProtected, akStrictProtected,
    akPublic, akPublished
  );

  TVirtualKind = (
    vkStatic, vkVirtual, vkOverride, vkAbstract, vkDynamic, vkMessage
  );

  TCallConvention = (
    ccRegister, ccStdCall, ccCdecl, ccPascal, ccSafecall
  );

  TArgModifier = (
    amNone, amVar, amOut, amConst
  );

  TMethodType = (
    mtConstructor, mtDestructor, mtFunction, mtProcedure
  );

  TVarListMode = (
    vmFunctionArguments, vmFields, vmVariables
  );

  { TLocation represents the location in a file }
  TLocation = record
    Line, Column: Integer;
    Filename: string;
    Index: Integer; // linear position
  end;

  { TCodeSymbol is the base class for all symbols of the parser. }
  TCodeSymbol = class(TObject)
  private
    FName: string;
    FAccess: TAccessKind;
    FLocation: TLocation;
    FStartLocation: TLocation;
    FEndLocation: TLocation;
  protected
    class function FindSymbol(List: TObjectList; const AName: string): TCodeSymbol;
  public
    constructor Create(const AName: string; const ALocation: TLocation);

    function ToString: string; override;

    property Name: string read FName;
    { Location returns the location of the symbol's name. }
    property Location: TLocation read FLocation;
    { BeginLocation return the location of the symbol's start. }
    property StartLocation: TLocation read FStartLocation;
    { EndLocation return the location of the symbol's end. }
    property EndLocation: TLocation read FEndLocation;
    property Access: TAccessKind read FAccess;
  end;

  { TCodeField represents a field of a class. }
  TCodeField = class(TCodeSymbol)
  private
    FTypeName: string;
    FIsClassField: Boolean;
  protected
    procedure SetTypeNameLater(const ATypeName: string);
  public
    function ToString: string; override;

    { TypeName returns the reformated type as string. E.g. "array [ 0 .. 20 ] of string" }
    property TypeName: string read FTypeName;
    { IsClassField return True if the field is a class variable (static)}
    property IsClassField: Boolean read FIsClassField;
  end;

  TCodeProperty = class(TCodeSymbol)
  private
    FTypeName: string;
    FSetter: string;
    FAdder: string;
    FIsDefault: Boolean;
    FRemover: string;
    FStorer: string;
    FGetter: string;
    FIsArray: Boolean;
    FIsClassProperty: Boolean;
    FIsInheritChange: Boolean;
    FIsNoDefault: Boolean;
  public
    function ToString: string; override;

    { TypeName returns the reformated type as string. E.g. "array [ 0 .. 20 ] of string" }
    property TypeName: string read FTypeName;
    property Getter: string read FGetter;
    property Setter: string read FSetter;
    property Adder: string read FAdder;
    property Remover: string read FRemover;
    property Storer: string read FStorer;
    property IsDefault: Boolean read FIsDefault;
    property IsNoDefault: Boolean read FIsNoDefault;
    property IsArray: Boolean read FIsArray;
    property IsClassProperty: Boolean read FIsClassProperty;
    property IsInheritChange: Boolean read FIsInheritChange; // specifies if the property is inherited but with differnent visibility/default value.
  end;

  TCodeLocalVariable = class(TCodeField)
  end;

  TCodeMethodArg = class(TCodeField)
  private
    FModifier: TArgModifier;
  public
    function ToString: string; override;

    { Returns the parameter modifier (none, var, out, const). }
    property Modifier: TArgModifier read FModifier;
  end;

  TCodeMethod = class(TCodeSymbol)
  private
    FArgs: TObjectList;
    FCodeInsert: TLocation;
    FVarInsert: TLocation;
    FLocalVariables: TObjectList;
    FLocalProcs: TObjectList;
    FImplLocation: TLocation;
    FReturnTypeName: string;
    FIsClassStatic: Boolean;
    FIsClassMethod: Boolean;
    FVirtualKind: TVirtualKind;
    FConvention: TCallConvention;
    FMethodType: TMethodType;
    FIsImplemented: Boolean;
    FIsDeclared: Boolean;
    FImplEndLocation: TLocation;
    FImplBeginLocation: TLocation;
    FIsImplEmpty: Boolean;
    function GetArg(Index: Integer): TCodeMethodArg;
    function GetArgCount: Integer;
    function GetLocalVariable(Index: Integer): TCodeLocalVariable;
    function GetLocalVariableCount: Integer;
    function GetLocalProc(Index: Integer): TCodeMethod;
    function GetLocalProcCount: Integer;
  protected
    function AddArg(Item: TCodeMethodArg): Integer;
    function AddLocalVariable(Item: TCodeLocalVariable): Integer;
    function AddLocalProc(Item: TCodeMethod): Integer;
  public
    constructor Create(const AName: string; const ALocation: TLocation);
    destructor Destroy; override;

    function ToString: string; override;

    { FindArg return nil if no matching parameter was found with the name. }
    function FindArg(const AName: string): TCodeMethodArg;

    { IsCompatible returns True if both methods have the same signature
      (no name comparision, only type and arg-types). }
    function IsCompatible(M: TCodeMethod): Boolean;

    { ImplLocation returns the location where the method is implemented. }
    property ImplLocation: TLocation read FImplLocation;
    { ImplBeginLocation returns the location of the method's impl. start }
    property ImplBeginLocation: TLocation read FImplBeginLocation;
    { ImplEndLocation returns the location of the method's impl. end }
    property ImplEndLocation: TLocation read FImplEndLocation;
    { IsImplEmpty returns True if the impl. method consists only of "begin end" }
    property IsImplEmpty: Boolean read FIsImplEmpty;
    { CodeInsert returns the location after the BEGIN in the implementation. }
    property CodeInsert: TLocation read FCodeInsert;

    { MethodType returns the type of the method (constructor, destructor,
      function, procedure). }
    property MethodType: TMethodType read FMethodType;
    { ReturnTypeName returns the reformated return-type as string. E.g. "array [ 0 .. 20 ] of string" }
    property ReturnTypeName: string read FReturnTypeName;
    { IsClassMethod returns True if the method is a class method. }
    property IsClassMethod: Boolean read FIsClassMethod;
    { IsClassStatic returns True if the method is a static class method. }
    property IsClassStatic: Boolean read FIsClassStatic;
    { VirtualKind returns the methods' virtuality kind. }
    property VirtualKind: TVirtualKind read FVirtualKind;
    { Convention returns call convention of the method. }
    property Convention: TCallConvention read FConvention;

    { IsImplemented return True if the method is in the implementation section.
      Is IsDeclared <> IsImplemented then one is missing. }
    property IsImplemented: Boolean read FIsImplemented;
    { IsDeclared return True if the method is in the interface section. }
    property IsDeclared: Boolean read FIsDeclared;

    { Returns the number of parameters. }
    property ArgCount: Integer read GetArgCount;
    { Returns the parameters. }
    property Args[Index: Integer]: TCodeMethodArg read GetArg;

    { Returns the number of local variables. }
    property LocalVariableCount: Integer read GetLocalVariableCount;
    { Returns the local variables. }
    property LocalVariables[Index: Integer]: TCodeLocalVariable read GetLocalVariable;

    { Returns the number of local procedures. }
    property LocalProcCount: Integer read GetLocalProcCount;
    { Returns the local procedures. }
    property LocalProcss[Index: Integer]: TCodeMethod read GetLocalProc;
  end;

  { TCodeClass contains all information the designer parser extracted for a
    class (methods, properties, fields). }
  TCodeClass = class(TCodeSymbol)
  private
    FAccessInsert: array[TAccessKind] of TLocation;
    FBaseClass: string;
    FIsUnitPrivate: Boolean;

    FInterfaces: TStringList;
    FFields: TObjectList;
    FProperties: TObjectList;
    FMethods: TObjectList;

    function GetInterfaceCount: Integer;
    function GetInterfaceItem(Index: Integer): string;
    function GetAccessInsert(Access: TAccessKind): TLocation;
    function GetField(Index: Integer): TCodeField;
    function GetFieldCount: Integer;
    function GetMethodCount: Integer;
    function GetMethodItem(Index: Integer): TCodeMethod;
    function GetProperty(Index: Integer): TCodeProperty;
    function GetPropertyCount: Integer;
  protected
    function AddInterface(const Item: string): Integer;
    function AddField(Item: TCodeField): Integer;
    function AddProperty(Item: TCodeProperty): Integer;
    function AddMethod(Item: TCodeMethod): Integer;
  public
    constructor Create(const AName: string; const ALocation: TLocation);
    destructor Destroy; override;

    { SupportsInterface returns True if the class implements the given
      interface directly. }
    function SupportsInterface(const AName: string): Boolean;
    { FindProperty return nil if no matching property was found. }
    function FindProperty(const AName: string): TCodeProperty;
    { FindField return nil if no matching field was found. }
    function FindField(const AName: string): TCodeField;
    { FindMethod return nil if no matching method was found. }
    function FindMethod(const AName: string): TCodeMethod;

    { AccessInsert[] returns the location of the access-kind (private, public, ...).
      This is the before the next access-kind. }
    property AccessInsert[Access: TAccessKind]: TLocation read GetAccessInsert;

    { BaseClass returns the name of the base class. }
    property BaseClass: string read FBaseClass;
    { IsUnitPrivate returns True if the class is declared in the implementation
      section. }
    property IsUnitPrivate: Boolean read FIsUnitPrivate;

    { Supported interfaces. }
    property InterfaceCount: Integer read GetInterfaceCount;
    property Interfaces[Index: Integer]: string read GetInterfaceItem;

    { Member methods. }
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TCodeMethod read GetMethodItem;

    { Member properties. }
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[Index: Integer]: TCodeProperty read GetProperty;

    { Member fields. }
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TCodeField read GetField;
  end;

  { TCodeClassList contains a list of code-classes }
  TCodeClassList = class(TObject)
  private
    FItems: TObjectList;
    function GetItem(Index: Integer): TCodeClass;
    function GetCount: Integer;
  protected
    procedure Clear;
    function Add(Item: TCodeClass): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Find returns nil if the class was not found. }
    function Find(const ClassName: string): TCodeClass;

    { Classes }
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCodeClass read GetItem; default;
  end;

  TUsesItem = class(TCodeSymbol)
  private
    FFilename: string;
  public
    constructor Create(const AName, AFilename: string; const ALocation: TLocation);

    { Name returns the included unit name. }
    property Name;

    { Filename returns the specified filename ("bla in 'bla.pas'"), if no
      filename was specified it return ''. }
    property Filename: string read FFilename;
  end;

  { TUsesList contains a list of USES-items. }
  TUsesList = class(TObject)
  private
    FItems: TObjectList;
    FLocation: TLocation;
    FEndLocation: TLocation;
    function GetCount: Integer;
    function GetItem(Index: Integer): TUsesItem;
  protected
    procedure Clear;
    function AddUses(Item: TUsesItem): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { FindUses return nil if no matching USES-item was found. }
    function FindUses(const AName: string): TUsesItem;
    function IndexOf(AItem: TUsesItem): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUsesItem read GetItem; default;

    { Location returns the location of the USES-keyword. }
    property Location: TLocation read FLocation;
    property EndLocation: TLocation read FEndLocation;
  end;

  { TDesignerParser parses a file in highspeed. It does not really parse the
    code, it simply searches for certain keywords.
    After the designer parser has finished it's lists are filled with
    information the designer needs. }
  TDesignerParser = class(TObject)
  private
    p: TDelphiPreprocessor;
    FImplementation: Boolean;
    FClasses: TCodeClassList;
    FModuleName: string;
    FFormResource: string;
    FFilename: string;
    FText: UTF8String;
    FRepeatLastToken: Boolean;
    FImplUses: TUsesList;
    FInterfaceUses: TUsesList;
    FImplLocation: TLocation;
    FInterfaceLocation: TLocation;
    FResources: TStringList;
    FIncludeFiles: TStringList;
    FContains: TUsesList;
    FRequires: TUsesList;
    FModuleType: TModuleType;
    FFunctions: TObjectList;
    FIncludeDirs: TStrings;

    procedure IncludeFile(Sender: TObject; const Name: string);
    procedure Directive(Sender: TObject; Token: TToken);
    procedure GetDeclared(Sender: TObject; const Name: string; var Value: Boolean);
    procedure GetConstValue(Sender: TObject; const Name: string; var Kind: TTokenKind; var Value: string);
    procedure GetConstBoolValue(Sender: TObject; const Name: string; var Value: Boolean);

    procedure Clear;

    function Look: TToken;
    function Next: Boolean;
    procedure NextA;
    procedure IgnoreProc;
    procedure IgnoreRecord;
    procedure IgnoreInterface;
    function IgnoreBlock(out Empty: Boolean): TLocation;
    procedure IgnoreToClassAccess;
    procedure IgnoreExternal;
    procedure IgnoreFunctionType;
    procedure ParseClass;
    procedure ParseMethodDecl(C: TCodeClass; Access: TAccessKind; IsClass: Boolean);
    procedure ParseProperty(C: TCodeClass; Access: TAccessKind; IsClass: Boolean);
    procedure ParseField(C: TCodeClass; Access: TAccessKind; IsClass: Boolean);
    function ParseTypeName(const Stop: TTokenKindSet): string;
    procedure ParseFunctionArgs(M: TCodeMethod);
    procedure ParseUses(List: TUsesList);
    procedure ParseFunctionImpl(AParent: TCodeMethod);
    procedure ParseDirectives(M: TCodeMethod);
    procedure ParseVariableList(List: TObjectList; Mode: TVarListMode);
    procedure ParseVariableDecl(List: TObjectList);

    function GetResource(Index: Integer): string;
    function GetResourceCount: Integer;
    function GetIncludeFiles(Index: Integer): string;
    function GetIncludeFileCount: Integer;

    function GetFunctionCount: Integer;
    function GetFunctionItem(Index: Integer): TCodeMethod;
    function AddFunction(Item: TCodeMethod): Integer;
    function ParseQualifiedName: string;
    procedure ParseHead;
  protected
    procedure InitDefines(Parser: TDelphiPreprocessor); virtual;
  public
    constructor Create(const AFilename: string; const AText: UTF8String);
    destructor Destroy; override;

    property IncludeDirs: TStrings read FIncludeDirs;

    { Parses the text which was specified in the constructor call. It fills
      the following structures. }
    procedure Parse;
    { Parses the head and the interface/implementation uses. }
    procedure ParseUsesOnly;

    { Contains every class the parser found in this file (and include files) }
    property Classes: TCodeClassList read FClasses;

    { ModuleType can be either mtUnit, mtProgram, mtLibrary or mtPackage }
    property ModuleType: TModuleType read FModuleType;
    { ModuleName is the name of the module read from the file content (UNIT name) }
    property ModuleName: string read FModuleName;
    { FormResource contains *.dfm, *.nfm or *.xfm if the file includes such a
      resource. Otherwise it is '' }
    property FormResource: string read FFormResource;

    { ResourceCount returns the number of included resources. }
    property ResourceCount: Integer read GetResourceCount;
    { Resources[] returns the resource filename (or *.xxx). The FormResource is
      also in this list. }
    property Resources[Index: Integer]: string read GetResource;

    { ResourceCount returns the number of included resources. }
    property IncludeFileCount: Integer read GetIncludeFileCount;
    { Resources[] returns the resource filename (or *.xxx). The FormResource is
      also in this list. }
    property IncludeFiles[Index: Integer]: string read GetIncludeFiles;

    { InterfaceUses contains every unit name (+filename if specified) of the
      interface USES-clausel (program/library uses). }
    property InterfaceUses: TUsesList read FInterfaceUses;
    { ImplUses contains every unit name (+filename if specified) of the
      implementation USES-clausel (unit only). }
    property ImplUses: TUsesList read FImplUses;
    { Requires contains every required package of the REQUIRES-clausel (package only). }
    property Requires: TUsesList read FRequires;
    { Contains contains every contained unit name (+filename if specified) of
      the CONTAINS-clausel (package only). }
    property Contains: TUsesList read FContains;

    { Functions }
    function FindFunction(const AName: string): TCodeMethod;
    property FunctionCount: Integer read GetFunctionCount;
    property Functions[Index: Integer]: TCodeMethod read GetFunctionItem;

    { InterfaceLocation returns the location of the INTERFACE-keyword. }
    property InterfaceLocation: TLocation read FInterfaceLocation;
    { ImplLocation returns the location of the IMPLEMENTATION-keyword. }
    property ImplLocation: TLocation read FImplLocation;

    { Filename returns the name of the file to parse (-> constructor) }
    property Filename: string read FFilename;
    { Text returns the text to parse (-> constructor) }
    property Text: UTF8String read FText;
  end;

const
  EmptyLocation: TLocation = (Line: 0; Column: 0; Filename: '');

implementation

uses
  StrUtils;

const
  ClassAvailableTokens = [tkIdent..tkIdentStrictReserved] -
    [tkI_public, tkI_private, tkI_protected, tkI_strict, tkI_published,
     tkI_end, tkI_class, tkI_var,  tkI_const, tkI_out, tkI_property,
     tkI_implementation];

  SectionEndTokens = [tkIdentStrictReserved..tkLast];
  ClassSectionEndTokens = SectionEndTokens + [tkI_strict];
  ArgModifierTokens = [tkI_var, tkI_out, tkI_const];

  PropertyTokens = [tkI_read, tkI_write, tkI_add, tkI_remove, tkI_stored, tkI_default];

function TokenToLocation(Token: TToken): TLocation;
begin
  Result.Line := Token.Line;
  Result.Column := Token.Column;
  Result.Filename := Token.Filename;
  Result.Index := Token.Index;
end;

{ TCodeSymbol }

constructor TCodeSymbol.Create(const AName: string; const ALocation: TLocation);
begin
  inherited Create;
  FName := AName;
  FLocation := ALocation;
  FAccess := akPublished;
end;

class function TCodeSymbol.FindSymbol(List: TObjectList; const AName: string): TCodeSymbol;
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    Result := TCodeSymbol(List[i]);
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TCodeSymbol.ToString: string;
begin
  Result := FName;
end;

{ TCodeField }

procedure TCodeField.SetTypeNameLater(const ATypeName: string);
begin
  FTypeName := ATypeName;
end;

function TCodeField.ToString: string;
begin
  Result := Name + ': ' + TypeName;
end;

{ TCodeProperty }

function TCodeProperty.ToString: string;
begin
  Result := Name;
  if IsArray then
    Result := Result + '[]';
  Result := Result + ': ' + TypeName;
  if Getter <> '' then
    Result := Result + ' read ' + Getter;
  if Adder <> '' then
    Result := Result + ' add ' + Adder;
  if Setter <> '' then
    Result := Result + ' write ' + Setter;
  if Remover <> '' then
    Result := Result + ' remove ' + Remover;
  if Storer <> '' then
    Result := Result + ' stored ' + Storer;
  if IsDefault then
    Result := Result + '; default'
  else
  if IsNoDefault then
    Result := Result + '; nodefault';
end;

{ TCodeMethodArg }

function TCodeMethodArg.ToString: string;
begin
  case Modifier of
    amVar: Result := 'var ';
    amOut: Result := 'out ';
    amConst: Result := 'const ';
  else
    Result := '';
  end;
  Result := Result + inherited ToString;
end;

{ TCodeMethod }

function TCodeMethod.AddArg(Item: TCodeMethodArg): Integer;
begin
  Result :=  FArgs.Add(Item);
end;

function TCodeMethod.AddLocalVariable(Item: TCodeLocalVariable): Integer;
begin
  Result := FLocalVariables.Add(Item);
end;

function TCodeMethod.AddLocalProc(Item: TCodeMethod): Integer;
begin
  Result := FLocalProcs.Add(Item);
end;

constructor TCodeMethod.Create(const AName: string; const ALocation: TLocation);
begin
  inherited Create(AName, ALocation);
  FArgs := TObjectList.Create;
  FLocalVariables := TObjectList.Create;
  FLocalProcs := TObjectList.Create;
end;

destructor TCodeMethod.Destroy;
begin
  FLocalProcs.Free;
  FLocalVariables.Free;
  FArgs.Free;
  inherited Destroy;
end;

function TCodeMethod.IsCompatible(M: TCodeMethod): Boolean;
var
  i: Integer;
begin
  if M = Self then
    Result := True
  else if M = nil then
    Result := False
  else
  begin
    Result := (MethodType = M.MethodType) and (Convention = M.Convention) and
        (IsClassMethod = M.IsClassMethod) and (IsClassStatic = M.IsClassStatic) and
        (ArgCount = M.ArgCount) and (ReturnTypeName = M.ReturnTypeName);

    if Result then
    begin
      for i := 0 to ArgCount - 1 do
      begin
        Result := (Args[i].Modifier = M.Args[i].Modifier) and
                  (Args[i].TypeName = M.Args[i].TypeName);
        if not Result then
          Exit;
      end;
    end;
  end;
end;

function TCodeMethod.FindArg(const AName: string): TCodeMethodArg;
begin
  Result := TCodeMethodArg(FindSymbol(FArgs, AName));
end;

function TCodeMethod.GetArg(Index: Integer): TCodeMethodArg;
begin
  Result := TCodeMethodArg(FArgs[Index]);
end;

function TCodeMethod.GetArgCount: Integer;
begin
  Result := FArgs.Count;
end;

function TCodeMethod.ToString: string;
var
  i: Integer;
begin
  case MethodType of
    mtConstructor: Result := 'constructor';
    mtDestructor: Result := 'destructor';
    mtFunction: Result := 'function';
    mtProcedure: Result := 'procedure';
  end;
  Result := Result + ' ' + Name + '(';
  for i := 0 to ArgCount - 1 do
  begin
    Result := Result + Args[i].ToString;
    if i < ArgCount - 1 then
      Result := Result + '; ';
  end;
  Result := Result + ')';
  if ReturnTypeName <> '' then
    Result := Result + ': ' + ReturnTypeName;
end;

function TCodeMethod.GetLocalVariableCount: Integer;
begin
  Result := FLocalVariables.Count;
end;

function TCodeMethod.GetLocalVariable(Index: Integer): TCodeLocalVariable;
begin
  Result := TCodeLocalVariable(FLocalVariables[Index]);
end;

function TCodeMethod.GetLocalProcCount: Integer;
begin
  Result := FLocalProcs.Count;
end;

function TCodeMethod.GetLocalProc(Index: Integer): TCodeMethod;
begin
  Result := TCodeMethod(FLocalProcs[Index]);
end;

{ TCodeClass }

constructor TCodeClass.Create(const AName: string; const ALocation: TLocation);
begin
  inherited Create(AName, ALocation);
  FInterfaces := TStringList.Create;
  FFields := TObjectList.Create;
  FProperties := TObjectList.Create;
  FMethods := TObjectList.Create;
end;

destructor TCodeClass.Destroy;
begin
  FInterfaces.Free;
  FFields.Free;
  FProperties.Free;
  FMethods.Free;
  inherited Destroy;
end;

function TCodeClass.GetInterfaceItem(Index: Integer): string;
begin
  Result := FInterfaces[Index];
end;

function TCodeClass.GetInterfaceCount: Integer;
begin
  Result := FInterfaces.Count;
end;

function TCodeClass.GetAccessInsert(Access: TAccessKind): TLocation;
begin
  Result := FAccessInsert[Access];
end;

function TCodeClass.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TCodeClass.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

function TCodeClass.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TCodeClass.GetProperty(Index: Integer): TCodeProperty;
begin
  Result := TCodeProperty(FProperties[Index]);
end;

function TCodeClass.GetMethodItem(Index: Integer): TCodeMethod;
begin
  Result := TCodeMethod(FMethods[Index]);
end;

function TCodeClass.GetField(Index: Integer): TCodeField;
begin
  Result := TCodeField(FFields[Index]);
end;

function TCodeClass.FindField(const AName: string): TCodeField;
begin
  Result := TCodeField(FindSymbol(FFields, AName));
end;

function TCodeClass.FindProperty(const AName: string): TCodeProperty;
begin
  Result := TCodeProperty(FindSymbol(FProperties, AName));
end;

function TCodeClass.FindMethod(const AName: string): TCodeMethod;
begin
  Result := TCodeMethod(FindSymbol(FMethods, AName));
end;

function TCodeClass.AddProperty(Item: TCodeProperty): Integer;
begin
  Result := FProperties.Add(Item);
end;

function TCodeClass.AddMethod(Item: TCodeMethod): Integer;
begin
  Result := FMethods.Add(Item);
end;

function TCodeClass.AddField(Item: TCodeField): Integer;
begin
  Result := FFields.Add(Item);
end;

function TCodeClass.AddInterface(const Item: string): Integer;
begin
  Result := FInterfaces.Add(Item);
end;

function TCodeClass.SupportsInterface(const AName: string): Boolean;
begin
  Result := FInterfaces.IndexOf(AName) >= 0;
end;

{ TCodeClassList }

constructor TCodeClassList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TCodeClassList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TCodeClassList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCodeClassList.Find(const ClassName: string): TCodeClass;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if SameText(Result.Name, ClassName) then
      Exit;
  end;
  Result := nil;
end;

function TCodeClassList.Add(Item: TCodeClass): Integer;
begin
  Result := FItems.Add(Item);
end;

function TCodeClassList.GetItem(Index: Integer): TCodeClass;
begin
  Result := TCodeClass(FItems[Index]);
end;

procedure TCodeClassList.Clear;
begin
  FItems.Clear;
end;

{ TUsesItem }

constructor TUsesItem.Create(const AName, AFilename: string; const ALocation: TLocation);
begin
  inherited Create(AName,  ALocation);
  FName := AName;
  FFilename := AFilename;
  FLocation := ALocation;
end;

{ TUsesList }

constructor TUsesList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TUsesList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TUsesList.AddUses(Item: TUsesItem): Integer;
begin
  Result := FItems.Add(Item);
end;

function TUsesList.FindUses(const AName: string): TUsesItem;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TUsesList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TUsesList.GetItem(Index: Integer): TUsesItem;
begin
  Result := TUsesItem(FItems[Index]);
end;

function TUsesList.IndexOf(AItem: TUsesItem): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

procedure TUsesList.Clear;
begin
  FItems.Clear;
end;

{ TDesignerParser }

constructor TDesignerParser.Create(const AFilename: string; const AText: UTF8String);
begin
  inherited Create;
  FClasses := TCodeClassList.Create;
  FFilename := AFilename;
  FText := AText;
  FInterfaceUses := TUsesList.Create;
  FImplUses := TUsesList.Create;
  FResources := TStringList.Create;
  FIncludeFiles := TStringList.Create;
  FRequires := TUsesList.Create;
  FContains := TUsesList.Create;
  FFunctions := TObjectList.Create;
  FIncludeDirs := TStringList.Create;
end;

destructor TDesignerParser.Destroy;
begin
  FIncludeDirs.Free;
  FFunctions.Free;
  FInterfaceUses.Free;
  FImplUses.Free;
  FClasses.Free;
  FIncludeFiles.Free;
  FResources.Free;
  FRequires.Free;
  FContains.Free;
  inherited Destroy;
end;

procedure TDesignerParser.Clear;
begin
  FInterfaceUses.Clear;
  FImplUses.Clear;
  FResources.Clear;
  FIncludeFiles.Clear;
  FClasses.Clear;
  FModuleName := '';
  FModuleType := mtUnit;
  FFormResource := '';
  FImplLocation := EmptyLocation;
  FInterfaceLocation := EmptyLocation;
  FImplementation := False;
  FRepeatLastToken := False;
  FRequires.Clear;
  FContains.Clear;
  FFunctions.Clear;
end;

function TDesignerParser.GetResource(Index: Integer): string;
begin
  Result := FResources[Index];
end;

function TDesignerParser.GetResourceCount: Integer;
begin
  Result := FResources.Count;
end;

function TDesignerParser.GetIncludeFiles(Index: Integer): string;
begin
  Result := FIncludeFiles[Index];
end;

function TDesignerParser.GetIncludeFileCount: Integer;
begin
  Result := FIncludeFiles.Count;
end;

function TDesignerParser.AddFunction(Item: TCodeMethod): Integer;
begin
  Result := FFunctions.Add(Item);
end;

function TDesignerParser.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TDesignerParser.GetFunctionItem(Index: Integer): TCodeMethod;
begin
  Result := TCodeMethod(FFunctions[Index]);
end;

function TDesignerParser.Look: TToken;
begin
  Result := p.CurrentToken;
end;

function TDesignerParser.Next: Boolean;
begin
  if FRepeatLastToken then
  begin
    Result := p.CurrentToken <> nil;
    FRepeatLastToken := False;
  end
  else if p <> nil then
    Result := p.GetToken <> nil
  else
    Result := False;
end;

procedure TDesignerParser.NextA;
begin
  if not Next then
    Abort;
end;

procedure TDesignerParser.IncludeFile(Sender: TObject; const Name: string);
var
  Dir, FileName: string;
  I: Integer;
  LName: string;
begin
  LName := AnsiDequotedStr(Name, '''');
  Dir := ExtractFileDir((Sender as TDelphiPreprocessor).CurrentFileName);
  if (Length(LName) >= 2) and (LName[1] <> '\') and (LName[2] <> ':') then
  begin
    FileName := Dir + '\' + LName;
    if (Dir <> '') and FileExists(FileName) then
      TDelphiPreprocessor(Sender).AddInclude(ExpandFileName(FileName), LoadTextFileToUtf8String(FileName))
    else
    begin
      for I := 0 to IncludeDirs.Count - 1 do
      begin
        FileName := ExcludeTrailingPathDelimiter(IncludeDirs[I]) + '\' + LName;
        if FileExists(FileName) then
        begin
          TDelphiPreprocessor(Sender).AddInclude(ExpandFileName(FileName), LoadTextFileToUtf8String(FileName));
          Break;
        end;
      end;
    end;
  end;

  FIncludeFiles.Add(LName);
end;

procedure TDesignerParser.InitDefines(Parser: TDelphiPreprocessor);
begin
  {$IFDEF COMPILER6_UP}
  p.Define('CONDITIONALEXPRESSION');
  if CompilerVersion = 18.5 then
     p.Define('VER180'); // Delphi 2007 defines both
  p.Define('VER' + IntToStr(Trunc(CompilerVersion * 10)));
  {$ELSE}
  p.Define('VER130'); // Delphi 5
  {$ENDIF COMPILER6_UP}

  {$IFDEF MSWINDOWS}
  p.Define('MSWINDOWS');
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  p.Define('LINUX');
  {$ENDIF LINUX}
  {$IFDEF UNICODE}
  p.Define('UNICODE');
  {$ENDIF UNICODE}
end;

procedure TDesignerParser.Directive(Sender: TObject; Token: TToken);
var
  Op: string;
begin
  Op := Token.Value;
  if Op[1] = '{' then
    Op := Trim(Copy(Op, 2, Length(Op) - 2))
  else
    Op := Trim(Copy(Op, 3, Length(Op) - 4));
  if StartsText('$R', Op) and (PChar(Op)[2] <= ' ') then
  begin
    // resource inclusion
    Op := AnsiDequotedStr(Trim(Copy(Op, 4, MaxInt)), '''');
    if EndsText('*.dfm', Op) or EndsText('*.fmx', Op) or
       EndsText('*.nfm', Op) or EndsText('*.xfm', Op) or
       EndsText('*.lfm', Op) then
      FFormResource := Op;
    FResources.Add(Op);
  end;
end;

procedure TDesignerParser.ParseHead;
begin
  p := TDelphiPreprocessor.Create(FFilename, FText);
  try
    InitDefines(p);

    p.OnIncludeFile := IncludeFile;
    p.OnDirective := Directive;
    p.OnGetDeclared := GetDeclared;
    p.OnGetConstValue := GetConstValue;
    p.OnGetConstBoolValue := GetConstBoolValue;

    FImplementation := False;

    // Without a module name the whole file will be ignored.
    // Find "program", "unit", "library", "package"
    while Next do
    begin
      case Look.Kind of
        tkI_Unit: FModuleType := mtUnit;
        tkI_Program: FModuleType := mtProgram;
        tkI_Library: FModuleType := mtLibrary;
        tkI_Package: FModuleType := mtPackage;
      end;

      case Look.Kind of
        tkI_Unit,
        tkI_Program,
        tkI_Library,
        tkI_Package:
          begin
            NextA;
            FModuleName := ParseQualifiedName;
            if Look.Kind = tkSemicolon then
              NextA;
            Break;
          end;
      end;
    end;
    if (Look <> nil) and (Look.Kind = tkI_interface) then
    begin
      FInterfaceLocation := TokenToLocation(Look);
      NextA;
    end;

    if (Look <> nil) and (Look.Kind = tkI_uses) then
    begin
      ParseUses(FInterfaceUses);
      if Look.Kind = tkSemicolon then
        Next;
    end;

    if ModuleType = mtPackage then
    begin
      while (Look <> nil) and (Look.Kind <> tkI_requires) do
        Next;
      if (Look <> nil) and (Look.Kind = tkI_requires) then
      begin
        ParseUses(FRequires);
        if Look.Kind = tkSemicolon then
          Next;
      end;

      while (Look <> nil) and (Look.Kind <> tkI_contains) do
        Next;
      if (Look <> nil) and (Look.Kind = tkI_contains) then
      begin
        ParseUses(FContains);
        if Look.Kind = tkSemicolon then
          Next;
      end;
    end;

    if Look.Kind <> tkSemicolon then
      FRepeatLastToken := True;
  except
    FreeAndNil(p);
  end;
end;

procedure TDesignerParser.ParseUsesOnly;
begin
  Clear;
  try
    ParseHead;
    try
      while Next do
      begin
        if Look.Kind = tkI_implementation then
        begin
          FImplLocation := TokenToLocation(Look);
          FImplementation := True;
          NextA;
          if Look.Kind = tkI_uses then
            ParseUses(FImplUses);
          Break;
        end;
      end;
    finally
      FreeAndNil(p);
    end;
  except
    on E: EAbort do
      ;
  end;
end;

procedure TDesignerParser.Parse;
begin
  Clear;
  try
    ParseHead;
    try
      // implementation
      while Next do
      begin
        case Look.Kind of
          tkI_class:
            if Next and ((Look.Kind = tkLParan) or not (Look.Kind in [tkI_of, tkI_helper, tkSemicolon])) then // "... class("
              if (p.TokenCount > 3) and (p.Tokens[p.TokenCount - 1 - 2].Kind = tkEqual) then
                ParseClass
              else
                FRepeatLastToken := True;
          tkI_function,
          tkI_procedure,
          tkI_constructor,
          tkI_destructor,
          tkI_operator:
            ParseFunctionImpl(nil);
          tkI_interface:
            IgnoreInterface;
          tkI_record,
          tkI_object:
            IgnoreRecord;

          tkI_implementation:
            begin
              FImplLocation := TokenToLocation(Look);
              FImplementation := True;
              NextA;
              if Look.Kind = tkI_uses then
                ParseUses(FImplUses);
            end;
        end;
      end;

    finally
      FreeAndNil(p);
    end;
  except
    on E: EAbort do
      ;
  end;
end;

procedure TDesignerParser.IgnoreRecord;
var
  OpenParan: Integer;
begin
  Next;
  if Look.Kind = tkLParan then
  begin
    OpenParan := 1;
    while Next and (OpenParan > 0) do
    begin
      if Look.Kind = tkLParan then
        Inc(OpenParan)
      else if Look.Kind = tkRParan then
        Dec(OpenParan);
    end;
  end;

  if Look.Kind = tkSemicolon then
    Exit; // prototype declaration

  repeat
    case Look.Kind of
      tkI_procedure,
      tkI_function,
      tkI_constructor,
      tkI_destructor,
      tkI_operator:
        IgnoreProc;

      tkI_interface,
      tkI_record,
      tkI_object: // does the developer know what he does?
        IgnoreRecord;

      tkI_reference:
        begin
          NextA;
          if Look.Kind = tkI_to then
          begin
            NextA;
            IgnoreProc;
          end;
        end;

      tkI_class:
        begin
          NextA;
          case Look.Kind of
            tkI_procedure,
            tkI_function,
            tkI_constructor,
            tkI_destructor,
            tkI_operator:
              IgnoreProc;
          else
            FRepeatLastToken := True;
            IgnoreRecord;
          end;
        end;

      tkI_end:
        Break;
    end;
  until not Next;
end;

procedure TDesignerParser.IgnoreProc;
var
  Empty: Boolean;
begin
  if not Next then
    Exit;

  if Look.Kind >= tkIdent then
    if not Next then
      Exit;
  if Look.Kind = tkLParan then
    ParseFunctionArgs(nil);

  if Look.Kind = tkColon then
  begin
    NextA;
    ParseTypeName([]);
  end;

  if Look.Kind <> tkSemicolon then
    NextA;

  while Next do
  begin
    case Look.Kind of
      tkI_implementation,
      tkI_public,
      tkI_private,
      tkI_strict,
      tkI_protected,
      tkI_published,
      tkI_property,
      tkI_class:
        begin
          FRepeatLastToken := True;
          Break;
        end;

      tkI_function,
      tkI_procedure,
      tkI_constructor,
      tkI_destructor,
      tkI_operator,
      tkI_type,
      tkI_var,
      tkI_const,
      tkI_threadvar,
      tkI_end:
        if not FImplementation then
        begin
          FRepeatLastToken := True;
          Break;
        end;

      tkI_begin:
        IgnoreBlock(Empty);
    end;
  end;
end;

function TDesignerParser.IgnoreBlock(out Empty: Boolean): TLocation;
var
  BlockCount: Integer;
begin
  Empty := True;
  if Look.Kind = tkI_begin then
  begin
    NextA;
    FRepeatLastToken := True;
  end;
  Result := TokenToLocation(Look);
  BlockCount := 1;
  while Next do
  begin
    case Look.Kind of
      tkI_end:
        begin
          Dec(BlockCount);
          if BlockCount = 0 then
            Break;
        end;
      tkI_case,
      tkI_try,
      tkI_begin:
        begin
          Inc(BlockCount);
          Empty := False;
        end;
      tkI_procedure,
      tkI_function: // anonymous method
        begin
          IgnoreProc;
          Empty := False;
        end;
    else
      Empty := False;
    end;
  end;
end;

procedure TDesignerParser.IgnoreToClassAccess;
begin
  while Next do
  begin
    case Look.Kind of
      tkI_private,
      tkI_protected,
      tkI_public,
      tkI_published,
      tkI_strict,
      tkI_end:
        Exit;
    end;
  end;
  NextA; // abort
end;

procedure TDesignerParser.IgnoreExternal;
begin
  while Next do
  begin
    case Look.Kind of
      tkI_name,
      tkI_index,
      tkString,
      tkIdent,
      tkPlus:
        ;
    else
      FRepeatLastToken := True;
      Break;
    end;
  end;
end;

function TDesignerParser.ParseQualifiedName: string;
begin
  Result := '';
  repeat
    if Look.Kind >= tkIdent then
      Result := Result + Look.Value
    else
      Break;
    NextA;
    if Look.Kind <> tkDot then
      Break;
    Result := Result + '.';
    NextA;
  until False;
end;

procedure TDesignerParser.ParseClass;
var
  NameToken: TToken;
  C: TCodeClass;
  Access, NewAccess: TAccessKind;
  IntfName: string;
begin
  try
    NameToken := p.Tokens[p.TokenCount - 1 - 3];
    if NameToken.Kind = tkIdent then
    begin
      C := TCodeClass.Create(NameToken.Value, TokenToLocation(NameToken));
      Classes.Add(C);
      C.FStartLocation := C.Location;
      C.FIsUnitPrivate := FImplementation;

      if Look.Kind = tkLParan then
      begin
        NextA;
        C.FBaseClass := ParseQualifiedName;
        while Look.Kind = tkComma do
        begin
          NextA;
          IntfName := ParseQualifiedName;
          if IntfName <> '' then
            C.AddInterface(IntfName);
          NextA;
        end;
        if Look.Kind = tkRParan then
          NextA;
      end
      else
        C.FBaseClass := 'TObject';

      if Look.Kind <> tkSemicolon then
      begin
        FRepeatLastToken := True;
        Access := akPublished;
        NewAccess := Access;

        while Next do
        begin
          case Look.Kind of
            tkI_class:
              begin
                NextA;
                case Look.Kind of
                  tkI_procedure,
                  tkI_function,
                  tkI_constructor,
                  tkI_destructor,
                  tkI_operator:
                    ParseMethodDecl(C, Access, True);
                  tkI_property:
                    ParseProperty(C, Access, True);
                  tkIdent..Pred(tkIdentStrictReserved):
                    ParseField(C, Access, True);
                end;
              end;

            tkI_procedure,
            tkI_function,
            tkI_constructor,
            tkI_destructor,
            tkI_operator:
              ParseMethodDecl(C, Access, False);
            tkI_property:
              ParseProperty(C, Access, False);

            tkIdent..Pred(tkIdentStrictReserved):
              begin
                case Look.Kind of
                  tkI_strict:
                    begin
                      NextA;
                      case Look.Kind of
                        tkI_private:
                          NewAccess := akStrictPrivate;
                        tkI_protected:
                          NewAccess := akStrictProtected;
                      end;
                    end;
                  tkI_private:
                    NewAccess := akPrivate;
                  tkI_protected:
                    NewAccess := akProtected;
                  tkI_public:
                    NewAccess := akPublic;
                  tkI_published:
                    NewAccess := akPublished;
                else
                  ParseField(C, Access, False);
                end;
              end;

            tkI_type,
            tkI_const:
              begin
                // inner classes/constants (Delphi 8+)
                IgnoreToClassAccess;
              end;

            tkI_implementation,
            tkI_begin:
              begin
                FRepeatLastToken := True;
                Break;
              end;
            tkI_end:
              begin
                C.FEndLocation := TokenToLocation(Look);
                Break;
              end;
          end;

          if NewAccess <> Access then
          begin
            if C.FAccessInsert[Access].Filename = '' then
              C.FAccessInsert[Access] := TokenToLocation(Look);
            Access := NewAccess;
          end;
        end;
      end;
    end
    else
      IgnoreRecord;
  except
    on E: EAbort do
      IgnoreRecord;
  end;
end;

procedure TDesignerParser.ParseField(C: TCodeClass; Access: TAccessKind; IsClass: Boolean);
var
  StartIndex, i: Integer;
  F: TCodeField;
begin
  FRepeatLastToken := True; // current token is already the NameToken
  StartIndex := C.FFields.Count;
  ParseVariableList(C.FFields, vmFields);
  for i := StartIndex to C.FFields.Count - 1 do
  begin
    F := C.Fields[i];
    F.FIsClassField := IsClass;
    F.FAccess := Access;
  end;
  if Look.Kind <> tkSemicolon then
    FRepeatLastToken := True;
end;

procedure TDesignerParser.ParseProperty(C: TCodeClass; Access: TAccessKind; IsClass: Boolean);
var
  P: TCodeProperty;
  PropToken, NameToken: TToken;
  IsArray: Boolean;
  IsInheritChange: Boolean;
begin
  PropToken := Look;
  NextA;
  NameToken := Look;
  NextA;

  IsArray := False;
  if Look.Kind = tkLBracket then
  begin
    // array
    NextA;
    IsArray := True;
    while Next and not (Look.Kind in [tkRBracket] + PropertyTokens + ClassSectionEndTokens) do
      ;
  end;

  IsInheritChange := Look.Kind <> tkColon;
  if not IsInheritChange then
    NextA;

  P := TCodeProperty.Create(NameToken.Value, TokenToLocation(NameToken));
  C.AddProperty(P);
  P.FStartLocation := TokenToLocation(PropToken);
  P.FIsClassProperty := IsClass;
  P.FAccess := Access;
  P.FIsArray := IsArray;
  P.FIsInheritChange := IsInheritChange;
  if not IsInheritChange then
    P.FTypeName := ParseTypeName(PropertyTokens + ClassSectionEndTokens);

  if Look.Kind = tkI_read then
  begin
    NextA;
    if Look.Kind >= tkIdent then
      P.FGetter := Look.Value;
    NextA;
  end;
  if Look.Kind = tkI_write then
  begin
    NextA;
    if Look.Kind >= tkIdent then
      P.FSetter := Look.Value;
    NextA;
  end;
  if Look.Kind = tkI_add then
  begin
    NextA;
    if Look.Kind >= tkIdent then
      P.FAdder := Look.Value;
    NextA;
  end;
  if Look.Kind = tkI_remove then
  begin
    NextA;
    if Look.Kind >= tkIdent then
      P.FRemover := Look.Value;
    NextA;
  end;
  if Look.Kind = tkI_read then
  begin
    NextA;
    if Look.Kind >= tkIdent then
      P.FGetter := Look.Value;
    NextA;
  end;
  if Look.Kind = tkI_stored then
  begin
    NextA;
    if Look.Kind >= tkIdent then
      P.FStorer := Look.Value;
    NextA;
  end;
  if Look.Kind = tkI_default then
  begin
    NextA;
    while not (Look.Kind in [tkSemicolon] + ClassSectionEndTokens) do
      NextA;
  end;

  if Look.Kind = tkSemicolon then
  begin
    NextA;
    case Look.Kind of
      tkI_default:
        begin
          P.FIsDefault := True;
          NextA;
        end;
      tkI_nodefault:
        begin
          P.FIsNoDefault := True;
          NextA;
        end;
    else
      FRepeatLastToken := True;
    end;
  end;
  P.FEndLocation := TokenToLocation(Look);
end;

function TDesignerParser.ParseTypeName(const Stop: TTokenKindSet): string;
begin
  Result := Look.Value;
  case Look.Kind of
    tkI_array:
      begin
        NextA;
        Result := 'array';
        if Look.Kind = tkLBracket then
        begin
          Result := 'array[';
          while Next and (Look.Kind <> tkRBracket) do
            if Result[Length(Result)] = ',' then
              Result := Result + ' ' + Look.Value
            else
              Result := Result + Look.Value;
          Result := Result + ']';
          NextA;
        end;
        if Look.Kind = tkI_of then
        begin
          NextA;
          Result := Result + ' of ' + ParseTypeName(Stop);
        end
        else
        begin
          FRepeatLastToken := True;
          Result := Result + ' of errornous';
        end;
      end;
    tkI_set:
      begin
        NextA;
        if Look.Kind = tkI_of then
        begin
          NextA;
          Result := 'set of ';
          if Look.Kind = tkLBracket then
          begin
            Result := Result + '[';
            while Next and (Look.Kind <> tkRBracket) do
              if Result[Length(Result)] = ',' then
                Result := Result + ' ' + Look.Value
              else
                Result := Result + Look.Value;
            Result := Result + ']';
          end
          else if Look.Kind in ClassAvailableTokens then
            Result := Result + Look.Value
          else
          begin
            FRepeatLastToken := True;
            Result := Result + 'errornous';
          end;
        end;
      end;

    tkI_record,
    tkI_object:
      IgnoreRecord;

    tkI_function,
    tkI_procedure:
      IgnoreProc;

  else
    if not (Look.Kind in ClassAvailableTokens) and (Look.Kind <> tkI_string) then
    begin
      Result := 'errornous';
      FRepeatLastToken := True;
    end
    else if Look.Kind = tkI_reference then
    begin
      NextA;
      if Look.Kind = tkI_to then
      begin
        NextA;
        IgnoreProc;
      end;
    end

    else if Look.Kind >= tkIdent then
      Result := ParseQualifiedName;
  end;
end;

procedure TDesignerParser.ParseFunctionArgs(M: TCodeMethod);
begin
  if M = nil then
    ParseVariableList(nil, vmFunctionArguments)
  else
    ParseVariableList(M.FArgs, vmFunctionArguments);
end;

procedure TDesignerParser.ParseVariableList(List: TObjectList; Mode: TVarListMode);
var
  A: TCodeField;
  ArgModifier: TArgModifier;
  StartIndex: Integer;
  NameToken: TToken;
  TypeName: string;
begin
  ArgModifier := amNone;
  StartIndex := 0;
  if List <> nil then
    StartIndex := List.Count;
  while Next do
  begin
    if Mode = vmFunctionArguments then
    begin
      ArgModifier := amNone;
      case Look.Kind of
        tkI_const:
          ArgModifier := amConst;
        tkI_var:
          ArgModifier := amVar;
        tkI_out:
          ArgModifier := amOut;
      end;
      if ArgModifier <> amNone then
        NextA;
    end;

    A := nil;
    if Look.Kind >= tkIdent then
    begin
      NameToken := Look;
      NextA;
      if List <> nil then
      begin
        if Mode = vmFunctionArguments then
        begin
          A := TCodeMethodArg.Create(NameToken.Value, TokenToLocation(NameToken));
          TCodeMethodArg(A).FModifier := ArgModifier;
        end
        else
          A := TCodeField.Create(NameToken.Value, TokenToLocation(NameToken));
        A.FStartLocation := A.Location;
        List.Add(A);
      end;
    end;

    case Look.Kind of
      tkColon:
        begin
          NextA;
          TypeName := ParseTypeName([tkRParan, tkComma]);
          if Look.Kind = tkSemicolon then
          begin
            if A <> nil then
              A.FEndLocation := TokenToLocation(Look);
            NextA;
            FRepeatLastToken := True;
          end
          else
          if A <> nil then
            A.FEndLocation := TokenToLocation(Look);

          if List <> nil then
          begin
            while StartIndex < List.Count do
            begin
              TCodeField(List[StartIndex]).SetTypeNameLater(TypeName);
              Inc(StartIndex);
            end;
          end;
        end;
      tkComma:
        begin
          if A <> nil then
            A.FEndLocation := TokenToLocation(Look);
          Continue;
        end;
    end;

    case Mode of
      vmFunctionArguments:
        if Look.Kind in [tkRParan] + SectionEndTokens - ArgModifierTokens then
        begin
          NextA;
          Break;
        end;
      vmVariables:
        if Look.Kind in SectionEndTokens then
        begin
          //FRepeatLastToken := True;
          Break;
        end;
      vmFields:
        if Look.Kind in ClassSectionEndTokens then
        begin
          //FRepeatLastToken := True;
          Break;
        end;
    end;
  end;
end;

procedure TDesignerParser.ParseMethodDecl(C: TCodeClass; Access: TAccessKind; IsClass: Boolean);
var
  M: TCodeMethod;
  MethodType: TMethodType;
  NameToken: TToken;
  StartToken: TToken;
begin
  StartToken := Look;
  if IsClass then
    StartToken := p.Tokens[p.TokenCount - 1 - 1];
  MethodType := mtFunction;
  case Look.Kind of
    tkI_constructor: MethodType := mtConstructor;
    tkI_destructor: MethodType := mtDestructor;
    tkI_function: MethodType := mtFunction;
    tkI_procedure: MethodType := mtProcedure;
  end;
  NextA;
  if Look.Kind < tkIdent then
  begin
    IgnoreProc;
    Exit;
  end;
  NameToken := Look;
  NextA;

  M := TCodeMethod.Create(NameToken.Value, TokenToLocation(NameToken));
  C.AddMethod(M);
  M.FStartLocation := TokenToLocation(StartToken);
  M.FIsDeclared := True;
  M.FAccess := Access;
  M.FIsClassMethod := IsClass;
  M.FMethodType := MethodType;
  if Look.Kind = tkLParan then
    ParseFunctionArgs(M);
  if Look.Kind = tkColon then
  begin
    NextA;
    M.FReturnTypeName := ParseTypeName([tkI_cdecl, tkI_stdcall, tkI_register,
      tkI_safecall, tkI_pascal, tkI_overload,
      tkI_reintroduce, tkI_virtual, tkI_override, tkI_abstract, tkI_message, tkI_dynamic,
      tkI_forward, tkI_export, tkI_external]);
  end;

  ParseDirectives(M);
  if Look.Kind = tkSemicolon then
  begin
    NextA;
    FRepeatLastToken := True;
  end;
  M.FEndLocation := TokenToLocation(Look);
end;

procedure TDesignerParser.ParseUses(List: TUsesList);
var
  StartToken, NameToken: TToken;
  Name, Filename: string;
  Item: TUsesItem;
begin
  List.FLocation := TokenToLocation(Look);
  while Next do
  begin
    if Look.Kind in SectionEndTokens then
    begin
      FRepeatLastToken := True;
      Break;
    end;
    if Look.Kind >= tkIdent then
    begin
      StartToken := Look;
      NameToken := StartToken;
      Name := ParseQualifiedName;
      Filename := '';
      if Look.Kind = tkI_in then
      begin
        NextA;
        if Look.Kind = tkString then
          Filename := AnsiDequotedStr(Look.Value, '''');
        if Look.Kind = tkSemicolon then
          Break;
        NextA;
      end;

      Item := TUsesItem.Create(Name, Filename, TokenToLocation(NameToken));
      List.AddUses(Item);
      Item.FStartLocation := TokenToLocation(StartToken);
      Item.FEndLocation := TokenToLocation(Look);
    end;
    if Look.Kind = tkSemicolon then
    begin
      List.FEndLocation := TokenToLocation(Look);
      FRepeatLastToken := True;
      Break;
    end;
  end;
end;

procedure TDesignerParser.ParseFunctionImpl(AParent: TCodeMethod);
var
  M, MatchM: TCodeMethod;
  MethodType: TMethodType;
  StartToken, NameToken: TToken;
  MethodClass: TCodeClass;
  i: Integer;
  IsClass, Empty: Boolean;
begin
  FRepeatLastToken := False;

  IsClass := (p.TokenCount > 1) and (p.Tokens[p.TokenCount - 1 - 1].Kind = tkI_class);
  StartToken := Look;
  if IsClass then
    StartToken := p.Tokens[p.TokenCount - 1 - 1];
  if (p.TokenCount > 2) then
    if p.Tokens[p.TokenCount - 2].Kind in [tkColon, tkEqual] then // ignore function variables/types
    begin
      IgnoreFunctionType;
      Exit;
    end;

  MethodType := mtFunction;
  case Look.Kind of
    tkI_constructor: MethodType := mtConstructor;
    tkI_destructor: MethodType := mtDestructor;
    tkI_function: MethodType := mtFunction;
    tkI_procedure: MethodType := mtProcedure;
  end;
  NextA;
  if Look.Kind < tkIdent then
  begin
    IgnoreProc;
    Exit;
  end;
  NameToken := Look;
  NextA;

  MethodClass := nil;
  while Look.Kind = tkDot do
  begin
    MethodClass := Classes.Find(NameToken.Value);
    if MethodClass = nil then
    begin
      IgnoreProc;
      Exit;
    end;
    NextA;
    NameToken := Look;
    if NameToken.Kind < tkIdent then
    begin
      IgnoreProc;
      Exit;
    end;
    NextA;
  end;
  M := TCodeMethod.Create(NameToken.Value, EmptyLocation);
  try
    M.FIsImplemented := True;
    M.FImplLocation := TokenToLocation(NameToken);
    M.FIsClassMethod := IsClass;
    M.FMethodType := MethodType;

    if Look.Kind = tkLParan then
      ParseFunctionArgs(M);
    if Look.Kind = tkColon then
    begin
      NextA;
      M.FReturnTypeName := ParseTypeName([tkI_Cdecl, tkI_Stdcall, tkI_Register,
        tkI_Safecall, tkI_Pascal, tkI_external, tkI_export, tkI_forward]);
    end
    else if Look.Kind = tkI_of then
    begin
      NextA;
      if Look.Kind = tkI_object then
        NextA;
    end;

    MatchM := nil;
    if MethodClass <> nil then
    begin
      // find a matching method
      for i := 0 to MethodClass.MethodCount - 1 do
      begin
        if SameText(MethodClass.Methods[i].Name, M.Name) and
           MethodClass.Methods[i].IsCompatible(M) then
        begin
          MatchM := MethodClass.Methods[i];
          Break;
        end;
      end;
      if (MatchM = nil) and (M.ArgCount = 0) and (M.ReturnTypeName = '') then
        MatchM := MethodClass.FindMethod(M.Name);
    end
    else
    begin
      // find a matching method
      for i := 0 to FunctionCount - 1 do
      begin
        if SameText(Functions[i].Name, M.Name) and
           Functions[i].IsCompatible(M) then
        begin
          MatchM := Functions[i];
          Break;
        end;
      end;
      {if (MatchM = nil) and (M.ArgCount = 0) and (M.ReturnTypeName = '') then
        MatchM := MethodClass.FindMethod(M.Name);}
    end;

    if MatchM <> nil then
    begin
      MatchM.FImplLocation := M.ImplLocation;
      MatchM.FIsImplemented := True;
    end
    else
    begin
      if AParent <> nil then
      begin
        M.FIsImplemented := True;
        AParent.AddLocalProc(M);
      end
      else if MethodClass = nil then
      begin
        M.FIsImplemented := True;
        if FImplementation then
          AddFunction(M);
      end
      else
        MethodClass.AddMethod(M);
      MatchM := M;
      if (MethodClass <> nil) or FImplementation then
        M := nil;
    end;

    if (MethodClass <> nil) and (M <> nil) then
      ParseDirectives(M) // here M not MatchM
    else
      ParseDirectives(MatchM);

    if Look.Kind = tkI_forward then
      NextA
    else if FImplementation then
    begin
      MatchM.FIsImplEmpty := True;
      while (Look <> nil) and (Look.Kind <> tkI_begin) do
      begin
        MatchM.FIsImplEmpty := False;
        case Look.Kind of
          tkI_record,
          tkI_object,
          tkI_class:
            IgnoreRecord;

          tkI_procedure,
          tkI_function:
            ParseFunctionImpl(MatchM);

          tkI_forward,
          tkI_end:
            Break;

          tkI_var:
            begin
              MatchM.FVarInsert := TokenToLocation(Look);
              NextA;
              ParseVariableDecl(MatchM.FLocalVariables);
            end;
        end;
        NextA;
      end;
    end;

    if (Look <> nil) and (Look.Kind = tkI_begin) then
      MatchM.FCodeInsert := IgnoreBlock(Empty);
    MatchM.FIsImplEmpty := MatchM.FIsImplEmpty and Empty;

    if Look.Kind = tkSemicolon then
    begin
      NextA;
      FRepeatLastToken := True;
    end;
    if FImplementation and (Look.Kind = tkI_end) then
    begin
      NextA;
      if Look.Kind = tkI_end then
        NextA;
      FRepeatLastToken := True;
    end;

    MatchM.FImplBeginLocation := TokenToLocation(StartToken);
    MatchM.FImplEndLocation := TokenToLocation(Look);
  finally
    M.Free;
  end;
end;

procedure TDesignerParser.ParseDirectives(M: TCodeMethod);
begin
  repeat
    case Look.Kind of
      tkSemicolon:
        ;
      tkI_Overload:
        ;

      tkI_static:
        M.FIsClassStatic := True;
      tkI_message:
        M.FVirtualKind := vkMessage;
      tkI_override:
        M.FVirtualKind := vkOverride;
      tkI_virtual:
        M.FVirtualKind := vkVirtual;
      tkI_abstract:
        M.FVirtualKind := vkAbstract;
      tkI_dynamic:
        M.FVirtualKind := vkDynamic;

      tkI_cdecl:
        M.FConvention := ccCdecl;
      tkI_stdcall:
        M.FConvention := ccStdcall;
      tkI_register:
        M.FConvention := ccRegister;
      tkI_safecall:
        M.FConvention := ccSafecall;
      tkI_pascal:
        M.FConvention := ccPascal;
      tkI_reintroduce:
        ;

      tkI_assembler:
        ;

      tkI_external:
        IgnoreExternal;

      {tkI_begin,
      tkI_end:
        Break;}

    else
      FRepeatLastToken := True;
      Break;
    end;
    NextA;
  until False;
end;

procedure TDesignerParser.IgnoreFunctionType;
var
  M: TCodeMethod;
begin
  Next; 
  if Look.Kind = tkLParan then
  begin
    M := TCodeMethod.Create('', TokenToLocation(Look));
    try
      ParseFunctionArgs(M);
    finally
      M.Free;
    end;
  end;
  if Look.Kind = tkColon then
  begin
    NextA;
    ParseTypeName([tkI_Cdecl, tkI_Stdcall, tkI_Register,
      tkI_Safecall, tkI_Pascal, tkI_external, tkI_export]);
  end;
  if Look.Kind = tkI_of then
  begin
    NextA;
    if Look.Kind = tkI_object then
      NextA;
  end;
  if Look.Kind = tkSemicolon then
  begin
    NextA;
    FRepeatLastToken := True;
  end;
end;

procedure TDesignerParser.IgnoreInterface;
begin
  IgnoreRecord;
end;

procedure TDesignerParser.GetDeclared(Sender: TObject; const Name: string;
  var Value: Boolean);
begin
  Value := InterfaceUses.FindUses(Name) <> nil;
  if not Value then
    Value := ImplUses.FindUses(Name) <> nil;
  if not Value then
    Value := Classes.Find(Name) <> nil;
  if not Value then
    Value := FindFunction(Name) <> nil;
end;

procedure TDesignerParser.GetConstValue(Sender: TObject; const Name: string; var Kind: TTokenKind;
  var Value: string);
var
  S: ShortString;
begin
  if SameText(Name, 'CompilerVersion') then
  begin
    Kind := tkFloat;
    Str(CompilerVersion:1:1, S);
    Value := UTF8ToString(S);
  end;
end;

procedure TDesignerParser.GetConstBoolValue(Sender: TObject; const Name: string; var Value: Boolean);
begin

end;

function TDesignerParser.FindFunction(const AName: string): TCodeMethod;
var
  i: Integer;
begin
  for i := 0 to FunctionCount - 1 do
    if SameText(Functions[i].Name, AName) then
    begin
      Result := Functions[i];
      Exit;
    end;
  Result := nil;
end;

procedure TDesignerParser.ParseVariableDecl(List: TObjectList);
begin
  ParseVariableList(List, vmVariables);
end;

end.
