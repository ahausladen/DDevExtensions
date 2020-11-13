{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit PluginConfig;

{$I DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, SimpleXmlIntf, SimpleXmlImport, FrmTreePages, TypInfo;

type
  TPluginConfig = class(TComponent)
  private
    FFilename: string;
    FRootNodeName: string;
    FLoading: Boolean;
    procedure LoadFromFile(const Filename: string); 
  protected
    function GetOptionPages: TTreePage; virtual;
    procedure Init; virtual;
    procedure RegisterOptionPages; virtual;

    property Loading: Boolean read FLoading;
  public
    constructor Create(const AFilename, ARootNodeName: string); reintroduce;

    procedure SaveToXml(Node: IXmlNode); virtual;
    procedure LoadFromXml(Node: IXmlNode); virtual;

    procedure Save;

    property RootNodeName: string read FRootNodeName;
  end;

  TConfiguration = class(TObject)
  private
    FModified: Boolean;
    FFilename: string;
    FUpdateLock: Integer;
    FXml: IXmlDocument;
  public
    constructor Create;
    destructor Destroy; override;

    function HasNode(const NodeName: string): Boolean;
    function FindNode(const NodeName: string): IXmlNode;
    function GetNode(const NodeName: string): IXmlNode; overload;
    class function GetNode(ParentNode: IXmlNode; const NodeName: string): IXmlNode; overload;
    procedure Modified;

    procedure SaveToFile(const AFilename: string);
    procedure LoadFromFile(const AFilename: string);
    procedure Save;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Filename: string read FFilename;
  end;

function Configuration: TConfiguration;

implementation

uses
  FrmDDevExtOptions, IDEUtils, IDEHooks, Main;

var
  GlobalConfiguration: TConfiguration;

function Configuration: TConfiguration;
begin
  if GlobalConfiguration = nil then
  begin
    if AppDataDirectory = '' then
      InitAppDataDirectory;
    
    GlobalConfiguration := TConfiguration.Create;
    GlobalConfiguration.LoadFromFile(AppDataDirectory + '\DDevExtensions' + DelphiVersion + '.xml');
  end;
  Result := GlobalConfiguration;
end;


{$IFDEF COMPILER5}
function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer; overload;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function GetPropList(AObject: TObject; out PropList: PPropList): Integer; overload;
begin
  Result := GetPropList(PTypeInfo(AObject.ClassInfo), PropList);
end;
{$ENDIF COMPILER5}

{ TPluginConfig }

constructor TPluginConfig.Create(const AFilename, ARootNodeName: string);
begin
  inherited Create(nil);
  FFilename := ChangeFileExt(AFilename, '') + DelphiVersion + ExtractFileExt(AFilename);
  FRootNodeName := ARootNodeName;

  Init;

  if Configuration.HasNode(RootNodeName) then
    LoadFromXml(Configuration.GetNode(RootNodeName))
  else
  if FileExists(FFilename) then
  try
    LoadFromFile(FFilename);
    Save; // save to Configuration
    DeleteFile(FFilename);
  except
  end;

  RegisterOptionPages;
end;

function TPluginConfig.GetOptionPages: TTreePage;
begin
  Result := nil;
end;

procedure TPluginConfig.Init;
begin
end;

procedure TPluginConfig.LoadFromFile(const Filename: string);
var
  Doc: IXmlDocument;
begin
  Doc := LoadXmlDocument(Filename);
  LoadFromXml(Doc.DocumentElement);
end;

procedure TPluginConfig.RegisterOptionPages;
begin
  TFormDDevExtOptions.RegisterPages(GetOptionPages);
end;

procedure TPluginConfig.Save;
begin
  Configuration.GetNode(FRootNodeName).ChildNodes.Clear;
  SaveToXml(Configuration.GetNode(FRootNodeName));
  Configuration.Modified;
end;

procedure TPluginConfig.SaveToXml(Node: IXmlNode);
var
  i: Integer;
  PropList: PPropList;
  Info: PPropInfo;
  Count: Integer;
  HasActive: Boolean;
  Obj: TObject;
  PropName: string;
begin
  HasActive := False;
  Info := GetPropInfo(Self, 'Active', tkProperties);
  if (Info <> nil) and (Info.PropType^.Kind = tkEnumeration) then
  begin
    Node.Attributes['Active'] := GetEnumProp(Self, Info);
    HasActive := True;
  end;

  Count := GetPropList(ClassInfo, PropList);
  try
    for i := 0 to Count - 1 do
    begin
      PropName := string(PropList[i].Name);
      if ((AnsiCompareText(PropName, 'Active') <> 0) or not HasActive) and
         (AnsiCompareText(PropName, 'Tag') <> 0) and
         (AnsiCompareText(PropName, 'Name') <> 0) then
      begin
        Node.ChildNodes.DeleteNodes(PropName);;
        case PropList[i].PropType^.Kind of
          tkInteger:
            TConfiguration.GetNode(Node, PropName).Attributes['Value'] := GetOrdProp(Self, PropList[i]);
          tkString, tkUString, tkLString:
            TConfiguration.GetNode(Node, PropName).NodeValue := GetStrProp(Self, PropList[i]);
          tkWString:
            TConfiguration.GetNode(Node, PropName).NodeValue := GetWideStrProp(Self, PropList[i]);
          tkVariant:
            TConfiguration.GetNode(Node, PropName).NodeValue := GetVariantProp(Self, PropList[i]);
          tkFloat:
            TConfiguration.GetNode(Node, PropName).Attributes['Value'] := GetFloatProp(Self, PropList[i]);
          tkEnumeration:
            TConfiguration.GetNode(Node, PropName).Attributes['Value'] := GetEnumProp(Self, PropList[i]);
          tkClass:
            begin
              Obj := GetObjectProp(Self, PropList[i]);
              if Obj is TStrings then
                TConfiguration.GetNode(Node, PropName).NodeValue := TStrings(Obj).CommaText;
            end;
        end;
      end;
    end;
  finally
    if Assigned(PropList) then
      FreeMem(PropList);
  end;
end;

procedure TPluginConfig.LoadFromXml(Node: IXmlNode);
var
  i: Integer;
  PropList: PPropList;
  Info: PPropInfo;
  Count: Integer;
  HasActive: Boolean;
  Xml, N: IXmlNode;
  Obj: TObject;
  PropName: string;
begin
  FLoading := True;
  try
    if Node <> nil then
    begin
      Info := GetPropInfo(Self, 'Active', tkProperties);
      HasActive := (Info <> nil) and (Info.PropType^.Kind = tkEnumeration) and (Node.Attributes['Active'] <> Null);

      Count := GetPropList(ClassInfo, PropList);
      try
        for i := 0 to Count - 1 do
        begin
          PropName := string(PropList[i].Name);
          if ((AnsiCompareText(PropName, 'Active') <> 0) or not HasActive) and
             (AnsiCompareText(PropName, 'Tag') <> 0) and
             (AnsiCompareText(PropName, 'Name') <> 0) then
          begin
            Xml := Node.ChildNodes.FindNode(PropName);
            if (Xml <> nil) and ((PropList[i].PropType^.Kind in [tkString, tkUString, tkLString, tkWString, tkVariant]) or (Xml.Attributes['Value'] <> Null)) then
            begin
              case PropList[i].PropType^.Kind of
                tkInteger:
                  SetOrdProp(Self, PropList[i], Xml.Attributes['Value']);
                tkString, tkLString, tkUString:
                  SetStrProp(Self, PropList[i], VarToStr(Xml.NodeValue));
                tkWString:
                  SetWideStrProp(Self, PropList[i], VarToWideStr(Xml.NodeValue));
                tkVariant:
                  SetVariantProp(Self, PropList[i], Xml.NodeValue);
                tkFloat:
                  SetFloatProp(Self, PropList[i], Xml.Attributes['Value']);
                tkEnumeration:
                  if VarToStr(Xml.Attributes['Value']) <> '' then
                  begin
                    try
                      SetEnumProp(Self, PropList[i], VarToStr(Xml.Attributes['Value']));
                    except
                      // ignore exception if the enumeration item doesn't exist (anymore)
                    end;
                  end;
                tkClass:
                  begin
                    Obj := GetObjectProp(Self, PropList[i]);
                    if Obj is TStrings then
                    begin
                      N := Node.ChildNodes.FindNode(PropName);
                      if N <> nil then
                        TStrings(Obj).CommaText := N.NodeValue;
                    end;
                  end;
              end;
            end;
          end;
        end;
        if HasActive and (VarToStr(Node.Attributes['Active']) <> '') then
          SetEnumProp(Self, Info, VarToStr(Node.Attributes['Active']));
      finally
        if Assigned(PropList) then
          FreeMem(PropList);
      end;
    end;
  finally
    FLoading := False;
    Loaded;
  end;
end;

{ TConfiguration }

constructor TConfiguration.Create;
begin
  inherited Create;
  FXml := NewXmlDocument;
  FXml.DocumentElement := FXml.CreateElement('DDevExtensions', '');
end;

destructor TConfiguration.Destroy;
begin
  FXml := nil;
  inherited Destroy;
end;

procedure TConfiguration.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TConfiguration.EndUpdate;
begin
  Assert(FUpdateLock > 0, 'Unpaired call of EndUpdate');
  Dec(FUpdateLock);
  if (FUpdateLock = 0) and FModified then
    Save;
end;

function TConfiguration.GetNode(const NodeName: string): IXMLNode;
begin
  Result := GetNode(FXml.DocumentElement, NodeName);
end;

class function TConfiguration.GetNode(ParentNode: IXmlNode; const NodeName: string): IXmlNode;
begin
  Result := ParentNode.ChildNodes.FindNode(NodeName);
  if Result = nil then
    Result := ParentNode.AddChild(NodeName);
end;

function TConfiguration.HasNode(const NodeName: string): Boolean;
begin
  Result := FXml.DocumentElement.ChildNodes.FindNode(NodeName) <> nil;
end;

function TConfiguration.FindNode(const NodeName: string): IXmlNode;
begin
  Result := FXml.DocumentElement.ChildNodes.FindNode(NodeName);
end;

procedure TConfiguration.LoadFromFile(const AFilename: string);
begin
  FFilename := AFilename;
  try
    if FileExists(Filename) then
      FXml := LoadXmlDocument(AFilename)
    else
    begin
      FXml := NewXmlDocument;
      FXml.DocumentElement := FXml.CreateElement('DDevExtensions', '');
    end;
    FXml.Options := FXml.Options + [doNodeAutoIndent];
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
  FModified := False;
end;

procedure TConfiguration.Modified;
begin
  FModified := True;
  if (Filename <> '') and (FUpdateLock = 0) then
    Save;
end;

procedure TConfiguration.Save;
begin
  Assert(Filename <> '');

  ForceDirectories(ExtractFileDir(Filename));
  FXml.SaveToFile(Filename);
  FModified := False;
end;

procedure TConfiguration.SaveToFile(const AFilename: string);
begin
  FXml.SaveToFile(AFilename);
end;

initialization

finalization
  FreeAndNil(GlobalConfiguration);

end.
