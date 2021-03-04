{******************************************************************************}
{*                                                                            *}
{* (C) 2005,2006 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit SimpleXmlDoc;

{$I ..\jedi\jedi.inc}

interface

{$IF CompilerVersion < 23.0}
{.$DEFINE JEDI_XML}
{$IFEND}

uses
  {$IFDEF JEDI_XML}
  JclSimpleXml, JclStreams,
  {$ELSE}
  XMLIntf, XMLDoc, xmldom,
  {$ENDIF JEDI_XML}
  SimpleXmlIntf,
  Classes;

type
  {$IFDEF COMPILER5}
  IInterface = IUnknown;
  {$ENDIF COMPILER5}

  TXmlDocument = class(TComponent, IInterface, IXmlDocument)
  private
    {$IFDEF JEDI_XML}
    FXml: TJclSimpleXML;
    FActive: Boolean;
    {$ELSE}
    FXml: XMLIntf.IXMLDocument;
    {$ENDIF JEDI_XML}
    FRefCount: Integer;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetDocumentElement: IXmlNode;
    procedure SetDocumentElement(Value: IXmlNode);
    function GetRoot: IXmlNode;
    function GetOptions: TXmlDocOptions;
    procedure SetOptions(const Value: TXmlDocOptions);
    function GetChildNodes: IXmlChildNodes;

    function CreateElement(const Tag, NamespaceURI: string): IXmlNode;
    function CreateNode(const Name: string; NodeType: TNodeType = ntElement;
      const AddlData: string = ''): IXmlNode;

    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);

    property Active: Boolean read GetActive write SetActive;
    property Root: IXmlNode read GetRoot;
    property DocumentElement: IXmlNode read GetDocumentElement write SetDocumentElement;
    property Version: string read GetVersion write SetVersion;
    property Options: TXmlDocOptions read GetOptions write SetOptions;
    property ChildNodes: IXmlChildNodes read GetChildNodes;
  end;

function LoadXmlDocument(const Filename: string): IXmlDocument;
function NewXmlDocument(const Version: string = ''): IXmlDocument;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  SysUtils, Contnrs;

{$IFDEF COMPILER5}
function InterlockedIncrement(var Addend: Integer): Integer; stdcall;
  external 'kernel32.dll' name 'InterlockedIncrement';

function InterlockedDecrement(var Addend: Integer): Integer; stdcall;
  external 'kernel32.dll' name 'InterlockedDecrement';
{$ENDIF COMPILER5}

{$IFDEF JEDI_XML}
type
  IInternalXmlNode = interface
    ['{0757223F-94C9-4FE6-AB72-E8E88494195E}']
    function GetNode: TJclSimpleXMLElem;
  end;

  TXmlAttributes = class(TInterfacedObject, IXmlAttributes)
  private
    FNode: TJclSimpleXMLElem;
  public
    constructor Create(ANode: TJclSimpleXMLElem);

    procedure SetAttribute(const Index: string; const Value: Variant);
    function GetAttribute(const Index: string): Variant;
    property Attributes[const Index: string]: Variant read GetAttribute write SetAttribute; default;
  end;

  TXmlChildNodes = class(TInterfacedObject, IXmlChildNodes)
  private
    FNodes: TJclSimpleXMLElems;
  public
    constructor Create(ANodes: TJclSimpleXMLElems);

    function GetCount: Integer;
    function GetNode(Index: Integer): IXmlNode;
    function FindNode(const NodeName: string): IXmlNode;

    procedure Add(Node: IXmlNode);
    procedure Clear;
    procedure DeleteNodes(const NodeName: string);

    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: IXmlNode read GetNode; default;
  end;

  TXmlNode = class(TInterfacedObject, IXmlNode, IInternalXmlNode)
  private
    FChilds: IXmlChildNodes;
    FAttributes: IXmlAttributes;
    FNode: TJclSimpleXMLElem;
  protected
    function GetNode: TJclSimpleXMLElem;
  public
    constructor Create(ANode: TJclSimpleXMLElem);
    destructor Destroy; override;

    function GetNodeName: string;
    function GetNodeValue: Variant;
    function GetChildNodes: IXmlChildNodes;
    function GetAttributes: IXmlAttributes;
    function GetText: string;
    procedure SetText(const Value: string);
    //procedure SetNodeName(const Value: string);
    procedure SetNodeValue(const Value: Variant);

    function AddChild(const NodeName: string): IXmlNode;

    property NodeName: string read GetNodeName {write SetNodeName};
    property NodeValue: Variant read GetNodeValue write SetNodeValue;
    property ChildNodes: IXmlChildNodes read GetChildNodes;
    property Attributes: IXmlAttributes read GetAttributes;
    property Text: string read GetText write SetText;
  end;

function LoadXmlDocument(const Filename: string): IXmlDocument;
begin
  Result := TXmlDocument.Create(nil);
  Result.LoadFromFile(Filename);
  Result.Active := True;
end;

function NewXmlDocument(const Version: string): IXmlDocument;
begin
  Result := TXmlDocument.Create(nil);
  Result.Active := True;
  if Version <> '' then
    Result.Version := Version;
end;

{ TXmlDocument }

constructor TXmlDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXml := TJclSimpleXML.Create;
  FXml.Prolog.Encoding := 'UTF-8';
end;

function TXmlDocument.CreateElement(const Tag, NamespaceURI: string): IXmlNode;
var
  JvNode: TJclSimpleXMLElemClassic;
begin
  JvNode := TJclSimpleXMLElemClassic.Create(FXml.Root);
  JvNode.NameSpace := NamespaceURI;
  JvNode.Name := Tag;
  Result := TXmlNode.Create(JvNode);
end;

function TXmlDocument.CreateNode(const Name: string; NodeType: TNodeType;
  const AddlData: string): IXmlNode;
begin
  Result := Root; // trick to ignore it
end;

destructor TXmlDocument.Destroy;
begin
  FXml.Free;
  inherited Destroy;
end;

function TXmlDocument.GetActive: Boolean;
begin
  Result := FActive;
end;

function TXmlDocument.GetChildNodes: IXmlChildNodes;
begin
  Result := Root.ChildNodes;
end;

function TXmlDocument.GetDocumentElement: IXMLNode;
begin
  Result := Root;
end;

function TXmlDocument.GetOptions: TXmlDocOptions;
begin
  Result := [];
end;

function TXmlDocument.GetRoot: IXmlNode;
begin
  Result := TXmlNode.Create(FXml.Root);
end;

function TXmlDocument.GetVersion: string;
begin
  Result := FXml.Prolog.Version;
end;

procedure TXmlDocument.LoadFromFile(const Filename: string);
begin
  FXml.LoadFromFile(Filename);
end;

procedure TXmlDocument.SaveToFile(const Filename: string);
begin
  FXml.SaveToFile(Filename, seUTF8);
end;

procedure TXmlDocument.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

procedure TXmlDocument.SetDocumentElement(Value: IXMLNode);
begin
  if Value <> nil then
    FXml.Root := (Value as IInternalXmlNode).GetNode as TJclSimpleXMLElemClassic
  else
    FXml.Root := nil;
end;

procedure TXmlDocument.SetOptions(const Value: TXmlDocOptions);
begin
end;

procedure TXmlDocument.SetVersion(const Value: string);
begin
  FXml.Prolog.Version := Version;
end;

function TXmlDocument._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount)
end;

function TXmlDocument._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if (Result = 0) and (Owner = nil) then
    Destroy;
end;

{ TXmlAttributes }

constructor TXmlAttributes.Create(ANode: TJclSimpleXMLElem);
begin
  inherited Create;
  FNode := ANode;
end;

function TXmlAttributes.GetAttribute(const Index: string): Variant;
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := FNode.Properties.ItemNamed[Index];
  if Prop <> nil then
    Result := Prop.Value
  else
    Result := '';
end;

procedure TXmlAttributes.SetAttribute(const Index: string; const Value: Variant);
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := FNode.Properties.ItemNamed[Index];
  if Prop = nil then
    Prop := FNode.Properties.Add(Index, '');
  Prop.Value := Value
end;

{ TXmlNode }

constructor TXmlNode.Create(ANode: TJclSimpleXMLElem);
begin
  inherited Create;
  FNode := ANode;
  FChilds := TXmlChildNodes.Create(ANode.Items);
  FAttributes := TXmlAttributes.Create(ANode);
end;

destructor TXmlNode.Destroy;
begin
  FAttributes := nil;
  FChilds := nil;
  inherited Destroy;
end;

function TXmlNode.AddChild(const NodeName: string): IXmlNode;
begin
  Result := TXmlNode.Create(FNode.Items.Add(NodeName));
end;

function TXmlNode.GetAttributes: IXmlAttributes;
begin
  Result := FAttributes;
end;

function TXmlNode.GetChildNodes: IXmlChildNodes;
begin
  Result := FChilds;
end;

function TXmlNode.GetNode: TJclSimpleXMLElem;
begin
  Result := FNode;
end;

function TXmlNode.GetNodeName: string;
begin
  Result := FNode.Name;
end;

function TXmlNode.GetNodeValue: Variant;
begin
  Result := FNode.Value;
end;

function TXmlNode.GetText: string;
begin
  Result := FNode.Value
end;

{procedure TXmlNode.SetNodeName(const Value: string);
begin
  FNode.Name := Value;
end;}

procedure TXmlNode.SetNodeValue(const Value: Variant);
begin
  FNode.Value := Value;
end;

procedure TXmlNode.SetText(const Value: string);
begin
  FNode.Value := Value;
end;

{ TXmlChildNodes }

procedure TXmlChildNodes.Add(Node: IXmlNode);
begin
  if Node <> nil then
    FNodes.Add((Node as IInternalXmlNode).GetNode);
end;

constructor TXmlChildNodes.Create(ANodes: TJclSimpleXMLElems);
begin
  inherited Create;
  FNodes := ANodes;
end;

procedure TXmlChildNodes.DeleteNodes(const NodeName: string);
var
  I: Integer;
begin
  for I := FNodes.Count - 1 downto 0 do
    if AnsiSameText(FNodes[I].Name, NodeName) then
      FNodes.Delete(I);
end;

function TXmlChildNodes.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

function TXmlChildNodes.GetNode(Index: Integer): IXmlNode;
begin
  Result := TXmlNode.Create(FNodes[Index]);
end;

function TXmlChildNodes.FindNode(const NodeName: string): IXmlNode;
var
  i: Integer;
begin
  for i := 0 to FNodes.Count - 1 do
    if CompareText(NodeName, FNodes[i].Name) = 0 then
    begin
      Result := Nodes[i];
      Exit;
    end;
  Result := nil;
end;

procedure TXmlChildNodes.Clear;
begin
  FNodes.Clear;
end;

{$ELSE ~JEDI_XML}

type
  IInternalXmlNode = interface
    ['{0757223F-94C9-4FE6-AB72-E8E88494195E}']
    function GetNode: XMLIntf.IXMLNode;
  end;

  TXmlAttributes = class(TInterfacedObject, IXmlAttributes)
  private
    FNode: XMLIntf.IXMLNode;
  public
    constructor Create(ANode: XMLIntf.IXMLNode);

    procedure SetAttribute(const Index: string; const Value: Variant);
    function GetAttribute(const Index: string): Variant;
    property Attributes[const Index: string]: Variant read GetAttribute write SetAttribute; default;
  end;

  TXmlChildNodes = class(TInterfacedObject, IXmlChildNodes)
  private
    FNodes: XMLIntf.IXMLNodeList;
  public
    constructor Create(ANodes: XMLIntf.IXMLNodeList);

    function GetCount: Integer;
    function GetNode(Index: Integer): IXmlNode;
    function FindNode(const NodeName: string): IXmlNode;

    procedure Add(Node: IXmlNode);
    procedure Clear;
    procedure DeleteNodes(const NodeName: string);

    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: IXmlNode read GetNode; default;
  end;

  TXmlNode = class(TInterfacedObject, IXmlNode, IInternalXmlNode)
  private
    FChilds: IXmlChildNodes;
    FAttributes: IXmlAttributes;
    FNode: XMLIntf.IXMLNode;
  protected
    function GetNode: XMLIntf.IXMLNode;
  public
    constructor Create(ANode: XMLIntf.IXMLNode);
    destructor Destroy; override;

    function GetNodeName: string;
    function GetNodeValue: Variant;
    function GetChildNodes: IXmlChildNodes;
    function GetAttributes: IXmlAttributes;
    function GetText: string;
    procedure SetText(const Value: string);
    //procedure SetNodeName(const Value: string);
    procedure SetNodeValue(const Value: Variant);

    function AddChild(const NodeName: string): IXmlNode;

    property NodeName: string read GetNodeName {write SetNodeName};
    property NodeValue: Variant read GetNodeValue write SetNodeValue;
    property ChildNodes: IXmlChildNodes read GetChildNodes;
    property Attributes: IXmlAttributes read GetAttributes;
    property Text: string read GetText write SetText;
  end;

function LoadXmlDocument(const Filename: string): IXmlDocument;
begin
  Result := TXmlDocument.Create(nil);
  Result.LoadFromFile(Filename);
  Result.Active := True;
end;

function NewXmlDocument(const Version: string): IXmlDocument;
begin
  Result := TXmlDocument.Create(nil);
  Result.Active := True;
  if Version <> '' then
    Result.Version := Version;
end;

{ TXmlDocument }

constructor TXmlDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXml := XmlDoc.NewXMLDocument();
  FXml.Encoding := 'UTF-8';
end;

function TXmlDocument.CreateElement(const Tag, NamespaceURI: string): IXmlNode;
begin
  Result := TXmlNode.Create(FXml.CreateElement(Tag, NamespaceURI));
end;

function TXmlDocument.CreateNode(const Name: string; NodeType: TNodeType;
  const AddlData: string): IXmlNode;
begin
  Result := Root; // trick to ignore it
end;

destructor TXmlDocument.Destroy;
begin
  FXml := nil;
  inherited Destroy;
end;

function TXmlDocument.GetActive: Boolean;
begin
  Result := FXml.Active;
end;

function TXmlDocument.GetChildNodes: IXmlChildNodes;
begin
  Result := Root.ChildNodes;
end;

function TXmlDocument.GetDocumentElement: IXMLNode;
begin
  Result := Root;
end;

function TXmlDocument.GetOptions: TXmlDocOptions;
begin
  Result := [];
end;

function TXmlDocument.GetRoot: IXmlNode;
begin
  Result := TXmlNode.Create(FXml.DocumentElement);
end;

function TXmlDocument.GetVersion: string;
begin
  Result := FXml.Version;
end;

procedure TXmlDocument.LoadFromFile(const Filename: string);
var
  Lines: TStrings;
  S: string;
begin
  try
    FXml.LoadFromFile(Filename);
  except
    on EDOMParseError do
    begin
      // The JclSimpleXml writes '<?xml encoding="UTF-8" version="1.0" ...' but that isn't valid
      // XML, because it must be '<?xml version="1.0" encoding="UTF-8" ...' according to MSXML.
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(FileName);
        while (Lines.Count > 0) and (Trim(Lines[0]) = '') do
          Lines.Delete(0);
        if Lines.Count > 0 then
        begin
          S := Trim(Lines[0]);
          if StrLIComp('<?xml', PChar(S), 5) = 0 then
          begin
            Lines[0] := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>';
            Lines.SaveToFile(Filename);
            FXml.LoadFromFile(Filename);
            Exit;
          end;
        end;
      finally
        Lines.Free;
      end;
      raise;
    end;
  end;
end;

procedure TXmlDocument.SaveToFile(const Filename: string);
begin
  FXml.SaveToFile(Filename);
end;

procedure TXmlDocument.SetActive(Value: Boolean);
begin
  FXml.Active := Value;
end;

procedure TXmlDocument.SetDocumentElement(Value: IXMLNode);
begin
  if Value <> nil then
    FXml.DocumentElement := (Value as IInternalXmlNode).GetNode
  else
    FXml.DocumentElement := nil;
end;

procedure TXmlDocument.SetOptions(const Value: TXmlDocOptions);
begin
  FXml.Options := XMLIntf.TXMLDocOptions(Value);
end;

procedure TXmlDocument.SetVersion(const Value: string);
begin
  FXml.Version := Version;
end;

function TXmlDocument._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount)
end;

function TXmlDocument._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if (Result = 0) and (Owner = nil) then
    Destroy;
end;

{ TXmlAttributes }

constructor TXmlAttributes.Create(ANode: XMLIntf.IXmlNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TXmlAttributes.GetAttribute(const Index: string): Variant;
begin
  if FNode.HasAttribute(Index) then
    Result := FNode.Attributes[Index]
  else
    Result := '';
end;

procedure TXmlAttributes.SetAttribute(const Index: string; const Value: Variant);
begin
  FNode.Attributes[Index] := Value;
end;

{ TXmlNode }

constructor TXmlNode.Create(ANode: XMLIntf.IXmlNode);
begin
  inherited Create;
  FNode := ANode;
  FChilds := TXmlChildNodes.Create(ANode.ChildNodes);
  FAttributes := TXmlAttributes.Create(ANode);
end;

destructor TXmlNode.Destroy;
begin
  FAttributes := nil;
  FChilds := nil;
  inherited Destroy;
end;

function TXmlNode.AddChild(const NodeName: string): IXmlNode;
begin
  Result := TXmlNode.Create(FNode.AddChild(NodeName));
end;

function TXmlNode.GetAttributes: IXmlAttributes;
begin
  Result := FAttributes;
end;

function TXmlNode.GetChildNodes: IXmlChildNodes;
begin
  Result := FChilds;
end;

function TXmlNode.GetNode: XMLIntf.IXmlNode;
begin
  Result := FNode;
end;

function TXmlNode.GetNodeName: string;
begin
  Result := FNode.NodeName;
end;

function TXmlNode.GetNodeValue: Variant;
begin
  Result := FNode.NodeValue;
end;

function TXmlNode.GetText: string;
begin
  Result := FNode.Text
end;

{procedure TXmlNode.SetNodeName(const Value: string);
begin
  FNode.NodeName := Value;
end;}

procedure TXmlNode.SetNodeValue(const Value: Variant);
begin
  FNode.NodeValue := Value;
end;

procedure TXmlNode.SetText(const Value: string);
begin
  FNode.Text := Value;
end;

{ TXmlChildNodes }

procedure TXmlChildNodes.Add(Node: IXmlNode);
begin
  if Node <> nil then
    FNodes.Add((Node as IInternalXmlNode).GetNode);
end;

constructor TXmlChildNodes.Create(ANodes: XMLIntf.IXMLNodeList);
begin
  inherited Create;
  FNodes := ANodes;
end;

procedure TXmlChildNodes.DeleteNodes(const NodeName: string);
var
  I: Integer;
begin
  for I := FNodes.Count - 1 downto 0 do
    if AnsiSameText(FNodes[I].NodeName, NodeName) then
      FNodes.Delete(I);
end;

function TXmlChildNodes.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

function TXmlChildNodes.GetNode(Index: Integer): IXmlNode;
begin
  Result := TXmlNode.Create(FNodes[Index]);
end;

function TXmlChildNodes.FindNode(const NodeName: string): IXmlNode;
var
  i: Integer;
begin
  for i := 0 to FNodes.Count - 1 do
    if CompareText(NodeName, FNodes[i].NodeName) = 0 then
    begin
      Result := Nodes[i];
      Exit;
    end;
  Result := nil;
end;

procedure TXmlChildNodes.Clear;
begin
  FNodes.Clear;
end;

{$ENDIF JEDI_XML}

end.
