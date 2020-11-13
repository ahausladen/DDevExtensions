{******************************************************************************}
{*                                                                            *}
{* (C) 2005,2006 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit SimpleXmlIntf;

interface

type
  TNodeType = (ntReserved, ntElement, ntAttribute, ntText, ntCData,
    ntEntityRef, ntEntity, ntProcessingInstr, ntComment, ntDocument,
    ntDocType, ntDocFragment, ntNotation);

  TXmlDocOption = (doNodeAutoCreate, doNodeAutoIndent, doAttrNull,
    doAutoPrefix, doNamespaceDecl, doAutoSave);
  TXmlDocOptions = set of TXmlDocOption;

  IXmlNode = interface;

  IXmlChildNodes = interface
    ['{AEC5C57C-75E1-4143-BFD6-5C91C48178D6}']
    function GetCount: Integer;
    function GetNode(Index: Integer): IXmlNode;
    function FindNode(const NodeName: string): IXmlNode;

    procedure Add(Node: IXmlNode);
    procedure Clear;
    procedure DeleteNodes(const NodeName: string);

    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: IXmlNode read GetNode; default;
  end;

  IXmlAttributes = interface
    ['{3EDE3B74-F849-4169-B245-28447BCF8AFB}']
    procedure SetAttribute(const Index: string; const Value: Variant);
    function GetAttribute(const Index: string): Variant;
    property Attributes[const Index: string]: Variant read GetAttribute write SetAttribute; default;
  end;

  IXmlNode = interface
    ['{550EAFEC-429D-4C37-8F7C-B552E2071D59}']
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
    property Text: string read GetText write SetText;
    property ChildNodes: IXmlChildNodes read GetChildNodes;
    property Attributes: IXmlAttributes read GetAttributes;
  end;

  IXmlDocument = interface
    ['{6C02535D-924E-48A9-83C3-5A79897A54B5}']
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetDocumentElement: IXMLNode;
    procedure SetDocumentElement(Value: IXMLNode);
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
    property DocumentElement: IXMLNode read GetDocumentElement write SetDocumentElement;
    property Version: string read GetVersion write SetVersion;
    property Options: TXmlDocOptions read GetOptions write SetOptions;
    property ChildNodes: IXmlChildNodes read GetChildNodes;
  end;

implementation

end.
