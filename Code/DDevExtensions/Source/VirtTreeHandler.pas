unit VirtTreeHandler;

interface

{$RTTI EXPLICIT METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
                PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])
                FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  Windows, Messages, SysUtils, Classes, Controls, ImgList, Rtti, IDEHooks;

type
  PVirtualNode = type Pointer;

  TVSTTextType = (
    ttNormal,      // normal label of the node, this is also the text which can be edited
    ttStatic       // static (non-editable) text after the normal text
  );

  TVSTGetTextEvent = procedure(Sender: TObject; Node: PVirtualNode; Column: Integer;
    TextType: TVSTTextType; var CellText: WideString) of object;


  TreeMethod = class(TCustomAttribute)
  end;

  TreePropertyAccessor = class(TCustomAttribute)
  private
    FPropertyName: string;
  public
    constructor Create(const APropertyName: string);
    property PropertyName: string read FPropertyName;
  end;

  TreePropertyGetter = class(TreePropertyAccessor);
  TreePropertySetter = class(TreePropertyAccessor);

  TreeImport = class(TCustomAttribute)
  private
    FSignature: AnsiString;
  public
    constructor Create(const ASignature: AnsiString);
    property Signature: AnsiString read FSignature;
  end;

  TIDEVirtualTreeHandler = class(TObject)
  private
    FTree: TCustomControl;

    [TreeImport('@Idevirtualtrees@TCustomVirtualStringTree@GetText$qqrp28Idevirtualtrees@TVirtualNodei')]
    FTextGetter: function(Node: PVirtualNode; Column: Integer): WideString of object;
    [TreeImport('@Idevirtualtrees@TBaseVirtualTree@GetSelected$qqrp28Idevirtualtrees@TVirtualNode')]
    FSelectedGetter: function(Node: PVirtualNode): Boolean of object;
    [TreeImport('@Idevirtualtrees@TBaseVirtualTree@SetSelected$qqrp28Idevirtualtrees@TVirtualNodeo')]
    FSelectedSetter: procedure(Node: PVirtualNode; Value: Boolean) of object;
    [TreeImport('@Idevirtualtrees@TBaseVirtualTree@GetNodeParent$qqrp28Idevirtualtrees@TVirtualNode')]
    FNodeParentGetter: function(Node: PVirtualNode): PVirtualNode of object;

    [TreePropertySetter('FocusedNode')]
    FFocusedNodeSetter: procedure(Node: PVirtualNode) of object;


    procedure InitMethods;
    procedure GetMethod(TreeType: TRttiType; const MethodName: string; var M);
    procedure GetProperty(TreeType: TRttiType; const PropertyName: string; Setter: Boolean; var M);

    function GetText(Node: PVirtualNode; Column: Integer): string;
    function GetSelected(Node: PVirtualNode): Boolean;
    procedure SetSelected(Node: PVirtualNode; const Value: Boolean);
    function GetFocusedNode: PVirtualNode;
    procedure SetFocusedNode(Node: PVirtualNode);
    function GetImages: TCustomImageList;
    function GetNodeParent(Node: PVirtualNode): PVirtualNode;
    function GetOnGetText: TVSTGetTextEvent;
    procedure SetOnGetText(const Value: TVSTGetTextEvent);
  public
    constructor Create(ATree: TCustomControl);
    procedure DblClick;
    procedure Invalidate;

    property Tree: TCustomControl read FTree;
  public
    [TreeMethod] GetFirst: function: PVirtualNode of object;
    [TreeMethod] GetFirstSelected: function: PVirtualNode of object;
    [TreeMethod] GetFirstVisible: function: PVirtualNode of object;
    [TreeMethod] GetFirstVisibleChild: function(Node: PVirtualNode): PVirtualNode of object;

    [TreeMethod] GetNext: function(Node: PVirtualNode): PVirtualNode of object;
    [TreeMethod] GetNextSelected: function(Node: PVirtualNode): PVirtualNode of object;
    [TreeMethod] GetNextSibling: function(Node: PVirtualNode): PVirtualNode of object;
    [TreeMethod] GetNextVisible: function(Node: PVirtualNode): PVirtualNode of object;
    [TreeMethod] GetNextVisibleSibling: function(Node: PVirtualNode): PVirtualNode of object;

    [TreeMethod] GetNodeData: function(Node: PVirtualNode): Pointer of object;
    [TreeMethod] GetNodeLevel: function(Node: PVirtualNode): Cardinal of object;

    [TreeMethod] ClearSelection: procedure of object;
    [TreeMethod] ScrollIntoView: function(Node: PVirtualNode; Center: Boolean; Horizontally: Boolean = False): Boolean of object;
    [TreeMethod] IsEditing: function: Boolean of object;

    property Text[Node: PVirtualNode; Column: Integer]: string read GetText;
    property Selected[Node: PVirtualNode]: Boolean read GetSelected write SetSelected;
    property FocusedNode: PVirtualNode read GetFocusedNode write SetFocusedNode;
    property NodeParent[Node: PVirtualNode]: PVirtualNode read GetNodeParent;

    property OnGetText: TVSTGetTextEvent read GetOnGetText write SetOnGetText;

    property Images: TCustomImageList read GetImages;
  end;

implementation

uses
  TypInfo;

type
  TOpenControl = class(TControl);

procedure NotSupported; overload;
begin
  raise Exception.Create('Not Supported');
end;

procedure NotSupportedMsg(const Msg: string); overload;
begin
  raise Exception.Create(Msg);
end;

procedure GetPropAccessor(Instance: TObject; Accessor: Pointer; var M);
type
  PINT_PTR = ^INT_PTR;
var
  Offset: INT_PTR;
begin
  TMethod(M).Data := Instance;
  if (Instance = nil) or (Accessor = nil) then
    TMethod(M).Code := @NotSupported
  else
  begin
    Offset := INT_PTR(Accessor);
    if (Offset and $FF000000) = $FE000000 then
    begin
      // Virtual dispatch, but with offset, not slot
      TMethod(M).Code := PPointer(PINT_PTR(Instance)^ + SmallInt(Offset))^;
    end
    else
    begin
      // Static dispatch
      TMethod(M).Code := Pointer(Offset);
    end;
  end;
end;

{ TreePropertyAccessor }

constructor TreePropertyAccessor.Create(const APropertyName: string);
begin
  inherited Create;
  FPropertyName := APropertyName;
end;

{ TreeImport }

constructor TreeImport.Create(const ASignature: AnsiString);
begin
  inherited Create;
  FSignature :=  ASignature;
end;

{ TIDEVirtualTreeHandler }

constructor TIDEVirtualTreeHandler.Create(ATree: TCustomControl);
begin
  inherited Create;
  FTree := ATree;
  InitMethods;
end;

procedure TIDEVirtualTreeHandler.DblClick;
begin
  TOpenControl(FTree).DblClick;
end;

procedure TIDEVirtualTreeHandler.Invalidate;
begin
  FTree.Invalidate;
end;

procedure TIDEVirtualTreeHandler.GetMethod(TreeType: TRttiType; const MethodName: string; var M);
var
  Method: TRttiMethod;
begin
  TMethod(M).Data := FTree;
  TMethod(M).Code := @NotSupported;
  if TreeType <> nil then
  begin
    Method := TreeType.GetMethod(MethodName);
    if Method <> nil then
      TMethod(M).Code := Method.CodeAddress;
  end;
end;

procedure TIDEVirtualTreeHandler.GetProperty(TreeType: TRttiType; const PropertyName: string; Setter: Boolean; var M);
var
  Prop: TRttiProperty;
begin
  TMethod(M).Data := FTree;
  TMethod(M).Code := @NotSupported;
  if TreeType <> nil then
  begin
    Prop := TreeType.GetProperty(PropertyName);
    if Prop is TRttiInstanceProperty then
    begin
      if Setter then
        GetPropAccessor(FTree, TRttiInstanceProperty(Prop).PropInfo.SetProc, M)
      else
        GetPropAccessor(FTree, TRttiInstanceProperty(Prop).PropInfo.GetProc, M);
    end;
  end;
end;

function TIDEVirtualTreeHandler.GetSelected(Node: PVirtualNode): Boolean;
begin
  Result := FSelectedGetter(Node);
end;

procedure TIDEVirtualTreeHandler.SetSelected(Node: PVirtualNode; const Value: Boolean);
begin
  FSelectedSetter(Node, Value);
end;

function TIDEVirtualTreeHandler.GetText(Node: PVirtualNode; Column: Integer): string;
begin
  Result := FTextGetter(Node, Column);
end;

function TIDEVirtualTreeHandler.GetFocusedNode: PVirtualNode;
var
  Ctx: TRttiContext;
  TreeType: TRttiType;
  Prop: TRttiProperty;
begin
  Result := nil;
  Ctx := TRttiContext.Create;
  try
    TreeType := Ctx.GetType(FTree.ClassInfo);
    if TreeType <> nil then
    begin
      Prop := TreeType.GetProperty('FocusedNode');
      if Prop <> nil then
        Result := PVirtualNode(Prop.GetValue(FTree).AsObject);
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TIDEVirtualTreeHandler.SetFocusedNode(Node: PVirtualNode);
begin
  FFocusedNodeSetter(Node);
end;

function TIDEVirtualTreeHandler.GetNodeParent(Node: PVirtualNode): PVirtualNode;
begin
  Result := FNodeParentGetter(Node);
end;

function TIDEVirtualTreeHandler.GetImages: TCustomImageList;
begin
  Result := TCustomImageList(GetObjectProp(FTree, 'Images', TCustomImageList));
end;

function TIDEVirtualTreeHandler.GetOnGetText: TVSTGetTextEvent;
begin
  Result := TVSTGetTextEvent(GetMethodProp(FTree, 'OnGetText'));
end;

procedure TIDEVirtualTreeHandler.SetOnGetText(const Value: TVSTGetTextEvent);
begin
  SetMethodProp(FTree, 'OnGetText', TMethod(Value));
end;

procedure TIDEVirtualTreeHandler.InitMethods;
var
  Ctx: TRttiContext;
  TreeType, SelfType: TRttiType;
  Fields: TArray<TRttiField>;
  Field: TRttiField;
  I: Integer;
  Attributes: TArray<TCustomAttribute>;
  AttrIndex: Integer;
  M: TMethod;
  VclIdeLib: THandle;
begin
  Ctx := TRttiContext.Create;
  try
    TreeType := Ctx.GetType(FTree.ClassInfo);
    if TreeType <> nil then
    begin
      VclIdeLib := GetModuleHandle(vclide_bpl);

      SelfType := Ctx.GetType(ClassInfo);
      Fields := SelfType.GetFields;
      Assert(Fields <> nil);
      for I := 0 to High(Fields) do
      begin
        Field := Fields[I];
        Attributes := Field.GetAttributes; // Error Insight Generic Array bug
        M.Code := nil;
        for AttrIndex := 0 to High(Attributes) do
        begin
          if Attributes[AttrIndex] is TreeMethod then
          begin
            GetMethod(TreeType, Field.Name, M);
            Break;
          end
          else if Attributes[AttrIndex] is TreePropertyAccessor then
          begin
            GetProperty(TreeType, TreePropertyAccessor(Attributes[AttrIndex]).PropertyName,
              Attributes[AttrIndex] is TreePropertySetter, M);
            Break;
          end
          else if Attributes[AttrIndex] is TreeImport then
          begin
            M.Data := FTree;
            M.Code := @NotSupported;
            if VclIdeLib <> 0 then
            begin
              M.Code := DbgStrictGetProcAddress(VclIdeLib, PAnsiChar(TreeImport(Attributes[AttrIndex]).Signature));
              if M.Code = nil then
                M.Code := @NotSupported;
            end;
            Break;
          end;
        end;

        if M.Code <> nil then
        begin
          TMethod(Pointer(INT_PTR(Self) + Field.Offset)^) := M;
          //Field.SetValue(Self, TValue.From<TMethod>(M));  => exception
          if M.Code = @NotSupported then
             NotSupportedMsg(Field.Name);
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;

end;


end.

