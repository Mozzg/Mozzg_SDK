{$I SDK_common_defines.inc}

unit uXML;

interface

uses
  System.SysUtils, System.Classes, System.Math, System.Character,
  cuHashTables, cuConsts, cuClasses,
  uStream, uCustomExceptions;

type
  TXMLNode = class;
  TXMLNodes = class;
  TXMLProperty = class;
  TXMLProperties = class;

  // Class for storing hash related things for general search by nodes in all children classes
  TXMLNodeNameNodeTextGeneralItem = class(THashBaseItem)
  protected
    fParentNode: TXMLNode;
    fGeneralNodeName: string;
    fGeneralNodeTextValue: string;
    fGeneralNodeNameChain: THashChain;
    fGeneralNodeTextValueChain: THashChain;
    procedure InitHashChains; override;
  public
    constructor Create(aParent: TXMLNode; const aNodeName, aNodeTextValue: string);
  end;

  // Class for storing hash related things for general search by nodes in all children classes
  TXMLNodeNameNodeTextGeneralItems = class(THashBaseItems)
  protected
    fParentNode: TXMLNode;
    fGeneralNamesStringHashTable: TStringHashTable;
    fGeneralTextValuesStringHashTable: TStringHashTable;
    procedure InitHashTables(aHashSize: Cardinal); override;
    procedure AddToHashTable(aHashItem: THashBaseItem); override;
    procedure RemoveFromHashTable(aHashItem: THashBaseItem); override;
    procedure SetHashSize(aHashSize: Cardinal); override;
    procedure DestroyHashTables; override;
  public
    constructor Create(aParent: TXMLNode; aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aFreeItemsOnDestroy: Boolean = False);

    function GetFirst: TXMLNodeNameNodeTextGeneralItem;
    function GetLast: TXMLNodeNameNodeTextGeneralItem;

    procedure Add(aItem: TXMLNodeNameNodeTextGeneralItem);
    procedure Delete(aItem: TXMLNodeNameNodeTextGeneralItem; aFreeItem: Boolean = False);
  end;

  TXMLNode = class(THashBaseItem)
  protected
    fNodeName: string;
    fNodeTextValue: string;
    fParent: TXMLNode;
    fHashGeneralChildren: Boolean;
    fHashChildrenProperties: Boolean;

    fImmediateChildren: TXMLNodes;
    fNodeProperties: TXMLProperties;

    fImmediateNodeNameChain: THashChain;
    fImmediateNodeTextValueChain: THashChain;

    {$IFNDEF STATIC_CHAINS}
    fGeneralNodeHashItem: TXMLNodeNameNodeTextGeneralItem;
    fGeneralChildren: TXMLNodeNameNodeTextGeneralItems;
    fGeneralChildrenProperties: TXMLProperties;

    procedure CreateAndAddGeneralPropertyToNodeAndParents(aProperty: TXMLProperty);
    procedure RemoveAndFreeGeneralPropertyFromNodeAndParents(aProperty: TXMLProperty);

    procedure SetNodeNameInGeneral(const aNodeName: string; aHashValue: Cardinal);
    procedure SetNodeTextValueInGeneral(const aNodeTextValue: string; aHashValue: Cardinal);
    {$ENDIF}
    procedure SetNodeName(const aNodeName: string);
    procedure SetNodeTextValue(const aNodeTextValue: string);
    procedure SetParent(aNewParent: TXMLNode);
    function GetParent: TXMLNode; inline;
    function GetImmediateChildren: TXMLNodes; inline;
    {$IFNDEF STATIC_CHAINS}
    function GetGeneralChildrenCount: Int64;
    function GetGeneralHashSize: Cardinal;
    function GetGeneralHashElementCount: Int64;
    procedure AssignGeneralSettings(aFromNode: TXMLNode);
    {$ENDIF}

    function ReadXMLNodeFromStream(aStream: TXMLStream; aIsRoot: Boolean): Boolean;
    function ReadNextXMLPropertyFromStream(aStream: TXMLStream): Boolean;
    procedure WriteXMLNodeToStream(aStream: TXMLStream; aNestingLevel: Integer = 0);
    procedure WriteXMLNodePropertiesToStream(aStream: TXMLStream);

    function InternalConvertValueToVariant(const aTextValue: string): Variant;

    procedure InitHashChains; override;
    // Hiding properties from parent class
    property Owner;
    property Prev;
    property Next;
  public
    constructor Create(const aNodeName: string = EmptyString; const aNodeTextValue: string = EmptyString);
    destructor Destroy; override;

    procedure LoadFromXMLStream(aStream: TXMLStream; aExpectedEncoding: TEncoding);
    procedure SaveToXMLStream(aStream: TXMLStream; aWriteXMLHeader: Boolean = True; aWritePreamble: Boolean = True; aUseTabIndents: Boolean = True);

    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string; aEncoding: TEncoding = nil);

    function GetNodeAsText(aWriteXMLHeader: Boolean = False; aWritePreamble: Boolean = False): string;

    procedure AddAsChild(aNode: TXMLNode);
    procedure InsertAsChild(aNode, aBeforeNode: TXMLNode);
    function CreateNodeAsChild(const aNodeName, aNodeTextValue: string): TXMLNode;
    procedure RemoveFromChildren(aNode: TXMLNode);

    {$IFNDEF STATIC_CHAINS}
    procedure AddFromNodeGeneralPropertiesToNodeAndParents(aFromNode: TXMLNode);
    procedure RemoveGeneralPropertiesFromNodeAndParents(aFromNode: TXMlNode);
    {$ENDIF}

    procedure AddProperty(aProperty: TXMLProperty);
    function CreateAndAddProperty(const aPropertyName: string; const aPropertyValue: Variant): TXMLProperty;
    procedure RemoveProperty(aProperty: TXMLProperty);

    procedure ClearAllChildren;

    function FindFirstByNodeNameInImmediate(const aSearchNodeName: string): TXMLNode;
    function FindNextByNodeNameInImmediate(aNode: TXMLNode): TXMLNode;
    function FindFirstByNodeValueInImmediate(const aSearchNodeValue: string): TXMLNode;
    function FindNextByNodeValueInImmediate(aNode: TXMLNode): TXMLNode;

    {$IFNDEF STATIC_CHAINS}
    function FindFirstByNodeNameInGeneral(const aSearchNodeName: string): TXMLNode;
    function FindNextByNodeNameInGeneral(aNode: TXMLNode): TXMLNode;
    function FindFirstByNodeValueInGeneral(const aSearchNodeValue: string): TXMLNode;
    function FindNextByNodeValueInGeneral(aNode: TXMLNode): TXMLNode;

    function FindFirstByPropertyNameInGeneral(const aSearchPropertyName: string): TXMLProperty;
    function FindNextByPropertyNameInGeneral(aProperty: TXMLProperty): TXMLProperty;
    {$ENDIF}

    function GetFirstChild: TXMLNode; inline;
    function GetLastChild: TXMLNode; inline;
    function GetNextTraversal: TXMLNode; inline;
    function GetPrevTraversal: TXMLNode; inline;
    function GetNextSibling: TXMLNode; inline;
    function GetPrevSibling: TXMLNode; inline;
    function GetRoot: TXMLNode; inline;

    function IsParent(aChildNode: TXMLNode): Boolean;

    property NodeName: string read fNodeName write SetNodeName;
    property NodeTextValue: string read fNodeTextValue write SetNodeTextValue;
    property Parent: TXMLNode read GetParent write SetParent;
    property ImmediateChildren: TXMLNodes read GetImmediateChildren;
    property Properties: TXMLProperties read fNodeProperties;
    property HashGeneralChildren: Boolean read fHashGeneralChildren write fHashGeneralChildren;
    property HashChildrenProperties: Boolean read fHashChildrenProperties write fHashChildrenProperties;

    {$IFNDEF STATIC_CHAINS}
    property GeneralChildrenCount: Int64 read GetGeneralChildrenCount;
    property GeneralHashSize: Cardinal read GetGeneralHashSize;
    property GeneralHashElementCount: Int64 read GetGeneralHashElementCount;
    {$ENDIF}
  end;

  TXMLNodes = class(THashBaseItems)
  protected
    fParent: TXMLNode;
    fImmediateNodeNamesStringHashTable: TStringHashTable;
    fImmediateNodeTextValuesStringHashTable: TStringHashTable;

    procedure AddToImmediate(aNode: TXMLNode);
    procedure InsertToImmediate(aNode, aBeforeNode: TXMLNode);
    procedure DeleteFromImmediate(aNode: TXMLNode; aFreeNode: Boolean = True);

    procedure InitHashTables(aHashSize: Cardinal); override;
    procedure AddToHashTable(aHashItem: THashBaseItem); override;
    procedure RemoveFromHashTable(aHashItem: THashBaseItem); override;
    procedure SetHashSize(aHashSize: Cardinal); override;
    procedure DestroyHashTables; override;
  public
    constructor Create(aParent: TXMLNode; aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aFreeItemsOnDestroy: Boolean = True);
    procedure BeforeDestruction; override;

    function GetChildrenCount(aRecursive: Boolean = False): Integer;

    function GetFirst: TXMLNode; inline;
    function GetLast: TXMLNode; inline;

    property NameHashElementCount: UInt64 read fImmediateNodeNamesStringHashTable.HashTable.PointerTableElementCount;
  end;

  TXMLProperty = class(TNameVariantItem)
  protected
    fParentNode: TXMLNode;
    // Hiding properties from parent class
    property Owner;
    property Prev;
    property Next;
  public
    constructor Create(aParent: TXMLNode; const aName: string); overload;
    constructor Create(aParent: TXMLNode; const aName: string; const aValue: Variant); overload;

    function GetNext: TXMLProperty; inline;
    function GetPrev: TXMLProperty; inline;

    property ParentNode: TXMLNode read fParentNode;
  end;

  TXMLProperties = class(TNameVariantItems)
  protected
    fParentNode: TXMLNode;
  public
    constructor Create(aParent: TXMLNode; aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aAllowDuplicates: Boolean = True; aFreeItemsOnDestroy: Boolean = True);

    procedure Add(aProperty: TXMLProperty);

    function FindFirstByName(const aSearchName: string): TXMLProperty; inline;
    function FindNextByName(aItem: TXMLProperty): TXMLProperty; inline;
    function FindFirstByStringValue(const aSearchValue: string): TXMLProperty; inline;
    function FindNextByStringValue(aItem: TXMLProperty): TXMLProperty; inline;
    function FindFirstByIntegerValue(const aSearchValue: Integer): TXMLProperty; inline;
    function FindNextByIntegerValue(aItem: TXMLProperty): TXMLProperty; inline;
    function FindFirstByInt64Value(const aSearchValue: Int64): TXMLProperty; inline;
    function FindNextByInt64Value(aItem: TXMLProperty): TXMLProperty; inline;
    function FindFirstByDoubleValue(const aSearchValue: Double): TXMLProperty; inline;
    function FindNextByDoubleValue(aItem: TXMLProperty): TXMLProperty; inline;
    function FindFirstByBooleanValue(const aSearchValue: Boolean): TXMLProperty; inline;
    function FindNextByBooleanValue(aItem: TXMLProperty): TXMLProperty; inline;

    function GetFirst: TXMLProperty;
    function GetLast: TXMLProperty;
  end;

implementation

{ TXMLNodeNameNodeTextGeneralItem }

constructor TXMLNodeNameNodeTextGeneralItem.Create(aParent: TXMLNode; const aNodeName, aNodeTextValue: string);
begin
  inherited Create;
  fParentNode := aParent;
  fGeneralNodeName := aNodeName;
  fGeneralNodeNameChain.HashValue := TStringHashTable.GetStringHashValue(fGeneralNodeName);
  fGeneralNodeTextValue := aNodeTextValue;
  fGeneralNodeTextValueChain.HashValue := TStringHashTable.GetStringHashValue(fGeneralNodeTextValue);
end;

procedure TXMLNodeNameNodeTextGeneralItem.InitHashChains;
begin
  fGeneralNodeNameChain.Item := Self;
  fGeneralNodeTextValueChain.Item := Self;
end;

{ TXMLNodeNameNodeTextGeneralItems }

constructor TXMLNodeNameNodeTextGeneralItems.Create(aParent: TXMLNode; aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aFreeItemsOnDestroy: Boolean = False);
begin
  inherited Create(aHashSize, aAutoExtendHash, aFreeItemsOnDestroy);
  fParentNode := aParent;
end;

procedure TXMLNodeNameNodeTextGeneralItems.InitHashTables(aHashSize: Cardinal);
begin
  fGeneralNamesStringHashTable.Init(aHashSize, Integer(@TXMLNodeNameNodeTextGeneralItem(0).fGeneralNodeName));
  fGeneralTextValuesStringHashTable.Init(aHashSize, Integer(@TXMLNodeNameNodeTextGeneralItem(0).fGeneralNodeTextValue));
end;

procedure TXMLNodeNameNodeTextGeneralItems.AddToHashTable(aHashItem: THashBaseItem);
begin
  fGeneralNamesStringHashTable.AddToTable(@TXMLNodeNameNodeTextGeneralItem(aHashItem).fGeneralNodeNameChain);
  fGeneralTextValuesStringHashTable.AddToTable(@TXMLNodeNameNodeTextGeneralItem(aHashItem).fGeneralNodeTextValueChain);
end;

procedure TXMLNodeNameNodeTextGeneralItems.RemoveFromHashTable(aHashItem: THashBaseItem);
begin
  fGeneralNamesStringHashTable.RemoveFromTable(@TXMLNodeNameNodeTextGeneralItem(aHashItem).fGeneralNodeNameChain);
  fGeneralTextValuesStringHashTable.RemoveFromTable(@TXMLNodeNameNodeTextGeneralItem(aHashItem).fGeneralNodeTextValueChain);
end;

procedure TXMLNodeNameNodeTextGeneralItems.SetHashSize(aHashSize: Cardinal);
begin
  fGeneralNamesStringHashTable.SetTableSize(aHashSize);
  fGeneralTextValuesStringHashTable.SetTableSize(aHashSize);
end;

procedure TXMLNodeNameNodeTextGeneralItems.DestroyHashTables;
begin
  fGeneralNamesStringHashTable.Destroy;
  fGeneralTextValuesStringHashTable.Destroy;
end;

function TXMLNodeNameNodeTextGeneralItems.GetFirst: TXMLNodeNameNodeTextGeneralItem;
begin
  Result := TXMLNodeNameNodeTextGeneralItem(fFirst);
end;

function TXMLNodeNameNodeTextGeneralItems.GetLast: TXMLNodeNameNodeTextGeneralItem;
begin
  Result := TXMLNodeNameNodeTextGeneralItem(fLast);
end;

procedure TXMLNodeNameNodeTextGeneralItems.Add(aItem: TXMLNodeNameNodeTextGeneralItem);
begin
  InternalAddItem(aItem);
  aItem.fOwner := nil;
end;

procedure TXMLNodeNameNodeTextGeneralItems.Delete(aItem: TXMLNodeNameNodeTextGeneralItem; aFreeItem: Boolean = False);
begin
  aItem.fOwner := Self;
  InternalDeleteItem(aItem, aFreeItem);
end;

{ TXMLNode }

constructor TXMLNode.Create(const aNodeName: string = EmptyString; const aNodeTextValue: string = EmptyString);
begin
  inherited Create;
  fParent := nil;
  fHashGeneralChildren := False;
  fHashChildrenProperties := False;
  SetNodeName(aNodeName);
  SetNodeTextValue(aNodeTextValue);
  fImmediateChildren := TXMLNodes.Create(Self, 11, True, True);
  fNodeProperties := TXMLProperties.Create(Self, 11, True, False, True);
  {$IFNDEF STATIC_CHAINS}
  // We must create GeneralHashItem after we assign NodeName and NodeTextValue to avoid trying to change parent hash tables
  fGeneralNodeHashItem := TXMLNodeNameNodeTextGeneralItem.Create(Self, aNodeName, aNodeTextValue);
  fGeneralChildren := TXMLNodeNameNodeTextGeneralItems.Create(Self, 17, True, False);
  fGeneralChildrenProperties := TXMLProperties.Create(Self, 17, True, True, True);
  {$ENDIF}
end;

destructor TXMLNode.Destroy;
begin
  fParent := nil;
  {$IFNDEF STATIC_CHAINS}
  FreeAndNil(fGeneralChildren);
  FreeAndNil(fGeneralChildrenProperties);
  FreeAndNil(fGeneralNodeHashItem);
  {$ENDIF}
  FreeAndNil(fImmediateChildren);
  FreeAndNil(fNodeProperties);
  inherited Destroy;
end;

{$IFNDEF STATIC_CHAINS}
procedure TXMLNode.CreateAndAddGeneralPropertyToNodeAndParents(aProperty: TXMLProperty);
var
  lNewGeneralProperty: TXMLProperty;
  lCurrentNode: TXMLNode;
begin
  lCurrentNode := Self;
  while lCurrentNode <> nil do
  begin
    lNewGeneralProperty := nil;
    try
      lNewGeneralProperty := TXMLProperty.Create(Self, aProperty.Name, aProperty.Value);
      lCurrentNode.fGeneralChildrenProperties.Add(lNewGeneralProperty);
    except
      lNewGeneralProperty.Free;
      raise;
    end;
    lCurrentNode := lCurrentNode.Parent;
  end;
end;

procedure TXMLNode.RemoveAndFreeGeneralPropertyFromNodeAndParents(aProperty: TXMLProperty);
var
  lCurrentNode: TXMLNode;
  lCurrentProperty, lPropertyForDelete: TXMLProperty;
begin
  lCurrentNode := Self;
  while lCurrentNode <> nil do
  begin
    lCurrentProperty := lCurrentNode.fGeneralChildrenProperties.GetFirst;
    while lCurrentProperty <> nil do
    begin
      if (lCurrentProperty.ParentNode = aProperty.ParentNode)
          and SameStr(lCurrentProperty.Name, aProperty.Name)
          and (lCurrentProperty.Value = aProperty.Value)
      then
      begin
        lPropertyForDelete := lCurrentProperty;
        lCurrentProperty := lCurrentProperty.GetNext;
        lCurrentNode.fGeneralChildrenProperties.Delete(lPropertyForDelete, True);
      end
      else
        lCurrentProperty := lCurrentProperty.GetNext;
    end;
    lCurrentNode := lCurrentNode.Parent;
  end;
end;

procedure TXMLNode.SetNodeNameInGeneral(const aNodeName: string; aHashValue: Cardinal);
var
  lCurrentNode: TXMLNode;
begin
  if not fHashGeneralChildren then Exit;
  // First Delete current node value from all parents
  lCurrentNode := Self;
  while lCurrentNode <> nil do
  begin
    Self.fGeneralNodeHashItem.fOwner := lCurrentNode.fGeneralChildren;
    lCurrentNode.fGeneralChildren.fGeneralNamesStringHashTable.RemoveFromTable(@Self.fGeneralNodeHashItem.fGeneralNodeNameChain);
    lCurrentNode := lCurrentNode.Parent;
  end;
  // Change value and hashvalue of current node
  Self.fGeneralNodeHashItem.fGeneralNodeName := aNodeName;
  Self.fGeneralNodeHashItem.fGeneralNodeNameChain.HashValue := aHashValue;
  // Adding back to all parents new value of item
  lCurrentNode := Self;
  while lCurrentNode <> nil do
  begin
    Self.fGeneralNodeHashItem.fOwner := lCurrentNode.fGeneralChildren;
    lCurrentNode.fGeneralChildren.fGeneralNamesStringHashTable.AddToTable(@Self.fGeneralNodeHashItem.fGeneralNodeNameChain);
    lCurrentNode := lCurrentNode.Parent;
  end;
  // Cleaning owner, because this item may be in multiple hash tables
  Self.fGeneralNodeHashItem.fOwner := nil;
end;

procedure TXMLNode.SetNodeTextValueInGeneral(const aNodeTextValue: string; aHashValue: Cardinal);
var
  lCurrentNode: TXMLNode;
begin
  if not fHashGeneralChildren then Exit;
  // First Delete current node value from all parents
  lCurrentNode := Self;
  while lCurrentNode <> nil do
  begin
    Self.fGeneralNodeHashItem.fOwner := lCurrentNode.fGeneralChildren;
    lCurrentNode.fGeneralChildren.fGeneralTextValuesStringHashTable.RemoveFromTable(@Self.fGeneralNodeHashItem.fGeneralNodeTextValueChain);
    lCurrentNode := lCurrentNode.Parent;
  end;
  // Change value and hashvalue of current node
  Self.fGeneralNodeHashItem.fGeneralNodeTextValue := aNodeTextValue;
  Self.fGeneralNodeHashItem.fGeneralNodeTextValueChain.HashValue := aHashValue;
  // Adding back to all parents new value of item
  lCurrentNode := Self;
  while lCurrentNode <> nil do
  begin
    Self.fGeneralNodeHashItem.fOwner := lCurrentNode.fGeneralChildren;
    lCurrentNode.fGeneralChildren.fGeneralTextValuesStringHashTable.AddToTable(@Self.fGeneralNodeHashItem.fGeneralNodeTextValueChain);
    lCurrentNode := lCurrentNode.Parent;
  end;
  // Cleaning owner, because this item may be in multiple hash tables
  Self.fGeneralNodeHashItem.fOwner := nil;
end;
{$ENDIF}

procedure TXMLNode.SetNodeName(const aNodeName: string);
begin
  if SameStr(fNodeName, aNodeName) then Exit;
  if fOwner = nil then
  begin
    fNodeName := aNodeName;
    fImmediateNodeNameChain.HashValue := TStringHashTable.GetStringHashValue(fNodeName);
    Exit;
  end;
  TXMLNodes(fOwner).fImmediateNodeNamesStringHashTable.RemoveFromTable(@fImmediateNodeNameChain);
  fNodeName := aNodeName;
  fImmediateNodeNameChain.HashValue := TStringHashTable.GetStringHashValue(fNodeName);
  TXMLNodes(fOwner).fImmediateNodeNamesStringHashTable.AddToTable(@fImmediateNodeNameChain);

  {$IFNDEF STATIC_CHAINS}
  // Changing HashValue and hash tables for general items
  // Checking for nil to avoid changind in constructor
  if (fGeneralNodeHashItem <> nil) and fHashGeneralChildren then
    SetNodeNameInGeneral(aNodeName, fImmediateNodeNameChain.HashValue);
  {$ENDIF}
end;

procedure TXMLNode.SetNodeTextValue(const aNodeTextValue: string);
begin
  if SameStr(fNodeTextValue, aNodeTextValue) then Exit;
  if fOwner = nil then
  begin
    fNodeTextValue := aNodeTextValue;
    fImmediateNodeTextValueChain.HashValue := TStringHashTable.GetStringHashValue(fNodeTextValue);
    Exit;
  end;
  TXMLNodes(fOwner).fImmediateNodeTextValuesStringHashTable.RemoveFromTable(@fImmediateNodeTextValueChain);
  fNodeTextValue := aNodeTextValue;
  fImmediateNodeTextValueChain.HashValue := TStringHashTable.GetStringHashValue(fNodeTextValue);
  TXMLNodes(fOwner).fImmediateNodeTextValuesStringHashTable.AddToTable(@fImmediateNodeTextValueChain);

  {$IFNDEF STATIC_CHAINS}
  // Changing HashValue and hash tables for general items
  // Checking for nil to avoid changind in constructor
  if (fGeneralNodeHashItem <> nil) and fHashGeneralChildren then
    SetNodeTextValueInGeneral(aNodeTextValue, fImmediateNodeTextValueChain.HashValue);
  {$ENDIF}
end;

procedure TXMLNode.SetParent(aNewParent: TXMLNode);
begin
  if fParent = aNewParent then Exit;
  if fParent <> nil then
    fParent.RemoveFromChildren(Self);
  if aNewParent <> nil then
    aNewParent.AddAsChild(Self);
end;

function TXMLNode.GetParent: TXMLNode;
begin
  Result := fParent;
end;

function TXMLNode.GetImmediateChildren: TXMLNodes;
begin
  Result := fImmediateChildren;
end;

{$IFNDEF STATIC_CHAINS}
function TXMLNode.GetGeneralChildrenCount: Int64;
begin
  Result := fGeneralChildren.Count;
end;

function TXMLNode.GetGeneralHashSize: Cardinal;
begin
  Result := fGeneralChildren.HashSize;
end;

function TXMLNode.GetGeneralHashElementCount: Int64;
begin
  Result := fGeneralChildren.fGeneralNamesStringHashTable.HashTable.PointerTableElementCount;
end;

procedure TXMLNode.AssignGeneralSettings(aFromNode: TXMLNode);
begin
  fHashGeneralChildren := aFromNode.fHashGeneralChildren;
  fHashChildrenProperties := aFromNode.fHashChildrenProperties;
end;
{$ENDIF}

function TXMLNode.ReadXMLNodeFromStream(aStream: TXMLStream; aIsRoot: Boolean): Boolean;
var
  lOpeningTagName, lClosingTagName: string;
  lNodeValue: string;
  lNodeValueBuilder: TUnicodeStringBuilder;
  lChr: Char;
  lChildNode: TXMLNode;
begin
  aStream.SkipSpaces;
  aStream.CheckAndReadNextChar(XML_NODE_START_CHAR);
  aStream.SkipSpaces;

  // Reading opening tag name
  lOpeningTagName := aStream.ReadIdentifier;
  aStream.SkipSpaces;
  // Reading properties
  repeat
  until not ReadNextXMLPropertyFromStream(aStream);
  aStream.SkipSpaces;

  // Checking for empty tag
  if (aStream.GetNextChar = XML_NODE_CLOSE_CHAR) and (aStream.GetNextChar(1) = XML_NODE_END_CHAR) then
  begin
    aStream.ReadNextChar;
    aStream.ReadNextChar;
    aStream.SkipSpaces;
    Self.NodeName := lOpeningTagName;
    Exit(True);
  end;

  // Checking for tag end
  aStream.CheckAndReadNextChar(XML_NODE_END_CHAR);

  // Read 1 char at a time to read node value and children
  lNodeValueBuilder.Init(lNodeValue);
  try
    repeat
      aStream.SkipControls;
      // Check for end tag
      if (aStream.GetNextChar(0) = XML_NODE_START_CHAR) and (aStream.GetNextChar(1) = XML_NODE_CLOSE_CHAR) then
        Break;
      // Check for normal symbol
      if aStream.GetNextChar <> XML_NODE_START_CHAR then
      begin
        if lNodeValueBuilder.StringLength = 0 then
        begin
          aStream.SkipSpaces;
          if aStream.GetNextChar = XML_NODE_START_CHAR then
            Continue;
        end;
        lChr := aStream.ReadNextChar;
        lNodeValueBuilder.Add(lChr);
        Continue;
      end;
      // Check for child tag start
      if aStream.GetNextChar = XML_NODE_START_CHAR then
      begin
        lChildNode := nil;
        try
          lChildNode := TXMLNode.Create;
          Self.AddAsChild(lChildNode);
        except
          FreeAndNil(lChildNode);
          raise;
        end;
        lChildNode.ReadXMLNodeFromStream(aStream, False);
      end;
    until False;
  finally
    lNodeValueBuilder.Done;
  end;

  // Checking end node start
  aStream.CheckAndReadNextChar(XML_NODE_START_CHAR);
  aStream.CheckAndReadNextChar(XML_NODE_CLOSE_CHAR);

  // Resding closing node name
  aStream.SkipSpaces;
  lClosingTagName := aStream.ReadIdentifier;
  aStream.SkipSpaces;
  aStream.CheckAndReadNextChar(XML_NODE_END_CHAR);

  // Checking if opening name is same as closing name
  if not SameText(lOpeningTagName, lClosingTagName) then
    aStream.RaiseInvalidXMLFormat;

  // Apply name and value to node
  Self.NodeName := lOpeningTagName;
  Self.NodeTextValue := lNodeValue;

  Result := True;
end;

function TXMLNode.ReadNextXMLPropertyFromStream(aStream: TXMLStream): Boolean;
var
  lChr: Char;
  lIdentifier, lValue: string;
  lVariantValue: Variant;
begin
  aStream.SkipSpaces;
  lChr := aStream.GetNextChar;
  if (lChr = XML_NODE_CLOSE_CHAR) or (lChr = XML_NODE_END_CHAR) then
    Exit(False);
  // Reading property with value
  lIdentifier := aStream.ReadIdentifier;
  aStream.SkipSpaces;
  aStream.CheckAndReadNextChar(XML_EQUALS_CHAR);
  aStream.SkipSpaces;
  lValue := aStream.ReadStringValue;
  // Converting string value to specific variant type
  lVariantValue := InternalConvertValueToVariant(lValue);
  // Creating and adding property
  CreateAndAddProperty(lIdentifier, lVariantValue);
  Result := True;
end;

procedure TXMLNode.WriteXMLNodeToStream(aStream: TXMLStream; aNestingLevel: Integer = 0);
var
  lChildNode: TXMLNode;
begin
  if aNestingLevel <> 0 then
    aStream.WriteCRLF;
  aStream.WriteIndent(aNestingLevel);

  if (Length(fNodeTextValue) = 0) and (fImmediateChildren.Count = 0) and (aNestingLevel <> 0) then
  begin
    // Empty node
    aStream.WriteXMLNodeStartChar;
    aStream.WriteString(NodeName);
    if fNodeProperties.Count <> 0 then
    begin
      WriteXMLNodePropertiesToStream(aStream);
      aStream.WriteSpaceChar;
    end;
    aStream.WriteXMLNodeCloseChar;
    aStream.WriteXMLNodeEndChar;
    Exit;
  end;

  // Writing opening tag
  aStream.WriteXMLNodeStartChar;
  aStream.WriteString(NodeName);
  WriteXMLNodePropertiesToStream(aStream);
  aStream.WriteXMLNodeEndChar;
  // If we only have text value, write it as single line
  if (fImmediateChildren.Count = 0) and (aNestingLevel <> 0) then
  begin
    aStream.WriteString(NodeTextValue);
    // Writing closing tag
    aStream.WriteXMLNodeStartChar;
    aStream.WriteXMLNodeCloseChar;
    aStream.WriteString(NodeName);
    aStream.WriteXMLNodeEndChar;
    Exit;
  end;

  // if We potentially have both, we need to expand it vertically
  Inc(aNestingLevel);
  if Length(fNodeTextValue) <> 0 then
  begin
    aStream.WriteCRLF;
    aStream.WriteIndent(aNestingLevel);
    aStream.WriteString(NodeTextValue);
  end;

  lChildNode := fImmediateChildren.GetFirst;
  while lChildNode <> nil do
  begin
    lChildNode.WriteXMLNodeToStream(aStream, aNestingLevel);
    lChildNode := lChildNode.GetNextSibling;
  end;

  // Writing closing tag
  Dec(aNestingLevel);
  aStream.WriteCRLF;
  aStream.WriteIndent(aNestingLevel);
  aStream.WriteXMLNodeStartChar;
  aStream.WriteXMLNodeCloseChar;
  aStream.WriteString(NodeName);
  aStream.WriteXMLNodeEndChar;
end;

procedure TXMLNode.WriteXMLNodePropertiesToStream(aStream: TXMLStream);
var
  lCurrentProperty: TXMLProperty;
begin
  lCurrentProperty := fNodeProperties.GetFirst;
  while lCurrentProperty <> nil do
  begin
    aStream.WriteSpaceChar;
    aStream.WriteString(lCurrentProperty.Name);
    aStream.WriteEqualsChar;
    aStream.WriteQuotationChar;
    aStream.WriteString(lCurrentProperty.AsString);
    aStream.WriteQuotationChar;
    lCurrentProperty := lCurrentProperty.GetNext;
  end;
end;

function TXMLNode.InternalConvertValueToVariant(const aTextValue: string): Variant;
const
  DOT_CHAR = '.';
  COMMA_CHAR = ',';
var
  lFormat: TFormatSettings;
  lDoubleValue: Double;
  lIntegerValue: Integer;
  lInt64Value: Int64;
  lDateTimeValue: TDateTime;
  lBooleanValue: Boolean;
begin
  lFormat := TFormatSettings.Create;

  // Check for double value
  if (Pos(DOT_CHAR, aTextValue) <> 0) or (Pos(COMMA_CHAR, aTextValue) <> 0) then
    if TryStrToFloat(aTextValue, lDoubleValue, lFormat) then
      Exit(lDoubleValue);

  // Check for integer value
  if TryStrToInt(aTextValue, lIntegerValue) then
    Exit(lIntegerValue);

  if TryStrToInt64(aTextValue, lInt64Value) then
    Exit(lInt64Value);

  // Check for datetime
  if TryStrToDate(aTextValue, lDateTimeValue, lFormat) then
    Exit(lDateTimeValue);

  if TryStrToTime(aTextValue, lDateTimeValue, lFormat) then
    Exit(lDateTimeValue);

  if TryStrToDateTime(aTextValue, lDateTimeValue, lFormat) then
    Exit(lDateTimeValue);

  // Check for bool
  if TryStrToBool(aTextValue, lBooleanValue) then
    Exit(lBooleanValue);

  // If no checks are passed, we left with string value
  Result := aTextValue;
end;

procedure TXMLNode.InitHashChains;
begin
  fImmediateNodeNameChain.Item := Self;
  fImmediateNodeTextValueChain.Item := Self;
end;

procedure TXMLNode.LoadFromXMLStream(aStream: TXMLStream; aExpectedEncoding: TEncoding);
var
  lOriginalBuffer, lConvertedBuffer: TBytes;
  lStreamDataSize: Integer;
  lTempStream: TXMLStream;
begin
  // Check encoding by preamble and header
  aStream.ReadHeaderAndExamineEncoding(aExpectedEncoding);

  // Check stream encoding. If encoding is not Unicode, we must convert stream to it
  if aStream.XMLStreamEncoding.CodePage = TEncoding.Unicode.CodePage then
  begin
    if not ReadXMLNodeFromStream(aStream, True) then
      aStream.RaiseInvalidXMLFormat;
  end
  else
  begin
    // Converting stream
    lStreamDataSize := aStream.Size - aStream.Position;
    Setlength(lOriginalBuffer, lStreamDataSize);
    aStream.ReadBuffer(lOriginalBuffer[0], lStreamDataSize);
    lConvertedBuffer := TEncoding.Convert(aStream.XMLStreamEncoding, TEncoding.Unicode, lOriginalBuffer);
    SetLength(lOriginalBuffer, 0);
    lTempStream := TXMLStream.Create(TEncoding.Unicode);
    try
      lTempStream.Size := Length(lConvertedBuffer);
      lTempStream.WriteBuffer(lConvertedBuffer[0], Length(lConvertedBuffer));
      SetLength(lConvertedBuffer, 0);
      lTempStream.Seek(0, soBeginning);
      if not ReadXMLNodeFromStream(lTempStream, True) then
        lTempStream.RaiseInvalidXMLFormat;
    finally
      lTempStream.Free;
    end;
  end;
end;

procedure TXMLNode.SaveToXMLStream(aStream: TXMLStream; aWriteXMLHeader: Boolean = True; aWritePreamble: Boolean = True; aUseTabIndents: Boolean = True);
var
  lTempStream: TXMLStream;
begin
  aStream.Seek(0, soBeginning);

  // We must write all data in unicode first
  lTempStream := TXMLStream.Create(TEncoding.Unicode);
  try
    lTempStream.UseTabIndent := aUseTabIndents;
    if aWriteXMLHeader then
      lTempStream.WriteXMLHeader(aStream.XMLStreamEncoding);
    WriteXMLNodeToStream(lTempStream, 0);

    // We must write preamble directly to destination stream to avoid converting it
    if aWritePreamble then
      aStream.WritePreamble;
    // Encoding conversion is inside if we need it
    aStream.CopyFromXMLStream(lTempStream);
  finally
    lTempStream.Free;
  end;
end;

procedure TXMLNode.LoadFromFile(const aFileName: string);
var
  lXMLStream: TXMLStream;
begin
  lXMLStream := TXMLStream.Create;
  try
    lXMLStream.LoadFromFile(aFileName);
    // Position should be 0 here
    try
      LoadFromXMLStream(lXMLStream, TEncoding.UTF8);
    except
      on E: EInvalidFormatXMLException do
        raise EInvalidFormatXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_FAILED_LOAD_FILE, [aFileName, E.Message], E.LineNumber, E.NearText, E.NearTextPosition);
      on E: Exception do
        raise EXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_FAILED_LOAD_FILE, [aFileName, E.Message]);
    end;
  finally
    lXMLStream.Free;
  end;
end;

procedure TXMLNode.SaveToFile(const aFileName: string; aEncoding: TEncoding = nil);
var
  lXMLstream: TXMLStream;
begin
  if aEncoding = nil then
    lXMLStream := TXMLStream.Create(TEncoding.UTF8)
  else
    lXMLStream := TXMLStream.Create(aEncoding);
  try
    SaveToXMLStream(lXMLStream, True, True, True);
    lXMLStream.SaveToFile(aFileName);
  finally
    lXMLStream.Free;
  end;
end;

function TXMLNode.GetNodeAsText(aWriteXMLHeader: Boolean = False; aWritePreamble: Boolean = False): string;
var
  lXMLstream: TXMLStream;
begin
  lXMLStream := TXMLStream.Create(TEncoding.Unicode);
  try
    SaveToXMLStream(lXMLStream, aWriteXMLHeader, aWritePreamble, False);
    Result := lXMLStream.GetDataAsString;
  finally
    lXMLStream.Free;
  end;
end;

procedure TXMLNode.AddAsChild(aNode: TXMLNode);
{$IFNDEF STATIC_CHAINS}
var
  lCurrentNode: TXMLNode;
{$ENDIF}
begin
  {$IFNDEF STATIC_CHAINS}
  aNode.AssignGeneralSettings(Self);
  {$ENDIF}
  fImmediateChildren.AddToImmediate(aNode);
  {$IFNDEF STATIC_CHAINS}
  if fHashGeneralChildren then
  begin
    lCurrentNode := Self;
    while lCurrentNode <> nil do
    begin
      lCurrentNode.fGeneralChildren.Add(aNode.fGeneralNodeHashItem);
      lCurrentNode := lCurrentNode.Parent;
    end;
  end;
  // We need to add general properties, starting from itself
  aNode.AddFromNodeGeneralPropertiesToNodeAndParents(aNode);
  {$ENDIF}
end;

procedure TXMLNode.InsertAsChild(aNode, aBeforeNode: TXMLNode);
{$IFNDEF STATIC_CHAINS}
var
  lCurrentNode: TXMLNode;
{$ENDIF}
begin
  {$IFNDEF STATIC_CHAINS}
  aNode.AssignGeneralSettings(Self);
  {$ENDIF}
  fImmediateChildren.InsertToImmediate(aNode, aBeforeNode);
  {$IFNDEF STATIC_CHAINS}
  if fHashGeneralChildren then
  begin
    lCurrentNode := Self;
    while lCurrentNode <> nil do
    begin
      lCurrentNode.fGeneralChildren.Add(aNode.fGeneralNodeHashItem);
      lCurrentNode := lCurrentNode.Parent;
    end;
  end;
  // We need to add general properties, starting from itself
  aNode.AddFromNodeGeneralPropertiesToNodeAndParents(aNode);
  {$ENDIF}
end;

function TXMLNode.CreateNodeAsChild(const aNodeName, aNodeTextValue: string): TXMLNode;
begin
  Result := nil;
  try
    Result := TXMLNode.Create(aNodeName, aNodeTextValue);
    AddAsChild(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXMLNode.RemoveFromChildren(aNode: TXMLNode);
{$IFNDEF STATIC_CHAINS}
var
  lCurrentNode: TXMLNode;
{$ENDIF}
begin
  {$IFNDEF STATIC_CHAINS}
  // We need to remove all general properties starting from itself
  aNode.RemoveGeneralPropertiesFromNodeAndParents(aNode);
  if fHashGeneralChildren then
  begin
    lCurrentNode := Self;
    while lCurrentNode <> nil do
    begin
      lCurrentNode.fGeneralChildren.Delete(aNode.fGeneralNodeHashItem, False);
      lCurrentNode := lCurrentNode.Parent;
    end;
  end;
  {$ENDIF}
  fImmediateChildren.DeleteFromImmediate(aNode, False);
end;

{$IFNDEF STATIC_CHAINS}
procedure TXMLNode.AddFromNodeGeneralPropertiesToNodeAndParents(aFromNode: TXMLNode);
var
  lCurrentProperty: TXMLProperty;
begin
  if not fHashChildrenProperties then Exit;
  lCurrentProperty := aFromNode.Properties.GetFirst;
  while lCurrentProperty <> nil do
  begin
    CreateAndAddGeneralPropertyToNodeAndParents(lCurrentProperty);
    lCurrentProperty := lCurrentProperty.GetNext;
  end;
end;

procedure TXMLNode.RemoveGeneralPropertiesFromNodeAndParents(aFromNode: TXMlNode);
var
  lCurrentProperty: TXMLProperty;
begin
  if not fHashChildrenProperties then Exit;
  lCurrentProperty := aFromNode.Properties.GetFirst;
  while lCurrentProperty <> nil do
  begin
    RemoveAndFreeGeneralPropertyFromNodeAndParents(lCurrentProperty);
    lCurrentProperty := lCurrentProperty.GetNext;
  end;
end;
{$ENDIF}

procedure TXMLNode.AddProperty(aProperty: TXMLProperty);
begin
  try
    fNodeProperties.Add(aProperty);
    {$IFNDEF STATIC_CHAINS}
    if fHashChildrenProperties then
      CreateAndAddGeneralPropertyToNodeAndParents(aProperty);
    {$ENDIF}
  except
    fNodeProperties.Delete(aProperty);
    raise;
  end;
end;

function TXMLNode.CreateAndAddProperty(const aPropertyName: string; const aPropertyValue: Variant): TXMLProperty;
begin
  Result := nil;
  try
    Result := TXMLProperty.Create(Self, aPropertyName, aPropertyValue);
    AddProperty(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXMLNode.RemoveProperty(aProperty: TXMLProperty);
begin
  {$IFNDEF STATIC_CHAINS}
  if fHashChildrenProperties then
    RemoveAndFreeGeneralPropertyFromNodeAndParents(aProperty);
  {$ENDIF}
  fNodeProperties.Delete(aProperty, False);
end;

procedure TXMLNode.ClearAllChildren;
begin
  fImmediateChildren.Clear;
  fNodeProperties.Clear;
  {$IFNDEF STATIC_CHAINS}
  fGeneralChildren.Clear;
  fGeneralChildrenProperties.Clear;
  {$ENDIF}
end;

function TXMLNode.FindFirstByNodeNameInImmediate(const aSearchNodeName: string): TXMLNode;
begin
  Result := TXMLNode(fImmediateChildren.fImmediateNodeNamesStringHashTable.FindFirst(aSearchNodeName));
end;

function TXMLNode.FindNextByNodeNameInImmediate(aNode: TXMLNode): TXMLNode;
begin
  Result := TXMLNode(fImmediateChildren.fImmediateNodeNamesStringHashTable.FindNext(@aNode.fImmediateNodeNameChain));
end;

function TXMLNode.FindFirstByNodeValueInImmediate(const aSearchNodeValue: string): TXMLNode;
begin
  Result := TXMLNode(fImmediateChildren.fImmediateNodeTextValuesStringHashTable.FindFirst(aSearchNodeValue));
end;

function TXMLNode.FindNextByNodeValueInImmediate(aNode: TXMLNode): TXMLNode;
begin
  Result := TXMLNode(fImmediateChildren.fImmediateNodeTextValuesStringHashTable.FindNext(@aNode.fImmediateNodeTextValueChain));
end;

{$IFNDEF STATIC_CHAINS}
function TXMLNode.FindFirstByNodeNameInGeneral(const aSearchNodeName: string): TXMLNode;
var
  lGeneralHashItem: TXMLNodeNameNodeTextGeneralItem;
begin
  lGeneralHashItem := TXMLNodeNameNodeTextGeneralItem(fGeneralChildren.fGeneralNamesStringHashTable.FindFirst(aSearchNodeName));
  if lGeneralHashItem = nil then Exit(nil);
  Result := lGeneralHashItem.fParentNode;
end;

function TXMLNode.FindNextByNodeNameInGeneral(aNode: TXMLNode): TXMLNode;
var
  lGeneralHashItem: TXMLNodeNameNodeTextGeneralItem;
begin
  lGeneralHashItem := TXMLNodeNameNodeTextGeneralItem(fGeneralChildren.fGeneralNamesStringHashTable.FindNext(@(aNode.fGeneralNodeHashItem.fGeneralNodeNameChain)));
  if lGeneralHashItem = nil then Exit(nil);
  Result := lGeneralHashItem.fParentNode;
end;

function TXMLNode.FindFirstByNodeValueInGeneral(const aSearchNodeValue: string): TXMLNode;
var
  lGeneralHashItem: TXMLNodeNameNodeTextGeneralItem;
begin
  lGeneralHashItem := TXMLNodeNameNodeTextGeneralItem(fGeneralChildren.fGeneralTextValuesStringHashTable.FindFirst(aSearchNodeValue));
  if lGeneralHashItem = nil then Exit(nil);
  Result := lGeneralHashItem.fParentNode;
end;

function TXMLNode.FindNextByNodeValueInGeneral(aNode: TXMLNode): TXMLNode;
var
  lGeneralHashItem: TXMLNodeNameNodeTextGeneralItem;
begin
  lGeneralHashItem := TXMLNodeNameNodeTextGeneralItem(fGeneralChildren.fGeneralTextValuesStringHashTable.FindNext(@(aNode.fGeneralNodeHashItem.fGeneralNodeTextValueChain)));
  if lGeneralHashItem = nil then Exit(nil);
  Result := lGeneralHashItem.fParentNode;
end;

function TXMLNode.FindFirstByPropertyNameInGeneral(const aSearchPropertyName: string): TXMLProperty;
begin
  Result := fGeneralChildrenProperties.FindFirstByName(aSearchPropertyName);
end;

function TXMLNode.FindNextByPropertyNameInGeneral(aProperty: TXMLProperty): TXMLProperty;
begin
  Result := fGeneralChildrenProperties.FindNextByName(aProperty);
end;
{$ENDIF}

function TXMLNode.GetFirstChild: TXMLNode;
begin
  Result := Self.ImmediateChildren.GetFirst;
end;

function TXMLNode.GetLastChild: TXMLNode;
begin
  Result := Self.ImmediateChildren.GetLast;
end;

function TXMLNode.GetNextTraversal: TXMLNode;
begin
  if (fImmediateChildren <> nil) and (fImmediateChildren.fFirst <> nil) then
  begin
    Result := TXMLNode(fImmediateChildren.fFirst);
    Exit;
  end;
  if fNext <> nil then
  begin
    Result := TXMLNode(fNext);
    Exit;
  end;
  if fParent <> nil then
  begin
    Result := fParent;
    while Result <> nil do
      if Result.fNext <> nil then
        Exit(TXMLNode(Result.fNext))
      else
        Result := Result.fParent;
  end;
  Result := nil;
end;

function TXMLNode.GetPrevTraversal: TXMLNode;
begin
  if fPrev <> nil then
  begin
    Result := TXMLNode(fPrev);
    while Result.fImmediateChildren.fLast <> nil do
      Result := TXMLNode(Result.fImmediateChildren.fLast);
    Exit;
  end;
  Result := fParent;
end;

function TXMLNode.GetNextSibling: TXMLNode;
begin
  Result := TXMLNode(fNext);
end;

function TXMLNode.GetPrevSibling: TXMLNode;
begin
  Result := TXMLNode(fPrev);
end;

function TXMLNode.GetRoot: TXMLNode;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Parent.Parent;
end;

function TXMLNode.IsParent(aChildNode: TXMLNode): Boolean;
begin
  while aChildNode <> nil do
  begin
    aChildNode := aChildNode.Parent;
    if Self = aChildNode then
      Exit(True);
  end;
  Result := False;
end;

{ TXMLNodes }

constructor TXMLNodes.Create(aParent: TXMLNode; aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aFreeItemsOnDestroy: Boolean = True);
begin
  inherited Create(aHashSize, aAutoExtendHash, aFreeItemsOnDestroy);
  fParent := aParent;
end;

procedure TXMLNodes.AddToImmediate(aNode: TXMLNode);
begin
  InternalAddItem(aNode);
  aNode.fParent := fParent;
end;

procedure TXMLNodes.InsertToImmediate(aNode, aBeforeNode: TXMLNode);
begin
  InternalInsertItem(aNode, aBeforeNode);
  aNode.fParent := fParent;
end;

procedure TXMLNodes.DeleteFromImmediate(aNode: TXMLNode; aFreeNode: Boolean = True);
begin
  aNode.fParent := nil;
  InternalDeleteItem(aNode, aFreeNode);
end;

procedure TXMLNodes.InitHashTables(aHashSize: Cardinal);
begin
  fImmediateNodeNamesStringHashTable.Init(aHashSize, Integer(@TXMLNode(0).fNodeName));
  fImmediateNodeTextValuesStringHashTable.Init(aHashSize, Integer(@TXMLNode(0).fNodeTextValue));
end;

procedure TXMLNodes.AddToHashTable(aHashItem: THashBaseItem);
begin
  fImmediateNodeNamesStringHashTable.AddToTable(@TXMLNode(aHashItem).fImmediateNodeNameChain);
  fImmediateNodeTextValuesStringHashTable.AddToTable(@TXMLNode(aHashItem).fImmediateNodeTextValueChain);
end;

procedure TXMLNodes.RemoveFromHashTable(aHashItem: THashBaseItem);
begin
  fImmediateNodeNamesStringHashTable.RemoveFromTable(@TXMLNode(aHashItem).fImmediateNodeNameChain);
  fImmediateNodeTextValuesStringHashTable.RemoveFromTable(@TXMLNode(aHashItem).fImmediateNodeTextValueChain);
end;

procedure TXMLNodes.SetHashSize(aHashSize: Cardinal);
begin
  fImmediateNodeNamesStringHashTable.SetTableSize(aHashSize);
  fImmediateNodeTextValuesStringHashTable.SetTableSize(aHashSize);
end;

procedure TXMLNodes.DestroyHashTables;
begin
  fImmediateNodeNamesStringHashTable.Destroy;
  fImmediateNodeTextValuesStringHashTable.Destroy;
end;

procedure TXMLNodes.BeforeDestruction;
begin
  Clear;
  inherited BeforeDestruction;
end;

function TXMLNodes.GetChildrenCount(aRecursive: Boolean = False): Integer;
var
  lNodeItem: TXMLNode;
begin
  Result := Count;
  if not aRecursive then Exit;
  lNodeItem := GetFirst;
  while lNodeItem <> nil do
  begin
    Inc(Result, lNodeItem.ImmediateChildren.GetChildrenCount(aRecursive));
    lNodeItem := lNodeItem.GetNextSibling;
  end;
end;

function TXMLNodes.GetFirst: TXMLNode;
begin
  Result := TXMLNode(fFirst);
end;

function TXMLNodes.GetLast: TXMLNode;
begin
  Result := TXMLNode(fLast);
end;

{ TXMLProperty }

constructor TXMLProperty.Create(aParent: TXMLNode; const aName: string);
begin
  inherited Create(aName);
  fParentNode := aParent;
end;

constructor TXMLProperty.Create(aParent: TXMLNode; const aName: string; const aValue: Variant);
begin
  inherited Create(aName, aValue);
  fParentNode := aParent;
end;

function TXMLProperty.GetNext: TXMLProperty;
begin
  Result := TXMLProperty(fNext);
end;

function TXMLProperty.GetPrev: TXMLProperty;
begin
  Result := TXMLProperty(fPrev);
end;

{ TXMLProperties }

constructor TXMLProperties.Create(aParent: TXMLNode; aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aAllowDuplicates: Boolean = True; aFreeItemsOnDestroy: Boolean = True);
begin
  inherited Create(aHashSize, aAutoExtendHash, aAllowDuplicates, aFreeItemsOnDestroy);
  fParentNode := aParent;
end;

procedure TXMLProperties.Add(aProperty: TXMLProperty);
begin
  inherited Add(aProperty);
end;

function TXMLProperties.FindFirstByName(const aSearchName: string): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindFirstByName(aSearchName));
end;

function TXMLProperties.FindNextByName(aItem: TXMLProperty): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindNextByName(aItem));
end;

function TXMLProperties.FindFirstByStringValue(const aSearchValue: string): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindFirstByStringValue(aSearchValue));
end;

function TXMLProperties.FindNextByStringValue(aItem: TXMLProperty): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindNextByStringValue(aItem));
end;

function TXMLProperties.FindFirstByIntegerValue(const aSearchValue: Integer): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindFirstByIntegerValue(aSearchValue));
end;

function TXMLProperties.FindNextByIntegerValue(aItem: TXMLProperty): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindNextByIntegerValue(aItem));
end;

function TXMLProperties.FindFirstByInt64Value(const aSearchValue: Int64): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindFirstByInt64Value(aSearchValue));
end;

function TXMLProperties.FindNextByInt64Value(aItem: TXMLProperty): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindNextByInt64Value(aItem));
end;

function TXMLProperties.FindFirstByDoubleValue(const aSearchValue: Double): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindFirstByDoubleValue(aSearchValue));
end;

function TXMLProperties.FindNextByDoubleValue(aItem: TXMLProperty): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindNextByDoubleValue(aItem));
end;

function TXMLProperties.FindFirstByBooleanValue(const aSearchValue: Boolean): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindFirstByBooleanValue(aSearchValue));
end;

function TXMLProperties.FindNextByBooleanValue(aItem: TXMLProperty): TXMLProperty;
begin
  Result := TXMLProperty(inherited FindNextByBooleanValue(aItem));
end;

function TXMLProperties.GetFirst: TXMLProperty;
begin
  Result := TXMLProperty(fFirst);
end;

function TXMLProperties.GetLast: TXMLProperty;
begin
  Result := TXMLProperty(fLast);
end;

end.
