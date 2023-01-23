unit cuClasses;

interface

uses
  System.SysUtils,
  cuConsts,
  uCustomExceptions
  ;

type
  // Forward declaration
  TAbstractList = class;

  ByteArray  = array[0 .. $fffffff] of Byte;
  PByteArray = ^ByteArray;

  WideCharArray  = array[0 .. $fffffff] of WideChar;
  PWideCharArray = ^WideCharArray;

  // Record for strings to minimize memory realocation
  TUnicodeStringBuilder = record
  private
    fStringPointer: PString;
    fFirstCharPointer: PWideChar;
    fReservedLength: NativeUInt;
    fStringLength: NativeUInt;

    procedure ExtendReservedStringSize(aMinimalExtensionSize: NativeUInt = 1);
    procedure SetWorkingLength(aLength: NativeUInt);
  public
    procedure Init(var aString: string; aReservedLength: NativeUInt = 100);
    procedure Clear;
    procedure Done;

    procedure Add(const aChar: WideChar); overload;
    procedure Add(const aString: string); overload;

    property StringLength: NativeUInt read fStringLength;
  end;

  // Base class for list items
  TAbstractListItem = class abstract(TObject)
  protected
    fOwner: TAbstractList;
    fPrev: TAbstractListItem;
    fNext: TAbstractListItem;
  public
    property Owner: TAbstractList read fOwner write fOwner;
    property Prev: TAbstractListItem read fPrev write fPrev;
    property Next: TAbstractListItem read fNext write fNext;
  end;

  // Base class for lists
  TAbstractList = class abstract(TObject)
  strict private
    fCount: Int64;
  protected
    fFirst: TAbstractListItem;
    fLast: TAbstractListItem;
    procedure InternalAddToListEnd(aItem: TAbstractListItem); inline;
    procedure InternalInsertToList(aItem, aBeforeItem: TAbstractListItem); inline;
    procedure InternalRemoveFromList(aItem: TAbstractListItem); inline;
  public
    property Count: Int64 read fCount;
  end;

  // Basic list item
  TCustomDynamicListItem = class(TAbstractListItem)
  public
    destructor Destroy; override;
  end;

  // Basic dynamic bidirectional list
  TCustomDynamicList = class(TAbstractList)
  protected
    fFreeItemsOnDestroy: Boolean;
  public
    constructor Create(const aFreeItemsOnDestroy: Boolean = True);
    procedure BeforeDestruction; override;
    procedure AddToList(aItem: TCustomDynamicListItem); inline;
    procedure InsertToList(aItem, aBeforeItem: TCustomDynamicListItem); inline;
    procedure DeleteFromList(aItem: TCustomDynamicListItem; aFreeItem: Boolean = True); inline;
    procedure Clear; virtual;
  end;

implementation

{ TStringBuilder }

procedure TUnicodeStringBuilder.ExtendReservedStringSize(aMinimalExtensionSize: NativeUInt = 1);
var
  lExtensionSize: NativeUInt;
begin
  lExtensionSize := fReservedLength shr 1;
  if lExtensionSize < aMinimalExtensionSize then
    lExtensionSize := aMinimalExtensionSize;
  Inc(fReservedLength, lExtensionSize);
  SetWorkingLength(fReservedLength);
end;

procedure TUnicodeStringBuilder.SetWorkingLength(aLength: NativeUInt);
begin
  SetLength(fStringPointer^, aLength);
  fFirstCharPointer := Pointer(fStringPointer^);
end;

procedure TUnicodeStringBuilder.Init(var aString: string; aReservedLength: NativeUInt = 100);
begin
  fReservedLength := aReservedLength;
  SetLength(aString, fReservedLength);
  fStringPointer := @aString;
  fStringLength := 0;
  fFirstCharPointer := Pointer(fStringPointer^);
end;

procedure TUnicodeStringBuilder.Clear;
begin
  fStringLength := 0;
end;

procedure TUnicodeStringBuilder.Done;
begin
  SetWorkingLength(fStringLength);
end;

procedure TUnicodeStringBuilder.Add(const aChar: WideChar);
begin
  if fReservedLength <= fStringLength then
    ExtendReservedStringSize;
  PWideCharArray(fFirstCharPointer)^[fStringLength] := aChar;
  Inc(fStringLength);
end;

procedure TUnicodeStringBuilder.Add(const aString: string);
var
  lAddedLength: NativeUInt;
begin
  lAddedLength := Length(aString);
  if lAddedLength = 0 then Exit;
  if fReservedLength <= (fStringLength + lAddedLength) then
    ExtendReservedStringSize(lAddedLength);
  Move(Pointer(aString)^,  PWideCharArray(fFirstCharPointer)^[fStringLength], lAddedLength * SizeOf(WideChar));
  Inc(fStringLength, lAddedLength);
end;

{ TAbstractList }

procedure TAbstractList.InternalAddToListEnd(aItem: TAbstractListItem);
begin
  if aItem = nil then Exit;
  if aItem.fOwner <> nil then
    raise EListException.Create(EXCEPTION_MESSAGE_LIST_ITEM_ALREADY_OWNED_ADDING);
  aItem.fOwner := Self;
  aItem.fPrev := fLast;
  aItem.fNext := nil;
  if fLast = nil then
    fFirst := aItem
  else
    fLast.fNext := aItem;
  fLast := aItem;
  Inc(fCount);
end;

procedure TAbstractList.InternalInsertToList(aItem, aBeforeItem: TAbstractListItem);
begin
  if aBeforeItem = nil then
  begin
    InternalAddToListEnd(aItem);
    Exit;
  end;
  if aItem.fOwner <> nil then
    raise EListException.Create(EXCEPTION_MESSAGE_LIST_ITEM_ALREADY_OWNED_ADDING);
  if aBeforeItem.fOwner <> Self then
    raise EListException.Create(EXCEPTION_MESSAGE_LIST_ITEM_ALREADY_OWNED);
  // Inserting into list
  aItem.fOwner := Self;
  if aBeforeItem = fFirst then
  begin
    fFirst := aItem;
    aItem.fPrev := nil;
    aItem.fNext := aBeforeItem;
    aBeforeItem.fPrev := aItem;
  end
  else
  begin
    aItem.fPrev := aBeforeItem.fPrev;
    aItem.fPrev.fNext := aItem;
    aBeforeItem.fPrev := aItem;
    aItem.fNext := aBeforeItem;
  end;
  Inc(fCount);
end;

procedure TAbstractList.InternalRemoveFromList(aItem: TAbstractListItem);
begin
  if aItem = nil then Exit;
  if aItem.fOwner = nil then
    raise EListException.Create(EXCEPTION_MESSAGE_LIST_ITEM_NOT_OWNED);
  if aItem.fOwner <> Self then
    raise EListException.Create(EXCEPTION_MESSAGE_LIST_ITEM_ALREADY_OWNED);
  if aItem.fPrev = nil then
    fFirst := aItem.fNext
  else
    aItem.fPrev.fNext := aItem.fNext;
  if aItem.fNext = nil then
    fLast := aItem.fPrev
  else
    aItem.fNext.fPrev := aItem.fPrev;
  Dec(fCount);
  aItem.fOwner := nil;
end;

{ TCustomDynamicListItem }

destructor TCustomDynamicListItem.Destroy;
begin
  if fOwner <> nil then TCustomDynamicList(fOwner).DeleteFromList(Self, False);
  inherited Destroy;
end;

{ TCustomDynamicList }

constructor TCustomDynamicList.Create(const aFreeItemsOnDestroy: Boolean = True);
begin
  inherited Create;
  fFreeItemsOnDestroy := aFreeItemsOnDestroy;
end;

procedure TCustomDynamicList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if fFreeItemsOnDestroy then
    Clear;
end;

procedure TCustomDynamicList.AddToList(aItem: TCustomDynamicListItem);
begin
  InternalAddToListEnd(aItem);
end;

procedure TCustomDynamicList.InsertToList(aItem, aBeforeItem: TCustomDynamicListItem);
begin
  InternalInsertToList(aItem, aBeforeItem);
end;

procedure TCustomDynamicList.DeleteFromList(aItem: TCustomDynamicListItem; aFreeItem: Boolean = True);
begin
  InternalRemoveFromList(aItem);
  if aFreeItem then
    aItem.Free;
end;

procedure TCustomDynamicList.Clear;
begin
  while fLast <> nil do FreeAndNil(fLast);
end;

end.
