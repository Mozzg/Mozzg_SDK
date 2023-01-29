unit cuClasses;

interface

uses
  System.SysUtils,
  cuConsts,
  uCustomExceptions
  ;

const
  TwoDigitLookup : packed array[0..99] of array[1..2] of WideChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

type
  // Forward declaration
  TAbstractList = class;

  ByteArray  = array[0 .. $fffffff] of Byte;
  PByteArray = ^ByteArray;

  WideCharArray  = array[0 .. $fffffff] of WideChar;
  PWideCharArray = ^WideCharArray;

  DWORD = FixedUInt;
  PDWORD = ^FixedUInt;

  // Record for strings to minimize memory realocation
  TUnicodeStringBuilder = record
  private const
    ORD_ZERO_CHAR = ord('0');
    MINUS_CHAR: WideChar = '-';
    JSON_COMMA: WideChar = ',';
    JSON_OPENING_BRACKET: WideChar = '{';
    JSON_CLOSING_BRACKET: WideChar = '}';
    JSON_OPENING_ARRAY_BRACKET: WideChar = '[';
    JSON_CLOSING_ARRAY_BRACKET: WideChar = ']';
    JSON_DOUBLE_BRACKETS: WideChar = '"';
    JSON_COLON: WideChar = ':';
    JSON_ESCAPE_CHAR: WideChar = '\';
  private
    fStringPointer: PString;
    fFirstCharPointer: PWideChar;
    fReservedLength: NativeUInt;
    fStringLength: NativeUInt;
    fJSONNeedComma: Boolean;

    procedure AddFastInt32(aValue: Integer; aNegative: Boolean); inline;
    procedure AddFastInt64(aValue: Int64; aNegative: Boolean); inline;

    procedure JSON_AddValueCommaPrefix; inline;
    procedure JSON_AddParameterName(const aName: string); inline;

    procedure ExtendReservedStringSize(aMinimalExtensionSize: NativeUInt = 1);
    procedure SetWorkingLength(aLength: NativeUInt);
  public
    procedure Init(var aString: string; aReservedLength: NativeUInt = 100);
    procedure Clear;
    procedure Done;

    procedure Add(const aChar: WideChar); overload;
    procedure Add(const aString: string); overload;
    procedure AddInteger(aIntegerValue: Integer); overload; inline;
    procedure AddInteger(aIntegervalue: Int64); overload; inline;

    procedure JSON_StartArray; inline;
    procedure JSON_CloseArray; inline;
    procedure JSON_StartObject; inline;
    procedure JSON_CloseObject; inline;

    procedure JSON_AddStringParameter(const aName, aValue: string); inline;
    procedure JSON_AddIntegerParameter(const aName: string; aValue: Integer); inline;
    procedure JSON_AddEscapedString(const aString: string); inline;

    procedure JSON_AddArrayItemObject; inline;

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

procedure TUnicodeStringBuilder.AddFastInt32(aValue: Integer; aNegative: Boolean);
var
  I, J, K: Cardinal;
  lDigits: Integer;
  lAddedLength: Integer;
  lOrdNegative: Byte;
  P: PWideChar;
begin
  // Method copied from System.SysUtils unit and adapted for this record
  I := aValue;
  if I >= 10000 then
    if I >= 1000000 then
      if I >= 100000000 then
        lDigits := 9 + Ord(I >= 1000000000)
      else
        lDigits := 7 + Ord(I >= 10000000)
    else
      lDigits := 5 + Ord(I >= 100000)
  else
    if I >= 100 then
      lDigits := 3 + Ord(I >= 1000)
    else
      lDigits := 1 + Ord(I >= 10);

  lOrdNegative := Ord(aNegative);
  lAddedLength := lDigits + lOrdNegative;
  if fReservedLength <= (fStringLength + lAddedLength) then
    ExtendReservedStringSize(lAddedLength);

  P := @PWideCharArray(fFirstCharPointer)^[fStringLength];
  PWideCharArray(fFirstCharPointer)^[fStringLength] := MINUS_CHAR;
  Inc(P, lOrdNegative);

  if lDigits > 2 then
    repeat
      J  := I div 100;           {Dividend div 100}
      K  := J * 100;
      K  := I - K;               {Dividend mod 100}
      I  := J;                   {Next Dividend}
      Dec(lDigits, 2);
      PDWord(P + lDigits)^ := DWord(TwoDigitLookup[K]);
    until lDigits <= 2;
  if lDigits = 2 then
    PDWord(P+ lDigits-2)^ := DWord(TwoDigitLookup[I])
  else
    PChar(P)^ := WideChar(I or ORD_ZERO_CHAR);

  Inc(fStringLength, lAddedLength);
end;

procedure TUnicodeStringBuilder.AddFastInt64(aValue: Int64; aNegative: Boolean);
var
  I64, J64, K64: UInt64;
  I32, J32, K32, L32: Cardinal;
  lDigits: Byte;
  P: PChar;
  lAddedLength: Integer;
  lOrdNegative: Byte;
begin
  {Within Integer Range - Use Faster Integer Version}
  if (aNegative and (aValue <= High(Integer)))
      or (not aNegative and (aValue <= High(Cardinal)))
  then
  begin
    AddFastInt32(aValue, aNegative);
    Exit;
  end;

  I64 := aValue;
  if I64 >= 100000000000000 then
    if I64 >= 10000000000000000 then
      if I64 >= 1000000000000000000 then
        if I64 >= 10000000000000000000 then
          lDigits := 20
        else
          lDigits := 19
      else
        lDigits := 17 + Ord(I64 >= 100000000000000000)
    else
      lDigits := 15 + Ord(I64 >= 1000000000000000)
  else
    if I64 >= 1000000000000 then
      lDigits := 13 + Ord(I64 >= 10000000000000)
    else
      if I64 >= 10000000000 then
        lDigits := 11 + Ord(I64 >= 100000000000)
      else
        lDigits := 10;

  lOrdNegative := Ord(aNegative);
  lAddedLength := lDigits + lOrdNegative;
  if fReservedLength <= (fStringLength + lAddedLength) then
    ExtendReservedStringSize(lAddedLength);

  P := @PWideCharArray(fFirstCharPointer)^[fStringLength];
  PWideCharArray(fFirstCharPointer)^[fStringLength] := MINUS_CHAR;
  Inc(P, lOrdNegative);

  if lDigits = 20 then
  begin
    P^ := '1';
    Inc(P);
    Dec(I64, 10000000000000000000);
    Dec(lDigits);
  end;
  if lDigits > 17 then
  begin {18 or 19 Digits}
    if lDigits = 19 then
    begin
      P^ := '0';
      while I64 >= 1000000000000000000 do
      begin
        Dec(I64, 1000000000000000000);
        Inc(P^);
      end;
      Inc(P);
    end;
    P^ := '0';
    while I64 >= 100000000000000000 do
    begin
      Dec(I64, 100000000000000000);
      Inc(P^);
    end;
    Inc(P);
    lDigits := 17;
  end;
  J64 := I64 div 100000000;
  K64 := I64 - (J64 * 100000000); {Remainder = 0..99999999}
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PDWord(P + lDigits - 2)^ := DWord(TwoDigitLookup[K32]);
  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  PDWord(P + lDigits - 4)^ := DWord(TwoDigitLookup[L32]);
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PDWord(P + lDigits - 6)^ := DWord(TwoDigitLookup[K32]);
  PDWord(P + lDigits - 8)^ := DWord(TwoDigitLookup[J32]);
  Dec(lDigits, 8);
  I32 := J64; {Dividend now Fits within Integer - Use Faster Version}
  if lDigits > 2 then
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;
      I32 := J32;
      Dec(lDigits, 2);
      PDWord(P + lDigits)^ := DWord(TwoDigitLookup[K32]);
    until lDigits <= 2;
  if lDigits = 2 then
    PDWord(P + lDigits-2)^ := DWord(TwoDigitLookup[I32])
  else
    P^ := WideChar(I32 or ORD_ZERO_CHAR);

  Inc(fStringLength, lAddedLength);
end;

procedure TUnicodeStringBuilder.JSON_AddValueCommaPrefix;
begin
  if fJSONNeedComma then
    Add(JSON_COMMA);
  fJSONNeedComma := True;
end;

procedure TUnicodeStringBuilder.JSON_AddParameterName(const aName: string);
begin
  JSON_AddValueCommaPrefix;
  Add(JSON_DOUBLE_BRACKETS);
  JSON_AddEscapedString(aName);
  Add(JSON_DOUBLE_BRACKETS);
  Add(JSON_COLON);
end;

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
  fJSONNeedComma := False;
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

procedure TUnicodeStringBuilder.AddInteger(aIntegerValue: Integer);
begin
  if aIntegerValue < 0 then
    AddFastInt32(-aIntegerValue, True)
  else
    AddFastInt32(aIntegerValue, False);
end;

procedure TUnicodeStringBuilder.AddInteger(aIntegervalue: Int64);
begin
  if aIntegerValue < 0 then
    AddFastInt64(-aIntegerValue, True)
  else
    AddFastInt64(aIntegerValue, False);
end;

procedure TUnicodeStringBuilder.JSON_StartArray;
begin
  fJSONNeedComma := False;
  Add(JSON_OPENING_ARRAY_BRACKET);
end;

procedure TUnicodeStringBuilder.JSON_CloseArray;
begin
  fJSONNeedComma := True;
  Add(JSON_CLOSING_ARRAY_BRACKET);
end;

procedure TUnicodeStringBuilder.JSON_StartObject;
begin
  fJSONNeedComma := False;
  Add(JSON_OPENING_BRACKET);
end;

procedure TUnicodeStringBuilder.JSON_CloseObject;
begin
  fJSONNeedComma := True;
  Add(JSON_CLOSING_BRACKET);
end;

procedure TUnicodeStringBuilder.JSON_AddStringParameter(const aName, aValue: string);
begin
  JSON_AddParameterName(aName);
  Add(JSON_DOUBLE_BRACKETS);
  JSON_AddEscapedString(aValue);
  Add(JSON_DOUBLE_BRACKETS);
end;

procedure TUnicodeStringBuilder.JSON_AddIntegerParameter(const aName: string; aValue: Integer);
begin
  JSON_AddParameterName(aName);
  AddInteger(aValue);
end;

procedure TUnicodeStringBuilder.JSON_AddEscapedString(const aString: string);
var
  lMaxStringLength: NativeUInt;
  i: Integer;
begin
  lMaxStringLength := Length(aString) * 2;
  if lMaxStringLength = 0 then Exit;
  if fReservedLength <= (fStringLength + lMaxStringLength) then
    ExtendReservedStringSize(lMaxStringLength);
  for i := Low(aString) to High(aString) do
  begin
    case aString[i] of
      '"', '\', '/':  // special symbols
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := JSON_ESCAPE_CHAR;
        Inc(fStringLength);
        PWideCharArray(fFirstCharPointer)^[fStringLength] := aString[i];
        Inc(fStringLength);
      end;
      #12:  // formfeed
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := JSON_ESCAPE_CHAR;
        Inc(fStringLength);
        PWideCharArray(fFirstCharPointer)^[fStringLength] := 'f';
        Inc(fStringLength);
      end;
      #10:  // linefeed
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := JSON_ESCAPE_CHAR;
        Inc(fStringLength);
        PWideCharArray(fFirstCharPointer)^[fStringLength] := 'n';
        Inc(fStringLength);
      end;
      #13:  // carriage return
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := JSON_ESCAPE_CHAR;
        Inc(fStringLength);
        PWideCharArray(fFirstCharPointer)^[fStringLength] := 'r';
        Inc(fStringLength);
      end;
      #8:  // backspace
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := JSON_ESCAPE_CHAR;
        Inc(fStringLength);
        PWideCharArray(fFirstCharPointer)^[fStringLength] := 'b';
        Inc(fStringLength);
      end;
      #9:  // horizontal tab
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := JSON_ESCAPE_CHAR;
        Inc(fStringLength);
        PWideCharArray(fFirstCharPointer)^[fStringLength] := 't';
        Inc(fStringLength);
      end;
    else
      begin
        PWideCharArray(fFirstCharPointer)^[fStringLength] := aString[i];
        Inc(fStringLength);
      end;
    end;
  end;
end;

procedure TUnicodeStringBuilder.JSON_AddArrayItemObject;
begin
  JSON_AddValueCommaPrefix;
  Add(JSON_OPENING_BRACKET);
  fJSONNeedComma := False;
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
