{$I SDK_common_defines.inc}

unit cuHashTables;

interface

uses
  {System.Math, }System.SysUtils, System.Variants,
  cuClasses, cuConsts,
  uCustomExceptions;

type
  TObjectCompareFunction = function(aItem1, aItem2: TObject): Integer of object;

  // Record for hash chain element
  PHashChain = ^THashChain;
  PPHashChain = ^PHashChain;
  THashChain = record
    Item: TAbstractListItem;
    HashValue: Cardinal;
    NextChainLink: PHashChain;
    PrevChainLink: PHashChain;
  end;

  // Max allowed data type size
  TObjectArray = array[0..$EFFFFFF] of TObject;
  PObjectArray = ^TObjectArray;

  // Record for base hash table
  TBaseHashTable = record
  private
    {$IFNDEF STATIC_CHAINS}
    procedure ClearDynamicChains; inline;
    {$ENDIF}
    class function HashLittleBobJenkinsUnsigned(const Data; Len, InitVal: Cardinal): Cardinal; inline; static;
  public
    PointerTable: PPointerArray;
    PointerTableSize: Cardinal;
    PointerTableElementCount: UInt64;

    procedure Init; inline;
    procedure SetTableSize(aNewTableSize: Cardinal); inline;
    procedure AddToTable(aHashChain: PHashChain); inline;
    procedure RemoveFromTable(aHashChain: PHashChain); inline;
    procedure Destroy; inline;

    class function GetHashValue(const AData; ALength: Cardinal; AInitialValue: Cardinal = 0): Cardinal; inline; static;
  end;

  // Record for string hash table
  TStringHashTable = record
    HashTable: TBaseHashTable;
    StringOffset: Integer;
    procedure Init(aSize: Cardinal; aStringOffset: Integer);
    procedure SetTableSize(aNewTableSize: Cardinal); inline;
    procedure AddToTable(aHashChain: PHashChain); inline;
    procedure RemoveFromTable(aHashChain: PHashChain); inline;
    procedure Destroy; inline;
    class function GetStringHashValue(const aValue: string): Cardinal; inline; static;
    function FindFirst(const aSearchValue: string): TAbstractListItem;
    function FindNext(aHashChain: PHashChain): TAbstractListItem;

    property HashTableSize: Cardinal read HashTable.PointerTableSize;
  end;

  // Record for Integer hash table and all other smaller integer types
  TIntegerHashTable = record
    HashTable: TBaseHashTable;
    IntegerOffset: Integer;
    procedure Init(aSize: Cardinal; aIntegerOffset: Integer);
    procedure SetTableSize(aNewTableSize: Cardinal); inline;
    procedure AddToTable(aHashChain: PHashChain); inline;
    procedure RemoveFromTable(aHashChain: PHashChain); inline;
    procedure Destroy; inline;
    class function GetIntegerHashValue(const aValue: Integer): Cardinal; inline; static;
    function FindFirst(const aSearchValue: Integer): TAbstractListItem;
    function FindNext(aHashChain: PHashChain): TAbstractListItem;

    property HashTableSize: Cardinal read HashTable.PointerTableSize;
  end;

  // Record for Int64 hash table
  TInt64HashTable = record
    HashTable: TBaseHashTable;
    Int64Offset: Integer;
    procedure Init(aSize: Cardinal; aInt64Offset: Integer);
    procedure SetTableSize(aNewTableSize: Cardinal); inline;
    procedure AddToTable(aHashChain: PHashChain); inline;
    procedure RemoveFromTable(aHashChain: PHashChain); inline;
    procedure Destroy; inline;
    class function GetInt64HashValue(const aValue: Int64): Cardinal; inline; static;
    function FindFirst(const aSearchValue: Int64): TAbstractListItem;
    function FindNext(aHashChain: PHashChain): TAbstractListItem;

    property HashTableSize: Cardinal read HashTable.PointerTableSize;
  end;

  // Record for Double hash table
  TDoubleHashTable = record
    HashTable: TBaseHashTable;
    DoubleOffset: Integer;
    procedure Init(aSize: Cardinal; aDoubleOffset: Integer);
    procedure SetTableSize(aNewTableSize: Cardinal); inline;
    procedure AddToTable(aHashChain: PHashChain); inline;
    procedure RemoveFromTable(aHashChain: PHashChain); inline;
    procedure Destroy; inline;
    class function GetDoubleHashValue(const aValue: Double): Cardinal; inline; static;
    function FindFirst(const aSearchValue: Double): TAbstractListItem;
    function FindNext(aHashChain: PHashChain): TAbstractListItem;

    property HashTableSize: Cardinal read HashTable.PointerTableSize;
  end;

  TByteHashTable = record
    HashTable: TBaseHashTable;
    ByteOffset: Integer;
    procedure Init(aSize: Cardinal; aByteOffset: Integer);
    procedure SetTableSize(aNewTableSize: Cardinal); inline;
    procedure AddToTable(aHashChain: PHashChain); inline;
    procedure RemoveFromTable(aHashChain: PHashChain); inline;
    procedure Destroy; inline;
    class function GetByteHashValue(const aValue: Byte): Cardinal; inline; static;
    class function GetBooleanHashValue(const aValue: Boolean): Cardinal; inline; static;
    function FindFirst(const aSearchValue: Byte): TAbstractListItem;
    function FindNext(aHashChain: PHashChain): TAbstractListItem;

    property HashTableSize: Cardinal read HashTable.PointerTableSize;
  end;

  // Base class for hashed list item
  THashBaseItem = class abstract(TCustomDynamicListItem)
  protected
    procedure InitHashChains; virtual; abstract;
  public
    procedure AfterConstruction; override;
  end;

  // Base class for hashed list (items are owned by list)
  THashBaseItems = class abstract(TCustomDynamicList)
  protected
    fHashSize: Cardinal;
    fAutoExtensionThreshold: Cardinal;
    fAutoExtendHash: Boolean;
    procedure InitHashTables(aHashSize: Cardinal); virtual; abstract;
    procedure AddToHashTable(aHashItem: THashBaseItem); virtual; abstract;
    procedure RemoveFromHashTable(aHashItem: THashBaseItem); virtual; abstract;
    procedure SetHashSize(aHashSize: Cardinal); virtual; abstract;
    procedure DestroyHashTables; virtual; abstract;

    procedure CheckExtendAndRehash; virtual;
    procedure InternalAddItem(aItem: THashBaseItem);
    procedure InternalInsertItem(aItem, aBeforeItem: THashBaseItem);
    procedure InternalDeleteItem(aItem: THashBaseItem; aFreeItem: Boolean = True);

    // Hide this methods from user in HashTable. User must provide their own Add or Delete in descendant class.
    procedure AddToList(aItem: TCustomDynamicListItem);
    procedure InsertToList(aItem, aBeforeItem: TCustomDynamicListItem);
    procedure DeleteFromList(aItem: TCustomDynamicListItem; aFreeItem: Boolean = True);

    function GetHashSize: Cardinal; inline;
  public
    constructor Create(aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aFreeItemsOnDestroy: Boolean = True);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Sort(aCompareFunction: TObjectCompareFunction);
    procedure Clear; override;

    property HashSize: Cardinal read GetHashSize;
    property ExtensionThresholdSize: Cardinal read fAutoExtensionThreshold;
    property AutoExtendHash: Boolean read fAutoExtendHash;
  end;

  // Type of variant, contained in TNameVariantItem
  TVariantItemType = (
    vitVariant,
    vitInteger,
    vitInt64,
    vitUInt64,
    vitDouble,
    vitDateTime,
    vitBoolean,
    vitUnicodeString
  );

  // Record for containing different types of variants
  TVariantDataRec = packed record
    VariantValue: Variant;
    VariantStringValue: UnicodeString;
    case TVariantItemType of
      vitInteger: (fVDInteger: Integer);
      vitInt64: (fVDInt64: Int64);
      vitDouble: (fVDDouble: Double);
      vitBoolean: (fVDBoolean: Boolean);
  end;

  // Hash table item, containing Name(string)-Value(Variant) pair
  TNameVariantItem = class(THashBaseItem)
  protected
    fName: string;
    fVariantData: TVariantDataRec;
    fValueType: TVariantItemType;

    fNameHashChain: THashChain;
    fStringValueHashChain: THashChain;
    fIntegerValueHashChain: THashChain;
    fInt64ValueHashChain: THashChain;
    fDoubleValueHashChain: THashChain;
    fBooleanValueHashChain: THashChain;

    procedure RemoveFromPreviousOwnerHashTable; virtual;
    procedure SetStringValue(const aValue: Variant); inline;
    procedure SetIntegerValue(const aValue: Variant); inline;
    procedure SetInt64Value(const aValue: Variant; aType: TVarType); inline;
    procedure SetDoubleValue(const aValue: Variant; aType: TVarType); inline;
    procedure SetBooleanValue(const aValue: Variant); inline;
    procedure SetVariantValue(const aValue: Variant); virtual;

    procedure InitHashChains; override;
    procedure SetName(const aName: string);
    procedure SetValue(const aValue: Variant); virtual;

    function GetValueAsInteger: Integer;
    function GetValueAsInt64: Int64;
    function GetValueAsUInt64: UInt64;
    function GetValueAsDouble: Double;
    function GetValueAsDateTime: TDateTime;
    function GetValueAsBoolean: Boolean;
    function GetValueAsString: string;

    procedure SetValueAsInteger(const aValue: Integer);
    procedure SetValueAsInt64(const aValue: Int64);
    procedure SetValueAsUInt64(const aValue: UInt64);
    procedure SetValueAsDouble(const aValue: Double);
    procedure SetValueAsDateTime(const aValue: TDateTime);
    procedure SetValueAsBoolean(const aValue: Boolean);
    procedure SetValueAsString(const aValue: string);
  public
    constructor Create(const aName: string); overload;
    constructor Create(const aName: string; const aValue: Variant); overload;

    function GetNext: TNameVariantItem; inline;
    function GetPrev: TNameVariantItem; inline;

    property Name: string read fName write SetName;
    property Value: Variant read fVariantData.VariantValue write SetValue;
    property AsInteger: Integer read GetValueAsInteger write SetValueAsInteger;
    property AsInt64: Int64 read GetValueAsInt64 write SetValueAsInt64;
    property AsUInt64: UInt64 read GetValueAsUInt64 write SetValueAsUInt64;
    property AsDOuble: Double read GetValueAsDouble write SetValueAsDouble;
    property AsDateTime: TDateTime read GetValueAsDateTime write SetValueAsDateTime;
    property AsBoolean: Boolean read GetValueAsBoolean write SetValueAsBoolean;
    property AsString: string read GetValueAsString write SetValueAsString;
    property ValueType: TVariantItemType read fValueType;
  end;

  // Hash table for Name(string)-Value(Variant) pair items
  TNameVariantItems = class(THashBaseItems)
  protected
    fNamesStringHashTable: TStringHashTable;
    fStringValuesHashTable: TStringHashTable;
    fIntegerValuesHashTable: TIntegerHashTable;
    fInt64ValuesHashTable: TInt64HashTable;
    fDoubleValuesHashTable: TDoubleHashTable;
    fBooleanValueHashTable: TByteHashTable;
    fAllowDiplicates: Boolean;

    procedure InitHashTables(aHashSize: Cardinal); override;
    procedure AddToHashTable(aHashItem: THashBaseItem); override;
    procedure RemoveFromHashTable(aHashItem: THashBaseItem); override;
    procedure SetHashSize(aHashSize: Cardinal); override;
    procedure DestroyHashTables; override;

    function GetItemValue(const aName: string): Variant;
    procedure SetItemValue(const aName: string; const aValue: Variant);
  public
    constructor Create(aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aAllowDuplicates: Boolean = True; aFreeItemsOnDestroy: Boolean = True);

    procedure Add(aItem: TNameVariantItem); inline;
    procedure Delete(aItem: TNameVariantItem; aFreeItem: Boolean = True); inline;

    function FindFirstByName(const aSearchName: string): TNameVariantItem; inline;
    function FindNextByName(aItem: TNameVariantItem): TNameVariantItem; inline;
    function FindFirstByStringValue(const aSearchValue: string): TNameVariantItem; inline;
    function FindNextByStringValue(aItem: TNameVariantItem): TNameVariantItem; inline;
    function FindFirstByIntegerValue(const aSearchValue: Integer): TNameVariantItem; inline;
    function FindNextByIntegerValue(aItem: TNameVariantItem): TNameVariantItem; inline;
    function FindFirstByInt64Value(const aSearchValue: Int64): TNameVariantItem; inline;
    function FindNextByInt64Value(aItem: TNameVariantItem): TNameVariantItem; inline;
    function FindFirstByDoubleValue(const aSearchValue: Double): TNameVariantItem; inline;
    function FindNextByDoubleValue(aItem: TNameVariantItem): TNameVariantItem; inline;
    function FindFirstByBooleanValue(const aSearchValue: Boolean): TNameVariantItem; inline;
    function FindNextByBooleanValue(aItem: TNameVariantItem): TNameVariantItem; inline;

    function GetFirst: TNameVariantItem;
    function GetLast: TNameVariantItem;

    property Values[const Name: string]: Variant read GetItemValue write SetItemValue;
  end;

implementation

{ }

procedure QuickSortObjectArray(aObjectArray: PObjectArray; aLeftIndex, aRightIndex: Integer; aCompareFunc: TObjectCompareFunction);
var
  lLL, lRR: Integer;
  lLLComp, lRRComp: Integer;
  mItem, cItem: TObject;
begin
  repeat
    lLL := aLeftIndex;
    lRR := aRightIndex;
    mItem := aObjectArray^[(aLeftIndex + aRightIndex) shr 1];
    repeat
      repeat
        lLLComp := aCompareFunc(aObjectArray^[lLL], mItem);
        if lLLComp < 0 then Inc(lLL);
      until lLLComp >= 0;
      repeat
        lRRComp := aCompareFunc(aObjectArray^[lRR], mItem);
        if lRRComp > 0 then Dec(lRR);
      until lRRComp <= 0;
      if lLL <= lRR then
      begin
        if (lLLComp > 0) or (lRRComp < 0) then
        begin
          cItem := aObjectArray^[lLL];
          aObjectArray^[lLL] := aObjectArray^[lRR];
          aObjectArray^[lRR] := cItem;
        end;
        Inc(lLL);
        Dec(lRR);
      end;
    until lLL > lRR;
    if aLeftIndex < lRR then QuickSortObjectArray(aObjectArray, aLeftIndex, lRR, aCompareFunc);
    aLeftIndex := lLL;
  until aLeftIndex >= aRightIndex;
end;

function VarIsSame(const aVariant1, aVariant2: Variant): Boolean;
var
  lVariant1Data, lVariant2Data: TVarData;
begin
  lVariant1Data := FindVarData(aVariant1)^;
  lVariant2Data := FindVarData(aVariant2)^;

  if lVariant1Data.VType = varEmpty then
    Result := lVariant2Data.VType = varEmpty
  else if lVariant1Data.VType = varNull then
    Result := lVariant2Data.VType = varNull
  else if lVariant2Data.VType in [varEmpty, varNull] then
    Result := False
  else if lVariant1Data.VType = lVariant2Data.VType then
    Result := aVariant1 = aVariant2
  else
    Result := False;
end;

{ TBaseHashTable }

{$POINTERMATH ON}
class function TBaseHashTable.HashLittleBobJenkinsUnsigned(const Data; Len, InitVal: Cardinal): Cardinal;
  function Rot(x, k: Cardinal): Cardinal; inline;
  begin
    Result := (x shl k) or (x shr (32 - k));
  end;

  procedure Mix(var a, b, c: Cardinal); inline;
  begin
    Dec(a, c); a := a xor Rot(c, 4); Inc(c, b);
    Dec(b, a); b := b xor Rot(a, 6); Inc(a, c);
    Dec(c, b); c := c xor Rot(b, 8); Inc(b, a);
    Dec(a, c); a := a xor Rot(c,16); Inc(c, b);
    Dec(b, a); b := b xor Rot(a,19); Inc(a, c);
    Dec(c, b); c := c xor Rot(b, 4); Inc(b, a);
  end;

  procedure Final(var a, b, c: Cardinal); inline;
  begin
    c := c xor b; Dec(c, Rot(b,14));
    a := a xor c; Dec(a, Rot(c,11));
    b := b xor a; Dec(b, Rot(a,25));
    c := c xor b; Dec(c, Rot(b,16));
    a := a xor c; Dec(a, Rot(c, 4));
    b := b xor a; Dec(b, Rot(a,14));
    c := c xor b; Dec(c, Rot(b,24));
  end;

var
  pb: PByte;
  pd: PCardinal absolute pb;
  a, b, c: Cardinal;
label
  case_1, case_2, case_3, case_4, case_5, case_6,
  case_7, case_8, case_9, case_10, case_11, case_12;
begin
  a := Cardinal($DEADBEEF) + Cardinal(Len) + InitVal;
  b := a;
  c := a;

  pb := @Data;

  // 4-byte aligned data
  if (Cardinal(pb) and 3) = 0 then
  begin
    while Len > 12 do
    begin
      Inc(a, pd[0]);
      Inc(b, pd[1]);
      Inc(c, pd[2]);
      Mix(a, b, c);
      Dec(Len, 12);
      Inc(pd, 3);
    end;

    case Len of
      0: Exit(c);
      1: Inc(a, pd[0] and $FF);
      2: Inc(a, pd[0] and $FFFF);
      3: Inc(a, pd[0] and $FFFFFF);
      4: Inc(a, pd[0]);
      5:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1] and $FF);
      end;
      6:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1] and $FFFF);
      end;
      7:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1] and $FFFFFF);
      end;
      8:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
      end;
      9:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2] and $FF);
      end;
      10:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2] and $FFFF);
      end;
      11:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2] and $FFFFFF);
      end;
      12:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2]);
      end;
    end;
  end
  else
  begin
    // Ignoring rare case of 2-byte aligned data. This handles all other cases.
    while Len > 12 do
    begin
      Inc(a, pb[0] + pb[1] shl 8 + pb[2] shl 16 + pb[3] shl 24);
      Inc(b, pb[4] + pb[5] shl 8 + pb[6] shl 16 + pb[7] shl 24);
      Inc(c, pb[8] + pb[9] shl 8 + pb[10] shl 16 + pb[11] shl 24);
      Mix(a, b, c);
      Dec(Len, 12);
      Inc(pb, 12);
    end;

    case Len of
      0: Exit(c);
      1: goto case_1;
      2: goto case_2;
      3: goto case_3;
      4: goto case_4;
      5: goto case_5;
      6: goto case_6;
      7: goto case_7;
      8: goto case_8;
      9: goto case_9;
      10: goto case_10;
      11: goto case_11;
      12: goto case_12;
    end;

case_12:
    Inc(c, pb[11] shl 24);
case_11:
    Inc(c, pb[10] shl 16);
case_10:
    Inc(c, pb[9] shl 8);
case_9:
    Inc(c, pb[8]);
case_8:
    Inc(b, pb[7] shl 24);
case_7:
    Inc(b, pb[6] shl 16);
case_6:
    Inc(b, pb[5] shl 8);
case_5:
    Inc(b, pb[4]);
case_4:
    Inc(a, pb[3] shl 24);
case_3:
    Inc(a, pb[2] shl 16);
case_2:
    Inc(a, pb[1] shl 8);
case_1:
    Inc(a, pb[0]);
  end;

  Final(a, b, c);
  Result := c;
end;
{$POINTERMATH OFF}

// Extended syntax off to check if referencing pointers was correct
{$X-}

{$IFNDEF STATIC_CHAINS}
procedure TBaseHashTable.ClearDynamicChains;
var
  i: Integer;
  lCurrentChain, lChainForDispose: PHashChain;
begin
  for i := 0 to PointerTableSize - 1 do
  begin
    lCurrentChain := PointerTable^[i];
    while lCurrentChain <> nil do
    begin
      lChainForDispose := lCurrentChain;
      lCurrentChain := lCurrentChain^.NextChainLink;
      Dispose(lChainForDispose);
      Dec(PointerTableElementCount);
    end;
  end;
end;
{$ENDIF}

procedure TBaseHashTable.Init;
begin
  PointerTable := nil;
  PointerTableSize := 0;
  PointerTableElementCount := 0;
end;

procedure TBaseHashTable.SetTableSize(aNewTableSize: Cardinal);
{$IFDEF STATIC_CHAINS}
begin
  if aNewTableSize = PointerTableSize then Exit;
  FreeMem(PointerTable);
  PointerTableSize := aNewTableSize;
  PointerTable := AllocMem(PointerTableSize * SizeOf(Pointer));
  PointerTableElementCount := 0;
{$ELSE}
var
  lTempChainArray: array of PHashChain;
  i: UInt64;
  lCurrentTempArrayIndex: UInt64;
  lCurrentChain: PHashChain;
  lHashChainItem: PPHashChain;
begin
  if aNewTableSize = PointerTableSize then Exit;
  if PointerTableSize = 0 then
    PointerTable := AllocMem(aNewTableSize * SizeOf(Pointer))
  else if aNewTableSize <> 0 then
  begin
    // Saving for rehash
    SetLength(lTempChainArray, PointerTableElementCount);
    lCurrentTempArrayIndex := 0;
    for i := 0 to PointerTableSize - 1 do
    begin
      lCurrentChain := PointerTable^[i];
      while lCurrentChain <> nil do
      begin
        Assert(lCurrentTempArrayIndex < PointerTableElementCount, 'SetTableSize rehash index out of bounds, lCurrentTempArrayIndex=' + IntToStr(lCurrentTempArrayIndex) + ', ArrLen=' + IntToStr(Length(lTempChainArray)));
        lTempChainArray[lCurrentTempArrayIndex] := lCurrentChain;
        Inc(lCurrentTempArrayIndex);
        lCurrentChain := lCurrentChain^.NextChainLink;
      end;
    end;
    PointerTableSize := aNewTableSize;
    FreeMem(PointerTable);
    PointerTable := AllocMem(aNewTableSize * SizeOf(Pointer));
    // Adding again to rehash
    for i := Low(lTempChainArray) to High(lTempChainArray) do
    begin
      lCurrentChain := lTempChainArray[i];
      lCurrentChain^.NextChainLink := nil;
      lCurrentChain^.PrevChainLink := nil;
      lHashChainItem := @PointerTable^[lCurrentChain^.HashValue mod PointerTableSize];
      if lHashChainItem^ = nil then
        lHashChainItem^ := lCurrentChain
      else
      begin
        while (lHashChainItem^)^.NextChainLink <> nil do
          lHashChainItem := @((lHashChainItem^)^.NextChainLink);
        (lHashChainItem^)^.NextChainLink := lCurrentChain;
        lCurrentChain^.PrevChainLink := lHashChainItem^;
      end;
    end;
    Exit;
  end
  else
  begin
    ClearDynamicChains;
    FreeMem(PointerTable);
  end;
  PointerTableSize := aNewTableSize;
{$ENDIF}
end;

procedure TBaseHashTable.AddToTable(aHashChain: PHashChain);
{$IFDEF STATIC_CHAINS}
var
  lHashChainItem: PPHashChain;
begin
  aHashChain^.NextChainLink := nil;
  aHashChain^.PrevChainLink := nil;
  lHashChainItem := @PointerTable^[aHashChain^.HashValue mod PointerTableSize];
  if lHashChainItem^ <> nil then
  begin
    aHashChain^.NextChainLink := lHashChainItem^;
    lHashChainItem^.PrevChainLink := aHashChain;
  end;
  lHashChainItem^ := aHashChain;
  Inc(PointerTableElementCount);
{$ELSE}
var
  lHashChainItem: PPHashChain;
  lNewHashChain: PHashChain;
begin
  New(lNewHashChain);
  lNewHashChain^.Item := aHashChain^.Item;
  lNewHashChain^.HashValue := aHashChain^.HashValue;
  lNewHashChain^.NextChainLink := nil;
  lNewHashChain^.PrevChainLink := nil;
  lHashChainItem := @PointerTable^[lNewHashChain^.HashValue mod PointerTableSize];
  if lHashChainItem^ = nil then
    lHashChainItem^ := lNewHashChain
  else
  begin
    while (lHashChainItem^)^.NextChainLink <> nil do
      lHashChainItem := @((lHashChainItem^)^.NextChainLink);
    (lHashChainItem^)^.NextChainLink := lNewHashChain;
    lNewHashChain^.PrevChainLink := lHashChainItem^;
  end;
  Inc(PointerTableElementCount);
{$ENDIF}
end;

procedure TBaseHashTable.RemoveFromTable(aHashChain: PHashChain);
{$IFDEF STATIC_CHAINS}
begin
  if aHashChain^.PrevChainLink = nil then
    PointerTable^[aHashChain^.HashValue mod PointerTableSize] := aHashChain^.NextChainLink
  else
    aHashChain^.PrevChainLink^.NextChainLink := aHashChain^.NextChainLink;
  if aHashChain^.NextChainLink <> nil then
    aHashChain^.NextChainLink^.PrevChainLink := aHashChain^.PrevChainLink;
  Dec(PointerTableElementCount);
{$ELSE}
var
  lCurrentChain: PHashChain;
begin
  lCurrentChain := PointerTable^[aHashChain^.HashValue mod PointerTableSize];
  while lCurrentChain <> nil do
  begin
    if aHashChain^.Item = lCurrentChain^.Item then
    begin
      if lCurrentChain^.PrevChainLink = nil then
        PointerTable^[aHashChain^.HashValue mod PointerTableSize] := lCurrentChain^.NextChainLink
      else
        lCurrentChain^.PrevChainLink^.NextChainLink := lCurrentChain^.NextChainLink;
      if lCurrentChain^.NextChainLink <> nil then
        lCurrentChain^.NextChainLink^.PrevChainLink := lCurrentChain^.PrevChainLink;
      Dec(PointerTableElementCount);
      Dispose(lCurrentChain);
      Exit;
    end;
    lCurrentChain := lCurrentChain^.NextChainLink;
  end;
{$ENDIF}
end;

procedure TBaseHashTable.Destroy;
begin
  {$IFNDEF STATIC_CHAINS}
  ClearDynamicChains;
  {$ENDIF}
  FreeMem(PointerTable);
  PointerTable := nil;
  PointerTableSize := 0;
  PointerTableElementCount := 0;
end;
{$X+}

class function TBaseHashTable.GetHashValue(const AData; ALength: Cardinal; AInitialValue: Cardinal = 0): Cardinal;
begin
  Result := HashLittleBobJenkinsUnsigned(AData, ALength, AInitialValue);
end;

{ TStringHashTable }

procedure TStringHashTable.Init(aSize: Cardinal; aStringOffset: Integer);
begin
  HashTable.Init;
  HashTable.SetTableSize(aSize);
  StringOffset := aStringOffset;
end;

procedure TStringHashTable.SetTableSize(aNewTableSize: Cardinal);
begin
  HashTable.SetTableSize(aNewTableSize);
end;

procedure TStringHashTable.AddToTable(aHashChain: PHashChain);
begin
  HashTable.AddToTable(aHashChain);
end;

procedure TStringHashTable.RemoveFromTable(aHashChain: PHashChain);
begin
  HashTable.RemoveFromTable(aHashChain);
end;

procedure TStringHashTable.Destroy;
begin
  HashTable.Destroy;
end;

class function TStringHashTable.GetStringHashValue(const aValue: string): Cardinal;
begin
  Result := TBaseHashTable.GetHashValue(aValue[1], Length(aValue) * SizeOf(Char));
end;

function TStringHashTable.FindFirst(const aSearchValue: string): TAbstractListItem;
var
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchHashValue := TStringHashTable.GetStringHashValue(aSearchValue);
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and SameStr(aSearchValue, PString(@(PByteArray(lHashChain^.Item)^[StringOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

function TStringHashTable.FindNext(aHashChain: PHashChain): TAbstractListItem;
var
  lSearchValue: string;
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchValue := PString(@(PByteArray(aHashChain^.Item)^[StringOffset]))^;
  lSearchHashValue := aHashChain^.HashValue;
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if aHashChain^.Item = lHashChain^.Item then
      Break;
    lHashChain := lHashChain^.NextChainLink;
  end;
  if lHashChain = nil then Exit(nil);
  lHashChain := lHashChain^.NextChainLink;
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and SameStr(lSearchValue, PString(@(PByteArray(lHashChain^.Item)^[StringOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

{ TIntegerHashTable }

procedure TIntegerHashTable.Init(aSize: Cardinal; aIntegerOffset: Integer);
begin
  HashTable.Init;
  HashTable.SetTableSize(aSize);
  IntegerOffset := aIntegerOffset;
end;

procedure TIntegerHashTable.SetTableSize(aNewTableSize: Cardinal);
begin
  HashTable.SetTableSize(aNewTableSize);
end;

procedure TIntegerHashTable.AddToTable(aHashChain: PHashChain);
begin
  HashTable.AddToTable(aHashChain);
end;

procedure TIntegerHashTable.RemoveFromTable(aHashChain: PHashChain);
begin
  HashTable.RemoveFromTable(aHashChain);
end;

procedure TIntegerHashTable.Destroy;
begin
  HashTable.Destroy;
end;

class function TIntegerHashTable.GetIntegerHashValue(const aValue: Integer): Cardinal;
begin
  Result := TBaseHashTable.GetHashValue(aValue, SizeOf(aValue));
end;

function TIntegerHashTable.FindFirst(const aSearchValue: Integer): TAbstractListItem;
var
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchHashValue := TIntegerHashTable.GetIntegerHashValue(aSearchValue);
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (aSearchValue = PInteger(@(PByteArray(lHashChain^.Item)^[IntegerOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

function TIntegerHashTable.FindNext(aHashChain: PHashChain): TAbstractListItem;
var
  lSearchValue: Integer;
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchValue := PInteger(@(PByteArray(aHashChain^.Item)^[IntegerOffset]))^;
  lSearchHashValue := aHashChain^.HashValue;
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if aHashChain^.Item = lHashChain^.Item then
      Break;
    lHashChain := lHashChain^.NextChainLink;
  end;
  if lHashChain = nil then Exit(nil);
  lHashChain := lHashChain^.NextChainLink;
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (lSearchValue = PInteger(@(PByteArray(lHashChain^.Item)^[IntegerOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

{ TInt64HashTable }

procedure TInt64HashTable.Init(aSize: Cardinal; aInt64Offset: Integer);
begin
  HashTable.Init;
  HashTable.SetTableSize(aSize);
  Int64Offset := aInt64Offset;
end;

procedure TInt64HashTable.SetTableSize(aNewTableSize: Cardinal);
begin
  HashTable.SetTableSize(aNewTableSize);
end;

procedure TInt64HashTable.AddToTable(aHashChain: PHashChain);
begin
  HashTable.AddToTable(aHashChain);
end;

procedure TInt64HashTable.RemoveFromTable(aHashChain: PHashChain);
begin
  HashTable.RemoveFromTable(aHashChain);
end;

procedure TInt64HashTable.Destroy;
begin
  HashTable.Destroy;
end;

class function TInt64HashTable.GetInt64HashValue(const aValue: Int64): Cardinal;
begin
  Result := TBaseHashTable.GetHashValue(aValue, SizeOf(aValue));
end;

function TInt64HashTable.FindFirst(const aSearchValue: Int64): TAbstractListItem;
var
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchHashValue := TIntegerHashTable.GetIntegerHashValue(aSearchValue);
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (aSearchValue = PInt64(@(PByteArray(lHashChain^.Item)^[Int64Offset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

function TInt64HashTable.FindNext(aHashChain: PHashChain): TAbstractListItem;
var
  lSearchValue: Int64;
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchValue := PInt64(@(PByteArray(aHashChain^.Item)^[Int64Offset]))^;
  lSearchHashValue := aHashChain^.HashValue;
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if aHashChain^.Item = lHashChain^.Item then
      Break;
    lHashChain := lHashChain^.NextChainLink;
  end;
  if lHashChain = nil then Exit(nil);
  lHashChain := lHashChain^.NextChainLink;
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (lSearchValue = PInt64(@(PByteArray(lHashChain^.Item)^[Int64Offset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

{ TDoubleHashTable }

procedure TDoubleHashTable.Init(aSize: Cardinal; aDoubleOffset: Integer);
begin
  HashTable.Init;
  HashTable.SetTableSize(aSize);
  DoubleOffset := aDoubleOffset;
end;

procedure TDoubleHashTable.SetTableSize(aNewTableSize: Cardinal);
begin
  HashTable.SetTableSize(aNewTableSize);
end;

procedure TDoubleHashTable.AddToTable(aHashChain: PHashChain);
begin
  HashTable.AddToTable(aHashChain);
end;

procedure TDoubleHashTable.RemoveFromTable(aHashChain: PHashChain);
begin
  HashTable.RemoveFromTable(aHashChain);
end;

procedure TDoubleHashTable.Destroy;
begin
  HashTable.Destroy;
end;

class function TDoubleHashTable.GetDoubleHashValue(const aValue: Double): Cardinal;
begin
  Result := TBaseHashTable.GetHashValue(aValue, SizeOf(aValue));
end;

function TDoubleHashTable.FindFirst(const aSearchValue: Double): TAbstractListItem;
var
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchHashValue := TDoubleHashTable.GetDoubleHashValue(aSearchValue);
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (aSearchValue = PDouble(@(PByteArray(lHashChain^.Item)^[DoubleOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

function TDoubleHashTable.FindNext(aHashChain: PHashChain): TAbstractListItem;
var
  lSearchValue: Double;
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchValue := PDouble(@(PByteArray(aHashChain^.Item)^[DoubleOffset]))^;
  lSearchHashValue := aHashChain^.HashValue;
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if aHashChain^.Item = lHashChain^.Item then
      Break;
    lHashChain := lHashChain^.NextChainLink;
  end;
  if lHashChain = nil then Exit(nil);
  lHashChain := lHashChain^.NextChainLink;
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (lSearchValue = PDouble(@(PByteArray(lHashChain^.Item)^[DoubleOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

{ TByteHashTable }

procedure TByteHashTable.Init(aSize: Cardinal; aByteOffset: Integer);
begin
  HashTable.Init;
  HashTable.SetTableSize(aSize);
  ByteOffset := aByteOffset;
end;

procedure TByteHashTable.SetTableSize(aNewTableSize: Cardinal);
begin
  HashTable.SetTableSize(aNewTableSize);
end;

procedure TByteHashTable.AddToTable(aHashChain: PHashChain);
begin
  HashTable.AddToTable(aHashChain);
end;

procedure TByteHashTable.RemoveFromTable(aHashChain: PHashChain);
begin
  HashTable.RemoveFromTable(aHashChain);
end;

procedure TByteHashTable.Destroy;
begin
  HashTable.Destroy;
end;

class function TByteHashTable.GetByteHashValue(const aValue: Byte): Cardinal;
begin
  Result := TBaseHashTable.GetHashValue(aValue, SizeOf(aValue));
end;

class function TByteHashTable.GetBooleanHashValue(const aValue: Boolean): Cardinal;
begin
  if aValue then
    Result := 1
  else
    Result := 0;
end;

function TByteHashTable.FindFirst(const aSearchValue: Byte): TAbstractListItem;
var
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchHashValue := TByteHashTable.GetByteHashValue(aSearchValue);
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (aSearchValue = PByte(@(PByteArray(lHashChain^.Item)^[ByteOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

function TByteHashTable.FindNext(aHashChain: PHashChain): TAbstractListItem;
var
  lSearchValue: Byte;
  lSearchHashValue: Cardinal;
  lHashChain: PHashChain;
begin
  lSearchValue := PByte(@(PByteArray(aHashChain^.Item)^[ByteOffset]))^;
  lSearchHashValue := aHashChain^.HashValue;
  lHashChain := HashTable.PointerTable^[lSearchHashValue mod HashTable.PointerTableSize];
  while lHashChain <> nil do
  begin
    if aHashChain^.Item = lHashChain^.Item then
      Break;
    lHashChain := lHashChain^.NextChainLink;
  end;
  if lHashChain = nil then Exit(nil);
  lHashChain := lHashChain^.NextChainLink;
  while lHashChain <> nil do
  begin
    if (lHashChain^.HashValue = lSearchHashValue)
        and (lSearchValue = PByte(@(PByteArray(lHashChain^.Item)^[ByteOffset]))^)
    then
      Exit(lHashChain^.Item);
    lHashChain := lHashChain^.NextChainLink;
  end;
  Result := nil;
end;

{ THashBaseItem }

procedure THashBaseItem.AfterConstruction;
begin
  inherited AfterConstruction;
  InitHashChains;
end;

{ THashBaseItems }

constructor THashBaseItems.Create(aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aFreeItemsOnDestroy: Boolean = True);
begin
  inherited Create(aFreeItemsOnDestroy);
  fHashSize := aHashSize;
  fAutoExtensionThreshold := (fHashSize div 4) * 3;
  fAutoExtendHash := aAutoExtendHash;
end;

procedure THashBaseItems.AfterConstruction;
begin
  inherited AfterConstruction;
  InitHashTables(fHashSize);
end;

procedure THashBaseItems.BeforeDestruction;
begin
  inherited BeforeDestruction; // Items are freed here in parent class
  DestroyHashTables;
end;

procedure THashBaseItems.CheckExtendAndRehash;
var
  lCurrent: TAbstractListItem;
begin
  // If hash table extension is enabled and we reached extension threshold, resize and rehash table
  if fAutoExtendHash and (Count >= fAutoExtensionThreshold) then
  begin
    fHashSize := (fHashSize shl 1) or 1;
    fAutoExtensionThreshold := (fHashSize div 4) * 3;
    // Rehashing is performed inside TBaseHashTable.SetTableSize if we don't user STATIC_CHAINS
    SetHashSize(fHashSize); // abstract method
    {$IFDEF STATIC_CHAINS}
    // If we use STATIC_CHAINS, we need to rehash manually
    lCurrent := fFirst;
    while lCurrent <> nil do
    begin
      AddToHashTable(THashBaseItem(lCurrent)); // abstract method
      lCurrent := lCurrent.Next;
    end;
    {$ENDIF}
  end;
end;

procedure THashBaseItems.InternalAddItem(aItem: THashBaseItem);
begin
  AddToList(aItem);
  AddToHashTable(aItem); // abstract method
  CheckExtendAndRehash;
end;

procedure THashBaseItems.InternalInsertItem(aItem, aBeforeItem: THashBaseItem);
begin
  InsertToList(aItem, aBeforeItem);
  AddToHashTable(aItem); // abstract method
  CheckExtendAndRehash;
end;

procedure THashBaseItems.InternalDeleteItem(aItem: THashBaseItem; aFreeItem: Boolean = True);
begin
  DeleteFromList(aItem, False);
  RemoveFromHashTable(aItem); // abstract method
  if aFreeItem then aItem.Free;
end;

procedure THashBaseItems.AddToList(aItem: TCustomDynamicListItem);
begin
  inherited AddToList(aItem);
end;

procedure THashBaseItems.InsertToList(aItem, aBeforeItem: TCustomDynamicListItem);
begin
  inherited InsertToList(aItem, aBeforeItem);
end;

procedure THashBaseItems.DeleteFromList(aItem: TCustomDynamicListItem; aFreeItem: Boolean = True);
begin
  inherited DeleteFromList(aItem, aFreeItem);
end;

function THashBaseItems.GetHashSize: Cardinal;
begin
  Result := fHashSize;
end;

procedure THashBaseItems.Sort(aCompareFunction: TObjectCompareFunction);
var
  lSavedCount: Int64;
  lSortBuffer: PObjectArray;
  lItem, lNextItem: TAbstractListItem;
  lTempPointer: ^TAbstractListItem;
  i: Integer;
begin
  if Count < 2 then Exit;
  if not Assigned(aCompareFunction) then
    raise EHashException.CreateFmt(EXCEPTION_MESSAGE_HASH_ITEMS_COMPARE_MISSING, [ClassName]);
  lSavedCount := Count;
  GetMem(lSortBuffer, lSavedCount * SizeOf(TAbstractListItem));
  try
    lTempPointer := Pointer(lSortBuffer);
    lItem := fFirst;
    for i := 1 to lSavedCount do
    begin
      lTempPointer^ := lItem;
      lNextItem := lItem.Next;
      InternalDeleteItem(THashBaseItem(lItem), False);
      lItem := lNextItem;
      Inc(lTempPointer);
    end;

    QuickSortObjectArray(lSortBuffer, 0, lSavedCount - 1, aCompareFunction);

    lTempPointer := Pointer(lSortBuffer);
    Inc(lTempPointer, lSavedCount - 1);
    for i := 1 to lSavedCount do
    begin
      // Using internal method instead of InternalAddItem to avoid checking for hash extension
      InternalInsertToList(lTempPointer^, fFirst);
      AddToHashTable(THashBaseItem(lTempPointer^)); // abstract method
      Dec(lTempPointer);
    end;
  finally
    FreeMem(lSortBuffer);
  end;
end;

procedure THashBaseItems.Clear;
var
  lItem, lTempItem: THashBaseItem;
begin
  lItem := THashBaseItem(fFirst);
  while lItem <> nil do
  begin
    lTempItem := THashBaseItem(lItem.fNext);
    RemoveFromHashTable(lItem); // abstract method
    lItem.Free; // In item's destructor it will remove itself from list
    lItem := lTempItem;
  end;
end;

{ TNameVariantItem }

constructor TNameVariantItem.Create(const aName: string);
begin
  inherited Create;
  SetName(aName);
  fValueType := vitVariant;
end;

constructor TNameVariantItem.Create(const aName: string; const aValue: Variant);
begin
  Create(aName);
  SetValue(aValue);
end;

procedure TNameVariantItem.RemoveFromPreviousOwnerHashTable;
begin
  if fOwner = nil then Exit;

  case fValueType of
    vitInteger: TNameVariantItems(fOwner).fIntegerValuesHashTable.RemoveFromTable(@fIntegerValueHashChain);
    vitInt64, vitUInt64: TNameVariantItems(fOwner).fInt64ValuesHashTable.RemoveFromTable(@fInt64ValueHashChain);
    vitDouble, vitDateTime: TNameVariantItems(fOwner).fDoubleValuesHashTable.RemoveFromTable(@fDoubleValueHashChain);
    vitBoolean: TNameVariantItems(fOwner).fBooleanValueHashTable.RemoveFromTable(@fBooleanValueHashChain);
    vitUnicodeString: TNameVariantItems(fOwner).fStringValuesHashTable.RemoveFromTable(@fStringValueHashChain);
  end;
end;

procedure TNameVariantItem.SetStringValue(const aValue: Variant);
begin
  fVariantData.VariantValue := VarAsType(aValue, varUString);
  RemoveFromPreviousOwnerHashTable;
  fValueType := vitUnicodeString;

  if fOwner = nil then
  begin
    fVariantData.VariantStringValue := fVariantData.VariantValue;
    fStringValueHashChain.HashValue := TStringHashTable.GetStringHashValue(fVariantData.VariantStringValue);
    Exit;
  end;

  TNameVariantItems(fOwner).fStringValuesHashTable.RemoveFromTable(@fStringValueHashChain);
  fVariantData.VariantStringValue := fVariantData.VariantValue;
  fStringValueHashChain.HashValue := TStringHashTable.GetStringHashValue(fVariantData.VariantStringValue);
  TNameVariantItems(fOwner).fStringValuesHashTable.AddToTable(@fStringValueHashChain);
end;

procedure TNameVariantItem.SetIntegerValue(const aValue: Variant);
begin
  fVariantData.VariantValue := VarAsType(aValue, varInteger);
  RemoveFromPreviousOwnerHashTable;
  fValueType := vitInteger;

  if fOwner = nil then
  begin
    fVariantData.fVDInteger := fVariantData.VariantValue;
    fIntegerValueHashChain.HashValue := TIntegerHashTable.GetIntegerHashValue(fVariantData.fVDInteger);
    Exit;
  end;

  TNameVariantItems(fOwner).fIntegerValuesHashTable.RemoveFromTable(@fIntegerValueHashChain);
  fVariantData.fVDInteger := fVariantData.VariantValue;
  fIntegerValueHashChain.HashValue := TIntegerHashTable.GetIntegerHashValue(fVariantData.fVDInteger);
  TNameVariantItems(fOwner).fIntegerValuesHashTable.AddToTable(@fIntegerValueHashChain);
end;

procedure TNameVariantItem.SetInt64Value(const aValue: Variant; aType: TVarType);
begin
  fVariantData.VariantValue := VarAsType(aValue, varInt64);
  RemoveFromPreviousOwnerHashTable;
  if aType = varInt64 then
    fValueType := vitInt64
  else
    fValueType := vitUInt64;

  if fOwner = nil then
  begin
    fVariantData.fVDInt64 := fVariantData.VariantValue;
    fInt64ValueHashChain.HashValue := TInt64HashTable.GetInt64HashValue(fVariantData.fVDInt64);
    Exit;
  end;

  TNameVariantItems(fOwner).fInt64ValuesHashTable.RemoveFromTable(@fInt64ValueHashChain);
  fVariantData.fVDInt64 := fVariantData.VariantValue;
  fInt64ValueHashChain.HashValue := TInt64HashTable.GetInt64HashValue(fVariantData.fVDInt64);
  TNameVariantItems(fOwner).fInt64ValuesHashTable.AddToTable(@fInt64ValueHashChain);
end;

procedure TNameVariantItem.SetDoubleValue(const aValue: Variant; aType: TVarType);
begin
  fVariantData.VariantValue := VarAsType(aValue, varDouble);
  RemoveFromPreviousOwnerHashTable;
  if aType = varDate then
    fValueType := vitDateTime
  else
    fValueType := vitDouble;

  if fOwner = nil then
  begin
    fVariantData.fVDDouble := fVariantData.VariantValue;
    fDoubleValueHashChain.HashValue := TDoubleHashTable.GetDoubleHashValue(fVariantData.fVDDouble);
    Exit;
  end;

  TNameVariantItems(fOwner).fDoubleValuesHashTable.RemoveFromTable(@fDoubleValueHashChain);
  fVariantData.fVDDouble := fVariantData.VariantValue;
  fDoubleValueHashChain.HashValue := TDoubleHashTable.GetDoubleHashValue(fVariantData.fVDDouble);
  TNameVariantItems(fOwner).fDoubleValuesHashTable.AddToTable(@fDoubleValueHashChain);
end;

procedure TNameVariantItem.SetBooleanValue(const aValue: Variant);
begin
  fVariantData.VariantValue := VarAsType(aValue, varBoolean);
  RemoveFromPreviousOwnerHashTable;
  fValueType := vitBoolean;

  if fOwner = nil then
  begin
    fVariantData.fVDBoolean := fVariantData.VariantValue;
    fBooleanValueHashChain.HashValue := TByteHashTable.GetBooleanHashValue(fVariantData.fVDBoolean);
    Exit;
  end;

  TNameVariantItems(fOwner).fBooleanValueHashTable.RemoveFromTable(@fBooleanValueHashChain);
  fVariantData.fVDBoolean := fVariantData.VariantValue;
  fBooleanValueHashChain.HashValue := TByteHashTable.GetBooleanHashValue(fVariantData.fVDBoolean);
  TNameVariantItems(fOwner).fBooleanValueHashTable.AddToTable(@fBooleanValueHashChain);
end;

procedure TNameVariantItem.SetVariantValue(const aValue: Variant);
begin
  fVariantData.VariantValue := aValue;
  RemoveFromPreviousOwnerHashTable;
  fValueType := vitVariant;
end;

procedure TNameVariantItem.InitHashChains;
begin
  fNameHashChain.Item := Self;
  fStringValueHashChain.Item := Self;
  fIntegerValueHashChain.Item := Self;
  fInt64ValueHashChain.Item := Self;
  fDoubleValueHashChain.Item := Self;
  fBooleanValueHashChain.Item := Self;
end;

procedure TNameVariantItem.SetName(const aName: string);
begin
  if SameStr(aName, fName) then Exit;

  if fOwner = nil then
  begin
    fName := aName;
    fNameHashChain.HashValue := TStringHashTable.GetStringHashValue(fName);
    Exit;
  end;

  TNameVariantItems(fOwner).fNamesStringHashTable.RemoveFromTable(@fNameHashChain);
  fName := aName;
  fNameHashChain.HashValue := TStringHashTable.GetStringHashValue(fName);
  TNameVariantItems(fOwner).fNamesStringHashTable.AddToTable(@fNameHashChain);
end;

procedure TNameVariantItem.SetValue(const aValue: Variant);
var
  lVariantType: TVarType;
begin
  if VarIsSame(fVariantData.VariantValue, aValue) then Exit;

  lVariantType := VarType(aValue);
  case lVariantType of
    varSmallint, varInteger, varShortInt, varByte, varWord, varUInt32: SetIntegerValue(aValue);
    varInt64, varUInt64: SetInt64Value(aValue, lVariantType);
    varSingle, varDouble, varCurrency, varDate: SetDoubleValue(aValue, lVariantType);
    varBoolean: SetBooleanValue(aValue);
    varString, varUString: SetStringValue(aValue);
  else
    SetVariantValue(aValue);
  end;
end;

function TNameVariantItem.GetValueAsInteger: Integer;
begin
  if fValueType = vitInteger then
    Result := fVariantData.fVDInteger
  else
    Result := VarAsType(fVariantData.VariantValue, varInteger);
end;

function TNameVariantItem.GetValueAsInt64: Int64;
begin
  if fValueType = vitInt64 then
    Result := fVariantData.fVDInt64
  else
    Result := VarAsType(fVariantData.VariantValue, varInt64);
end;

function TNameVariantItem.GetValueAsUInt64: UInt64;
begin
  if fValueType = vitUInt64 then
    Result := UInt64(fVariantData.fVDInt64)
  else
    Result := VarAsType(fVariantData.VariantValue, varUInt64);
end;

function TNameVariantItem.GetValueAsDouble: Double;
begin
  if fValueType = vitDouble then
    Result := fVariantData.fVDDouble
  else
    Result := VarAsType(fVariantData.VariantValue, varDouble);
end;

function TNameVariantItem.GetValueAsDateTime: TDateTime;
begin
  if fValueType = vitDateTime then
    Result := TDateTime(fVariantData.fVDDouble)
  else
    Result := VarAsType(fVariantData.VariantValue, varDate);
end;

function TNameVariantItem.GetValueAsBoolean: Boolean;
begin
  if fValueType = vitBoolean then
    Result := fVariantData.fVDBoolean
  else
    Result := VarAsType(fVariantData.VariantValue, varBoolean);
end;

function TNameVariantItem.GetValueAsString: string;
begin
  if fValueType = vitUnicodeString then
    Result := fVariantData.VariantStringValue
  else
    Result := VarAsType(fVariantData.VariantValue, varUString);
end;

procedure TNameVariantItem.SetValueAsInteger(const aValue: Integer);
begin
  SetIntegerValue(aValue);
end;

procedure TNameVariantItem.SetValueAsInt64(const aValue: Int64);
begin
  SetInt64Value(aValue, varInt64);
end;

procedure TNameVariantItem.SetValueAsUInt64(const aValue: UInt64);
begin
  SetInt64Value(aValue, varUInt64);
end;

procedure TNameVariantItem.SetValueAsDouble(const aValue: Double);
begin
  SetDoubleValue(aValue, varDouble);
end;

procedure TNameVariantItem.SetValueAsDateTime(const aValue: TDateTime);
begin
  SetDoubleValue(aValue, varDate);
end;

procedure TNameVariantItem.SetValueAsBoolean(const aValue: Boolean);
begin
  SetBooleanValue(aValue);
end;

procedure TNameVariantItem.SetValueAsString(const aValue: string);
begin
  SetStringValue(aValue);
end;

function TNameVariantItem.GetNext: TNameVariantItem;
begin
  Result := TNameVariantItem(fNext);
end;

function TNameVariantItem.GetPrev: TNameVariantItem;
begin
  Result := TNameVariantItem(fPrev);
end;

{ TNameVariantItems }

constructor TNameVariantItems.Create(aHashSize: Cardinal; aAutoExtendHash: Boolean = True; aAllowDuplicates: Boolean = True; aFreeItemsOnDestroy: Boolean = True);
begin
  inherited Create(aHashSize, aAutoExtendHash, aFreeItemsOnDestroy);
  fAllowDiplicates := aAllowDuplicates;
end;

procedure TNameVariantItems.InitHashTables(aHashSize: Cardinal);
begin
  fNamesStringHashTable.Init(aHashSize, Integer(@TNameVariantItem(0).fName));
  fStringValuesHashTable.Init(aHashSize, Integer(@TNameVariantItem(0).fVariantData.VariantStringValue));
  fIntegerValuesHashTable.Init(aHashSize, Integer(@TNameVariantItem(0).fVariantData.fVDInteger));
  fInt64ValuesHashTable.Init(aHashSize, Integer(@TNameVariantItem(0).fVariantData.fVDInt64));
  fDoubleValuesHashTable.Init(aHashSize, Integer(@TNameVariantItem(0).fVariantData.fVDDouble));
  fBooleanValueHashTable.Init(aHashSize, Integer(@TNameVariantItem(0).fVariantData.fVDBoolean));
end;

procedure TNameVariantItems.AddToHashTable(aHashItem: THashBaseItem);
begin
  fNamesStringHashTable.AddToTable(@TNameVariantItem(aHashItem).fNameHashChain);
  fStringValuesHashTable.AddToTable(@TNameVariantItem(aHashItem).fStringValueHashChain);
  fIntegerValuesHashTable.AddToTable(@TNameVariantItem(aHashItem).fIntegerValueHashChain);
  fInt64ValuesHashTable.AddToTable(@TNameVariantItem(aHashItem).fInt64ValueHashChain);
  fDoubleValuesHashTable.AddToTable(@TNameVariantItem(aHashItem).fDoubleValueHashChain);
  fBooleanValueHashTable.AddToTable(@TNameVariantItem(aHashItem).fBooleanValueHashChain);
end;

procedure TNameVariantItems.RemoveFromHashTable(aHashItem: THashBaseItem);
begin
  fNamesStringHashTable.RemoveFromTable(@TNameVariantItem(aHashItem).fNameHashChain);
  fStringValuesHashTable.RemoveFromTable(@TNameVariantItem(aHashItem).fStringValueHashChain);
  fIntegerValuesHashTable.RemoveFromTable(@TNameVariantItem(aHashItem).fIntegerValueHashChain);
  fInt64ValuesHashTable.RemoveFromTable(@TNameVariantItem(aHashItem).fInt64ValueHashChain);
  fDoubleValuesHashTable.RemoveFromTable(@TNameVariantItem(aHashItem).fDoubleValueHashChain);
  fBooleanValueHashTable.RemoveFromTable(@TNameVariantItem(aHashItem).fBooleanValueHashChain);
end;

procedure TNameVariantItems.SetHashSize(aHashSize: Cardinal);
begin
  fNamesStringHashTable.SetTableSize(aHashSize);
  fStringValuesHashTable.SetTableSize(aHashSize);
  fIntegerValuesHashTable.SetTableSize(aHashSize);
  fInt64ValuesHashTable.SetTableSize(aHashSize);
  fDoubleValuesHashTable.SetTableSize(aHashSize);
  fBooleanValueHashTable.SetTableSize(aHashSize);
end;

procedure TNameVariantItems.DestroyHashTables;
begin
  fNamesStringHashTable.Destroy;
  fStringValuesHashTable.Destroy;
  fIntegerValuesHashTable.Destroy;
  fInt64ValuesHashTable.Destroy;
  fDoubleValuesHashTable.Destroy;
  fBooleanValueHashTable.Destroy;
end;

function TNameVariantItems.GetItemValue(const aName: string): Variant;
var
  lItem: TNameVariantItem;
begin
  lItem := FindFirstByName(aName);
  if lItem = nil then
    raise EHashItemException.CreateFmt(EXCEPTION_MESSAGE_HASH_ITEMS_NOT_FOUND, aName, [aName]);
  Result := lItem.Value;
end;

procedure TNameVariantItems.SetItemValue(const aName: string; const aValue: Variant);
var
  lItem: TNameVariantItem;
begin
  lItem := FindFirstByName(aName);
  if lItem = nil then
    Add(TNameVariantItem.Create(aName, aValue))
  else
    lItem.Value := aValue;
end;

procedure TNameVariantItems.Add(aItem: TNameVariantItem);
begin
  if fAllowDiplicates then
    InternalAddItem(aItem)
  else
  begin
    if FindFirstByName(aItem.fName) = nil then
      InternalAddItem(aItem)
    else
      raise EHashItemException.CreateFmt(EXCEPTION_MESSAGE_HASH_ITEMS_ALREADY_EXISTS, aItem.fName, [aItem.fName]);
  end;
end;

procedure TNameVariantItems.Delete(aItem: TNameVariantItem; aFreeItem: Boolean = True);
begin
  InternalDeleteItem(aItem, aFreeItem);
end;

function TNameVariantItems.FindFirstByName(const aSearchName: string): TNameVariantItem;
begin
  Result := TNameVariantItem(fNamesStringHashTable.FindFirst(aSearchName));
end;

function TNameVariantItems.FindNextByName(aItem: TNameVariantItem): TNameVariantItem;
begin
  Result := TNameVariantItem(fNamesStringHashTable.FindNext(@aItem.fNameHashChain));
end;

function TNameVariantItems.FindFirstByStringValue(const aSearchValue: string): TNameVariantItem;
begin
  Result := TNameVariantItem(fStringValuesHashTable.FindFirst(aSearchValue));
end;

function TNameVariantItems.FindNextByStringValue(aItem: TNameVariantItem): TNameVariantItem;
begin
  Result := TNameVariantItem(fStringValuesHashTable.FindNext(@aItem.fStringValueHashChain));
end;

function TNameVariantItems.FindFirstByIntegerValue(const aSearchValue: Integer): TNameVariantItem;
begin
  Result := TNameVariantItem(fIntegerValuesHashTable.FindFirst(aSearchValue));
end;

function TNameVariantItems.FindNextByIntegerValue(aItem: TNameVariantItem): TNameVariantItem;
begin
  Result := TNameVariantItem(fIntegerValuesHashTable.FindNext(@aItem.fIntegerValueHashChain));
end;

function TNameVariantItems.FindFirstByInt64Value(const aSearchValue: Int64): TNameVariantItem;
begin
  Result := TNameVariantItem(fInt64ValuesHashTable.FindFirst(aSearchValue));
end;

function TNameVariantItems.FindNextByInt64Value(aItem: TNameVariantItem): TNameVariantItem;
begin
  Result := TNameVariantItem(fInt64ValuesHashTable.FindNext(@aItem.fInt64ValueHashChain));
end;

function TNameVariantItems.FindFirstByDoubleValue(const aSearchValue: Double): TNameVariantItem;
begin
  Result := TNameVariantItem(fDoubleValuesHashTable.FindFirst(aSearchValue));
end;

function TNameVariantItems.FindNextByDoubleValue(aItem: TNameVariantItem): TNameVariantItem;
begin
  Result := TNameVariantItem(fDoubleValuesHashTable.FindNext(@aItem.fDoubleValueHashChain));
end;

function TNameVariantItems.FindFirstByBooleanValue(const aSearchValue: Boolean): TNameVariantItem;
begin
  Result := TNameVariantItem(fBooleanValueHashTable.FindFirst(Byte(aSearchValue)));
end;

function TNameVariantItems.FindNextByBooleanValue(aItem: TNameVariantItem): TNameVariantItem;
begin
  Result := TNameVariantItem(fBooleanValueHashTable.FindNext(@aItem.fBooleanValueHashChain));
end;

function TNameVariantItems.GetFirst: TNameVariantItem;
begin
  Result := TNameVariantItem(fFirst);
end;

function TNameVariantItems.GetLast: TNameVariantItem;
begin
  Result := TNameVariantItem(fLast);
end;

end.
