unit uStream;

interface

uses
  System.Classes, System.SysUtils, System.Math, System.Character,
  cuConsts, cuClasses,
  uCustomExceptions;

type
  TEncodingData = record
    EncodingCodePage: Cardinal;
    EncodingName: string;
  end;

const
  CHAR_SIZE = SizeOf(Char);
  ANSI_CHAR_SIZE = SizeOf(AnsiChar);
  XML_CR: Char = Char($000D);
  XML_LF: Char = Char($000A);
  XML_EXCEPTION_NEAR_CHAR_COUNT = 20;

  XML_HEADER_TEMPLATE: string = '<?xml version="%s" encoding="%s" ?>';

  XML_HEADER_ENCODING_UTF7 = 'UTF-7';
  XML_HEADER_ENCODING_UTF8 = 'UTF-8';
  XML_HEADER_ENCODING_UTF16 = 'UTF-16';
  XML_HEADER_ENCODING_UTF16LE = 'UTF-16LE';
  XML_HEADER_ENCODING_UTF16BE = 'UTF-16BE';

  // Array index must start with 0
  XML_STANDARD_ENCODINGS_ARRAY: array[0..4] of TEncodingData = (
    (EncodingCodePage: CP_UTF7; EncodingName: XML_HEADER_ENCODING_UTF7),
    (EncodingCodePage: CP_UTF8; EncodingName: XML_HEADER_ENCODING_UTF8),
    (EncodingCodePage: 1200; EncodingName: XML_HEADER_ENCODING_UTF16),
    (EncodingCodePage: 1200; EncodingName: XML_HEADER_ENCODING_UTF16LE),
    (EncodingCodePage: 1201; EncodingName: XML_HEADER_ENCODING_UTF16BE)
  );

  XML_NODE_START_CHAR: Char = '<';
  XML_NODE_END_CHAR: Char = '>';
  XML_NODE_CLOSE_CHAR: Char = '/';
  XML_NODE_COMMENT_START_CHAR: Char = '!';
  XML_NODE_HEADER_START_END_CHAR: Char = '?';
  XML_NODE_DASH_CHAR: Char = '-';
  XML_APOSTROPHE_CHAR: Char = '''';
  XML_QUOTATION_CHAR: Char = '"';
  XML_EQUALS_CHAR: Char = '=';
  XML_SPACE_CHAR: Char = ' ';
  XML_TAB_CHAR: Char = #9;
  XML_ESCAPE_START_CHAR: Char = '&';
  XML_ESCAPE_END_CHAR: Char = ';';
  XML_ESCAPE_CODE_CHAR: Char = '#';
  XML_ESCAPE_HEX_CODE_CHAR: Char = 'x';
  XML_DEFAULT_NODE_PATH_SEPARATOR: Char = '.';

type
  TCustomMemStream = class(TStream)
  protected
    fMemory: Pointer;
    fSize: Integer;
    fPosition: Integer;
    fCapacity: Integer;
    procedure SetCapacity(const aCapacity: Integer);

    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;

    // Read aCount bytes to buffer with advancing current position
    procedure InternalReadBuffer(var Buffer; aCount: Integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    // Read aCount bytes to buffer without advancing current position
    procedure InternalGetBuffer(var Buffer; aCount: Integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    // Read aCount bytes to buffer with byte offset without advancing current position
    procedure InternalGetBufferWithOffset(var Buffer; aCount, aOffset: Integer); {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure RaiseReadOutOfBounds; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure RaiseWriteOutOfBounds; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;

    procedure LoadFromStream(const aStream: TStream); virtual;
    procedure LoadFromFile(const aFileName: string); virtual;

    function SaveToStream(aDestStream: TStream; aCount: Integer = 0): Integer; virtual;
    procedure SaveToFile(const aFileName: string); virtual;
    function CopyFromCustomMemStream(const aSourceStream: TCustomMemStream; aCount: Integer = 0): Integer; virtual;
  end;

  TXMLStream = class(TCustomMemStream)
  strict protected
    class var fStandardEncodingsArray: array of TEncodingData;

    class constructor Create;
    class destructor Destroy;
    class procedure AddStandardEncoding(const aCodePade: Cardinal; const aEncodingName: string); static;
    class function GetStandardEncodingCodePageByName(const aEncodingName: string; out aCodePage: Cardinal): Boolean; static;
    class function GetStandardEncodingNameByCodePage(const aCodePage: Cardinal; out aEncodingName: string): Boolean; static;
  protected
    fCurrentLineNumber: Integer;
    fLastLineBreakPosition: Integer;
    fXMLStreamEncoding: TEncoding;
    fXMLStreamHasPreamble: Boolean;
    fXMLStreamDataStartPosition: Integer;
    fXMLVersionString: string;
    // If fUseTabIndent is False, tab is replaced with 2 spaces
    fUseTabIndent: Boolean;

    function ReadXMLHeaderUnicode(var aXMLEncoding: TEncoding; var aCodePage: Word; var aXMLVersion: string; var aNeedToFreeEncoding: Boolean): Boolean;
    function GetEncodingCodePageByName(const aEncodingName: string): Cardinal; virtual;
    function GetEncodingNameByCodePade(const aCodePage: Cardinal): string; virtual;

    procedure SetStreamEncoding(aNewEncoding: TEncoding);
  public
    constructor Create; overload;
    constructor Create(aStreamEncoding: TEncoding); overload;
    destructor Destroy; override;

    function GetDataAsString: string;
    function CopyFromXMLStream(aSourceStream: TXMLStream): Integer;

    procedure ReadHeaderAndExamineEncoding(aExpectedEncoding: TEncoding);
    function ReadCDATASection(var aReadedText: string): Boolean;
    function ReadIdentifier: string;
    function ReadStringValue: string;

    procedure SkipSpaces;
    procedure SkipControls;
    function CheckAndReadNextFullString(const aCheckString: string): Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckAndReadNextString(const aCheckString: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckAndReadNextChar(const aCheckChar: Char); {$IFNDEF DEBUG}inline;{$ENDIF}
    // Get char on current position with advancing position
    function ReadNextChar: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
    // Get char on current position without advancing position
    function GetNextChar(aCharCountOffset: Cardinal = 0): Char; {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure WritePreamble;
    procedure WriteXMLHeader(aHeaderEncodingProperty: TEncoding = nil; const aXMLVersion: string = '1.0');
    procedure WriteIndent(const aIndentLevel: Integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteSpacesIndent(const aSpaceCount: Integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteTabIndent(const aIndentLevel: Integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteChar(const aChar: Char); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteString(const aString: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteXMLNodeStartChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteXMLNodeEndChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteXMLNodeCloseChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteEqualsChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteQuotationChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteSpaceChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteTabChar; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure WriteCRLF;

    procedure RaiseInvalidXMLFormat;

    property XMLStreamEncoding: TEncoding read fXMLStreamEncoding;
    property XMLStreamHasPreamble: Boolean read fXMLStreamHasPreamble;
    property XMLStreamDataStartPosition: Integer read fXMLStreamDataStartPosition;
    property XMLVersionString: string read fXMLVersionString;
    property UseTabIndent: Boolean read fUseTabIndent write fUseTabIndent;
  end;

implementation

{ }

function TryGetEncodingByPreamble(aBuffer: Pointer; aSize: Integer): TEncoding; overload;
var
  lPreamble: TBytes;
begin
  if (not Assigned(aBuffer)) or (aSize <= 0) then Exit(nil);
  Result := nil;

  lPreamble := TEncoding.UTF8.GetPreamble;
  if (Length(lPreamble) <= aSize) and CompareMem(aBuffer, Pointer(lPreamble), Length(lPreamble)) then
    Exit(TEncoding.UTF8);

  lPreamble := TEncoding.Unicode.GetPreamble;
  if (Length(lPreamble) <= aSize) and CompareMem(aBuffer, Pointer(lPreamble), Length(lPreamble)) then
    Exit(TEncoding.Unicode);

  lPreamble := TEncoding.BigEndianUnicode.GetPreamble;
  if (Length(lPreamble) <= aSize) and CompareMem(aBuffer, Pointer(lPreamble), Length(lPreamble)) then
    Exit(TEncoding.BigEndianUnicode);
end;

function TryGetEncodingByPreamble(aStream: TStream): TEncoding; overload;
const
  MAX_PREAMBLE_SIZE = 10;
var
  lOldPosition: Int64;
  lBuffer: Pointer;
  lSize: Integer;
begin
  lOldPosition := aStream.Position;
  GetMem(lBuffer, MAX_PREAMBLE_SIZE);
  try
    lSize := Min(MAX_PREAMBLE_SIZE, aStream.Size - aStream.Position);
    if lSize > 0 then
      aStream.ReadBuffer(lBuffer^, lSize);
    Result := TryGetEncodingByPreamble(lBuffer, lSize);
  finally
    FreeMem(lBuffer);
    aStream.Position := lOldPosition;
  end;
end;

function SearchDataBuffer(aDataBuffer: Pointer; aDataBufferSize: Integer; aSearchBuffer: Pointer; aSearchBufferSize: Integer;
    aSearchStartOffset: Integer = -1; aSearchEndOffset: Integer = -1): Integer;
var
  i: Integer;
  lStartOff, lEndOff: Integer;
begin
  if (aDataBufferSize <= 0) or (aSearchBufferSize <= 0) or (aSearchStartOffset > aDataBufferSize)
      or (aSearchEndOffset > aDataBufferSize) or (aSearchBufferSize > aDataBufferSize)
      or ((aSearchStartOffset <> -1) and (aSearchEndOffset <> -1) and (aSearchStartOffset > aSearchEndOffset))
      or (aDataBuffer = nil) or (aSearchBuffer = nil)
  then
    Exit(-1);

  if aSearchStartOffset <> -1 then
    lStartOff := aSearchStartOffset
  else
    lStartOff := 0;
  if aSearchEndOffset <> -1 then
    lEndOff := aSearchEndOffset
  else
    lEndOff := aDataBufferSize;
  lEndOff := Min(lEndOff, aDataBufferSize - aSearchBufferSize);

  for i := lStartOff to lEndOff do
    if CompareMem(aSearchBuffer, @(cuClasses.PByteArray(aDataBuffer)^[i]), aSearchBufferSize) then
      Exit(i - lStartOff);

  Result := -1;
end;

{ TCustomMemStream }

destructor TCustomMemStream.Destroy;
begin
  SetCapacity(0);
  inherited Destroy;
end;

procedure TCustomMemStream.SetCapacity(const aCapacity: Integer);
begin
  if fCapacity = aCapacity then Exit;
  fCapacity := aCapacity;
  if fCapacity > 0 then
  begin
    if fMemory = nil then GetMem(fMemory, fCapacity)
    else ReallocMem(fMemory, fCapacity);
    Exit;
  end;
  FreeMem(fMemory);
  fMemory := nil;
end;

function TCustomMemStream.GetSize: Int64;
begin
  Result := fSize;
end;

procedure TCustomMemStream.SetSize(NewSize: Longint);
begin
  if fCapacity < NewSize then
    SetCapacity(NewSize);
  fSize := NewSize;
  if fPosition > fSize then
    fPosition := fSize;
end;

procedure TCustomMemStream.SetSize(const NewSize: Int64);
begin
  if (NewSize < Low(Integer)) or (NewSize > High(Integer)) then
    raise EXMLStreamException.Create(EXCEPTION_MESSAGE_STREAM_WRONG_SIZE);
  SetSize(LongInt(NewSize));
end;

procedure TCustomMemStream.InternalReadBuffer(var Buffer; aCount: Integer);
begin
  InternalGetBuffer(Buffer, aCount);
  Inc(fPosition, aCount);
end;

procedure TCustomMemStream.InternalGetBuffer(var Buffer; aCount: Integer);
begin
  if (fPosition + aCount) > fSize then
    RaiseReadOutOfBounds;
  Move(PByte(PByte(fMemory) + fPosition)^, Buffer, aCount);
end;

procedure TCustomMemStream.InternalGetBufferWithOffset(var Buffer; aCount, aOffset: Integer);
begin
  if (fPosition + aOffset + aCount) > fSize then
    RaiseReadOutOfBounds;
  Move(PByte(PByte(fMemory) + fPosition + aOffset)^, Buffer, aCount);
end;

procedure TCustomMemStream.RaiseReadOutOfBounds;
begin
  raise EStreamOutOfBoundsException.CreateFmt(EXCEPTION_MESSAGE_STREAM_OUT_OF_BOUNDS_READ, [Self.ClassName]);
end;

procedure TCustomMemStream.RaiseWriteOutOfBounds;
begin
  raise EStreamOutOfBoundsException.CreateFmt(EXCEPTION_MESSAGE_STREAM_OUT_OF_BOUNDS_WRITE, [Self.ClassName]);
end;

function TCustomMemStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (fPosition >= 0) and (Count > 0) then
  begin
    Result := fSize - fPosition;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move(Pointer(PByte(fMemory) + fPosition)^, Buffer, Result);
      Inc(fPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TCustomMemStream.Write(const Buffer; Count: Longint): Longint;
var
  lNewPos: Longint;
begin
  if (fPosition >= 0) and (Count > 0) then
  begin
    lNewPos := fPosition + Count;
    if lNewPos > fCapacity then
      SetCapacity(lNewPos + lNewPos div 3);
    if lNewPos > fSize then fSize := lNewPos;
    Move(Buffer, Pointer(PByte(fMemory) + fPosition)^, Count);
    fPosition := lNewPos;
    Result := Count;
    Exit;
  end;
  Result := 0;
end;

function TCustomMemStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fPosition := Offset;
    soFromCurrent: Inc(fPosition, Offset);
    soFromEnd: fPosition := fSize + Offset;
  end;
  Result := fPosition;
end;

function TCustomMemStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset < Low(Integer)) or (Offset > High(Integer)) then
    raise EXMLStreamException.Create(EXCEPTION_MESSAGE_STREAM_WRONG_OFFSET);
  Result := Seek(LongInt(Offset), Ord(Origin));
end;

procedure TCustomMemStream.LoadFromStream(const aStream: TStream);
var
  lCount: Integer;
begin
  aStream.Position := 0;
  lCount := aStream.Size;
  SetSize(lCount);
  if lCount <> 0 then
    aStream.ReadBuffer(fMemory^, lCount);
  fPosition := 0;
end;

procedure TCustomMemStream.LoadFromFile(const aFileName: string);
var
  lFileStream: TFileStream;
begin
  lFileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(lFileStream);
  finally
    lFileStream.Free;
  end;
end;

function TCustomMemStream.SaveToStream(aDestStream: TStream; aCount: Integer = 0): Integer;
begin
  if aCount <= 0 then
  begin
    Self.Position := 0;
    aCount := Self.Size;
  end;
  if (Self.fPosition + aCount) > Self.fSize then
    aCount := Self.fSize - Self.fPosition;

  Result := aCount;
  aDestStream.WriteBuffer(Pointer(PByte(fMemory) + fPosition)^, aCount);
end;

procedure TCustomMemStream.SaveToFile(const aFileName: string);
var
  lFileStream: TFileStream;
begin
  lFileStream := TFileStream.Create(aFileName, fmCreate or fmShareDenyWrite);
  try
    Self.SaveToStream(lFileStream);
  finally
    lFileStream.Free;
  end;
end;

function TCustomMemStream.CopyFromCustomMemStream(const aSourceStream: TCustomMemStream; aCount: Integer = 0): Integer;
var
  lStartPosition, lCopyCount: Integer;
begin
  if aCount > 0 then
    lStartPosition := aSourceStream.fPosition
  else
  begin
    lStartPosition := 0;
    aCount := aSourceStream.fSize;
  end;
  lCopyCount := aCount;
  if lStartPosition + lCopyCount > aSourceStream.fSize then
    lCopyCount := aSourceStream.fSize - lStartPosition;
  Result := lCopyCount;
  Self.SetSize(lCopyCount + fPosition);
  Move(Pointer(PByte(aSourceStream.fMemory) + lStartPosition)^, Pointer(PByte(fMemory) + fPosition)^, lCopyCount);
end;

{ TXMLStream }

constructor TXMLStream.Create;
begin
  Create(TEncoding.UTF8);
end;

constructor TXMLStream.Create(aStreamEncoding: TEncoding);
begin
  inherited Create;
  fCurrentLineNumber := 1;
  fLastLineBreakPosition := 1;
  SetStreamEncoding(aStreamEncoding);
  fXMLStreamHasPreamble := False;
  fUseTabIndent := True;
end;

destructor TXMLStream.Destroy;
begin
  FreeAndNil(fXMLStreamEncoding);
  inherited Destroy;
end;

class constructor TXMLStream.Create;
var
  i: Integer;
begin
  SetLength(fStandardEncodingsArray, Length(XML_STANDARD_ENCODINGS_ARRAY));
  for i := Low(fStandardEncodingsArray) to High(fStandardEncodingsArray) do
    fStandardEncodingsArray[i] := XML_STANDARD_ENCODINGS_ARRAY[i];
end;

class destructor TXMLStream.Destroy;
begin
  SetLength(fStandardEncodingsArray, 0);
end;

class procedure TXMLStream.AddStandardEncoding(const aCodePade: Cardinal; const aEncodingName: string);
var
  i: Integer;
begin
  i := Length(fStandardEncodingsArray);
  SetLength(fStandardEncodingsArray, i + 1);
  fStandardEncodingsArray[i].EncodingCodePage := aCodePade;
  fStandardEncodingsArray[i].EncodingName := aEncodingName;
end;

class function TXMLStream.GetStandardEncodingCodePageByName(const aEncodingName: string; out aCodePage: Cardinal): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(fStandardEncodingsArray) to High(fStandardEncodingsArray) do
    if SameText(aEncodingName, fStandardEncodingsArray[i].EncodingName) then
    begin
      aCodePage := fStandardEncodingsArray[i].EncodingCodePage;
      Exit(True);
    end;
end;

class function TXMLStream.GetStandardEncodingNameByCodePage(const aCodePage: Cardinal; out aEncodingName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(fStandardEncodingsArray) to High(fStandardEncodingsArray) do
    if aCodePage = fStandardEncodingsArray[i].EncodingCodePage then
    begin
      aEncodingName := fStandardEncodingsArray[i].EncodingName;
      Exit(True);
    end;
end;

function TXMLStream.ReadXMLHeaderUnicode(var aXMLEncoding: TEncoding; var aCodePage: Word; var aXMLVersion: string; var aNeedToFreeEncoding: Boolean): Boolean;
const
  XML_VERSION_IDENTIFIER = 'version';
  XML_ENCODING_IDENTIFIER = 'encoding';
var
  lIdentifier, lValue: string;
  lChr1, lChr2: Char;
begin
  if Assigned(aXMLEncoding) and aNeedToFreeEncoding then
    FreeAndNil(aXMLEncoding);
  aXMLEncoding := nil;
  aNeedToFreeEncoding := False;
  aCodePage := 0;

  SkipSpaces;
  lChr1 := GetNextChar;
  lChr2 := GetNextChar(1);
  if (lChr1 <> XML_NODE_START_CHAR) or (lChr2 <> XML_NODE_HEADER_START_END_CHAR) then
    Exit(False);

  if (GetNextChar <> XML_NODE_START_CHAR) and (GetNextChar(1) <> XML_NODE_HEADER_START_END_CHAR) then
    Exit(False);

  Result := False;
  ReadNextChar;
  ReadNextChar;
  CheckAndReadNextString('xml ');
  while True do
  begin
    SkipSpaces;
    if GetNextChar = XML_NODE_HEADER_START_END_CHAR then
      Break;
    lIdentifier := ReadIdentifier;
    SkipSpaces;
    CheckAndReadNextChar(XML_EQUALS_CHAR);
    SkipSpaces;
    lValue := ReadStringValue;

    if SameText(lIdentifier, XML_VERSION_IDENTIFIER) then
    begin
      aXMLVersion := lValue;
      Result := True;
    end
    else if SameText(lIdentifier, XML_ENCODING_IDENTIFIER) then
    begin
      aCodePage := GetEncodingCodePageByName(lValue);
      aXMLEncoding := TEncoding.GetEncoding(aCodePage);
      aNeedToFreeEncoding := True;

      {if SameText(lValue, XML_HEADER_ENCODING_UTF8) then
      begin
        aXMLEncoding := TEncoding.UTF8;
        aCodePage := aXMLEncoding.CodePage;
      end
      else if SameText(lValue, XML_HEADER_ENCODING_UTF16) then
      begin
        aXMLEncoding := TEncoding.Unicode;
        aCodePage := aXMLEncoding.CodePage;
      end
      else if lValue.StartsWith(XML_ENCODING_WINDOWS_START, True) then
      begin
        Delete(lValue, 1, Length(XML_ENCODING_WINDOWS_START));
        aCodePage := StrToIntDef(lValue, 0);
        if aCodePage = 0 then
          raise EXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_UNKNOWN_ENCODING, [XML_ENCODING_WINDOWS_START, lValue]);
        aXMLEncoding := TEncoding.GetEncoding(aCodePage);
        aNeedToFreeEncoding := True;
      end
      else
      begin
        aXMLEncoding := TEncoding.GetEncoding(lValue);
        aNeedToFreeEncoding := True;
        aCodePage := aXMLEncoding.CodePage;
      end;  }
    end;
  end;
  CheckAndReadNextChar(XML_NODE_HEADER_START_END_CHAR);
  CheckAndReadNextChar(XML_NODE_END_CHAR);
end;

function TXMLStream.GetEncodingCodePageByName(const aEncodingName: string): Cardinal;
const
  XML_ENCODING_WINDOWS_START = 'windows-';
var
  lCodePage: Cardinal;
  lTempName: string;
  lSearchPos: Integer;
begin
  if GetStandardEncodingCodePageByName(aEncodingName, lCodePage) then
    Result := lCodePage
  else if aEncodingName.StartsWith(XML_ENCODING_WINDOWS_START, True) then
  begin
    lTempName := aEncodingName;
    Delete(lTempName, 1, Length(XML_ENCODING_WINDOWS_START));
    Result := StrToIntDef(lTempName, 0);
  end
  else
  begin
    lSearchPos := Pos(XML_SPACE_CHAR, aEncodingName);
    if lSearchPos <> 0 then
    begin
      lTempName := Copy(aEncodingName, 1, lSearchPos - 1);
      Result := StrToIntDef(lTempName, 0);
      if Result <> 0 then Exit;
    end;

    Result := StrToIntDef(aEncodingName, 0);
  end;

  if Result = 0 then
    raise EXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_UNKNOWN_ENCODING, [aEncodingName]);
end;

function TXMLStream.GetEncodingNameByCodePade(const aCodePage: Cardinal): string;
var
  lEncoding: TEncoding;
begin
  if GetStandardEncodingNameByCodePage(aCodePage, Result) then Exit;

  lEncoding := TEncoding.GetEncoding(aCodePage);
  try
    Result := lEncoding.EncodingName;
  finally
    lEncoding.Free;
  end;
end;

procedure TXMLStream.SetStreamEncoding(aNewEncoding: TEncoding);
begin
  if aNewEncoding = nil then
    raise EXMLStreamException.Create(EXCEPTION_MESSAGE_STREAM_WRONG_ENCODING);
  FreeAndNil(fXMLStreamEncoding);
  fXMLStreamEncoding := aNewEncoding.Clone;
end;

function TXMLStream.GetDataAsString: string;
var
  lStreamDataSize, lConvertedBufferSize: Integer;
  lOriginalBuffer, lConvertedBuffer: TBytes;
begin
  Seek(0, soBeginning);
  if XMLStreamEncoding.CodePage = TEncoding.Unicode.CodePage then
  begin
    if (Self.fSize mod CHAR_SIZE) <> 0 then
      raise EXMLStreamException.Create(EXCEPTION_MESSAGE_STREAM_WRONG_ENCODING);

    SetLength(Result, Self.fSize div CHAR_SIZE);
    Self.ReadBuffer(Result[1], Self.fSize);
    Exit;
  end;
  // We need to convert from stream encoding to unicode
  lStreamDataSize := Self.fSize;
  Setlength(lOriginalBuffer, lStreamDataSize);
  Self.ReadBuffer(lOriginalBuffer[0], lStreamDataSize);
  lConvertedBuffer := TEncoding.Convert(XMLStreamEncoding, TEncoding.Unicode, lOriginalBuffer);
  SetLength(lOriginalBuffer, 0);
  lConvertedBufferSize := Length(lConvertedBuffer);
  if lConvertedBufferSize mod CHAR_SIZE <> 0 then
    raise EXMLStreamException.Create(EXCEPTION_MESSAGE_STREAM_WRONG_ENCODING);
  SetLength(Result, lConvertedBufferSize div CHAR_SIZE);
  Move(lConvertedBuffer[0], Result[1], lConvertedBufferSize);
end;

function TXMLStream.CopyFromXMLStream(aSourceStream: TXMLStream): Integer;
var
  lSourceStreamDataSize, lConvertedSize: Integer;
  lOriginalBuffer, lConvertedBuffer: TBytes;
begin
  if Self.XMLStreamEncoding.CodePage = aSourceStream.XMLStreamEncoding.CodePage then
    Exit(CopyFromCustomMemStream(aSourceStream));

  // Converting
  aSourceStream.Seek(0, soBeginning);
  lSourceStreamDataSize := aSourceStream.fSize;
  Result := lSourceStreamDataSize;
  SetLength(lOriginalBuffer, lSourceStreamDataSize);
  aSourceStream.ReadBuffer(lOriginalBuffer[0], lSourceStreamDataSize);
  lConvertedBuffer := TEncoding.Convert(aSourceStream.XMLStreamEncoding, Self.XMLStreamEncoding, lOriginalBuffer);
  lConvertedSize := Length(lConvertedBuffer);
  Self.Size := lConvertedSize + Self.fPosition;
  Self.WriteBuffer(lConvertedBuffer[0], lConvertedSize);
end;

procedure TXMLStream.ReadHeaderAndExamineEncoding(aExpectedEncoding: TEncoding);
const
  XML_HEADER_START_SEQUENCE: UnicodeString = '<?';
  XML_HEADER_END_SEQUENCE: UnicodeString = '?>';
  MAX_HEADER_SEARCH_SIZE = 1024;
var
  lPreambleEncoding, lAssumedEncoding, lHeaderEncoding: TEncoding;
  lNeedToFreeHeaderEncoding: Boolean;
  lPreamble, lConvertedBuffer: TBytes;
  lDataStartPosition: Integer;
  lCodePage: Word;
  lXMLVersion: string;
  lSearchSize: Integer;
  lHeaderStartPosition, lHeaderEndPosition: Integer;
  lReadHeaderResult, lAnsiReadHeaderResult: Boolean;

  function TryReadXMLHeaderWithEncoding(aEncoding: TEncoding; aHeaderStartPosition: Integer = -1; aHeaderEndPosition: Integer = -1): Boolean;
  var
    lEncodingHeaderStartPosition, lEncodingHeaderEndPosition, lEncodingHeaderSize: Integer;
    lOriginalBuffer: TBytes;
    lConvertXMLStream: TXMLStream;
  begin
    if (aHeaderStartPosition = -1) or (aHeaderEndPosition = -1) then
    begin
      lConvertedBuffer := aEncoding.GetBytes(XML_HEADER_START_SEQUENCE);
      lEncodingHeaderStartPosition := SearchDataBuffer(fMemory, lSearchSize, lConvertedBuffer, Length(lConvertedBuffer), lDataStartPosition, -1);
      if lEncodingHeaderStartPosition <> -1 then
      begin
        lConvertedBuffer := aEncoding.GetBytes(XML_HEADER_END_SEQUENCE);
        lEncodingHeaderEndPosition := SearchDataBuffer(fMemory, lSearchSize, lConvertedBuffer, Length(lConvertedBuffer), lDataStartPosition, -1);
      end
      else
        lEncodingHeaderEndPosition := -1;
    end
    else
    begin
      lEncodingHeaderStartPosition := aHeaderStartPosition;
      lEncodingHeaderEndPosition := aHeaderEndPosition;
    end;

    if (lEncodingHeaderStartPosition = -1) or (lEncodingHeaderEndPosition = -1) then
      Exit(False);

    lConvertedBuffer := aEncoding.GetBytes(XML_HEADER_END_SEQUENCE);
    lEncodingHeaderSize := lEncodingHeaderEndPosition - lEncodingHeaderStartPosition + Length(lConvertedBuffer);
    // Moving current position in stream to end of the header
    Seek(lDataStartPosition + lEncodingHeaderStartPosition + lEncodingHeaderSize, soBeginning);
    // If there is start and end positions, convert only header to unicode to read header
    lConvertXMLStream := TXMLStream.Create(TEncoding.Unicode);
    try
      SetLength(lOriginalBuffer, lEncodingHeaderSize);
      Move(PByte(PByte(fMemory) + lDataStartPosition + lEncodingHeaderStartPosition)^, lOriginalBuffer[0], lEncodingHeaderSize);
      lConvertedBuffer := TEncoding.Convert(aEncoding, TEncoding.Unicode, lOriginalBuffer);
      lConvertXMLStream.Size := Length(lConvertedBuffer);
      lConvertXMLStream.WriteBuffer(lConvertedBuffer[0], Length(lConvertedBuffer));
      lConvertXMLStream.Seek(0, soBeginning);
      Result := lConvertXMLStream.ReadXMLHeaderUnicode(lHeaderEncoding, lCodePage, lXMLVersion, lNeedToFreeHeaderEncoding);
    finally
      lConvertXMLStream.Free;
    end;
  end;

begin
  // Reading preamble and header to determine encoding
  // We must assign fXMLStreamEncoding (SetStreamEncoding), fXMLStreamHasPreamble, fXMLStreamDataStartPosition, fXMLVersionString in this method

  // Try reading known preamble and determine encoding by that
  lPreambleEncoding := TryGetEncodingByPreamble(Self);
  // Saving current position
  lDataStartPosition := Seek(0, soCurrent);

  // If we determined preamble encoding, we must skip it
  if lPreambleEncoding <> nil then
  begin
    lPreamble := lPreambleEncoding.GetPreamble;
    // Shifting position to consider preamble
    lDataStartPosition := Seek(Length(lPreamble), soCurrent);
    fXMLStreamHasPreamble := True;
  end;

  fXMLStreamDataStartPosition := lDataStartPosition;
  if lPreambleEncoding <> nil then
    lAssumedEncoding := lPreambleEncoding
  else
    lAssumedEncoding := aExpectedEncoding;

  // Assign preliminary stream encoding. If encoding is not defined, raise an exception.
  SetStreamEncoding(lAssumedEncoding);
  // Calculate search size for header search
  lSearchSize := Min(MAX_HEADER_SEARCH_SIZE, fSize - fPosition);
  lHeaderEncoding := nil;
  try
    {$MESSAGE warn 'WARNIGN!!!!  Проверить все ветки. Сделать для каждой свой xml и грузить их.'}

    if lAssumedEncoding = TEncoding.Unicode then
    begin
      // Search for start and finish of header
      lConvertedBuffer := lAssumedEncoding.GetBytes(XML_HEADER_START_SEQUENCE);
      lHeaderStartPosition := SearchDataBuffer(fMemory, lSearchSize, lConvertedBuffer, Length(lConvertedBuffer), lDataStartPosition, -1);
      if lHeaderStartPosition <> -1 then
      begin
        lConvertedBuffer := lAssumedEncoding.GetBytes(XML_HEADER_END_SEQUENCE);
        lHeaderEndPosition := SearchDataBuffer(fMemory, lSearchSize, lConvertedBuffer, Length(lConvertedBuffer), lDataStartPosition, -1);
      end
      else
        lHeaderEndPosition := -1;

      // If we found start and end of header, try to read it
      if (lHeaderStartPosition <> -1) and (lHeaderEndPosition <> -1) then
        lReadHeaderResult := ReadXMLHeaderUnicode(lHeaderEncoding, lCodePage, lXMLVersion, lNeedToFreeHeaderEncoding)
      else
        lReadHeaderResult := False;

      if not lReadHeaderResult then
      begin
        // If we failed to read header in unicode, try to read in ansi
        lAnsiReadHeaderResult := TryReadXMLHeaderWithEncoding(TEncoding.ANSI);

        if not lAnsiReadHeaderResult then
          SetStreamEncoding(aExpectedEncoding)
        else
        begin
          // If we readed ansi header, look for header encoding
          if lHeaderEncoding = nil then
            SetStreamEncoding(aExpectedEncoding)
          else
            SetStreamEncoding(lHeaderEncoding);
        end;
      end
      else
      begin
        // Check is header had encoding and set that encoding, otherwise we leave assumed encoding
        if lHeaderEncoding <> nil then
          SetStreamEncoding(lHeaderEncoding);
      end;
    end
    else
    begin
      // Try to read ansi header first
      lAnsiReadHeaderResult := TryReadXMLHeaderWithEncoding(TEncoding.ANSI);
      if lAnsiReadHeaderResult then
      begin
        // Check is header had encoding and set that encoding, otherwise we leave assumed encoding
        if lHeaderEncoding <> nil then
          SetStreamEncoding(lHeaderEncoding);
      end
      else
      begin
        // Search for start and finish of header
        lConvertedBuffer := lAssumedEncoding.GetBytes(XML_HEADER_START_SEQUENCE);
        lHeaderStartPosition := SearchDataBuffer(fMemory, lSearchSize, lConvertedBuffer, Length(lConvertedBuffer), lDataStartPosition, -1);
        if lHeaderStartPosition <> -1 then
        begin
          lConvertedBuffer := lAssumedEncoding.GetBytes(XML_HEADER_END_SEQUENCE);
          lHeaderEndPosition := SearchDataBuffer(fMemory, lSearchSize, lConvertedBuffer, Length(lConvertedBuffer), lDataStartPosition, -1);
        end
        else
          lHeaderEndPosition := -1;

        if (lHeaderStartPosition <> -1) and (lHeaderEndPosition <> -1) then
          lReadHeaderResult := TryReadXMLHeaderWithEncoding(lAssumedEncoding, lHeaderStartPosition, lHeaderEndPosition)
        else
          lReadHeaderResult := False;

        if lReadHeaderResult and (lHeaderEncoding <> nil) then
          SetStreamEncoding(lHeaderEncoding);
      end;
    end;

    // Assigning XMLVersion to stream field
    fXMLVersionString := lXMLVersion;

    // If we readed header, check if readed encoding is same as BOM
    if (lPreambleEncoding <> nil) and (lHeaderEncoding <> nil) and (lHeaderEncoding.CodePage <> lPreambleEncoding.CodePage) then
      raise EXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_DIFFERENT_HEADER_BOM_ENCODING, [lHeaderEncoding.EncodingName, lPreambleEncoding.EncodingName]);

  finally
    if Assigned(lHeaderEncoding) and lNeedToFreeHeaderEncoding then
      FreeAndNil(lHeaderEncoding);
  end;
end;

function TXMLStream.ReadCDATASection(var aReadedText: string): Boolean;
const
  CDATA_START_TEXT = '<![CDATA[';
  CDATA_END_TEXT = ']]>';
  CDATA_END_START_CHAR: Char = ']';
var
  lCDataTextBuilder: TUnicodeStringBuilder;
  lChr: Char;
begin
  Result := False;
  // Checking if there is a start to CDATA section
  if not CheckAndReadNextFullString(CDATA_START_TEXT) then
    Exit(False);

  lCDataTextBuilder.Init(aReadedText);
  try
    lCDataTextBuilder.Add(CDATA_START_TEXT);
    while not Result do
    begin
      lChr := GetNextChar;
      if lChr = CDATA_END_START_CHAR then
        if CheckAndReadNextFullString(CDATA_END_TEXT) then
        begin
          lCDataTextBuilder.Add(CDATA_END_TEXT);
          Result := True;
          Continue;
        end;
      lCDataTextBuilder.Add(ReadNextChar);
    end;
  finally
    lCDataTextBuilder.Done;
  end;
  Result := True;
end;

function TXMLStream.ReadIdentifier: string;
const
  START_CHARS: TSysCharSet = ['a'..'z', 'A'..'Z', '_', ':'];
  IDENTIFIER_CHARS: TSysCharSet = ['a'..'z', 'A'..'Z', '_', ':', '.', '0'..'9'];
var
  lBuilder: TUnicodeStringBuilder;
  lChr: Char;
begin
  lChr := ReadNextChar;
  if not CharInSet(lChr, START_CHARS) then
    RaiseInvalidXMLFormat;
  lBuilder.Init(Result);
  lBuilder.Add(lChr);
  while CharInSet(GetNextChar, IDENTIFIER_CHARS) do
    lBuilder.Add(ReadNextChar);
  lBuilder.Done;
end;

function TXMLStream.ReadStringValue: string;
var
  lBuilder: TUnicodeStringBuilder;
  lChr: Char;
begin
  lChr := ReadNextChar;
  if (lChr <> XML_APOSTROPHE_CHAR) and (lChr <> XML_QUOTATION_CHAR) then
    RaiseInvalidXMLFormat;
  lBuilder.Init(Result);
  try
    lChr := ReadNextChar;
    while (lChr <> XML_APOSTROPHE_CHAR) and (lChr <> XML_QUOTATION_CHAR) do
    begin
      lBuilder.Add(lChr);
      lChr := ReadNextChar;
    end;
  finally
    lBuilder.Done;
  end;
end;

procedure TXMLStream.SkipSpaces;
var
  lChr: Char;
  lCurrentPosition: Integer;
begin
  while fPosition + 1 < fSize do
  begin
    lChr := GetNextChar;
    if lChr.IsWhiteSpace or lChr.IsControl then
    begin
      ReadNextChar;
      Continue;
    end;

    if lChr = XML_NODE_START_CHAR then
    begin
      lCurrentPosition := Seek(0, soCurrent);
      ReadNextChar;
      if (GetNextChar = XML_NODE_COMMENT_START_CHAR) and (GetNextChar(1) = XML_NODE_DASH_CHAR) then
      begin
        ReadNextChar;
        CheckAndReadNextChar(XML_NODE_DASH_CHAR);
        CheckAndReadNextChar(XML_NODE_DASH_CHAR);
        while fPosition + 1 < fSize do
        begin
          lChr := ReadNextChar;
          if (lChr = XML_NODE_DASH_CHAR) and (ReadNextChar = XML_NODE_DASH_CHAR)
              and (ReadNextChar = XML_NODE_END_CHAR)
          then
            Break;
        end;
        Continue;
      end
      else
      begin
        Seek(lCurrentPosition, soBeginning);
        Exit;
      end;
    end;

    Break;
  end;
end;

procedure TXMLStream.SkipControls;
var
  lChr: Char;
  lCurrentPosition: Integer;
begin
  while fPosition + 1 < fSize do
  begin
    lChr := GetNextChar;
    if lChr.IsControl then
    begin
      ReadNextChar;
      Continue;
    end;

    if lChr = XML_NODE_START_CHAR then
    begin
      lCurrentPosition := Seek(0, soCurrent);
      ReadNextChar;
      if (GetNextChar = XML_NODE_COMMENT_START_CHAR) and (GetNextChar(1) = XML_NODE_DASH_CHAR) then
      begin
        ReadNextChar;
        CheckAndReadNextChar(XML_NODE_DASH_CHAR);
        CheckAndReadNextChar(XML_NODE_DASH_CHAR);
        while fPosition + 1 < fSize do
        begin
          lChr := ReadNextChar;
          if (lChr = XML_NODE_DASH_CHAR) and (ReadNextChar = XML_NODE_DASH_CHAR)
              and (ReadNextChar = XML_NODE_END_CHAR)
          then
            Break;
        end;
        Continue;
      end
      else
      begin
        Seek(lCurrentPosition, soBeginning);
        Exit;
      end;
    end;

    Break;
  end;
end;

function TXMLStream.CheckAndReadNextFullString(const aCheckString: string): Boolean;
var
  i, lStartPosition: Integer;
begin
  lStartPosition := Seek(0, soCurrent);

  for i := Low(aCheckString) to High(aCheckString) do
    if ReadNextChar <> aCheckString[i] then
    begin
      Seek(lStartPosition, soBeginning);
      Exit(False);
    end;
  Result := True;
end;

procedure TXMLStream.CheckAndReadNextString(const aCheckString: string);
var
  i: Integer;
begin
  for i := Low(aCheckString) to High(aCheckString) do
    CheckAndReadNextChar(aCheckString[i]);
end;

procedure TXMLStream.CheckAndReadNextChar(const aCheckChar: Char);
begin
  if ReadNextChar <> aCheckChar then
    RaiseInvalidXMLFormat;
end;

function TXMLStream.ReadNextChar: Char;
var
  lBytes: TBytes;
  lCharSize: Byte;
begin
  if not fXMLStreamEncoding.IsSingleByte then
  begin
    InternalReadBuffer(Result, CHAR_SIZE);
    lCharSize := CHAR_SIZE;
  end
  else
  begin
    SetLength(lBytes, 1);
    InternalReadBuffer(lBytes[0], ANSI_CHAR_SIZE);
    lCharSize := ANSI_CHAR_SIZE;
    Result := TEncoding.Unicode.GetString(TEncoding.Convert(TEncoding.ANSI, TEncoding.Unicode, lBytes))[1];
  end;

  if (Result = XML_LF) and (fLastLineBreakPosition < (fPosition - lCharSize)) then
  begin
    Inc(fCurrentLineNumber);
    fLastLineBreakPosition := fPosition - lCharSize;
  end;
end;

function TXMLStream.GetNextChar(aCharCountOffset: Cardinal = 0): Char;
var
  lBytes: TBytes;
begin
  if not fXMLStreamEncoding.IsSingleByte then
  begin
    if aCharCountOffset <> 0 then
      InternalGetBufferWithOffset(Result, CHAR_SIZE, aCharCountOffset * CHAR_SIZE)
    else
      InternalGetBuffer(Result, CHAR_SIZE);
  end
  else
  begin
    if aCharCountOffset <> 0 then
      Inc(fPosition, aCharCountOffset * ANSI_CHAR_SIZE);
    SetLength(lBytes, 1);
    InternalGetBuffer(lBytes[0], ANSI_CHAR_SIZE);
    Result := TEncoding.Unicode.GetString(TEncoding.Convert(TEncoding.ANSI, TEncoding.Unicode, lBytes))[1];
  end;
end;

procedure TXMLStream.WritePreamble;
var
  lPreamble: TBytes;
begin
  lPreamble := fXMLStreamEncoding.GetPreamble;
  Self.WriteBuffer(lPreamble, Length(lPreamble));
end;

procedure TXMLStream.WriteXMLHeader(aHeaderEncodingProperty: TEncoding = nil; const aXMLVersion: string = '1.0');
var
  lXMLEncodingString: string;
  lHeaderEncoding: TEncoding;
begin
  if aHeaderEncodingProperty = nil then
    lHeaderEncoding := Self.fXMLStreamEncoding
  else
    lHeaderEncoding := aHeaderEncodingProperty;

  lXMLEncodingString := GetEncodingNameByCodePade(lHeaderEncoding.CodePage);

  WriteString(Format(XML_HEADER_TEMPLATE, [aXMLVersion, lXMLEncodingString]));
  WriteCRLF;
end;

procedure TXMLStream.WriteIndent(const aIndentLevel: Integer);
const
  SPACES_PER_INDENT = 2;
begin
  if fUseTabIndent then
    WriteTabIndent(aIndentLevel)
  else
    WriteSpacesIndent(SPACES_PER_INDENT * aIndentLevel);
end;

procedure TXMLStream.WriteSpacesIndent(const aSpaceCount: Integer);
var
  i: Integer;
begin
  for i := 1 to aSpaceCount do
    WriteSpaceChar;
end;

procedure TXMLStream.WriteTabIndent(const aIndentLevel: Integer);
var
  i: Integer;
begin
  for i := 1 to aIndentLevel do
    WriteTabChar;
end;

procedure TXMLStream.WriteChar(const aChar: Char);
begin
  Write(aChar, CHAR_SIZE);
end;

procedure TXMLStream.WriteString(const aString: string);
begin
  Write(Pointer(aString)^, Length(aString) * CHAR_SIZE);
end;

procedure TXMLStream.WriteXMLNodeStartChar;
begin
  Write(XML_NODE_START_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteXMLNodeEndChar;
begin
  Write(XML_NODE_END_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteXMLNodeCloseChar;
begin
  Write(XML_NODE_CLOSE_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteEqualsChar;
begin
  Write(XML_EQUALS_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteQuotationChar;
begin
  Write(XML_QUOTATION_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteSpaceChar;
begin
  Write(XML_SPACE_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteTabChar;
begin
  Write(XML_TAB_CHAR, CHAR_SIZE);
end;

procedure TXMLStream.WriteCRLF;
begin
  Write(XML_CR, CHAR_SIZE);
  Write(XML_LF, CHAR_SIZE);
end;

procedure TXMLStream.RaiseInvalidXMLFormat;
var
  lNearText: string;
  lLeftBorder, lRightBorder, lRangeSize: Integer;
  lBuffer: TBytes;
begin
  if XML_EXCEPTION_NEAR_CHAR_COUNT <= 0 then
    raise EInvalidFormatXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_INVALID_FORMAT, [fCurrentLineNumber], fCurrentLineNumber)
  else
  begin
    if fXMLStreamEncoding.IsSingleByte then
      lRangeSize := XML_EXCEPTION_NEAR_CHAR_COUNT
    else
      lRangeSize := XML_EXCEPTION_NEAR_CHAR_COUNT * CHAR_SIZE;
    lLeftBorder := fPosition - lRangeSize;
    lRightBorder := fPosition + lRangeSize;
    if lLeftBorder < 0 then lLeftBorder := 0;
    if lRightBorder > fSize then lRightBorder := fSize;
    lRangeSize := lRightBorder - lLeftBorder;

    SetLength(lBuffer, lRangeSize);
    Move(PByte(PByte(fMemory) + lLeftBorder)^, lBuffer[0], lRangeSize);
    if fXMLStreamEncoding.CodePage = TEncoding.Unicode.CodePage then
      lNearText := TEncoding.Unicode.GetString(lBuffer)
    else
      lNearText := TEncoding.Unicode.GetString(TEncoding.Convert(fXMLStreamEncoding, TEncoding.Unicode, lBuffer));

    raise EInvalidFormatXMLException.CreateFmt(EXCEPTION_MESSAGE_XML_INVALID_FORMAT, [fCurrentLineNumber], fCurrentLineNumber, lNearText, (fPosition - lLeftBorder) div CHAR_SIZE);
  end;
end;

end.
