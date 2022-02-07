unit uCustomExceptions;

interface

uses
  System.SysUtils;

type
  EListException = class(Exception)
  end;

  EHashException = class(EListException)
  end;

  EHashItemException = class(EHashException)
  protected
    fItemName: string;
  public
    constructor Create(const aMsg, aItemName: string);
    constructor CreateFmt(const aMsg, aItemName: string; const aArgs: array of const);

    property ItemName: string read fItemName;
  end;

  EStreamException = class(Exception)
  end;

  EStreamOutOfBoundsException = class(EStreamException)
  end;

  EXMLStreamException = class(EStreamException)
  end;

  EXMLException = class(Exception)
  end;

  EInvalidFormatXMLException = class(EXMLException)
  protected
    fLineNumber: Integer;
    fNearText: string;
    fNearTextPosition: Integer;
  public
    constructor Create(const aMsg: string; aLineNumber: Integer = 0; const aNearText: string = ''; aNearTextPosition: Integer = -1);
    constructor CreateFmt(const aMsg: string; const aArgs: array of const; aLineNumber: Integer = 0; const aNearText: string = ''; aNearTextPosition: Integer = -1);

    property LineNumber: Integer read fLineNumber;
    property NearText: string read fNearText;
    property NearTextPosition: Integer read fNearTextPosition;
  end;

implementation

{ EHashItemException }

constructor EHashItemException.Create(const aMsg, aItemName: string);
begin
  inherited Create(aMsg);
  fItemName := aItemName;
end;

constructor EHashItemException.CreateFmt(const aMsg, aItemName: string; const aArgs: array of const);
begin
  inherited CreateFmt(aMsg, aArgs);
  fItemName := aItemName;
end;

{ EInvalidFormatXMLException }

constructor EInvalidFormatXMLException.Create(const aMsg: string; aLineNumber: Integer = 0; const aNearText: string = ''; aNearTextPosition: Integer = -1);
begin
  inherited Create(aMsg);
  fLineNumber := aLineNumber;
  fNearText := aNearText;
  fNearTextPosition := aNearTextPosition;
end;

constructor EInvalidFormatXMLException.CreateFmt(const aMsg: string; const aArgs: array of const; aLineNumber: Integer = 0; const aNearText: string = ''; aNearTextPosition: Integer = -1);
begin
  inherited CreateFmt(aMsg, aArgs);
  fLineNumber := aLineNumber;
  fNearText := aNearText;
  fNearTextPosition := aNearTextPosition;
end;

end.
