unit LibKoreAll;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  LibKoreIntfs,
  LibKoreDll;

type
  TcrChar = WideChar;
  TcrString = UnicodeString;

  TcrConsoleAddLineEvent =
    procedure(const AStringType: Pointer;
      const AStringData: TcrString) of object; stdcall;

  EcrException = class(Exception);

  EcrIntfMissing = class(EcrException)
  public
    { cdtor }
    constructor New;
  end;

  EcrLibKoreDllNotLoaded = class(EcrException)
  public
    { cdtor }
    constructor New;
  end;

  EcrLibKoreCreateFactoryFail = class(EcrException)
  private
    FLibKoreClassName: TcrString;
  public
    { cdtor }
    constructor New(const ALibKoreClassName: TcrString);
  end;

  EcrLibKoreFailed = class(EcrException)
  strict private
    FOriginalMessage: TcrString;
    FStatusCode: Cardinal;
    FOsError: Cardinal;
    FCodeLocalId: PAnsiChar;
    function GetOriginalMessage: TcrString; inline;
    function GetStatusCode: Cardinal; inline;
    function GetOsError: Cardinal; inline;
  public
    { cdtor }
    constructor New(const AOriginalMessage: TcrString;
      const AStatusCode, AOsError: Cardinal;
      const ACodeLocalId: PAnsiChar);

    { properties }
    property OriginalMessage: TcrString read GetOriginalMessage;
    property StatusCode: Cardinal read GetStatusCode;
    property OsError: Cardinal read GetOsError;
  end;

  EcrLibKoreUnknown = class(EcrException);

  TcrObject = class;
  TcrStream = class;
  TcrConsole = class;
  TcrConsoleUtils = class;
  TcrConsoleFactory = class;
  TcrWinPseudoConsole = class;
  TcrWinPseudoConsoleFactory = class;
  TcrValue = class;
  TcrValues = class;
  TcrDbFactory = class;
  TcrDbConnectParams = class;
  TcrDbConnectionPool = class;
  TcrDbConnection = class;
  TcrDbQuery = class;
  TcrCharset = class;
  TcrCharsets = class;
  TcrEncoding = class;
  TcrEncodings = class;
  TcrXmlItem = class;
  TcrXmlAttribute = class;
  TcrXmlNode = class;
  TcrXmlNodes = class;
  TcrXmlDocument = class;
  TcrXmlFactory = class;

  crLibKore = record
  public
    class procedure DefaultInitialize; static;
    class procedure Initialize(const ADllFileName: string); static;
  public
    class function EncodingUtf8: TcrEncoding; static;
    class function EncodingSimpleAnsi: TcrEncoding; static;
  end;

  TcrObject = class(TObject)
  private
    FIntf: Pointer;
    procedure CreateIntf; dynamic;
    procedure CreateIntfByName(const AName: TcrString);
    procedure DestroyIntf;

    class function NewEx<T: TcrObject>(
      const AIntf: Pointer; var AObject: T): T; inline;
    class function NewExPtr(const AIntf: Pointer): Pointer;

    function fgIntf: Pointer;
    function tgIntf: Pointer;
  protected
    procedure CheckIntfRes(const AIntfResult: TcrIntfResult); inline;
    procedure Disposed; virtual;
  public
    { Override }
    procedure FreeInstance; override;

    { cdtor }
    destructor Destroy; override;
  end;

  TcrStream = class(TcrObject)
  private
    function GetSize: Int64;
    procedure SetSize(const ASize: Int64);
  public
    function Read(ABytesPtr: PByte; ABytesCount: NativeInt): NativeInt;
    function Write(ABytesPtr: PByte; ABytesCount: NativeInt): NativeInt;
    function Seek(AOffset: Int64; AOrigin: Integer): Int64;
    procedure Flush;

    { properties }
    property Size: Int64 read GetSize write SetSize;
  end;

  TcrConsole = class(TcrObject)
  public
    procedure AlternativeBuffer(const AOnOff: Boolean);

    { Apagar a linha. Se AIdType
        = 0 -> Da posição atual até o fim da linha
        = 1 -> Do inicio até a posição atual
        = 2 -> Toda a linha }
    procedure ClearLine(const AIdType: Byte);

    { Apagar a tela. Se AIdType
        = 0 -> Da posição atual até o fim da tela
        = 1 -> Do ínicio até a posição atual
        = 2 -> Toda a tela }
    procedure ClearScreen(const AIdType: Byte);

    procedure CursorUp(const ACount: Cardinal);
    procedure CursorDown(const ACount: Cardinal);
    procedure CursorForward(const ACount: Cardinal);
    procedure CursorBack(const ACount: Cardinal);
    procedure CursorNextLine(const ACount: Cardinal);
    procedure CursorPreviousLine(const ACount: Cardinal);
    procedure CursorHorzAbsolute(const ACount: Cardinal);

    procedure DeviceStatusReport;

    procedure RestoreCurrentCursorPosition;
    procedure RestoreScreen;

    procedure SaveCurrentCursorPosition;
    procedure SaveScreen;

    procedure SetBracketPasteMode(const AOnOff: Boolean);
    procedure SetEnabledAuxPort(const AOnOff: Boolean);

    procedure SetCursorPosition(const AX, AY: Cardinal);
    procedure SetCursorVisible(const AOnOff: Boolean);

    procedure SetTitle(const ATitle: TcrString);

    procedure ScrollUp(const ACount: Cardinal);
    procedure ScrollDown(const ACount: Cardinal);

    procedure SelectGraphicRenditionRGB(const AId, ARGB: Cardinal);

    procedure Text(const AText: TcrString);
  end;

  TcrConsoleUtils = class(TcrObject)
  public
    procedure AddFromAnsiEscCodeBuffer(const AConsole: TcrConsole;
      const ABytesPtr: PByte; const ABytesCount: Integer);
    procedure AddFromAnsiEscCodeFile(const AConsole: TcrConsole;
      const AFileName: TcrString);
  end;

  TcrConsoleFactory = class(TcrObject)
  private
    procedure CreateIntf; override;
  public
    function CreateUtils: TcrConsoleUtils;
    function CreateDefault: TcrConsole;

    function CreateByProcessId(const AProcessId: Cardinal): TcrConsole;
    function CreateByAddLine(const AAddLine: TcrConsoleAddLineEvent): TcrConsole;
    function CreateByOutHandle(const AOutHandle: THandle): TcrConsole;
    function CreateCacheByWnd(const AWnd: THandle;
      const AInnerConsole: TcrConsole): TcrConsole;
  end;

  TcrWinPseudoConsole = class(TcrObject)
  public
    procedure Execute;
    function IsTerminated: Boolean;
    procedure BeginExecute;
    function ExitCode: Cardinal;
  end;

  TcrWinPseudoConsoleFactory = class(TcrObject)
  private
    procedure CreateIntf; override;
  public
    { RemoveAdministrativePermissions ->
        Remove os privilégios de administrador
        É útil quando o aplicativo 'pai' estiver sendo executado com
        privilégios de administrador e se deseja executar um processo
        'filho' somente com privilégios de usuário normal }
    procedure RemoveAdministrativePermissions;

    { SetOsLogin ->
        Reservado para versões futuras }
    procedure SetOsLogin(const AUserName, ADomain, APassword: TcrString);

    { SetCommandLine ->
        Indicar a linha de comando que deverá ser executada }
    procedure SetCommandLine(const ACommandLine: TcrString);

    { SetDirectory ->
        Indica qual será o diretório atual do processo 'filho' }
    procedure SetDirectory(const ADirectory: TcrString);

    { SetVariable ->
        Alterar o valor de uma variável de ambiente.
        Este valor alterado estará disponível apenas para o processo 'filho' }
    procedure SetVariable(const AName, AValue: TcrString);

    { SetRecordingStream ->
        Indica uma stream para 'gravar' o conteúdo de saída do console }
    procedure SetRecordingStream(const AStream: TcrStream);

    { SetRecordingFile ->
        Indica um arquivo para 'gravar' o conteúdo de saída do console }
    procedure SetRecordingFileName(const AFileName: TcrString);

    { SetConsole ->
        Indica um console sobre o qual o processo filho será criado }
    procedure SetConsole(const AConsole: TcrConsole);

    { Create ->
        Cria um objeto 'IcrWinPseudoConsole' }
    function CreateObject: TcrWinPseudoConsole;
  end;

  TcrValue = class(TcrObject)
  private
//    function GetOwner(var AOwner: IcrValues): WordBool; stdcall;
    function GetName: TcrString;
    function GetDataTypeId: Cardinal;
    function GetIsNull: Boolean;

    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);

    function GetAsUInt8: UInt8;
    procedure SetAsUInt8(const AValue: UInt8);
    function GetAsUInt16: UInt16;
    procedure SetAsUInt16(const AValue: UInt16);
    function GetAsUInt32: UInt32;
    procedure SetAsUInt32(const AValue: UInt32);
    function GetAsUInt64: UInt64;
    procedure SetAsUInt64(const AValue: UInt64);

    function GetAsSInt8: Int8;
    procedure SetAsSInt8(const AValue: Int8);
    function GetAsSInt16: Int16;
    procedure SetAsSInt16(const AValue: Int16);
    function GetAsSInt32: Int32;
    procedure SetAsSInt32(const AValue: Int32);
    function GetAsSInt64: Int64;
    procedure SetAsSInt64(const AValue: Int64);

//    function GetAsBigInteger(var AValue: IcrBigInteger): WordBool; stdcall;
//    function SetAsBigInteger(const AValue: IcrBigInteger): WordBool; stdcall;

    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const AValue: Currency);
    function GetAsReal32: Single;
    procedure SetAsReal32(const AValue: Single);
    function GetAsReal64: Double;
    procedure SetAsReal64(const AValue: Double);

//    function GetAsBigDecimal(var AValue: IcrBigDecimal): WordBool; stdcall;
//    function SetAsBigDecimal(const AValue: IcrBigDecimal): WordBool; stdcall;

    function GetAsChar: TcrChar;
    procedure SetAsChar(const AValue: TcrChar);
    function GetAsString: TcrString;
    procedure SetAsString(const AValue: TcrString);
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const AValue: TBytes);
    function GetAsUUID: TGUID;
    procedure SetAsUUID(const AValue: TGUID);

    function GetAsTime: TcrTime;
    procedure SetAsTime(const AValue: TcrTime);
    function GetAsTimeUTC: TcrTimeUTC;
    procedure SetAsTimeUTC(const AValue: TcrTimeUTC);
    function GetAsDate: TcrDate;
    procedure SetAsDate(const AValue: TcrDate);
    function GetAsTimeStamp: TcrTimeStamp;
    procedure SetAsTimeStamp(const AValue: TcrTimeStamp);
    function GetAsTimeStampUTC: TcrTimeStampUTC;
    procedure SetAsTimeStampUTC(const AValue: TcrTimeStampUTC);
  public
    procedure Clear;

    { properties }
    property Name: TcrString read GetName;
    property DataTypeId: Cardinal read GetDataTypeId;
    property IsNull: Boolean read GetIsNull;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property AsUInt8: UInt8 read GetAsUInt8 write SetAsUInt8;
    property AsUInt16: UInt16 read GetAsUInt16 write SetAsUInt16;
    property AsUInt32: UInt32 read GetAsUInt32 write SetAsUInt32;
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;

    property AsSInt8: Int8 read GetAsSInt8 write SetAsSInt8;
    property AsSInt16: Int16 read GetAsSInt16 write SetAsSInt16;
    property AsSInt32: Int32 read GetAsSInt32 write SetAsSInt32;
    property AsSInt64: Int64 read GetAsSInt64 write SetAsSInt64;

    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsReal32: Single read GetAsReal32 write SetAsReal32;
    property AsReal64: Double read GetAsReal64 write SetAsReal64;

    property AsChar: TcrChar read GetAsChar write SetAsChar;
    property AsString: TcrString read GetAsString write SetAsString;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsUUID: TGUID read GetAsUUID write SetAsUUID;

    property AsTime: TcrTime read GetAsTime write SetAsTime;
    property AsTimeUTC: TcrTimeUTC read GetAsTimeUTC write SetAsTimeUTC;
    property AsDate: TcrDate read GetAsDate write SetAsDate;
    property AsTimeStamp: TcrTimeStamp read GetAsTimeStamp write SetAsTimeStamp;
    property AsTimeStampUTC: TcrTimeStampUTC
      read GetAsTimeStampUTC write SetAsTimeStampUTC;
  end;

  TcrValues = class(TcrObject)
  private
    function GetCount: NativeInt;
    function GetItem(const AIndex: NativeInt): TcrValue;
  public
    function Find(const AName: TcrString): TcrValue;
    function ByName(const AName: TcrString): TcrValue;

    { properties }
    property Count: NativeInt read GetCount;
    property Items[const I: NativeInt]: TcrValue read GetItem; default;
  end;

  TcrDbFactory = class(TcrObject)
  private
    procedure CreateIntf; override;
  public
    function CreateDbConnectParams: TcrDbConnectParams;
    function CreateDbConnectionPool: TcrDbConnectionPool;

//    function CreateDbConnectParams(
//      var ADbConnectParams: IcrDbConnectParams): WordBool; stdcall;
//    function CreateDbConnectionPool(const AFlwCtrl: IcrFlwCtrl;
//      var ADbConnectionPool: IcrDbConnectionPool): WordBool; stdcall;


  end;

  TcrDbConnectParams = class(TcrObject)
  private
    function GetDbTypeId: Cardinal;
    procedure SetDbTypeId(const ADbTypeId: Cardinal);
    function GetServerPort: Word;
    procedure SetServerPort(const AServerPort: Word);
    function GetServerHost: TcrString;
    procedure SetServerHost(const AServerHost: TcrString);
    function GetDatabase: TcrString;
    procedure SetDatabase(const ADatabase: TcrString);
    function GetSchemaName: TcrString;
    procedure SetSchemaName(const ASchemaName: TcrString);
    function GetLoginUserName: TcrString;
    procedure SetLoginUserName(const ALoginUserName: TcrString);
    function GetLoginPassword: TcrString;
    procedure SetLoginPassword(const ALoginPassword: TcrString);
    function GetRoleName: TcrString;
    procedure SetRoleName(const ARoleName: TcrString);
    function GetCharSetName: TcrString;
    procedure SetCharSetName(const ACharSetName: TcrString);
    function GetCollateName: TcrString;
    procedure SetCollateName(const ACollateName: TcrString);
    function GetOptions: NativeUInt;
    procedure SetOptions(const AOptions: NativeUInt);
    function GetIPVersion: Cardinal;
    procedure SetIPVersion(const AIPVersion: Cardinal);
  public
    procedure LoadFromJsonFile(const AFileName: TcrString);
    procedure SaveToJsonFile(const AFileName: TcrString);

    { properties }
    property DbTypeId: Cardinal read GetDbTypeId write SetDbTypeId;
    property ServerPort: Word read GetServerPort write SetServerPort;
    property ServerHost: TcrString read GetServerHost write SetServerHost;
    property Database: TcrString read GetDatabase write SetDatabase;
    property SchemaName: TcrString read GetSchemaName write SetSchemaName;
    property LoginUserName: TcrString read GetLoginUserName write SetLoginUserName;
    property LoginPassword: TcrString read GetLoginPassword write SetLoginPassword;
    property RoleName: TcrString read GetRoleName write SetRoleName;
    property CharSetName: TcrString read GetCharSetName write SetCharSetName;
    property CollateName: TcrString read GetCollateName write SetCollateName;
    property Options: NativeUInt read GetOptions write SetOptions;
    property IPVersion: Cardinal read GetIPVersion write SetIPVersion;
  end;

  TcrDbConnectionPool = class(TcrObject)
  public
    procedure SetDbConnectParams(const ADbConnectParams: TcrDbConnectParams);

    function Enter: TcrDbConnection;
  end;

  TcrDbConnection = class(TcrObject)
  private
    function GetOwnerPool: TcrDbConnectionPool;
    function GetConnectionCrypt: Cardinal;
    function GetSupportsTimeZone: Boolean;
    function GetIsConnected: Boolean;
    function GetTransactionType: Cardinal;
  public
    procedure Leave;

    function CreateQuery: TcrDbQuery;

    function InTransaction: Boolean;
    procedure BeginTransaction(const AReadOnly: Boolean);
    procedure Rollback;
    procedure Commit;

    function BeginTransactionIf(const AReadOnly: Boolean): Boolean;
    function RollbackIf: Boolean;
    function CommitIf: Boolean;

//    function Connect(const AParams: IcrDbConnectParams): WordBool; stdcall;
    procedure Disconnect;

//    function Disconnect: WordBool; stdcall;

//    function GetConnectParams(var AParams: IcrDbConnectParams): WordBool; stdcall;
//
    procedure Execute(const ACommandText: TcrString);
    function ExecuteWthRowsCount(const ACommandText: TcrString): Int64;

    { properties }
    property OwnerPool: TcrDbConnectionPool read GetOwnerPool;
    property ConnectionCrypt: Cardinal read GetConnectionCrypt;
    property SupportsTimeZone: Boolean read GetSupportsTimeZone;
    property IsConnected: Boolean read GetIsConnected;
    property TransactionType: Cardinal read GetTransactionType;
  end;

  TcrDbQuery = class(TcrObject)
  private
    function GetDbConnection: TcrDbConnection;
    function GetRowsAffected: Int64;
    function GetCommandText: TcrString;
    procedure SetCommandText(const ACommandText: TcrString);
    function GetParams: TcrValues;
    function GetFields: TcrValues;
  public
    function FindParam(const AName: TcrString): TcrValue;
    function ParamByName(const AName: TcrString): TcrValue;

    function FindField(const AName: TcrString): TcrValue;
    function FieldByName(const AName: TcrString): TcrValue;

    procedure Execute;

    procedure Open;
    procedure Close;

    function OpenData: Boolean;
    function IsEmpty: Boolean;
    function MoveNext: Boolean;
    function DumpAllToJson(const AIsPretty: Boolean): TcrString;

    { properties }
    property DbConnection: TcrDbConnection read GetDbConnection;
    property RowsAffected: Int64 read GetRowsAffected;
    property CommandText: TcrString read GetCommandText write SetCommandText;
    property Params: TcrValues read GetParams;
    property Fields: TcrValues read GetFields;
  end;

  TcrCharset = class(TcrObject);

  TcrCharsets = class(TcrObject)
  private
    procedure CreateIntf; override;
  public
    function Utf8U: TcrCharset;
    function Utf8L: TcrCharset;
    function Utf8XmlDFeU: TcrCharset;
    function Utf8XmlDFeL: TcrCharset;
  end;

  TcrEncoding = class(TcrObject);

  TcrEncodings = class(TcrObject)
  private
    procedure CreateIntf; override;
  public
    function Utf8: TcrEncoding;
    function SimpleAnsi: TcrEncoding;
  end;

  TcrXmlItem = class(TcrObject)
  private
    function GetParent: TcrXmlNode;
    function GetPrefixAndName: TcrString;
    function GetName: TcrString;
    function GetPrefix: TcrString;
    function GetContent: TcrString;
    procedure SetContent(const AContent: TcrString);
  public
    function IsAncestral(const AXmlNode: TcrXmlNode): Boolean;
    procedure GetSourcePosLen(var APosition, ALength: Integer);
    function GetFullPath: TcrString; overload;
    function GetFullPath(const ADivisor: TcrString): TcrString; overload;

//function GetItemType(var AItemType: Cardinal): WordBool; stdcall;

    function DigestToBytes(const ARootNode: TcrXmlNode;
      const ACharset: TcrCharset; const AEncoding: TcrEncoding;
      const AXmlDigestTypeId: Cardinal): TBytes;
    function DigestToString(const ARootNode: TcrXmlNode;
      const ACharset: TcrCharset; const AEncoding: TcrEncoding;
      const AXmlDigestTypeId, ADigestEncoding: Cardinal): TcrString;

    function SaveToString(const ACharset: TcrCharset;
      const AOptions: NativeUInt): TcrString;

    procedure SaveToFile(const AFileName: TcrString;
      const AEncoding: TcrEncoding; const ACharset: TcrCharset;
      const AOptions: NativeUInt);

    { properties }
    property Parent: TcrXmlNode read GetParent;
    property PrefixAndName: TcrString read GetPrefixAndName;
    property Name: TcrString read GetName;
    property Prefix: TcrString read GetPrefix;
    property Content: TcrString read GetContent write SetContent;
  end;

  TcrXmlAttribute = class(TcrXmlItem)
  private
    function GetPrevious: TcrXmlAttribute;
    function GetNext: TcrXmlAttribute;
  public
    { properties }
    property Previous: TcrXmlAttribute read GetPrevious;
    property Next: TcrXmlAttribute read GetNext;
  end;

  TcrXmlNode = class(TcrXmlItem)
  private
    function GetPrevious: TcrXmlNode;
    function GetNext: TcrXmlNode;
    function GetPreviousData: TcrXmlNode;
    function GetNextData: TcrXmlNode;

    function GetFirstNodeChild: TcrXmlNode;
    function GetLastNodeChild: TcrXmlNode;
    function GetFirstNodeChildData: TcrXmlNode;
    function GetLastNodeChildData: TcrXmlNode;

    function GetFirstAttribute: TcrXmlAttribute;
    function GetLastAttribute: TcrXmlAttribute;
  public
    procedure AttributesClear;
    procedure NodesClear;

    procedure AddWhiteSpace(const AStringData: TcrString);
    procedure AddComment(const AStringData: TcrString);
    function AddElement(const APrefix, AName: TcrString): TcrXmlNode;
    function AddData(const APrefix, AName: TcrString): TcrXmlNode;
    function AddCData(const APrefix, AName: TcrString): TcrXmlNode;
    function AddOrSetAttribute(
      const APrefix, AName, AContent: TcrString): TcrXmlAttribute;
    function RemoveAttribute(const APrefix, AName: TcrString): Boolean;

    procedure AddOrSetParseAttributes(const ASource: TcrString);

    function FindAttribute(const APrefix, AName: TcrString): TcrXmlAttribute;
    function FindAttributeIgnorePrefix(const AName: TcrString): TcrXmlAttribute;

    function FindNode(const APrefix, AName: TcrString;
      const ASearchTree: Boolean): TcrXmlNode;
    function FindNodes(const APrefix, AName: TcrString;
      const ASearchTree: Boolean; const AMaxCount: NativeInt): TcrXmlNodes;

    function FindNext(const APrefix, AName: TcrString): TcrXmlNode;

    function FindAttributeEx(
      const APrefix, AName, AContent: TcrString): TcrXmlAttribute;

    { properties }
    property Previous: TcrXmlNode read GetPrevious;
    property Next: TcrXmlNode read GetNext;
    property PreviousData: TcrXmlNode read GetPreviousData;
    property NextData: TcrXmlNode read GetNextData;

    property FirstNodeChild: TcrXmlNode read GetFirstNodeChild;
    property LastNodeChild: TcrXmlNode read GetLastNodeChild;
    property FirstNodeChildData: TcrXmlNode read GetFirstNodeChildData;
    property LastNodeChildData: TcrXmlNode read GetLastNodeChildData;

    property FirstAttribute: TcrXmlAttribute read GetFirstAttribute;
    property LastAttribute: TcrXmlAttribute read GetLastAttribute;
  end;

  TcrXmlNodes = class(TcrObject)
  private
    function GetCount: NativeInt;
    function GetItem(const AIndex: NativeInt): TcrXmlNode;
  public
    { properties }
    property Count: NativeInt read GetCount;
    property Items[const I: NativeInt]: TcrXmlNode read GetItem; default;
  end;

  TcrXmlDocument = class(TcrXmlNode)
  public
    procedure LoadFromUCStr(const AUCStrPtr: PWideChar;
      const AUCStrLength: NativeInt; const AOptions: NativeUInt);
    procedure LoadFromString(const AContent: TcrString;
      const AOptions: NativeUInt);

    procedure LoadFromFile(const AFileName: TcrString;
      const AEncoding: TcrEncoding; const AOptions: NativeUInt);
    procedure LoadFromFileUtf8(const AFileName: TcrString;
      const AOptions: NativeUInt);

    procedure LoadFromBuffer(const ABytesPtr: PByte;
      const ABytesCount: NativeInt; const AEncoding: TcrEncoding;
      const AOptions: NativeUInt);

    procedure LoadFromBufferUtf8(const ABytesPtr: PByte;
      const ABytesCount: NativeInt; const AOptions: NativeUInt);
  end;

  TcrXmlFactory = class(TcrObject)
  private
    procedure CreateIntf; override;
  public
    function CreateXmlDocument: TcrXmlDocument;
  end;

procedure crUnknownRaiseLastError(const AIntf: IcrUnknown;
  const AIntfResult: TcrIntfResult; const AAt: Pointer);

implementation

function UserDataId: NativeUInt; inline;
begin
  Exit(NativeUInt(@PtrToNil));
end;

procedure LibKoreBytesAlloc(var ABytes: Pointer;
  const ABytesCount: NativeInt); stdcall;
begin
  SetLength(TBytes(ABytes), ABytesCount);
end;

procedure ICSTP_Clear(
  const AParams: PcrIntfCreateStringTypeParams;
  var AString: TcrIntfStringData); stdcall;
begin
  TcrString(AString) := '';
end;

procedure ICSTP_CreateFromUnicode(
  const AParams: PcrIntfCreateStringTypeParams;
  var AString: TcrIntfStringData;
  const ACharsPtr: PWideChar;
  const ACharsCount: NativeInt); stdcall;
begin
  SetString(TcrString(AString), ACharsPtr, ACharsCount);
end;

procedure ICSTP_CreateFromAnsi(
  const AParams: PcrIntfCreateStringTypeParams;
  var AString: TcrIntfStringData;
  const ACharsPtr: PAnsiChar;
  const ACharsCount: NativeInt); stdcall;
begin
  SetString(TcrString(AString), ACharsPtr, ACharsCount);
end;

function ICSTP_Length(
  const AParams: PcrIntfCreateStringTypeParams;
  const AString: TcrIntfStringData): NativeInt; stdcall;
begin
  Exit(Length(TcrString(AString)));
end;

function ICSTP_GetCharsUnicode(
  const AParams: PcrIntfCreateStringTypeParams;
  const AString: TcrIntfStringData;
  const ACharsPtr: PWideChar;
  const ACharsCount: NativeInt): NativeInt; stdcall;
var
  LCharsCount: NativeInt;
begin
  LCharsCount := Length(TcrString(AString));
  if (LCharsCount > ACharsCount) then
    LCharsCount := ACharsCount;

  if (LCharsCount > 0) then
  begin
    Move(Pointer(AString)^, ACharsPtr^, (LCharsCount * 2));
    Exit(LCharsCount);
  end;

  Exit(0);
end;

function ICSTP_GetCharsAnsi(
  const AParams: PcrIntfCreateStringTypeParams;
  const AString: TcrIntfStringData;
  const ACharsPtr: PAnsiChar;
  const ACharsCount: NativeInt): NativeInt; stdcall;
var
  LCharsCount: NativeInt;
  LAnsiBuffer: AnsiString;
begin
  LCharsCount := Length(TcrString(AString));
  if (LCharsCount > ACharsCount) then
    LCharsCount := ACharsCount;

  if (LCharsCount > 0) then
  begin
    SetString(LAnsiBuffer, PWideChar(AString), LCharsCount);
    Move(Pointer(LAnsiBuffer)^, ACharsPtr^, LCharsCount);
    Exit(LCharsCount);
  end;

  Exit(0);
end;

const
  cICSTP: TcrIntfCreateStringTypeParams = (
    Version: 1;
    UserData: nil;
    Clear: ICSTP_Clear;
    CreateFromUnicode: ICSTP_CreateFromUnicode;
    CreateFromAnsi: ICSTP_CreateFromAnsi;
    Length: ICSTP_Length;
    GetCharsUnicode: ICSTP_GetCharsUnicode;
    GetCharsAnsi: ICSTP_GetCharsAnsi);

  UserIdAction_Disposed = $01;

var
  _ICSTP: TcrIntfStringType;
  gvEncodingUTF8: TcrEncoding;
  gvEncodingSimpleAnsi: TcrEncoding;
  gvEncodings: TcrEncodings;

function ICSTP: TcrIntfStringType;
begin
  Result := _ICSTP;
  if (Result = nil) then
  begin
    crLibKore.DefaultInitialize;
    Result := _ICSTP;
  end;
end;

class procedure crLibKore.DefaultInitialize;
begin
  {$ifdef CPUX86}
  Initialize('LibKore32.dll');
  {$endif CPUX86}

  {$ifdef CPUX64}
  Initialize('LibKore64.dll');
  {$endif CPUX64}
end;

class function crLibKore.EncodingSimpleAnsi: TcrEncoding;
begin
  Result := gvEncodingSimpleAnsi;
  if (Result = nil) then
  begin
    DefaultInitialize;
    Result := gvEncodingSimpleAnsi;
  end;
end;

class function crLibKore.EncodingUtf8: TcrEncoding;
begin
  Result := gvEncodingUTF8;
  if (Result = nil) then
  begin
    DefaultInitialize;
    Result := gvEncodingUTF8;
  end;
end;

class procedure crLibKore.Initialize(const ADllFileName: string);
begin
  _ICSTP := LibKore_CreateStringType(PcrIntfCreateStringTypeParams(@cICSTP)^);

  gvEncodings := TcrEncodings.Create;
  gvEncodingUTF8 := gvEncodings.Utf8;
  gvEncodingSimpleAnsi := gvEncodings.SimpleAnsi;
end;

{ EcrIntfMissing }

constructor EcrIntfMissing.New;
begin
  Create('FIntf é obrigatório');
end;

{ EcrLibKoreDllNotLoaded }

constructor EcrLibKoreDllNotLoaded.New;
begin
  Create('A dll LibKore não foi carregada');
end;

{ EcrLibKoreCreateFactoryFail }

constructor EcrLibKoreCreateFactoryFail.New(
  const ALibKoreClassName: TcrString);
begin
  FLibKoreClassName := ALibKoreClassName;
  CreateFmt('O método CreateFactory falhou ao tentar instanciar um ' +
    '"Factory" a partir da classe ''%s''', [FLibKoreClassName]);
end;

{ EcrLibKoreFailed }

function EcrLibKoreFailed.GetOriginalMessage: TcrString;
begin
  Exit(FOriginalMessage);
end;

function EcrLibKoreFailed.GetOsError: Cardinal;
begin
  Exit(FOsError);
end;

function EcrLibKoreFailed.GetStatusCode: Cardinal;
begin
  Exit(FStatusCode);
end;

constructor EcrLibKoreFailed.New(
  const AOriginalMessage: TcrString;
  const AStatusCode, AOsError: Cardinal;
  const ACodeLocalId: PAnsiChar);
var
  LMessage: TcrString;
begin
  FOriginalMessage := AOriginalMessage;
  FStatusCode := AStatusCode;
  FOsError := AOsError;
  FCodeLocalId := ACodeLocalId;

  if FOriginalMessage.IsEmpty then
    LMessage := 'Erro desconhecido'
  else
    LMessage := FOriginalMessage;

  if (FStatusCode <> 0) then
    LMessage := LMessage + '. (' + IntToStr(FStatusCode) + ')';

  Create(LMessage);
end;

function TcrObjectIntfCallback(
  var AParams: TcrIntfUserCallbackParams): WordBool; stdcall;
var
  LObject: TcrObject;
begin
  try
    LObject := AParams.FUserData;
    case AParams.FIdAction of
      UserIdAction_Disposed: LObject.Disposed;
    end;
  except
    //
  end;

  Exit(True);
end;

function IntfResultIsError(const AIntfResult: TcrIntfResult): Boolean; inline;
begin
  Exit(AIntfResult < 0);
end;

procedure crObjectRaiseLastError(const AIntf: IcrObject;
  const AIntfResult: TcrIntfResult; const AAt: Pointer);
var
  LIntfErrorInfo: IcrErrorInfo;
  LMessage: TcrString;
  LStatusCode, LOsError: Cardinal;
  LCodeLocalId: PAnsiChar;
begin
  if ((AIntf <> nil) and AIntf.GetErrorInfo(AIntfResult, LIntfErrorInfo)
    and (LIntfErrorInfo <> nil)) then
  begin
    LMessage := '';
    LStatusCode := 0;
    LOsError := 0;
    LCodeLocalId := nil;

    LIntfErrorInfo.GetMessage(ICSTP, TcrIntfStringData(LMessage));
    LIntfErrorInfo.GetStatusCode(LStatusCode);
    LIntfErrorInfo.GetOsError(LOsError);
    LIntfErrorInfo.GetCodeLocalId(LCodeLocalId);
    raise EcrLibKoreFailed.New(LMessage,
      LStatusCode, LOsError, LCodeLocalId) at AAt;
  end;

  raise EcrLibKoreUnknown.Create('Erro desconhecido') at AAt;
end;

procedure crUnknownRaiseLastError(const AIntf: IcrUnknown;
  const AIntfResult: TcrIntfResult; const AAt: Pointer);
var
  LIntfObject: IcrObject;
begin
  if (AIntf <> nil) then
    LIntfObject := (AIntf as IcrObject)
  else
    LIntfObject := nil;

  crObjectRaiseLastError(LIntfObject, AIntfResult, AAt);
end;

function GetUserDataByIntf(const AIntf: IcrUnknown;
  var AUserData: Pointer): Boolean;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := AIntf.GetUserData(UserDataId, AUserData);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(AIntf, LIntfResult, ReturnAddress);
  Exit(AUserData <> nil);
end;

function TcrValueByIntf(const AIntf: IcrValue): TcrValue;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrValue.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrValuesByIntf(const AIntf: IcrValues): TcrValues;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrValues.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrConnectionByIntf(const AIntf: IcrDbConnection): TcrDbConnection;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrDbConnection.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrCharsetByIntf(const AIntf: IcrCharset): TcrCharset;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrCharset.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrEncodingByIntf(const AIntf: IcrEncoding): TcrEncoding;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrEncoding.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrXmlAttributeByIntf(const AIntf: IcrXmlAttribute): TcrXmlAttribute;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrXmlAttribute.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrXmlNodeByIntf(const AIntf: IcrXmlNode): TcrXmlNode;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrXmlNode.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrXmlNodesByIntf(const AIntf: IcrXmlNodes): TcrXmlNodes;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrXmlNodes.NewEx(Pointer(AIntf), Result).Create;
end;

function TcrXmlDocumentByIntf(const AIntf: IcrXmlDocument): TcrXmlDocument;
begin
  if (AIntf = nil) then Exit(nil);

  if (not GetUserDataByIntf(AIntf, Pointer(Result))) then
    TcrXmlDocument.NewEx(Pointer(AIntf), Result).Create;
end;

{ TcrObject }

procedure TcrObject.CheckIntfRes(const AIntfResult: TcrIntfResult);
begin
  if IntfResultIsError(AIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(fgIntf), AIntfResult, ReturnAddress);
end;

procedure TcrObject.CreateIntf;
begin
end;

procedure TcrObject.CreateIntfByName(const AName: TcrString);
var
  LIntfObject: IcrObject;
  LIntfResult: TcrIntfResult;
begin
  if (LibKore_CreateFactory(ICSTP, Pointer(AName), IInterface(FIntf))
    and (FIntf <> nil)) then
  begin
    LIntfObject := (IcrUnknown(FIntf) as IcrObject);
    if (LIntfObject <> nil) then
    begin
      LIntfResult := LIntfObject.SetUserData(UserDataId, Self);
      if IntfResultIsError(LIntfResult) then
        crObjectRaiseLastError(LIntfObject, LIntfResult, ReturnAddress);
      LIntfResult := LIntfObject.SetUserCallback(UserDataId, TcrObjectIntfCallback);
      if IntfResultIsError(LIntfResult) then
        crObjectRaiseLastError(LIntfObject, LIntfResult, ReturnAddress);
    end;

    Exit;
  end;

  raise EcrLibKoreCreateFactoryFail.New(AName);
end;

destructor TcrObject.Destroy;
begin
  DestroyIntf;
  inherited Destroy;
end;

procedure TcrObject.DestroyIntf;
var
  LIntf: Pointer;
  LIntfObject: IcrObject;
begin
  repeat
    LIntf := FIntf;
    if (LIntf = nil) then Exit;

    if (AtomicCmpExchange(FIntf, nil, LIntf) = LIntf) then
    begin
      try
        LIntfObject := (IcrUnknown(LIntf) as IcrObject);
        if (LIntfObject <> nil) then
        begin
          LIntfObject.SetUserData(UserDataId, nil);
          LIntfObject.SetUserCallback(UserDataId, nil);
        end;
      finally
        IcrUnknown(LIntf) := nil;
      end;
      Exit;
    end;
  until False;
end;

procedure TcrObject.Disposed;
begin
  DestroyIntf;
  Destroy;
end;

function TcrObject.fgIntf: Pointer;
begin
  Result := FIntf;
  if (Result = nil) then
  begin
    CreateIntf;
    Result := FIntf;

    if (Result = nil) then
      raise EcrIntfMissing.New;
  end;
end;

procedure TcrObject.FreeInstance;
begin
  DestroyIntf;
  inherited FreeInstance;
end;

class function TcrObject.NewEx<T>(const AIntf: Pointer; var AObject: T): T;
begin
  Result := NewExPtr(AIntf);
  AObject := Result;
end;

class function TcrObject.NewExPtr(const AIntf: Pointer): Pointer;
var
  LIntfObject: IcrObject;
  LIntfResult: TcrIntfResult;
begin
  Result := NewInstance;
  IInterface(TcrObject(Result).FIntf) := IInterface(AIntf);

  LIntfObject := (IcrUnknown(AIntf) as IcrObject);
  if (LIntfObject <> nil) then
  begin
    LIntfResult := LIntfObject.SetUserData(UserDataId, Result);
    if IntfResultIsError(LIntfResult) then
      crObjectRaiseLastError(LIntfObject, LIntfResult, ReturnAddress);
    LIntfResult := LIntfObject.SetUserCallback(UserDataId, TcrObjectIntfCallback);
    if IntfResultIsError(LIntfResult) then
      crObjectRaiseLastError(LIntfObject, LIntfResult, ReturnAddress);
  end;
end;

function TcrObject.tgIntf: Pointer;
begin
  if (Self <> nil) then
    Exit(FIntf)
  else
    Exit(nil);
end;

{ TcrStream }

procedure TcrStream.Flush;
begin
  CheckIntfRes(IcrStream(fgIntf).Flush);
end;

function TcrStream.GetSize: Int64;
begin
  CheckIntfRes(IcrStream(fgIntf).GetSize(Result));
end;

function TcrStream.Read(ABytesPtr: PByte; ABytesCount: NativeInt): NativeInt;
begin
  CheckIntfRes(IcrStream(fgIntf).Read(ABytesPtr, ABytesCount, Result));
end;

function TcrStream.Seek(AOffset: Int64; AOrigin: Integer): Int64;
begin
  CheckIntfRes(IcrStream(fgIntf).Seek(AOffset, AOrigin, Result));
end;

procedure TcrStream.SetSize(const ASize: Int64);
begin
  CheckIntfRes(IcrStream(fgIntf).SetSize(ASize));
end;

function TcrStream.Write(ABytesPtr: PByte; ABytesCount: NativeInt): NativeInt;
begin
  CheckIntfRes(IcrStream(fgIntf).Write(ABytesPtr, ABytesCount, Result));
end;

{ TcrConsole }

procedure TcrConsole.AlternativeBuffer(const AOnOff: Boolean);
begin
  CheckIntfRes(IcrConsole(fgIntf).AlternativeBuffer(AOnOff));
end;

procedure TcrConsole.ClearLine(const AIdType: Byte);
begin
  CheckIntfRes(IcrConsole(fgIntf).ClearLine(AIdType));
end;

procedure TcrConsole.ClearScreen(const AIdType: Byte);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).ClearScreen(AIdType);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorBack(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorBack(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorDown(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorDown(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorForward(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorForward(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorHorzAbsolute(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorHorzAbsolute(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorNextLine(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorNextLine(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorPreviousLine(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorPreviousLine(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.CursorUp(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).CursorUp(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.DeviceStatusReport;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).DeviceStatusReport;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.RestoreCurrentCursorPosition;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).RestoreCurrentCursorPosition;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.RestoreScreen;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).RestoreScreen;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SaveCurrentCursorPosition;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SaveCurrentCursorPosition;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SaveScreen;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SaveScreen;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.ScrollDown(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).ScrollDown(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.ScrollUp(const ACount: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).ScrollUp(ACount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SelectGraphicRenditionRGB(const AId, ARGB: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SelectGraphicRenditionRGB(AId, ARGB);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SetBracketPasteMode(const AOnOff: Boolean);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SetBracketPasteMode(AOnOff);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SetCursorPosition(const AX, AY: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SetCursorPosition(AX, AY);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SetCursorVisible(const AOnOff: Boolean);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SetCursorVisible(AOnOff);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SetEnabledAuxPort(const AOnOff: Boolean);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SetEnabledAuxPort(AOnOff);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.SetTitle(const ATitle: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).SetTitle(ICSTP, Pointer(ATitle));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsole.Text(const AText: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsole(fgIntf).Text(ICSTP, Pointer(AText));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrConsoleUtils }

procedure TcrConsoleUtils.AddFromAnsiEscCodeBuffer(
  const AConsole: TcrConsole; const ABytesPtr: PByte;
  const ABytesCount: Integer);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleUtils(fgIntf).AddFromAnsiEscCodeBuffer(
    IcrConsole(AConsole.fgIntf), ABytesPtr, ABytesCount);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrConsoleUtils.AddFromAnsiEscCodeFile(
  const AConsole: TcrConsole; const AFileName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleUtils(fgIntf).AddFromAnsiEscCodeFile(
    IcrConsole(AConsole.fgIntf), ICSTP, Pointer(AFileName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrConsoleFactory }

function TcrConsoleFactory.CreateByAddLine(
  const AAddLine: TcrConsoleAddLineEvent): TcrConsole;
var
  LIntf: IcrConsole;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleFactory(fgIntf).CreateByAddLine(
    TMethod(AAddLine).Code, TMethod(AAddLine).Data, ICSTP, LIntf);

  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrConsole.NewEx(LIntf, Result).Create;
end;

function TcrConsoleFactory.CreateByOutHandle(
  const AOutHandle: THandle): TcrConsole;
var
  LIntf: IcrConsole;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleFactory(fgIntf)
    .CreateByOutHandle(AOutHandle, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrConsole.NewEx(LIntf, Result).Create;
end;

function TcrConsoleFactory.CreateByProcessId(
  const AProcessId: Cardinal): TcrConsole;
var
  LIntf: IcrConsole;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleFactory(fgIntf)
    .CreateByProcessId(AProcessId, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrConsole.NewEx(LIntf, Result).Create;
end;

function TcrConsoleFactory.CreateCacheByWnd(const AWnd: THandle;
  const AInnerConsole: TcrConsole): TcrConsole;
var
  LIntf: IcrConsole;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleFactory(fgIntf).CreateCacheByWnd(
    AWnd, IcrConsole(AInnerConsole.fgIntf), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrConsole.NewEx(LIntf, Result).Create;
end;

function TcrConsoleFactory.CreateDefault: TcrConsole;
var
  LIntf: IcrConsole;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleFactory(fgIntf).CreateDefault(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrConsole.NewEx(LIntf, Result).Create;
end;

procedure TcrConsoleFactory.CreateIntf;
begin
  CreateIntfByName('Console');
end;

function TcrConsoleFactory.CreateUtils: TcrConsoleUtils;
var
  LIntf: IcrConsoleUtils;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrConsoleFactory(fgIntf).CreateUtils(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrConsoleUtils.NewEx(LIntf, Result).Create;
end;

{ TcrWinPseudoConsole }

procedure TcrWinPseudoConsole.BeginExecute;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsole(fgIntf).BeginExecute;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsole.Execute;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsole(fgIntf).Execute;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrWinPseudoConsole.ExitCode: Cardinal;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsole(fgIntf).GetExitCode(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrWinPseudoConsole.IsTerminated: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsole(fgIntf).IsTerminated(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

{ TcrWinPseudoConsoleFactory }

procedure TcrWinPseudoConsoleFactory.CreateIntf;
begin
  CreateIntfByName('WinPseudoConsole');
end;

function TcrWinPseudoConsoleFactory.CreateObject: TcrWinPseudoConsole;
var
  LIntf: IcrWinPseudoConsole;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf).Create(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrWinPseudoConsole.NewEx(LIntf, Result).Create;
end;

procedure TcrWinPseudoConsoleFactory.RemoveAdministrativePermissions;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .RemoveAdministrativePermissions;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetCommandLine(
  const ACommandLine: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .SetCommandLine(ICSTP, Pointer(ACommandLine));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetConsole(const AConsole: TcrConsole);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .SetConsole(IcrConsole(AConsole.fgIntf));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetDirectory(const ADirectory: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .SetDirectory(ICSTP, Pointer(ADirectory));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetOsLogin(
  const AUserName, ADomain, APassword: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf).SetOsLogin(
    ICSTP, Pointer(AUserName), Pointer(ADomain), Pointer(APassword));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetRecordingFileName(
  const AFileName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .SetRecordingFileName(ICSTP, Pointer(AFileName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetRecordingStream(
  const AStream: TcrStream);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .SetRecordingStream(IcrStream(AStream.fgIntf));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrWinPseudoConsoleFactory.SetVariable(
  const AName, AValue: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrWinPseudoConsoleFactory(fgIntf)
    .SetVariable(ICSTP, Pointer(AName), Pointer(AValue));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrValue }

procedure TcrValue.Clear;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).Clear;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsBoolean: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsBoolean(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrValue.GetAsBytes: TBytes;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsBytes(
    Pointer(Result), LibKoreBytesAlloc);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsChar: TcrChar;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsChar(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsCurrency: Currency;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsCurrency(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsDate: TcrDate;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsDate(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsReal32: Single;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsReal32(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsReal64: Double;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsReal64(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsSInt16: Int16;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsSInt16(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsSInt32: Int32;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsSInt32(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsSInt64: Int64;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsSInt64(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsSInt8: Int8;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsSInt8(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsString: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsString(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsTime: TcrTime;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsTime(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsTimeStamp: TcrTimeStamp;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsTimeStamp(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsTimeStampUTC: TcrTimeStampUTC;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsTimeStampUTC(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsTimeUTC: TcrTimeUTC;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsTimeUTC(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsUInt16: UInt16;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsUInt16(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsUInt32: UInt32;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsUInt32(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsUInt64: UInt64;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsUInt64(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsUInt8: UInt8;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsUInt8(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetAsUUID: TGUID;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetAsUUID(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetDataTypeId: Cardinal;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetDataTypeId(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValue.GetIsNull: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetIsNull(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrValue.GetName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).GetName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsBoolean(const AValue: Boolean);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsBoolean(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsBytes(const AValue: TBytes);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsBytes(Pointer(AValue), Length(AValue));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsChar(const AValue: TcrChar);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsChar(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsCurrency(const AValue: Currency);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsCurrency(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsDate(const AValue: TcrDate);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsDate(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsReal32(const AValue: Single);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsReal32(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsReal64(const AValue: Double);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsReal64(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsSInt16(const AValue: Int16);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsSInt16(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsSInt32(const AValue: Int32);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsSInt32(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsSInt64(const AValue: Int64);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsSInt64(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsSInt8(const AValue: Int8);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsSInt8(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsString(const AValue: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsString(ICSTP, Pointer(AValue));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsTime(const AValue: TcrTime);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsTime(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsTimeStamp(const AValue: TcrTimeStamp);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsTimeStamp(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsTimeStampUTC(const AValue: TcrTimeStampUTC);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsTimeStampUTC(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsTimeUTC(const AValue: TcrTimeUTC);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsTimeUTC(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsUInt16(const AValue: UInt16);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsUInt16(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsUInt32(const AValue: UInt32);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsUInt32(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsUInt64(const AValue: UInt64);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsUInt64(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsUInt8(const AValue: UInt8);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsUInt8(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrValue.SetAsUUID(const AValue: TGUID);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValue(fgIntf).SetAsUUID(AValue);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrValues }

function TcrValues.ByName(const AName: TcrString): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValues(fgIntf).ByName(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

function TcrValues.Find(const AName: TcrString): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValues(fgIntf).Find(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

function TcrValues.GetCount: NativeInt;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValues(fgIntf).GetCount(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrValues.GetItem(const AIndex: NativeInt): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrValues(fgIntf).GetItem(AIndex, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

{ TcrDbFactory }

function TcrDbFactory.CreateDbConnectionPool: TcrDbConnectionPool;
var
  LIntf: IcrDbConnectionPool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbFactory(fgIntf).CreateDbConnectionPool(nil, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrDbConnectionPool.NewEx(LIntf, Result).Create;
end;

function TcrDbFactory.CreateDbConnectParams: TcrDbConnectParams;
var
  LIntf: IcrDbConnectParams;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbFactory(fgIntf).CreateDbConnectParams(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrDbConnectParams.NewEx(LIntf, Result).Create;
end;

procedure TcrDbFactory.CreateIntf;
begin
  CreateIntfByName('Db');
end;

{ TcrDbConnectParams }

function TcrDbConnectParams.GetCharSetName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetCharSetName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetCollateName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetCollateName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetDatabase: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetDatabase(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetDbTypeId: Cardinal;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).GetDbTypeId(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetIPVersion: Cardinal;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).GetIPVersion(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetLoginPassword: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetLoginPassword(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetLoginUserName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetLoginUserName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetOptions: NativeUInt;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).GetOptions(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetRoleName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetRoleName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetSchemaName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetSchemaName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetServerHost: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .GetServerHost(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnectParams.GetServerPort: Word;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).GetServerPort(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.LoadFromJsonFile(const AFileName: TcrString);
begin
  CheckIntfRes(IcrDbConnectParams(fgIntf)
    .LoadFromJsonFile(ICSTP, Pointer(AFileName)));
end;

procedure TcrDbConnectParams.SaveToJsonFile(const AFileName: TcrString);
begin
  CheckIntfRes(IcrDbConnectParams(fgIntf)
    .SaveToJsonFile(ICSTP, Pointer(AFileName)));
end;

procedure TcrDbConnectParams.SetCharSetName(const ACharSetName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetCharSetName(ICSTP, Pointer(ACharSetName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetCollateName(const ACollateName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetCollateName(ICSTP, Pointer(ACollateName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetDatabase(const ADatabase: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetDatabase(ICSTP, Pointer(ADatabase));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetDbTypeId(const ADbTypeId: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).SetDbTypeId(ADbTypeId);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetIPVersion(const AIPVersion: Cardinal);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).SetIPVersion(AIPVersion);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetLoginPassword(const ALoginPassword: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetLoginPassword(ICSTP, Pointer(ALoginPassword));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetLoginUserName(const ALoginUserName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetLoginUserName(ICSTP, Pointer(ALoginUserName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetOptions(const AOptions: NativeUInt);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).SetOptions(AOptions);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetRoleName(const ARoleName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetRoleName(ICSTP, Pointer(ARoleName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetSchemaName(const ASchemaName: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetSchemaName(ICSTP, Pointer(ASchemaName));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetServerHost(const AServerHost: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf)
    .SetServerHost(ICSTP, Pointer(AServerHost));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnectParams.SetServerPort(const AServerPort: Word);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectParams(fgIntf).SetServerPort(AServerPort);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrDbConnectionPool }

function TcrDbConnectionPool.Enter: TcrDbConnection;
var
  LIntf: IcrDbConnection;
  LUserData: Pointer;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectionPool(fgIntf).Enter(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  LIntfResult := LIntf.GetUserData(UserDataId, LUserData);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  if (LUserData = nil) then
    TcrDbConnection.NewEx(LIntf, Result).Create
  else
    Result := LUserData;
end;

procedure TcrDbConnectionPool.SetDbConnectParams(
  const ADbConnectParams: TcrDbConnectParams);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnectionPool(fgIntf)
    .SetDbConnectParams(IcrDbConnectParams(ADbConnectParams.fgIntf));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrDbConnection }

procedure TcrDbConnection.BeginTransaction(const AReadOnly: Boolean);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).BeginTransaction(AReadOnly);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.BeginTransactionIf(const AReadOnly: Boolean): Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf)
    .BeginTransactionIf(AReadOnly, LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

procedure TcrDbConnection.Commit;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).Commit;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.CommitIf: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).CommitIf(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrDbConnection.CreateQuery: TcrDbQuery;
var
  LIntf: IcrDbQuery;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).CreateQuery(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  TcrDbQuery.NewEx(LIntf, Result).Create;
end;

procedure TcrDbConnection.Disconnect;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).Disconnect;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnection.Execute(const ACommandText: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf)
    .Execute(ICSTP, Pointer(ACommandText));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.ExecuteWthRowsCount(
  const ACommandText: TcrString): Int64;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf)
    .ExecuteWthRowsCount(ICSTP, Pointer(ACommandText), Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.GetConnectionCrypt: Cardinal;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).GetConnectionCrypt(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.GetIsConnected: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).GetIsConnected(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrDbConnection.GetOwnerPool: TcrDbConnectionPool;
var
  LIntf: IcrDbConnectionPool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).GetOwnerPool(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Result := nil;
  if (LIntf <> nil) then
  begin
    LIntfResult := LIntf.GetUserData(UserDataId, Pointer(Result));
    if IntfResultIsError(LIntfResult) then
      crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
  end;
end;

function TcrDbConnection.GetSupportsTimeZone: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).GetSupportsTimeZone(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrDbConnection.GetTransactionType: Cardinal;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).GetTransactionType(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.InTransaction: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).GetInTransaction(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
  Exit(LReturn);
end;

procedure TcrDbConnection.Leave;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).Leave;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrDbConnection.Rollback;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).Rollback;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbConnection.RollbackIf: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbConnection(fgIntf).RollbackIf(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

{ TcrDbQuery }

procedure TcrDbQuery.Close;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).Close;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbQuery.DumpAllToJson(const AIsPretty: Boolean): TcrString;
begin
  CheckIntfRes(IcrDbQuery(fgIntf).DumpAllToJson(
    ICSTP, Pointer(Result), AIsPretty));
end;

procedure TcrDbQuery.Execute;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).Execute;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbQuery.FieldByName(const AName: TcrString): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).FieldByName(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

function TcrDbQuery.FindField(const AName: TcrString): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).FindField(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

function TcrDbQuery.FindParam(const AName: TcrString): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).FindParam(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

function TcrDbQuery.GetCommandText: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).GetCommandText(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbQuery.GetDbConnection: TcrDbConnection;
var
  LIntf: IcrDbConnection;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).GetDbConnection(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrConnectionByIntf(LIntf));
end;

function TcrDbQuery.GetFields: TcrValues;
var
  LIntf: IcrValues;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).GetFields(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValuesByIntf(LIntf));
end;

function TcrDbQuery.GetParams: TcrValues;
var
  LIntf: IcrValues;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).GetParams(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValuesByIntf(LIntf));
end;

function TcrDbQuery.GetRowsAffected: Int64;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).GetRowsAffected(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbQuery.IsEmpty: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).GetIsEmpty(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrDbQuery.MoveNext: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).MoveNext(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

procedure TcrDbQuery.Open;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).Open;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrDbQuery.OpenData: Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).OpenData(LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

function TcrDbQuery.ParamByName(const AName: TcrString): TcrValue;
var
  LIntf: IcrValue;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf).ParamByName(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrValueByIntf(LIntf));
end;

procedure TcrDbQuery.SetCommandText(const ACommandText: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrDbQuery(fgIntf)
    .SetCommandText(ICSTP, Pointer(ACommandText));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrCharsets }

procedure TcrCharsets.CreateIntf;
begin
  CreateIntfByName('Charset');
end;

function TcrCharsets.Utf8L: TcrCharset;
var
  LIntf: IcrCharset;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrCharsets(fgIntf).Utf8L(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrCharsetByIntf(LIntf));
end;

function TcrCharsets.Utf8U: TcrCharset;
var
  LIntf: IcrCharset;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrCharsets(fgIntf).Utf8U(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrCharsetByIntf(LIntf));
end;

function TcrCharsets.Utf8XmlDFeL: TcrCharset;
var
  LIntf: IcrCharset;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrCharsets(fgIntf).Utf8XmlDFeL(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrCharsetByIntf(LIntf));
end;

function TcrCharsets.Utf8XmlDFeU: TcrCharset;
var
  LIntf: IcrCharset;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrCharsets(fgIntf).Utf8XmlDFeU(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrCharsetByIntf(LIntf));
end;

{ TcrEncodings }

procedure TcrEncodings.CreateIntf;
begin
  CreateIntfByName('Encoding');
end;

function TcrEncodings.SimpleAnsi: TcrEncoding;
var
  LIntf: IcrEncoding;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrEncodings(fgIntf).SimpleAnsi(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrEncodingByIntf(LIntf));
end;

function TcrEncodings.Utf8: TcrEncoding;
var
  LIntf: IcrEncoding;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrEncodings(fgIntf).Utf8(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrEncodingByIntf(LIntf));
end;

{ TcrXmlItem }

function TcrXmlItem.DigestToBytes(const ARootNode: TcrXmlNode;
  const ACharset: TcrCharset; const AEncoding: TcrEncoding;
  const AXmlDigestTypeId: Cardinal): TBytes;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).DigestToBytes(
    IcrXmlNode(ARootNode.tgIntf), IcrCharset(ACharset.tgIntf),
    IcrEncoding(AEncoding.tgIntf), AXmlDigestTypeId,
    Pointer(Result), LibKoreBytesAlloc);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.DigestToString(const ARootNode: TcrXmlNode;
  const ACharset: TcrCharset; const AEncoding: TcrEncoding;
  const AXmlDigestTypeId, ADigestEncoding: Cardinal): TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).DigestToString(
    IcrXmlNode(ARootNode.tgIntf), IcrCharset(ACharset.tgIntf),
    IcrEncoding(AEncoding.tgIntf), AXmlDigestTypeId, ADigestEncoding,
    ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.GetContent: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetContent(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.GetFullPath: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetFullPath(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.GetFullPath(const ADivisor: TcrString): TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetFullPath2(
    ICSTP, Pointer(ADivisor), Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.GetName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.GetParent: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetParent(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlItem.GetPrefix: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetPrefix(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.GetPrefixAndName: TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetPrefixAndName(ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlItem.GetSourcePosLen(var APosition, ALength: Integer);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).GetSourcePosLen(APosition, ALength);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.IsAncestral(const AXmlNode: TcrXmlNode): Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).IsAncestral(
    IcrXmlNode(AXmlNode.fgIntf), LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

procedure TcrXmlItem.SaveToFile(const AFileName: TcrString;
  const AEncoding: TcrEncoding; const ACharset: TcrCharset;
  const AOptions: NativeUInt);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).SaveToFile(ICSTP, Pointer(AFileName),
    IcrEncoding(AEncoding.tgIntf), IcrCharset(ACharset.tgIntf), AOptions);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlItem.SaveToString(const ACharset: TcrCharset;
  const AOptions: NativeUInt): TcrString;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).SaveToString(
    IcrCharset(ACharset.tgIntf), AOptions, ICSTP, Pointer(Result));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlItem.SetContent(const AContent: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlItem(fgIntf).SetContent(ICSTP, Pointer(AContent));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrXmlAttribute }

function TcrXmlAttribute.GetNext: TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlAttribute(fgIntf).GetNext(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

function TcrXmlAttribute.GetPrevious: TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlAttribute(fgIntf).GetPrevious(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

{ TcrXmlNode }

function TcrXmlNode.AddCData(const APrefix, AName: TcrString): TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .AddCData(ICSTP, Pointer(APrefix), Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

procedure TcrXmlNode.AddComment(const AStringData: TcrString);
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .AddComment(ICSTP, Pointer(AStringData), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlNode.AddData(const APrefix, AName: TcrString): TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .AddData(ICSTP, Pointer(APrefix), Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.AddElement(const APrefix, AName: TcrString): TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .AddElement(ICSTP, Pointer(APrefix), Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.AddOrSetAttribute(
  const APrefix, AName, AContent: TcrString): TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).AddOrSetAttribute(ICSTP, Pointer(APrefix),
    Pointer(AName), Pointer(AContent), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

procedure TcrXmlNode.AddOrSetParseAttributes(const ASource: TcrString);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .AddOrSetParseAttributes(ICSTP, Pointer(ASource));
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlNode.AddWhiteSpace(const AStringData: TcrString);
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .AddWhiteSpace(ICSTP, Pointer(AStringData), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlNode.AttributesClear;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).AttributesClear;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlNode.FindAttribute(
  const APrefix, AName: TcrString): TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).FindAttribute(ICSTP,
    Pointer(APrefix), Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

function TcrXmlNode.FindAttributeEx(
  const APrefix, AName, AContent: TcrString): TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).FindAttributeEx(ICSTP,
    Pointer(APrefix), Pointer(AName), Pointer(AContent), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

function TcrXmlNode.FindAttributeIgnorePrefix(
  const AName: TcrString): TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf)
    .FindAttributeIgnorePrefix(ICSTP, Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

function TcrXmlNode.FindNext(const APrefix, AName: TcrString): TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).FindNext(
    ICSTP, Pointer(APrefix), Pointer(AName), LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.FindNode(const APrefix, AName: TcrString;
  const ASearchTree: Boolean): TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).FindNode(ICSTP, Pointer(APrefix),
    Pointer(AName), ASearchTree, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.FindNodes(const APrefix, AName: TcrString;
  const ASearchTree: Boolean; const AMaxCount: NativeInt): TcrXmlNodes;
var
  LIntf: IcrXmlNodes;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).FindNodes(ICSTP, Pointer(APrefix),
    Pointer(AName), ASearchTree, AMaxCount, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodesByIntf(LIntf));
end;

function TcrXmlNode.GetFirstAttribute: TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetFirstAttribute(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

function TcrXmlNode.GetFirstNodeChild: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetFirstNodeChild(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetFirstNodeChildData: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetFirstNodeChildData(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetLastAttribute: TcrXmlAttribute;
var
  LIntf: IcrXmlAttribute;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetLastAttribute(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlAttributeByIntf(LIntf));
end;

function TcrXmlNode.GetLastNodeChild: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetLastNodeChild(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetLastNodeChildData: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetLastNodeChildData(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetNext: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetNext(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetNextData: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetNextData(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetPrevious: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetPrevious(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

function TcrXmlNode.GetPreviousData: TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).GetPreviousData(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

procedure TcrXmlNode.NodesClear;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).NodesClear;
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlNode.RemoveAttribute(const APrefix, AName: TcrString): Boolean;
var
  LReturn: WordBool;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNode(fgIntf).RemoveAttribute(
    ICSTP, Pointer(APrefix), Pointer(AName), LReturn);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(LReturn);
end;

{ TcrXmlNodes }

function TcrXmlNodes.GetCount: NativeInt;
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlNodes(fgIntf).GetCount(Result);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

function TcrXmlNodes.GetItem(const AIndex: NativeInt): TcrXmlNode;
var
  LIntf: IcrXmlNode;
  LIntfResult: TcrIntfResult;
begin
  LIntfresult := IcrXmlNodes(fgIntf).GetItem(AIndex, LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlNodeByIntf(LIntf));
end;

{ TcrXmlDocument }

procedure TcrXmlDocument.LoadFromBuffer(const ABytesPtr: PByte;
  const ABytesCount: NativeInt; const AEncoding: TcrEncoding;
  const AOptions: NativeUInt);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlDocument(fgIntf).LoadFromBuffer(ABytesPtr,
    ABytesCount, IcrEncoding(AEncoding.tgIntf), AOptions);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlDocument.LoadFromBufferUtf8(const ABytesPtr: PByte;
  const ABytesCount: NativeInt; const AOptions: NativeUInt);
begin
  LoadFromBuffer(ABytesPtr, ABytesCount, crLibKore.EncodingUtf8, AOptions);
end;

procedure TcrXmlDocument.LoadFromFile(const AFileName: TcrString;
  const AEncoding: TcrEncoding; const AOptions: NativeUInt);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlDocument(fgIntf).LoadFromFile(ICSTP,
    Pointer(AFileName), IcrEncoding(AEncoding.tgIntf), AOptions);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlDocument.LoadFromFileUtf8(const AFileName: TcrString;
  const AOptions: NativeUInt);
begin
  LoadFromFile(AFileName, crLibKore.EncodingUtf8, AOptions);
end;

procedure TcrXmlDocument.LoadFromString(const AContent: TcrString;
  const AOptions: NativeUInt);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlDocument(fgIntf).LoadFromString(
    ICSTP, Pointer(AContent), AOptions);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

procedure TcrXmlDocument.LoadFromUCStr(const AUCStrPtr: PWideChar;
  const AUCStrLength: NativeInt; const AOptions: NativeUInt);
var
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlDocument(fgIntf)
    .LoadFromUCStr(AUCStrPtr, AUCStrLength, AOptions);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);
end;

{ TcrXmlFactory }

procedure TcrXmlFactory.CreateIntf;
begin
  CreateIntfByName('Xml');
end;

function TcrXmlFactory.CreateXmlDocument: TcrXmlDocument;
var
  LIntf: IcrXmlDocument;
  LIntfResult: TcrIntfResult;
begin
  LIntfResult := IcrXmlFactory(fgIntf).CreateXmlDocument(LIntf);
  if IntfResultIsError(LIntfResult) then
    crUnknownRaiseLastError(IcrUnknown(FIntf), LIntfResult, ReturnAddress);

  Exit(TcrXmlDocumentByIntf(LIntf));
end;

initialization
finalization
  FreeAndNil(gvEncodingUTF8);
  FreeAndNil(gvEncodingSimpleAnsi);
  FreeAndNil(gvEncodings);

end.
