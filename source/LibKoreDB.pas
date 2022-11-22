unit LibKoreDB;

interface

uses
  System.SysUtils,
  System.Classes,
  LibKoreIntfs,
  LibKoreUtils,
  LibKoreValue;

type
  TDbConnectParams = class;
  TDbConnectionPool = class;
  TDbConnection = class;
  TDbQuery = class;

  TDbConnectParams = class(TcrObject)
  private
    FIntf: IcrDbConnectParams;

    function GetDbTypeId: Cardinal;
    procedure SetDbTypeId(const ADbTypeId: Cardinal);
    function GetServerPort: Word;
    procedure SetServerPort(const AServerPort: Word);
    function GetServerHost: string;
    procedure SetServerHost(const AServerHost: string);
    function GetDatabase: string;
    procedure SetDatabase(const ADatabase: string);
    function GetSchemaName: string;
    procedure SetSchemaName(const ASchemaName: string);
    function GetLoginUserName: string;
    procedure SetLoginUserName(const ALoginUserName: string);
    function GetLoginPassword: string;
    procedure SetLoginPassword(const ALoginPassword: string);
    function GetRoleName: string;
    procedure SetRoleName(const ARoleName: string);
    function GetCharSetName: string;
    procedure SetCharSetName(const ACharSetName: string);
    function GetCollateName: string;
    procedure SetCollateName(const ACollateName: string);
    function GetOptions: NativeUInt;
    procedure SetOptions(const AOptions: NativeUInt);
    function GetIPVersion: Cardinal;
    procedure SetIPVersion(const AIPVersion: Cardinal);
  public
    { cdtor }
    constructor Create;

    property DbTypeId: Cardinal read GetDbTypeId write SetDbTypeId;
    property ServerPort: Word read GetServerPort write SetServerPort;
    property ServerHost: string read GetServerHost write SetServerHost;
    property Database: string read GetDatabase write SetDatabase;
    property SchemaName: string read GetSchemaName write SetSchemaName;
    property LoginUserName: string read GetLoginUserName write SetLoginUserName;
    property LoginPassword: string read GetLoginPassword write SetLoginPassword;
    property RoleName: string read GetRoleName write SetRoleName;
    property CharSetName: string read GetCharSetName write SetCharSetName;
    property CollateName: string read GetCollateName write SetCollateName;
    property Options: NativeUInt read GetOptions write SetOptions;
    property IPVersion: Cardinal read GetIPVersion write SetIPVersion;
  end;

  TDbConnectionPool = class(TcrObject)
  private
    FIntf: IcrDbConnectionPool;
  protected
    { Override }
    procedure Disposed; override;
  public
    function Enter: TDbConnection;
    procedure SetDbConnectParams(const ADbConnectParams: TDbConnectParams);

    { cdtor }
    constructor Create;
    destructor Destroy; override;
  end;

  TDbConnection = class(TcrObject)
  private
    FIntf: IcrDbConnection;
  protected
    { Override }
    procedure Disposed; override;
  public
    procedure Leave;

    function CreateQuery: TDbQuery;

    function IdCrypt: Cardinal;
    function SupportsTimeZone: Boolean;

    function TransactionType: Cardinal;
    function InTransaction: Boolean;
    procedure BeginTransaction(const AReadOnly: Boolean);
    procedure Rollback;
    procedure Commit;

    function BeginTransactionIf(const AReadOnly: Boolean): Boolean;
    function RollbackIf: Boolean;
    function CommitIf: Boolean;

    function IsConnected: Boolean;

    procedure Execute(const ACommandText: string);
    function ExecuteWthRowsCount(const ACommandText: string): Int64;

    { cdtor }
    constructor Create(const AIntf: IcrDbConnection);
    destructor Destroy; override;
  end;

  TDbQuery = class(TcrObject)
  private
    FIntf: IcrDbQuery;
    function GetRowsAffected: Int64;
    function GetCommandText: string;
    procedure SetCommandText(const ACommandText: string);
    function GetParams: TValues;
    function GetFields: TValues;
    function ValueByIntf(const AIntf: IcrValue): TValue;
  protected
    { Override }
    procedure Disposed; override;
  public
    { cdtor }
    constructor Create(const AIntf: IcrDbQuery);

    function FindParam(const AName: string): TValue;
    function ParamByName(const AName: string): TValue;

    function FindField(const AName: string): TValue;
    function FieldByName(const AName: string): TValue;

    procedure Execute;
    procedure Open;
    procedure Close;

    function OpenData: Boolean;
    function IsEmpty: Boolean;
    function Next: Boolean;

    { properties }
    property RowsAffected: Int64 read GetRowsAffected;
    property CommandText: string read GetCommandText write SetCommandText;
    property Params: TValues read GetParams;
    property Fields: TValues read GetFields;
  end;

implementation

{ TDbConnectParams }

constructor TDbConnectParams.Create;
var
  LFactory: IcrDbFactory;
begin
  LibKoreCreateFactory('Db', IInterface(LFactory));
  if (not LFactory.CreateDbConnectParams(FIntf)) then
    LibKoreFailed(LFactory);
end;

function TDbConnectParams.GetCharSetName: string;
var
  T: WideString;
begin
  if (not FIntf.GetCharSetName(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetCollateName: string;
var
  T: WideString;
begin
  if (not FIntf.GetCollateName(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetDatabase: string;
var
  T: WideString;
begin
  if (not FIntf.GetDatabase(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetDbTypeId: Cardinal;
begin
  if (not FIntf.GetDbTypeId(Result)) then
    LibKoreFailed(FIntf);
end;

function TDbConnectParams.GetIPVersion: Cardinal;
begin
  if (not FIntf.GetIPVersion(Result)) then
    LibKoreFailed(FIntf);
end;

function TDbConnectParams.GetLoginPassword: string;
var
  T: WideString;
begin
  if (not FIntf.GetLoginPassword(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetLoginUserName: string;
var
  T: WideString;
begin
  if (not FIntf.GetLoginUserName(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetOptions: NativeUInt;
begin
  if (not FIntf.GetOptions(Result)) then
    LibKoreFailed(FIntf);
end;

function TDbConnectParams.GetRoleName: string;
var
  T: WideString;
begin
  if (not FIntf.GetRoleName(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetSchemaName: string;
var
  T: WideString;
begin
  if (not FIntf.GetSchemaName(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetServerHost: string;
var
  T: WideString;
begin
  if (not FIntf.GetServerHost(nil, T)) then
    LibKoreFailed(FIntf);
  Result := T;
end;

function TDbConnectParams.GetServerPort: Word;
begin
  if (not FIntf.GetServerPort(Result)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetCharSetName(const ACharSetName: string);
begin
  if (not FIntf.SetCharSetName(nil, ACharSetName)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetCollateName(const ACollateName: string);
begin
  if (not FIntf.SetCollateName(nil, ACollateName)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetDatabase(const ADatabase: string);
begin
  if (not FIntf.SetDatabase(nil, ADatabase)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetDbTypeId(const ADbTypeId: Cardinal);
begin
  if (not FIntf.SetDbTypeId(ADbTypeId)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetIPVersion(const AIPVersion: Cardinal);
begin
  if (not FIntf.SetIPVersion(AIPVersion)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetLoginPassword(const ALoginPassword: string);
begin
  if (not FIntf.SetLoginPassword(nil, ALoginPassword)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetLoginUserName(const ALoginUserName: string);
begin
  if (not FIntf.SetLoginUserName(nil, ALoginUserName)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetOptions(const AOptions: NativeUInt);
begin
  if (not FIntf.SetOptions(AOptions)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetRoleName(const ARoleName: string);
begin
  if (not FIntf.SetRoleName(nil, ARoleName)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetSchemaName(const ASchemaName: string);
begin
  if (not FIntf.SetSchemaName(nil, ASchemaName)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetServerHost(const AServerHost: string);
begin
  if (not FIntf.SetServerHost(nil, AServerHost)) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnectParams.SetServerPort(const AServerPort: Word);
begin
  if (not FIntf.SetServerPort(AServerPort)) then
    LibKoreFailed(FIntf);
end;

{ TDbConnectionPool }

constructor TDbConnectionPool.Create;
var
  LFactory: IcrDbFactory;
begin
  LibKoreCreateFactory('Db', IInterface(LFactory));
  if (not LFactory.CreateDbConnectionPool(nil, FIntf)) then
    LibKoreFailed(LFactory);
  LibKoreSetUserData(FIntf, Self);
end;

destructor TDbConnectionPool.Destroy;
begin
  LibKoreFinished(IInterface(FIntf));
  inherited Destroy;
end;

procedure TDbConnectionPool.Disposed;
begin
  FIntf := nil;
end;

function TDbConnectionPool.Enter: TDbConnection;
var
  LIntf: IcrDbConnection;
begin
  if (not FIntf.Enter(LIntf)) then
    LibKoreFailed(FIntf);

  Result := nil;
  LibKoreGetUserData(LIntf, Pointer(Result));
  if (Result = nil) then
    Result := TDbConnection.Create(LIntf);
end;

procedure TDbConnectionPool.SetDbConnectParams(
  const ADbConnectParams: TDbConnectParams);
begin
  if (ADbConnectParams <> nil) then
  begin
    if (not FIntf.SetDbConnectParams(ADbConnectParams.FIntf)) then
      LibKoreFailed(FIntf);
  end;
end;

{ TDbConnection }

procedure TDbConnection.BeginTransaction(const AReadOnly: Boolean);
begin
  if (not FIntf.BeginTransaction(AReadOnly)) then
    LibKoreFailed(FIntf);
end;

function TDbConnection.BeginTransactionIf(const AReadOnly: Boolean): Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.BeginTransactionIf(AReadOnly, LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

procedure TDbConnection.Commit;
begin
  if (not FIntf.Commit) then
    LibKoreFailed(FIntf);
end;

function TDbConnection.CommitIf: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.CommitIf(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

constructor TDbConnection.Create(const AIntf: IcrDbConnection);
begin
  FIntf := AIntf;
  LibKoreSetUserData(AIntf, Self);
end;

function TDbConnection.CreateQuery: TDbQuery;
var
  LIntf: IcrDbQuery;
begin
  if (not FIntf.CreateQuery(LIntf)) then
    LibKoreFailed(FIntf);

  Result := TDbQuery.Create(LIntf);
end;

destructor TDbConnection.Destroy;
begin
  LibKoreFinished(IInterface(FIntf));
  inherited Destroy;
end;

procedure TDbConnection.Disposed;
begin
  FIntf := nil;
  inherited Disposed;
end;

procedure TDbConnection.Execute(const ACommandText: string);
begin
  if (not FIntf.Execute(nil, ACommandText)) then
    LibKoreFailed(FIntf);
end;

function TDbConnection.ExecuteWthRowsCount(const ACommandText: string): Int64;
begin
  if (not FIntf.ExecuteWthRowsCount(nil, ACommandText, Result)) then
    LibKoreFailed(FIntf);
end;

function TDbConnection.IdCrypt: Cardinal;
begin
  if (not FIntf.GetConnectionCrypt(Result)) then
    LibKoreFailed(FIntf);
end;

function TDbConnection.InTransaction: Boolean;
var
  LTemp: WordBool;
begin
  if (not FIntf.GetInTransaction(LTemp)) then
    LibKoreFailed(FIntf);
  Exit(LTemp);
end;

function TDbConnection.IsConnected: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.GetIsConnected(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

procedure TDbConnection.Leave;
begin
  if (not FIntf.Leave) then
    LibKoreFailed(FIntf);
end;

procedure TDbConnection.Rollback;
begin
  if (not FIntf.Rollback) then
    LibKoreFailed(FIntf);
end;

function TDbConnection.RollbackIf: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.RollbackIf(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TDbConnection.SupportsTimeZone: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.GetSupportsTimeZone(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TDbConnection.TransactionType: Cardinal;
begin
  if (not FIntf.GetTransactionType(Result)) then
    LibKoreFailed(FIntf);
end;

{ TDbQuery }

procedure TDbQuery.Close;
begin
  if (not FIntf.Close) then
    LibKoreFailed(FIntf);
end;

constructor TDbQuery.Create(const AIntf: IcrDbQuery);
begin
  FIntf := AIntf;
  LibKoreSetUserData(AIntf, Self);
end;

procedure TDbQuery.Disposed;
begin
  FIntf := nil;
  inherited Disposed;
end;

procedure TDbQuery.Execute;
begin
  if (not FIntf.Execute) then
    LibKoreFailed(FIntf);
end;

function TDbQuery.FieldByName(const AName: string): TValue;
var
  LIntfField: IcrValue;
begin
  if (not FIntf.FieldByName(nil, AName, LIntfField)) then
    LibKoreFailed(LIntfField);
  Exit(ValueByIntf(LIntfField));
end;

function TDbQuery.FindField(const AName: string): TValue;
var
  LIntfField: IcrValue;
begin
  if (not FIntf.FindField(nil, AName, LIntfField)) then
    LibKoreFailed(LIntfField);

  if (LIntfField <> nil) then
    Exit(ValueByIntf(LIntfField))
  else
    Exit(nil);
end;

function TDbQuery.FindParam(const AName: string): TValue;
var
  LIntfParam: IcrValue;
begin
  if (not FIntf.FindParam(nil, AName, LIntfParam)) then
    LibKoreFailed(LIntfParam);

  if (LIntfParam <> nil) then
    Result := ValueByIntf(LIntfParam)
  else
    Result := nil;
end;

function TDbQuery.GetCommandText: string;
var
  LValue: WideString;
begin
  if (not FIntf.GetCommandText(nil, LValue)) then
    LibKoreFailed(FIntf);
  Exit(LValue);
end;

function TDbQuery.GetFields: TValues;
var
  LIntf: IcrValues;
begin
  if (not FIntf.GetFields(LIntf)) then
    LibKoreFailed(FIntf);

  Result := nil;
  LibKoreGetUserData(LIntf, Pointer(Result));
  if (Result = nil) then
    Result := TValues.Create(LIntf);
end;

function TDbQuery.GetParams: TValues;
var
  LIntf: IcrValues;
begin
  if (not FIntf.GetParams(LIntf)) then
    LibKoreFailed(FIntf);

  Result := nil;
  LibKoreGetUserData(LIntf, Pointer(Result));
  if (Result = nil) then
    Result := TValues.Create(LIntf);
end;

function TDbQuery.GetRowsAffected: Int64;
begin
  if (not FIntf.GetRowsAffected(Result)) then
    LibKoreFailed(FIntf);
end;

function TDbQuery.IsEmpty: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.GetIsEmpty(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TDbQuery.Next: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.MoveNext(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

procedure TDbQuery.Open;
begin
  if (not FIntf.Open) then
    LibKoreFailed(FIntf);
end;

function TDbQuery.OpenData: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.OpenData(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TDbQuery.ParamByName(const AName: string): TValue;
var
  LIntfParam: IcrValue;
begin
  if (not FIntf.ParamByName(nil, AName, LIntfParam)) then
    LibKoreFailed(LIntfParam);
  Exit(ValueByIntf(LIntfParam));
end;

procedure TDbQuery.SetCommandText(const ACommandText: string);
begin
  if (not FIntf.SetCommandText(nil, ACommandText)) then
    LibKoreFailed(FIntf);
end;

function TDbQuery.ValueByIntf(const AIntf: IcrValue): TValue;
begin
  Result := nil;
  LibKoreGetUserData(AIntf, Pointer(Result));
  if (Result = nil) then
    Result := TValue.Create(AIntf);
end;

end.
