unit LibKoreUtils;

interface

uses
  LibKoreIntfs,
  System.SysUtils,
  System.Classes;

type
  ELibKoreException = class(Exception);

  ELibKoreFailed = class(ELibKoreException)
  strict private
    FOriginalMessage: string;
    FStatusCode: Cardinal;
    FOsError: Cardinal;
    FCodeLocalId: PAnsiChar;
  public
    { cdtor }
    constructor New(const AMessage: WideString;
      const AStatusCode, AOsError: Cardinal;
      const ACodeLocalId: PAnsiChar);
  end;

  TcrObject = class(TObject)
  strict private
    FOnDisposed: TNotifyEvent;
  protected
    procedure InvokeOnDisposed;
    procedure Disposed; virtual;
  public
    property OnDisposed: TNotifyEvent read FOnDisposed write FOnDisposed;
  end;

procedure LibKoreBytesAlloc(var ABytes: Pointer; ABytesCount: Integer); stdcall;

procedure LibKoreCreateFactory(const AClassName: string;
  var AFactory: IUnknown);

procedure LibKoreFinished(var AIntf: IInterface);
procedure LibKoreFailed(const AIntf: IInterface);

function LibKoreSetUserData(const AIntf: IInterface;
  const AUserData: TObject): Boolean;
function LibKoreGetUserData(const AIntf: IInterface;
  var AUserData: Pointer): Boolean;

implementation

function TcrObject_IntfUserCallback(
  var AParams: TcrIntfUserCallbackParams): WordBool; stdcall;
begin
  try
    case AParams.FIdAction of
      UserIdAction_Disposed:
        TcrObject(AParams.FUserData).Disposed;
    end;
  except
  end;

  Result := True;
end;

{ ELibKoreFailed }

constructor ELibKoreFailed.New(const AMessage: WideString;
  const AStatusCode, AOsError: Cardinal;
  const ACodeLocalId: PAnsiChar);
begin
  FOriginalMessage := AMessage;
  FStatusCode := AStatusCode;
  FOsError := AOsError;
  FCodeLocalId := ACodeLocalId;

  if FOriginalMessage.IsEmpty then
  begin
    Create('Erro desconhecido');
    Exit;
  end;

  Create(FOriginalMessage);
end;

procedure LibKoreBytesAlloc(var ABytes: Pointer; ABytesCount: Integer); stdcall;
begin
  SetLength(TBytes(ABytes), ABytesCount);
end;

procedure LibKoreCreateFactory(const AClassName: string;
  var AFactory: IUnknown);
begin
  if (not CreateFactory(AClassName, AFactory)) then
    raise ELibKoreException.CreateFmt('CreateFactory falhou ao ' +
      'tentar criar um "Factory" da classe ''%s''', [AClassName]);
end;

procedure LibKoreFinished(var AIntf: IInterface);
var
  LOld: Pointer;
  LIntfObj: IcrObject;
begin
  repeat
    LOld := Pointer(AIntf);
    if (LOld = nil) then Exit;

    if (AtomicCmpExchange(Pointer(AIntf), nil, LOld) = LOld) then
    begin
      try
        LIntfObj := (IInterface(LOld) as IcrObject);
        if Assigned(LIntfObj) then
        begin
          LIntfObj.SetUserCallback(nil);
          LIntfObj.SetUserData(nil);
        end;
      finally
        LIntfObj := nil;
        IInterface(LOld) := nil;
      end;
      Exit;
    end;
  until False;
end;

procedure LibKoreFailed(const AIntf: IInterface);
var
  LIntfObj: IcrObject;
  LIntfErrorInfo: IcrErrorInfo;
  LMessage: WideString;
  LStatusCode, LOsError: Cardinal;
  LCodeLocalId: PAnsiChar;
begin
  LIntfErrorInfo := nil;
  if (AIntf <> nil) then
  begin
    LIntfObj := (AIntf as IcrObject);
    if ((LIntfObj <> nil) and
      (not LIntfObj.GetErrorInfo(LIntfErrorInfo))) then
        LIntfErrorInfo := nil;
  end;

  LMessage := '';
  LStatusCode := 0;
  LOsError := 0;
  LCodeLocalId := nil;

  if (LIntfErrorInfo <> nil) then
  begin
    LIntfErrorInfo.GetMessage(nil, LMessage);
    LIntfErrorInfo.GetStatusCode(LStatusCode);
    LIntfErrorInfo.GetOsError(LOsError);
    LIntfErrorInfo.GetCodeLocalId(LCodeLocalId);
  end;

  raise ELibKoreFailed.New(LMessage,
    LStatusCode, LOsError, LCodeLocalId);
end;

function LibKoreSetUserData(const AIntf: IInterface;
  const AUserData: TObject): Boolean;
var
  LIntfObj: IcrObject;
begin
  if (AIntf = nil) then Exit(False);
  LIntfObj := (AIntf as IcrObject);
  if (LIntfObj = nil) then Exit(False);

  if (not LIntfObj.SetUserData(AUserData)) then
    LibKoreFailed(AIntf);

  if (AUserData <> nil) then
  begin
    if AUserData.InheritsFrom(TcrObject) then
    begin
      if (not LIntfObj.SetUserCallback(TcrObject_IntfUserCallback)) then
        LibKoreFailed(AIntf);
    end;
  end else
  begin
    if (not LIntfObj.SetUserCallback(nil)) then
      LibKoreFailed(AIntf);
  end;

  Exit(True);
end;

function LibKoreGetUserData(const AIntf: IInterface;
  var AUserData: Pointer): Boolean;
var
  LIntfObj: IcrObject;
begin
  if (AIntf = nil) then Exit(False);
  LIntfObj := (AIntf as IcrObject);
  if (LIntfObj = nil) then Exit(False);
  if (not LIntfObj.GetUserData(AUserData)) then
    LibKoreFailed(AIntf);
  Exit(True);
end;

{ TcrObject }

procedure TcrObject.Disposed;
begin
  InvokeOnDisposed;
end;

procedure TcrObject.InvokeOnDisposed;
begin
  if Assigned(FOnDisposed) then
    FOnDisposed(Self);
end;

end.
