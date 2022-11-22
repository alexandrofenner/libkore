unit uFormDatabase;

interface

uses
  LibKoreIntfs,
  LibKoreAll,
  uUtils,

  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  SynEdit,
  SynEditHighlighter,
  SynHighlighterJSON;

type
  TFormDatabase = class(TForm)
    pcMain: TPageControl;
    tsConnect: TTabSheet;
    rgDBType: TRadioGroup;
    lbeServerHost: TLabeledEdit;
    lbeServerPort: TLabeledEdit;
    lbeDatabase: TLabeledEdit;
    lbeUserName: TLabeledEdit;
    lbePassword: TLabeledEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    tsSQL: TTabSheet;
    tsResults: TTabSheet;
    pnSqlTopBar: TPanel;
    reSQL: TRichEdit;
    btnSqlExec: TButton;
    synResult: TSynEdit;
    SynJSONSyn1: TSynJSONSyn;
    pnResultsTopBar: TPanel;
    lblResultTime: TLabel;
    btnSaveAs: TButton;
    SaveDialog: TSaveDialog;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSqlExecClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
  private
    FDbFactory: TcrDbFactory;
    FDbConnectParams: TcrDbConnectParams;
    FDbConnectionPool: TcrDbConnectionPool;
    FDatabaseConfigFileName: TcrString;

    procedure EnableCtrls(const AOnOff: Boolean);
    procedure AssignByObject(const AParams: TcrDbConnectParams);
    function AssignToObject(const AParams: TcrDbConnectParams): Boolean;
    function MsgErr(const S: string): Boolean;
  public
    class procedure Execute; static;

    { cdtor }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

function TcpPortToString(const APort: Word): string;
begin
  if (APort <> 0) then
    Result := UIntToStr(APort)
  else
    Result := '';
end;

{ TFormDatabase }

procedure TFormDatabase.AssignByObject(const AParams: TcrDbConnectParams);
begin
  case AParams.DbTypeId of
    crDbConnectParams_DbTypeId_Firebird: rgDBType.ItemIndex := 1;
    crDbConnectParams_DbTypeId_PostgreSql: rgDBType.ItemIndex := 0;
    else
      rgDBType.ItemIndex := -1;
  end;

  lbeServerHost.Text := AParams.ServerHost;
  lbeServerPort.Text := TcpPortToString(AParams.ServerPort);
  lbeDatabase.Text := AParams.Database;
  lbeUserName.Text := AParams.LoginUserName;
  lbePassword.Text := AParams.LoginPassword;
end;

function TFormDatabase.AssignToObject(
  const AParams: TcrDbConnectParams): Boolean;
var
  S: string;
  I: Cardinal;
begin
  case rgDBType.ItemIndex of
    0: AParams.DbTypeId := crDbConnectParams_DbTypeId_PostgreSql;
    1: AParams.DbTypeId := crDbConnectParams_DbTypeId_Firebird;
    else
      Exit(MsgErr('Tipo não informado'));
  end;

  S := Trim(lbeServerHost.Text);
  if S.IsEmpty then
    Exit(MsgErr('Host do servidor não informado'));
  AParams.ServerHost := S;

  I := StrToUIntDef(Trim(lbeServerPort.Text), 0);
  if ((I = 0) or (I > $ffff)) then
    Exit(MsgErr('Porta do servidor não informada'));

  AParams.ServerPort := I;

  S := Trim(lbeDatabase.Text);
  if S.IsEmpty then
    Exit(MsgErr('Base de dados não informada'));
  AParams.Database := S;

  S := Trim(lbeUserName.Text);
  if S.IsEmpty then
    Exit(MsgErr('Nome de usuário não informado'));
  AParams.LoginUserName := S;

  S := Trim(lbePassword.Text);
  if S.IsEmpty then
    Exit(MsgErr('Senha não informada'));
  AParams.LoginPassword := S;

  AParams.SchemaName := '';
  AParams.RoleName := '';
  AParams.CharSetName := '';
  AParams.CollateName := '';
  AParams.Options := 0;
  AParams.IPVersion := 4;

  Exit(True);
end;

procedure TFormDatabase.btnConnectClick(Sender: TObject);
begin
  if (not AssignToObject(FDbConnectParams)) then Exit;
  FDbConnectParams.SaveToJsonFile(FDatabaseConfigFileName);

  FreeAndNil(FDbConnectionPool);
  FDbConnectionPool := FDbFactory.CreateDbConnectionPool;
  FDbConnectionPool.SetDbConnectParams(FDbConnectParams);
  FDbConnectionPool.Enter.Leave;

  EnableCtrls(False);
  pcMain.ActivePage := tsSQL;
end;

procedure TFormDatabase.btnDisconnectClick(Sender: TObject);
begin
  FreeAndNil(FDbConnectionPool);
  EnableCtrls(True);
end;

procedure TFormDatabase.btnSaveAsClick(Sender: TObject);
var
  LFileName: string;
begin
  if SaveDialog.Execute(Handle) then
  begin
    LFileName := SaveDialog.FileName;
    if ExtractFileExt(LFileName).IsEmpty then
      LFileName := LFileName + '.json';

    synResult.Lines.SaveToFile(LFileName);
  end;
end;

procedure TFormDatabase.btnSqlExecClick(Sender: TObject);
const
  MSecsPerMin = (MSecsPerSec * 60);
  MSecsPerHour = (MSecsPerMin * 60);

var
  LDbConnection: TcrDbConnection;
  LDbQuery: TcrDbQuery;
  LJsonText, LDiffStr: string;
  LDtBefore, LDtAfter: TDateTime;
begin
  LDbConnection := FDbConnectionPool.Enter;
  try
    LDbConnection.BeginTransactionIf(True);
    LDbQuery := LDbConnection.CreateQuery;
    LDbQuery.CommandText := reSQL.Text;

    LDtBefore := Now;
    LDbQuery.Open;
    LJsonText := LDbQuery.DumpAllToJson(True);
    LDtAfter := Now;

    LDiffStr := IntervalToString(LDtAfter - LDtBefore);
    if LDiffStr.IsEmpty then
      LDiffStr := '0 milisegundo';

    lblResultTime.Caption := 'Executado em: ' + LDiffStr;

    tsResults.TabVisible := True;
    pcMain.ActivePage := tsResults;
    synResult.Text := LJsonText;
  finally
    LDbConnection.RollbackIf;
    LDbConnection.Leave;
  end;
end;

constructor TFormDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  tsResults.TabVisible := False;
  tsSQL.TabVisible := False;

  FDatabaseConfigFileName := ChangeFileExt(
    GetModuleName(MainInstance), '.DBConfig.json');

  FDbFactory := TcrDbFactory.Create;
  FDbConnectParams := FDbFactory.CreateDbConnectParams;

  if FileExists(FDatabaseConfigFileName) then
  try
    FDbConnectParams.LoadFromJsonFile(FDatabaseConfigFileName);
    AssignByObject(FDbConnectParams);
  except
    ShowException(ExceptObject, ExceptAddr);
  end;
end;

destructor TFormDatabase.Destroy;
begin
  FreeAndNil(FDbConnectionPool);
  FreeAndNil(FDbConnectParams);
  FreeAndNil(FDbFactory);
  inherited Destroy;
end;

procedure TFormDatabase.EnableCtrls(const AOnOff: Boolean);
var
  LNotOnOff: Boolean;
begin
  LNotOnOff := (not AOnOff);

  rgDBType.Enabled := AOnOff;
  lbeServerHost.Enabled := AOnOff;
  lbeServerPort.Enabled := AOnOff;
  lbeDatabase.Enabled := AOnOff;
  lbeUserName.Enabled := AOnOff;
  lbePassword.Enabled := AOnOff;
  btnConnect.Enabled := AOnOff;

  btnDisconnect.Enabled := LNotOnOff;
  tsSQL.TabVisible := LNotOnOff;
  tsResults.TabVisible := False;
end;

class procedure TFormDatabase.Execute;
var
  LForm: TFormDatabase;
begin
  LForm := TFormDatabase.Create(nil);
  try
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

function TFormDatabase.MsgErr(const S: string): Boolean;
begin
  MessageDlg(S, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
  Exit(False);
end;

end.
