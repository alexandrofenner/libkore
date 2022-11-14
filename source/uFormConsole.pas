unit uFormConsole;

interface

uses
  LibKoreIntfs,
  LibKoreUtils,

  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TFormConsole = class(TForm)
    reOutput: TRichEdit;
    pnTopBar: TPanel;
    lbeCommandLine: TLabeledEdit;
    btnExecute: TButton;
    TimerCheck: TTimer;
    cbxRemoveAdministrativePermissions: TCheckBox;
    procedure btnExecuteClick(Sender: TObject);
    procedure TimerCheckTimer(Sender: TObject);
  private
    FIntfConsoleFactory: IcrConsoleFactory;
    FIntfConsole: IcrConsole;
    FIntfConsoleCache: IcrConsole;

    FIntfWinPseudoConsole: IcrWinPseudoConsole;
    FCanClose: Boolean;

    procedure IntfsInitialize;
  protected
    { Override }
    procedure DoClose(var ACloseAction: TCloseAction); override;
  public
    class procedure Execute; static;

    { cdtor }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

procedure TcrConsoleAddLineMethod(
  const AUserData: Pointer; const S: WideString); stdcall;
var
  LForm: TFormConsole;
  LRichEdit: TRichEdit;
begin
  LForm := AUserData;
  LRichEdit := LForm.reOutput;

  LRichEdit.Lines.Add(S);
  LRichEdit.SelStart := LRichEdit.GetTextLen;
  LRichEdit.Perform(EM_SCROLLCARET, 0, 0);
end;

{ TFormConsole }

procedure TFormConsole.btnExecuteClick(Sender: TObject);
var
  LFactory: IcrWinPseudoConsoleFactory;
begin
  btnExecute.Enabled := False;
  lbeCommandLine.Enabled := False;
  cbxRemoveAdministrativePermissions.Enabled := False;
  FCanClose := False;
  reOutput.Clear;

  { -> Criar o WinPseudoConsole (que será o 'container' de
                                 execução do projeto filho) }
  LibKoreCreateFactory('WinPseudoConsole', IInterface(LFactory));
  if (not LFactory.SetCommandLine(lbeCommandLine.Text)) then
    LibKoreFailed(LFactory);
  if (not LFactory.SetConsole(FIntfConsoleCache)) then
    LibKoreFailed(LFactory);

  if cbxRemoveAdministrativePermissions.Checked then
    if (not LFactory.RemoveAdministrativePermissions) then
      LibKoreFailed(LFactory);

  if (not LFactory.Create(FIntfWinPseudoConsole)) then
    LibKoreFailed(LFactory);

  if (not FIntfWinPseudoConsole.BeginExecute) then
    LibKoreFailed(FIntfWinPseudoConsole);

  TimerCheck.Enabled := True;
end;

constructor TFormConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IntfsInitialize;
  FCanClose := True;
end;

procedure TFormConsole.DoClose(var ACloseAction: TCloseAction);
begin
  if (not FCanClose) then
  begin
    ACloseAction := TCloseAction.caNone;
    Exit;
  end;
  inherited DoClose(ACloseAction);
end;

class procedure TFormConsole.Execute;
var
  LForm: TFormConsole;
begin
  LForm := TFormConsole.Create(nil);
  try
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

procedure TFormConsole.IntfsInitialize;
begin
  { -> Criar o Factory dos objetos 'console' }
  LibKoreCreateFactory('Console', IInterface(FIntfConsoleFactory));

  { -> Criar o objeto console que receberá o conteúdo por linha
      Para adicionar no richedit }
  if (not FIntfConsoleFactory.CreateByAddLine(
    TcrConsoleAddLineMethod, Self, FIntfConsole)) then
    LibKoreFailed(FIntfConsoleFactory);

  { -> Criar o objeto console de 'cache' que desviará os dados para
        o objeto console criado no passo anterior.

    Importante:
        O objeto console de cache tem a função de receber dados a
        partir de qualquer 'thread' mas entregar para apenas uma 'thread'
        a 'thread' em que foi criado o handle HWND passado no parâmetro.

        No caso abaixo foi enviado no primeiro parãmetro o Handle do
        formulário atual. Este handle foi criado na thread principal do
        projeto. Assim, o objeto cache desviará todo conteúdo recebido
        de quaisquer threads para a thread principal do projeto atual.

        Como este objeto de cache garante a entrega de dados para o objeto
        console criado anteriormente a partir da thread principal, no
        objeto anterior não é preciso criar nenhum mecanismo de sincronização.

        Um mecanismo de sincronização poderia ser necessário, porquê
        a execução destes métodos envolvem controles da VCL (RichEdit) }
  if (not FIntfConsoleFactory.CreateCacheByWnd(Handle,
    FIntfConsole, FIntfConsoleCache)) then
      LibKoreFailed(FIntfConsoleFactory);
end;

procedure TFormConsole.TimerCheckTimer(Sender: TObject);
var
  LIsTerminated: WordBool;
begin
  if FIntfWinPseudoConsole.IsTerminated(LIsTerminated) then
  begin
    if LIsTerminated then
    begin
      TimerCheck.Enabled := False;
      btnExecute.Enabled := True;
      lbeCommandLine.Enabled := True;
      cbxRemoveAdministrativePermissions.Enabled := True;
      FCanClose := True;
      FIntfWinPseudoConsole := nil;
    end;
  end;
end;

end.
