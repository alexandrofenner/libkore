unit uFormConsole;

interface

uses
  LibKoreAll,

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
    cbxRecording: TCheckBox;
    btnLoadRecordingFile: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure TimerCheckTimer(Sender: TObject);
    procedure btnLoadRecordingFileClick(Sender: TObject);
  private
    FRecordingFileName: TcrString;
    FConsoleFactory: TcrConsoleFactory;
    FConsole: TcrConsole;
    FConsoleCache: TcrConsole;
    FWinPseudoConsole: TcrWinPseudoConsole;

    FCanClose: Boolean;

    procedure IntfsInitialize;
    procedure EnableControls;
    procedure DisableControls;

    procedure ConsoleAddLine(const AStringType: Pointer;
      const AStringData: TcrString); stdcall;
  protected
    { Override }
    procedure DoClose(var ACloseAction: TCloseAction); override;
  public
    class procedure Execute; static;

    { cdtor }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

procedure TcrConsoleAddLineMethod(
  const AUserData: Pointer;
  const AStringType: Pointer {TcrIntfStringType};
  const AStringData: Pointer {TcrIntfStringData}); stdcall;
var
  LForm: TFormConsole;
  LRichEdit: TRichEdit;
begin
  LForm := AUserData;
  LRichEdit := LForm.reOutput;

  LRichEdit.Lines.Add(string(AStringData));
  LRichEdit.SelStart := LRichEdit.GetTextLen;
  LRichEdit.Perform(EM_SCROLLCARET, 0, 0);
end;

{ TFormConsole }

procedure TFormConsole.btnExecuteClick(Sender: TObject);
var
  LFactory: TcrWinPseudoConsoleFactory;
begin
  DisableControls;
  reOutput.Clear;

  try
    { -> Criar o WinPseudoConsole (que será o 'container' de
                                   execução do projeto filho) }
    LFactory := TcrWinPseudoConsoleFactory.Create;
    LFactory.SetCommandLine(lbeCommandLine.Text);
    LFactory.SetConsole(FConsoleCache);
    if cbxRemoveAdministrativePermissions.Checked then
      LFactory.RemoveAdministrativePermissions;

    if cbxRecording.Checked then
      LFactory.SetRecordingFileName(FRecordingFileName);

    FWinPseudoConsole := LFactory.CreateObject;
    FWinPseudoConsole.BeginExecute;

    TimerCheck.Enabled := True;
    FreeAndNil(LFactory);
  except
    EnableControls;
    FreeAndNil(FWinPseudoConsole);
    FreeAndNil(LFactory);
    raise;
  end;
end;

procedure TFormConsole.btnLoadRecordingFileClick(Sender: TObject);
var
  LUtils: TcrConsoleUtils;
begin
  LUtils := nil;
  DisableControls;
  reOutput.Clear;
  try
    LUtils := FConsoleFactory.CreateUtils;
    LUtils.AddFromAnsiEscCodeFile(FConsoleCache, FRecordingFileName);
    FreeAndNil(LUtils);
    EnableControls;
  except
    FreeAndNil(LUtils);
    EnableControls;
    raise;
  end;
end;

procedure TFormConsole.ConsoleAddLine(const AStringType: Pointer;
  const AStringData: TcrString);
var
  LRichEdit: TRichEdit;
begin
  LRichEdit := reOutput;

  LRichEdit.Lines.Add(AStringData);
  LRichEdit.SelStart := LRichEdit.GetTextLen;
  LRichEdit.Perform(EM_SCROLLCARET, 0, 0);
end;

constructor TFormConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsoleFactory := TcrConsoleFactory.Create;

//  FConsole := FConsoleFactory.CreateByProcessId(10772);
//  FConsole.ClearScreen(2);
//  FConsole.SetCursorPosition(1, 1);
//  FConsole.SelectGraphicRenditionRGB(48, $00);
//  FConsole.SelectGraphicRenditionRGB(38, $ffffff);
//  FConsole.CursorPreviousLine(10);
//  FConsole.SetCursorVisible(False);
//  FConsole.Text('Alexandro Landmann Fenner' + sLineBreak);
//  FConsole.SetTitle('asdf');

  IntfsInitialize;
  FCanClose := True;
  FRecordingFileName := ChangeFileExt(GetModuleName(MainInstance), '.rec');
  btnLoadRecordingFile.Enabled := FileExists(FRecordingFileName);
end;

destructor TFormConsole.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FConsoleCache);
  FreeAndNil(FConsole);
  FreeAndNil(FConsoleFactory);
end;

procedure TFormConsole.DisableControls;
begin
  btnExecute.Enabled := False;
  lbeCommandLine.Enabled := False;
  cbxRemoveAdministrativePermissions.Enabled := False;
  cbxRecording.Enabled := False;
  btnLoadRecordingFile.Enabled := False;
  FCanClose := False;
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

procedure TFormConsole.EnableControls;
begin
  TimerCheck.Enabled := False;
  btnExecute.Enabled := True;
  lbeCommandLine.Enabled := True;
  cbxRemoveAdministrativePermissions.Enabled := True;
  cbxRecording.Enabled := True;
  btnLoadRecordingFile.Enabled := FileExists(FRecordingFileName);
  FCanClose := True;
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
  { -> Criar o objeto console que receberá o conteúdo por linha
      Para adicionar no richedit }
  FConsole := FConsoleFactory.CreateByAddLine(ConsoleAddLine);

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
  FConsoleCache := FConsoleFactory.CreateCacheByWnd(Handle, FConsole);
end;

procedure TFormConsole.TimerCheckTimer(Sender: TObject);
begin
  if ((FWinPseudoConsole = nil) or FWinPseudoConsole.IsTerminated) then
  begin
    EnableControls;
    FreeAndNil(FWinPseudoConsole);
  end;
end;

end.

