program LibKoreTest;

uses
  Vcl.Forms,
  uFormMain in 'source\uFormMain.pas' {FormMain},
  LibKoreIntfs in 'source\LibKoreIntfs.pas',
  LibKoreUtils in 'source\LibKoreUtils.pas',
  uFormConsole in 'source\uFormConsole.pas' {FormConsole};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
