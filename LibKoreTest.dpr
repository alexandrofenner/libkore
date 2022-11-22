program LibKoreTest;

uses
  Vcl.Forms,
  uFormMain in 'source\uFormMain.pas' {FormMain},
  LibKoreAll in 'source\LibKoreAll.pas',
  uFormConsole in 'source\uFormConsole.pas' {FormConsole},
  uFormDatabase in 'source\uFormDatabase.pas' {FormDatabase},
  uFormXml in 'source\uFormXml.pas' {FormXml},
  LibKoreIntfs in 'source\LibKoreIntfs.pas',
  LibKoreDll in 'source\LibKoreDll.pas',
  uUtils in 'source\uUtils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
