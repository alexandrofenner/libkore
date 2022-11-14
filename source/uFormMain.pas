unit uFormMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    btnConsole: TButton;
    procedure btnConsoleClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  uFormConsole;

procedure TFormMain.btnConsoleClick(Sender: TObject);
begin
  TFormConsole.Execute;
end;

end.
