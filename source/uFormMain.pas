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
    btnConsoleRichEdit: TButton;
    btnDatabase: TButton;
    btnXml: TButton;
    procedure btnConsoleRichEditClick(Sender: TObject);
    procedure btnDatabaseClick(Sender: TObject);
    procedure btnXmlClick(Sender: TObject);
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
  uFormConsole,
  uFormDatabase,
  uFormXml;

{ TFormMain }

procedure TFormMain.btnConsoleRichEditClick(Sender: TObject);
begin
  TFormConsole.Execute;
end;

procedure TFormMain.btnDatabaseClick(Sender: TObject);
begin
  TFormDatabase.Execute;
end;

procedure TFormMain.btnXmlClick(Sender: TObject);
begin
  TFormXml.Execute;
end;

end.
