unit uFormXml;

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
  Vcl.StdCtrls;

type
  TFormXml = class(TForm)
    btnTest1: TButton;
    procedure btnTest1Click(Sender: TObject);
  private
    FXmlFactory: TcrXmlFactory;
  public
    procedure Test1;

    { Override }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure Execute; static;
  end;

implementation

{$R *.dfm}

{ TFormXml }

procedure TFormXml.btnTest1Click(Sender: TObject);
begin
  Test1;
end;

constructor TFormXml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXmlFactory := TcrXmlFactory.Create;
end;

destructor TFormXml.Destroy;
begin
  FreeAndNil(FXmlFactory);
  inherited Destroy;
end;

class procedure TFormXml.Execute;
var
  LForm: TFormXml;
begin
  LForm := TFormXml.Create(nil);
  try
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

procedure TFormXml.Test1;
var
  LXmlDocument: TcrXmlDocument;
  LAttribute: TcrXmlAttribute;
  lta, ltb: Int64;
begin
  LXmlDocument := FXmlFactory.CreateXmlDocument;
  try
    LXmlDocument.LoadFromFile('F:\Temp\a.xml', nil, 0);

    GetSystemTimeAsFileTime(TFileTime(lta));
    LAttribute := LXmlDocument.FindAttributeEx('', 'Id', 'NFe41151208717747000177550010000017441447100001');
    GetSystemTimeAsFileTime(TFileTime(ltb));
    if (LAttribute <> nil) then
    begin
      Random(0);
    end;

    LXmlDocument.SaveToFile('F:\Temp\b.xml', nil, nil, 0);
  finally
    LXmlDocument.Free;
  end;
end;

end.
