unit LibKoreUtils;

interface

uses
  LibKoreIntfs,
  System.SysUtils,
  System.Classes;

type
  ELibKoreException = class(Exception);

procedure LibKoreCreateFactory(const AClassName: string;
  var AFactory: IUnknown);
procedure LibKoreFailed(const AIntf: IInterface);

implementation

procedure LibKoreCreateFactory(const AClassName: string;
  var AFactory: IUnknown);
begin
  if (not CreateFactory(AClassName, AFactory)) then
    raise ELibKoreException.CreateFmt('CreateFactory falhou ao ' +
      'tentar criar um "Factory" da classe ''%s''', [AClassName]);
end;

procedure LibKoreFailed(const AIntf: IInterface);
begin

end;

end.
