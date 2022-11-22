unit uUtils;

interface

uses
  System.SysUtils;

function IntervalToString(const AInterval: TDateTime): string;

implementation

const
  MSecsPerMin = (MSecsPerSec * 60);
  MSecsPerHour = (MSecsPerMin * 60);

function IntervalToString(const AInterval: TDateTime): string;
var
  LI: Int64;
  LS: string;

  procedure AddLS(const S: string);
  begin
    if LS.IsEmpty then
      LS := S
    else
      LS := LS + ', ' + S;
  end;

  procedure Days(ACount: Integer);
  begin
    if (ACount <= 0) then Exit;

    if (ACount = 1) then
      AddLS('um dia')
    else
      AddLS(IntToStr(ACount) + ' dias');

    Dec(LI, Int64(ACount) * MSecsPerDay);
  end;

  procedure Hours(ACount: Integer);
  begin
    if (ACount <= 0) then Exit;

    if (ACount = 1) then
      AddLS('uma hora')
    else
      AddLS(IntToStr(ACount) + ' horas');

    Dec(LI, Int64(ACount) * MSecsPerHour);
  end;

  procedure Mins(ACount: Integer);
  begin
    if (ACount <= 0) then Exit;

    if (ACount = 1) then
      AddLS('um minuto')
    else
      AddLS(IntToStr(ACount) + ' minutos');

    Dec(LI, Int64(ACount) * MSecsPerMin);
  end;

  procedure Secs(ACount: Integer);
  begin
    if (ACount <= 0) then Exit;

    if (ACount = 1) then
      AddLS('um segundo')
    else
      AddLS(IntToStr(ACount) + ' segundos');

    Dec(LI, Int64(ACount) * MSecsPerSec);
  end;

  procedure MSecs(ACount: Integer);
  begin
    if (ACount <= 0) then Exit;

    if (ACount = 1) then
      AddLS('um milisegundo')
    else
      AddLS(IntToStr(ACount) + ' milisegundos');
  end;

begin
  if (AInterval <= 0) then Exit('');

  LI := Round(AInterval * MSecsPerDay);
  LS := '';

  Days(LI div MSecsPerDay);
  Hours(LI div MSecsPerHour);
  Mins(LI div MSecsPerMin);
  Secs(LI div MSecsPerSec);
  MSecs(LI);

  Exit(LS);
end;

end.
