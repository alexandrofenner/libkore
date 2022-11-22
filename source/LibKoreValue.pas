unit LibKoreValue;

interface

uses
  System.SysUtils,
  System.Classes,
  LibKoreIntfs,
  LibKoreUtils;

type
  TValue = class;
  TValues = class;

  TValue = class(TObject)
  private
    FIntf: IcrValue;

    function GetName: string;

    function GetIsNull: Boolean;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsUInt8: UInt8;
    procedure SetAsUInt8(const AValue: UInt8);
    function GetAsUInt16: UInt16;
    procedure SetAsUInt16(const AValue: UInt16);
    function GetAsUInt32: UInt32;
    procedure SetAsUInt32(const AValue: UInt32);
    function GetAsUInt64: UInt64;
    procedure SetAsUInt64(const AValue: UInt64);
    function GetAsSInt8: Int8;
    procedure SetAsSInt8(const AValue: Int8);
    function GetAsSInt16: Int16;
    procedure SetAsSInt16(const AValue: Int16);
    function GetAsSInt32: Int32;
    procedure SetAsSInt32(const AValue: Int32);
    function GetAsSInt64: Int64;
    procedure SetAsSInt64(const AValue: Int64);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const AValue: Currency);
    function GetAsReal32: Single;
    procedure SetAsReal32(const AValue: Single);
    function GetAsReal64: Double;
    procedure SetAsReal64(const AValue: Double);
    function GetAsChar: WideChar;
    procedure SetAsChar(const AValue: WideChar);
    function GetAsString: string;
    procedure SetAsString(const AValue: string);
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const AValue: TBytes);
    function GetAsUUID: TGUID;
    procedure SetAsUUID(const AUUID: TGUID);

    function GetAsTime: TcrTime;
    procedure SetAsTime(const AValue: TcrTime);
    function GetAsTimeUTC: TcrTimeUTC;
    procedure SetAsTimeUTC(const AValue: TcrTimeUTC);
    function GetAsDate: TcrDate;
    procedure SetAsDate(const AValue: TcrDate);
    function GetAsTimeStamp: TcrTimeStamp;
    procedure SetAsTimeStamp(const AValue: TcrTimeStamp);
    function GetAsTimeStampUTC: TcrTimeStampUTC;
    procedure SetAsTimeStampUTC(const AValue: TcrTimeStampUTC);
  public
    procedure Clear;

    { cdtor }
    constructor Create(const AIntf: IcrValue);

    { properties }
    property Name: string read GetName;

    property IsNull: Boolean read GetIsNull;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;

    property AsUInt8: UInt8 read GetAsUInt8 write SetAsUInt8;
    property AsUInt16: UInt16 read GetAsUInt16 write SetAsUInt16;
    property AsUInt32: UInt32 read GetAsUInt32 write SetAsUInt32;
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;
    property AsSInt8: Int8 read GetAsSInt8 write SetAsSInt8;
    property AsSInt16: Int16 read GetAsSInt16 write SetAsSInt16;
    property AsSInt32: Int32 read GetAsSInt32 write SetAsSInt32;
    property AsSInt64: Int64 read GetAsSInt64 write SetAsSInt64;

    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsReal32: Single read GetAsReal32 write SetAsReal32;
    property AsReal64: Double read GetAsReal64 write SetAsReal64;

    property AsChar: WideChar read GetAsChar write SetAsChar;
    property AsString: string read GetAsString write SetAsString;

    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsUUID: TGUID read GetAsUUID write SetAsUUID;

    property AsTime: TcrTime read GetAsTime write SetAsTime;
    property AsTimeUTC: TcrTimeUTC read GetAsTimeUTC write SetAsTimeUTC;
    property AsDate: TcrDate read GetAsDate write SetAsDate;
    property AsTimeStamp: TcrTimeStamp read GetAsTimeStamp write SetAsTimeStamp;
    property AsTimeStampUTC: TcrTimeStampUTC
      read GetAsTimeStampUTC write SetAsTimeStampUTC;
  end;

  TValues = class(TObject)
  private
    FIntf: IcrValues;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TValue;
  public
    function ByIntf(const AIntf: IcrValue): TValue;

    function Find(const AName: string): TValue;
    function ByName(const AName: string): TValue;

    { cdtor }
    constructor Create(const AIntf: IcrValues);
    destructor Destroy; override;

    { properties }
    property Count: Integer read GetCount;
    property Items[const I: Integer]: TValue read GetItem; default;
  end;

implementation

//    function GetCount(var ACount: NativeInt): WordBool; stdcall;
//    function GetItem(const AIndex: NativeInt;
//      var AItem: IcrValue): WordBool; stdcall;
//
//    function Find(const AName: WideString;
//      var AItem: IcrValue): WordBool; stdcall;

{ TValue }

procedure TValue.Clear;
begin
  if (not FIntf.Clear) then
    LibKoreFailed(FIntf);
end;

constructor TValue.Create(const AIntf: IcrValue);
begin
  FIntf := AIntf;
  LibKoreSetUserData(AIntf, Self);
end;

function TValue.GetAsBoolean: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.GetAsBoolean(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TValue.GetAsBytes: TBytes;
begin
  Result := nil;
  if (not FIntf.GetAsBytes(Pointer(Result), LibKoreBytesAlloc)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsChar: WideChar;
begin
  if (not FIntf.GetAsChar(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsCurrency: Currency;
begin
  if (not FIntf.GetAsCurrency(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsDate: TcrDate;
begin
  if (not FIntf.GetAsDate(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsReal32: Single;
begin
  if (not FIntf.GetAsReal32(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsReal64: Double;
begin
  if (not FIntf.GetAsReal64(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsSInt16: Int16;
begin
  if (not FIntf.GetAsSInt16(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsSInt32: Int32;
begin
  if (not FIntf.GetAsSInt32(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsSInt64: Int64;
begin
  if (not FIntf.GetAsSInt64(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsSInt8: Int8;
begin
  if (not FIntf.GetAsSInt8(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsString: string;
var
  LReturn: TcrIntfStringData;
begin
  if (not FIntf.GetAsString(nil, LReturn)) then
    LibKoreFailed(FIntf);
  Result := LReturn;
end;

function TValue.GetAsTime: TcrTime;
begin
  if (not FIntf.GetAsTime(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsTimeStamp: TcrTimeStamp;
begin
  if (not FIntf.GetAsTimeStamp(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsTimeStampUTC: TcrTimeStampUTC;
begin
  if (not FIntf.GetAsTimeStampUTC(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsTimeUTC: TcrTimeUTC;
begin
  if (not FIntf.GetAsTimeUTC(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsUInt16: UInt16;
begin
  if (not FIntf.GetAsUInt16(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsUInt32: UInt32;
begin
  if (not FIntf.GetAsUInt32(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsUInt64: UInt64;
begin
  if (not FIntf.GetAsUInt64(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsUInt8: UInt8;
begin
  if (not FIntf.GetAsUInt8(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetAsUUID: TGUID;
begin
  if (not FIntf.GetAsUUID(Result)) then
    LibKoreFailed(FIntf);
end;

function TValue.GetIsNull: Boolean;
var
  LReturn: WordBool;
begin
  if (not FIntf.GetIsNull(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TValue.GetName: string;
var
  LReturn: WideString;
begin
  if (not FIntf.GetName(nil, LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

procedure TValue.SetAsBoolean(const AValue: Boolean);
begin
  if (not FIntf.SetAsBoolean(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsBytes(const AValue: TBytes);
begin
  if (not FIntf.SetAsBytes(Pointer(AValue), Length(AValue))) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsChar(const AValue: WideChar);
begin
  if (not FIntf.SetAsChar(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsCurrency(const AValue: Currency);
begin
  if (not FIntf.SetAsCurrency(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsDate(const AValue: TcrDate);
begin
  if (not FIntf.SetAsDate(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsReal32(const AValue: Single);
begin
  if (not FIntf.SetAsReal32(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsReal64(const AValue: Double);
begin
  if (not FIntf.SetAsReal64(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsSInt16(const AValue: Int16);
begin
  if (not FIntf.SetAsSInt16(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsSInt32(const AValue: Int32);
begin
  if (not FIntf.SetAsSInt32(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsSInt64(const AValue: Int64);
begin
  if (not FIntf.SetAsSInt64(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsSInt8(const AValue: Int8);
begin
  if (not FIntf.SetAsSInt8(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsString(const AValue: string);
begin
  if (not FIntf.SetAsString(nil, AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsTime(const AValue: TcrTime);
begin
  if (not FIntf.SetAsTime(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsTimeStamp(const AValue: TcrTimeStamp);
begin
  if (not FIntf.SetAsTimeStamp(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsTimeStampUTC(const AValue: TcrTimeStampUTC);
begin
  if (not FIntf.SetAsTimeStampUTC(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsTimeUTC(const AValue: TcrTimeUTC);
begin
  if (not FIntf.SetAsTimeUTC(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsUInt16(const AValue: UInt16);
begin
  if (not FIntf.SetAsUInt16(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsUInt32(const AValue: UInt32);
begin
  if (not FIntf.SetAsUInt32(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsUInt64(const AValue: UInt64);
begin
  if (not FIntf.SetAsUInt64(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsUInt8(const AValue: UInt8);
begin
  if (not FIntf.SetAsUInt8(AValue)) then
    LibKoreFailed(FIntf);
end;

procedure TValue.SetAsUUID(const AUUID: TGUID);
begin
  if (not FIntf.SetAsUUID(AUUID)) then
    LibKoreFailed(FIntf);
end;

{ TValues }

function TValues.ByIntf(const AIntf: IcrValue): TValue;
begin
  Result := nil;
  LibKoreGetUserData(AIntf, Pointer(Result));
  if (Result = nil) then
    Result := TValue.Create(AIntf);
end;

function TValues.ByName(const AName: string): TValue;
var
  LIntfValue: IcrValue;
begin
  if (not FIntf.ByName(nil, AName, LIntfValue)) then
    LibKoreFailed(FIntf);

  Exit(ByIntf(LIntfValue));
end;

constructor TValues.Create(const AIntf: IcrValues);
begin
  FIntf := AIntf;
end;

destructor TValues.Destroy;
begin
  inherited Destroy;
end;

function TValues.Find(const AName: string): TValue;
var
  LIntfValue: IcrValue;
begin
  if (not FIntf.Find(nil, AName, LIntfValue)) then
    LibKoreFailed(FIntf);

  Exit(ByIntf(LIntfValue));
end;

function TValues.GetCount: Integer;
var
  LReturn: NativeInt;
begin
  if (not FIntf.GetCount(LReturn)) then
    LibKoreFailed(FIntf);
  Exit(LReturn);
end;

function TValues.GetItem(const AIndex: Integer): TValue;
var
  LIntfValue: IcrValue;
begin
  if (not FIntf.GetItem(AIndex, LIntfValue)) then
    LibKoreFailed(FIntf);

  Exit(ByIntf(LIntfValue));
end;

end.
