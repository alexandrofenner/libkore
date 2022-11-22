unit LibKoreDll;

interface

uses
  LibKoreIntfs;

{$ifdef CPUX86}
const
  dll_LibKore = 'LibKore32.dll';
{$endif CPUX86}

{$ifdef CPUX64}
const
  dll_LibKore = 'LibKore64.dll';
{$endif CPUX64}

function LibKore_CreateStringType(
  var AParams: TcrIntfCreateStringTypeParams): TcrIntfStringType; stdcall;
  external dll_LibKore name 'CreateStringType';

function LibKore_CreateFactory(
  const AStringType: TcrIntfStringType;
  const AClassName: TcrIntfStringData;
  var AFactory: IUnknown): WordBool; stdcall;
  external dll_LibKore name 'CreateFactory';

implementation

end.
