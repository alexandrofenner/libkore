unit LibKoreIntfs;

interface

type
  TcrChain = array[0..1] of Pointer;

  TcrDate = packed record
    FValue: Int32;
  end;

  TcrUTC = packed record
    FValue: Int16;
  end;

  TcrTime = packed record
    FValue: Int64;
  end;

  TcrTimeUTC = packed record
    FTime: TcrTime;
    FUTC: TcrUTC;
  end;

  TcrTimeStamp = packed record
    FValue: Int64;
  end;

  TcrTimeStampUTC = packed record
    FTimeStamp: TcrTimeStamp;
    FUTC: TcrUTC;
  end;

const
  crHashAlgId_Md5 = 1;
  crHashAlgId_Sha1 = 2;

  crStringCodec_Base64 = 1;
  crStringCodec_HexUpper = 2;
  crStringCodec_HexLower = 3;


  crXmlOptionItem_IgnoreWhiteSpace = $01;
  crXmlOptionItem_IgnoreComment = $02;
  crXmlOptionItem_Pretty = $04;
  crXmlOptionItem_NamesAsLowerCase = $08;

type
  TcrIntfResult = NativeInt;

  TcrIntfStringType = Pointer;
  TcrIntfStringData = Pointer;

  TcrIntfBytesAllocMethod =
    procedure(var ABytes: Pointer;
      const ABytesCount: NativeInt); stdcall;

  PcrIntfCreateStringTypeParams = ^TcrIntfCreateStringTypeParams;
  TcrIntfCreateStringTypeParams = record
  public
    Version: NativeUInt;
    UserData: Pointer;

    Clear:
      procedure(
        const AParams: PcrIntfCreateStringTypeParams;
        var AString: TcrIntfStringData); stdcall;

    CreateFromUnicode:
      procedure(
        const AParams: PcrIntfCreateStringTypeParams;
        var AString: TcrIntfStringData;
        const ACharsPtr: PWideChar;
        const ACharsCount: NativeInt); stdcall;

    CreateFromAnsi:
      procedure(
        const AParams: PcrIntfCreateStringTypeParams;
        var AString: TcrIntfStringData;
        const ACharsPtr: PAnsiChar;
        const ACharsCount: NativeInt); stdcall;

    Length:
      function(
        const AParams: PcrIntfCreateStringTypeParams;
        const AString: TcrIntfStringData): NativeInt; stdcall;

    GetCharsUnicode:
      function(
        const AParams: PcrIntfCreateStringTypeParams;
        const AString: TcrIntfStringData;
        const ACharsPtr: PWideChar;
        const ACharsCount: NativeInt): NativeInt; stdcall;

    GetCharsAnsi:
      function(
        const AParams: PcrIntfCreateStringTypeParams;
        const AString: TcrIntfStringData;
        const ACharsPtr: PAnsiChar;
        const ACharsCount: NativeInt): NativeInt; stdcall;
  end;

const
  IID_ISP_WideString = TcrIntfStringData(0);
  IID_ISP_UString = TcrIntfStringData(1);

type
  IcrUnknown = interface
  ['{94C6D7EC-8D7F-418C-A246-A99478DA73C2}']
    function GetUserData(const AId: NativeUInt;
      var AUserData: Pointer): TcrIntfResult; stdcall;
  end;

function crIntfResultIsError(const AValue: TcrIntfResult): Boolean; inline;

type
  IcrBigInteger = interface(IcrUnknown)
  ['{DA4C32AC-982C-4570-9130-12A26B3ABF79}']
  end;

  IcrBigDecimal = interface(IcrUnknown)
  ['{F9C0DC6E-A230-42A1-9703-A1C78664EEE5}']
  end;

const
  UserIdAction_Disposed = $01;

type
  IcrObject = interface;
  IcrErrorInfo = interface;

  TcrIntfUserCallbackParams = record
  public
    FUserData: Pointer;
    FIdAction: NativeUInt;
  end;

  TcrIntfUserCallback =
    function(var AParams: TcrIntfUserCallbackParams): WordBool; stdcall;

  IcrObject = interface(IcrUnknown)
  ['{A3A61077-137B-447F-99CC-7C664BB532F2}']
    function GetErrorInfo(const AByResult: TcrIntfResult;
      var AErrorInfo: IcrErrorInfo): WordBool; stdcall;
    function SetUserData(const AId: NativeUInt;
      const AUserData: Pointer): TcrIntfResult; stdcall;
    function SetUserCallback(const AId: NativeUInt;
      const AUserCallback: TcrIntfUserCallback): TcrIntfResult; stdcall;
  end;

  IcrErrorInfo = interface(IcrUnknown)
  ['{7D0E797F-C219-4167-A618-D31470D0A8F2}']
    function GetMessage(const AStringType: TcrIntfStringType;
      var AMessage: TcrIntfStringData): WordBool; stdcall;
    function GetStatusCode(var AStatusCode: Cardinal): WordBool; stdcall;
    function GetOsError(var AOsError: Cardinal): WordBool; stdcall;
    function GetCodeLocalId(var ACodeLocalId: PAnsiChar): WordBool; stdcall;
  end;

  IcrFlwCtrl = interface;
  IcrFlwCtrlFactory = interface;

  IcrFlwCtrl = interface(IcrUnknown)
  ['{64F9FDFE-E918-4B3E-AF1F-1F538F4338F0}']

    function Abort: TcrIntfResult; stdcall;
    function Reset: TcrIntfResult; stdcall;
  end;

  IcrFlwCtrlFactory = interface(IcrUnknown)
  ['{474E7541-D5FA-40F2-A387-F978E3FF2913}']

    function Create(const AOuter: IcrFlwCtrl;
      var AFlwCtrl: IcrFlwCtrl): TcrIntfResult; stdcall;
  end;

  IcrEncoding = interface;
  IcrEncodings = interface;

  IcrEncoding = interface(IcrUnknown)
  ['{609CF5DC-D67C-4CE5-B432-4B5ED26FC612}']
  end;

  IcrEncodings = interface(IcrUnknown)
  ['{A42556F8-C3D0-4B95-A135-2E3ADCF44CD4}']
    function Utf8(var AEncoding: IcrEncoding): TcrIntfResult; stdcall;
    function SimpleAnsi(var AEncoding: IcrEncoding): TcrIntfResult; stdcall;
  end;

  IcrCharset = interface;
  IcrCharsets = interface;

  IcrCharset = interface(IcrUnknown)
  ['{4FF53C51-4698-4DDA-9B1E-B53543BA471C}']
  end;

  IcrCharsets = interface(IcrUnknown)
  ['{4FF53C51-4698-4DDA-9B1E-B53543BA471C}']
    function Utf8U(var ACharset: IcrCharset): TcrIntfResult; stdcall;
    function Utf8L(var ACharset: IcrCharset): TcrIntfResult; stdcall;
    function Utf8XmlDFeU(var ACharset: IcrCharset): TcrIntfResult; stdcall;
    function Utf8XmlDFeL(var ACharset: IcrCharset): TcrIntfResult; stdcall;
  end;

  IcrStream = interface(IcrUnknown)
  ['{C2395A79-0018-49F0-9615-C54A55987198}']

    function GetSize(var ASize: Int64): TcrIntfResult; stdcall;
    function SetSize(const ASize: Int64): TcrIntfResult; stdcall;

    function Read(ABytesPtr: PByte; ABytesCount: NativeInt;
      var ABytesRead: NativeInt): TcrIntfResult; stdcall;
    function Write(ABytesPtr: PByte; ABytesCount: NativeInt;
      var ABytesWritten: NativeInt): TcrIntfResult; stdcall;
    function Seek(AOffset: Int64; AOrigin: Integer;
      var AReturn: Int64): TcrIntfResult; stdcall;

    function Flush: TcrIntfResult; stdcall;
  end;

  IcrOsProcessInfo = interface(IcrUnknown)
  ['{1B3B8B6E-017C-404E-96A8-1DA6C08D9D80}']

    function GetBits(var ABits: Cardinal): TcrIntfResult; stdcall;
    function GetParentPId(var AParentPId: Cardinal): TcrIntfResult; stdcall;
    function GetCurrentDirectory(const AStringType: TcrIntfStringType;
      var ACurrentDirectory: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetDllPath(const AStringType: TcrIntfStringType;
      var ADllPath: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetImagePathName(const AStringType: TcrIntfStringType;
      var AImagePathName: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetCommandLine(const AStringType: TcrIntfStringType;
      var ACommandLine: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetWindowTitle(const AStringType: TcrIntfStringType;
      var AWindowTitle: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

type
  IcrConsole = interface;
  IcrConsoleUtils = interface;
  IcrConsoleFactory = interface;

  TcrConsoleAddLineMethod =
    procedure(const AUserData: Pointer;
      const AStringType: TcrIntfStringType;
      const AStringData: TcrIntfStringData); stdcall;

  IcrConsole = interface(IcrUnknown)
  ['{4AC82DD8-F944-43C8-9ED4-C2B9812AF766}']

    function AlternativeBuffer(AOnOff: WordBool): TcrIntfResult; stdcall;
    { Apagar a linha. Se AIdType
        = 0 -> Da posição atual até o fim da linha
        = 1 -> Do inicio até a posição atual
        = 2 -> Toda a linha }
    function ClearLine(const AIdType: Byte): TcrIntfResult; stdcall;
    { Apagar a tela. Se AIdType
        = 0 -> Da posição atual até o fim da tela
        = 1 -> Do ínicio até a posição atual
        = 2 -> Toda a tela }
    function ClearScreen(const AIdType: Byte): TcrIntfResult; stdcall;

    function CursorUp(const ACount: Cardinal): TcrIntfResult; stdcall;
    function CursorDown(const ACount: Cardinal): TcrIntfResult; stdcall;
    function CursorForward(const ACount: Cardinal): TcrIntfResult; stdcall;
    function CursorBack(const ACount: Cardinal): TcrIntfResult; stdcall;
    function CursorNextLine(const ACount: Cardinal): TcrIntfResult; stdcall;
    function CursorPreviousLine(const ACount: Cardinal): TcrIntfResult; stdcall;
    function CursorHorzAbsolute(const ACount: Cardinal): TcrIntfResult; stdcall;

    function DeviceStatusReport: TcrIntfResult; stdcall;
    function RestoreCurrentCursorPosition: TcrIntfResult; stdcall;
    function RestoreScreen: TcrIntfResult; stdcall;

    function SaveCurrentCursorPosition: TcrIntfResult; stdcall;
    function SaveScreen: TcrIntfResult; stdcall;

    function SetBracketPasteMode(const AOnOff: WordBool): TcrIntfResult; stdcall;
    function SetEnabledAuxPort(const AOnOff: WordBool): TcrIntfResult; stdcall;

    function SetCursorPosition(const AX, AY: Cardinal): TcrIntfResult; stdcall;
    function SetCursorVisible(const AOnOff: WordBool): TcrIntfResult; stdcall;

    function SetTitle(const AStringType: TcrIntfStringType;
      const ATitle: TcrIntfStringData): TcrIntfResult; stdcall;

    function ScrollUp(const ACount: Cardinal): TcrIntfResult; stdcall;
    function ScrollDown(const ACount: Cardinal): TcrIntfResult; stdcall;

    function SelectGraphicRenditionRGB(
      const AId, ARGB: Cardinal): TcrIntfResult; stdcall;

    function Text(const AStringType: TcrIntfStringType;
      const AText: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

  IcrConsoleUtils = interface(IcrUnknown)
  ['{50965E94-9A90-4A9A-B765-361FCE127FE3}']

    function AddFromAnsiEscCodeBuffer(
      const AConsole: IcrConsole;
      const ABytesPtr: PByte;
      const ABytesCount: Integer): TcrIntfResult; stdcall;

    function AddFromAnsiEscCodeFile(
      const AConsole: IcrConsole;
      const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

  IcrConsoleFactory = interface(IcrUnknown)
  ['{C2909600-8297-4897-A592-1594833A108B}']

    function CreateUtils(var AUtils: IcrConsoleUtils): TcrIntfResult; stdcall;

    function CreateDefault(var AConsole: IcrConsole): TcrIntfResult; stdcall;
    function CreateByProcessId(const AProcessId: Cardinal;
      var AConsole: IcrConsole): TcrIntfResult; stdcall;

    function CreateByAddLine(
      const AAddLineCode: TcrConsoleAddLineMethod;
      const AAddLineData: Pointer;
      const AStringType: TcrIntfStringType;
      var AConsole: IcrConsole): TcrIntfResult; stdcall;

    function CreateByOutHandle(const AOutHandle: THandle;
      var AConsole: IcrConsole): TcrIntfResult; stdcall;

    function CreateCacheByWnd(const AWnd: THandle;
      const AInnerConsole: IcrConsole;
      var AConsole: IcrConsole): TcrIntfResult; stdcall;
  end;

type
  IcrWinPseudoConsole = interface;
  IcrWinPseudoConsoleFactory = interface;

  IcrWinPseudoConsole = interface(IcrUnknown)
  ['{48D7005F-7D43-4C77-AFED-E191FF7C78B2}']

    function Execute: TcrIntfResult; stdcall;
    function IsTerminated(var AIsTerminated: WordBool): TcrIntfResult; stdcall;
    function BeginExecute: TcrIntfResult; stdcall;
    function GetExitCode(var AExitCode: Cardinal): TcrIntfResult; stdcall;
  end;

  IcrWinPseudoConsoleFactory = interface(IcrUnknown)
  ['{3A07BF1C-539C-4C32-A6CA-BE4D8BF88344}']

    { RemoveAdministrativePermissions ->
        Remove os privilégios de administrador
        É útil quando o aplicativo 'pai' estiver sendo executado com
        privilégios de administrador e se deseja executar um processo
        'filho' somente com privilégios de usuário normal }
    function RemoveAdministrativePermissions: TcrIntfResult; stdcall;

    { SetOsLogin ->
        Reservado para versões futuras }
    function SetOsLogin(const AStringType: TcrIntfStringType;
      const AUserName, ADomain,
      APassword: TcrIntfStringData): TcrIntfResult; stdcall;

    { SetCommandLine ->
        Indicar a linha de comando que deverá ser executada }
    function SetCommandLine(const AStringType: TcrIntfStringType;
      const ACommandLine: TcrIntfStringData): TcrIntfResult; stdcall;
    { SetDirectory ->
        Indica qual será o diretório atual do processo 'filho' }
    function SetDirectory(const AStringType: TcrIntfStringType;
      const ADirectory: TcrIntfStringData): TcrIntfResult; stdcall;
    { SetVariable ->
        Alterar o valor de uma variável de ambiente.
        Este valor alterado estará disponível apenas para o processo 'filho' }
    function SetVariable(const AStringType: TcrIntfStringType;
      const AName, AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    { SetRecordingStream ->
        Indica uma stream para 'gravar' o conteúdo de saída do console }
    function SetRecordingStream(const AStream: IcrStream): TcrIntfResult; stdcall;
    { SetRecordingFile ->
        Indica um arquivo para 'gravar' o conteúdo de saída do console }
    function SetRecordingFileName(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;
    { SetConsole ->
        Indica um console sobre o qual o processo filho será criado }
    function SetConsole(const AConsole: IcrConsole): TcrIntfResult; stdcall;
    { Create ->
        Cria um objeto 'IcrWinPseudoConsole' }
    function Create(var AIntf: IcrWinPseudoConsole): TcrIntfResult; stdcall;
  end;

type
  IcrBjsnReader = interface;
  IcrBjsnWriter = interface;

  PcrBjsnStoreOffset = ^TcrBjsnStoreOffset;
  TcrBjsnStoreOffset = record
  public
    FChain: TcrChain;
    FTempOffset: Integer;
    FFinalOffset: Integer;
    FIsValid: Boolean;
  end;

  IcrBjsnReader = interface(IcrUnknown)
  ['{851174CD-41EA-4FF9-A069-A96C7B0E08BC}']
    function LoadFromBytes(const ABytesPtr: PByte;
      const ABytesCount: NativeInt;
      const ADataIsStatic: WordBool): TcrIntfResult; stdcall;

    function LoadFromBytesSigned(const ABytesPtr: PByte;
      const ABytesCount: NativeInt;
      const ADataIsStatic: WordBool): TcrIntfResult; stdcall;

    function LoadFromFile(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;
    function LoadFromFileSigned(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;

    function LoadFromJsonBytes(const AJsonBytesPtr: PByte;
      const AJsonBytesCount: NativeInt): TcrIntfResult; stdcall;
    function LoadFromJsonBytesSigned(const AJsonBytesPtr: PByte;
      const AJsonBytesCount: NativeInt): TcrIntfResult; stdcall;

    function LoadFromJsonUCStr(const AUCStrCharsPtr: PWideChar;
      const AUCStrCharsCount: NativeInt): TcrIntfResult; stdcall;
    function LoadFromJsonString(const AStringType: TcrIntfStringType;
      const AStringData: TcrIntfStringData): TcrIntfResult; stdcall;

    function LoadFromJsonFile(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;
    function LoadFromJsonFileSigned(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;

    function LoadFromBytesAutoDetectType(const ABytesPtr: PByte;
      const ABytesCount: NativeInt;
      const ADataIsStatic: WordBool): TcrIntfResult; stdcall;
    function LoadFromFileAutoDetectType(
      const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetNextTokenType(var ATokenType: Cardinal): TcrIntfResult; stdcall;

    { Fci }
    function TryReadFciIndex(var AIndex: Cardinal;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadFciIndexElseObjEnd(var AIndex: Cardinal;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function TryReadName(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadNameElseObjEnd(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function TryReadID(var AId: UInt64;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadIDElseObjEnd(var AId: UInt64;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadNji(var ANji: Cardinal;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadNjiElseObjEnd(var ANji: Cardinal;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function TryReadInt32(var AValue: Int32;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadInt64(var AValue: Int64;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function TryReadString(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData; var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadArrayBegin(var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadArrayEnd(var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadObjBegin(var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadObjEnd(var AReturn: WordBool): TcrIntfResult; stdcall;

    function TryReadObjBeginElseArrayEnd(
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function ReadThreeState(var AReturn: Cardinal): TcrIntfResult; stdcall;
    function ReadBoolean(var AReturn: WordBool): TcrIntfResult; stdcall;
    function ReadBooleanNullable(var AReturn: WordBool): TcrIntfResult; stdcall;
    function ReadBooleanByString(var AReturn: WordBool): TcrIntfResult; stdcall;
    function ReadBooleanEx(const ATypeId: Cardinal;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function ReadNull: TcrIntfResult; stdcall;

    function ReadInt32(var AReturn: Int32): TcrIntfResult; stdcall;
    function ReadInt32Nullable(var AReturn: Int32): TcrIntfResult; stdcall;
    function ReadInt32ByString(var AReturn: Int32): TcrIntfResult; stdcall;
    function ReadInt32ByStringNullable(var AReturn: Int32): TcrIntfResult; stdcall;
    function ReadInt32Ex(const ATypeId: Cardinal;
      var AReturn: Int32): TcrIntfResult; stdcall;

    function ReadInt64(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadInt64Nullable(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadInt64ByString(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadInt64ByStringNullable(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadInt64Ex(const ATypeId: Cardinal;
      var AReturn: Int64): TcrIntfResult; stdcall;

    function ReadUInt32(var AReturn: UInt32): TcrIntfResult; stdcall;
    function ReadUInt32Nullable(var AReturn: UInt32): TcrIntfResult; stdcall;
    function ReadUInt32ByString(var AReturn: UInt32): TcrIntfResult; stdcall;
    function ReadUInt32ByStringNullable(var AReturn: UInt32): TcrIntfResult; stdcall;
    function ReadUInt32Ex(const ATypeId: Cardinal;
      var AReturn: UInt32): TcrIntfResult; stdcall;

    function ReadUInt64(var AReturn: UInt64): TcrIntfResult; stdcall;
    function ReadUInt64Nullable(var AReturn: UInt64): TcrIntfResult; stdcall;
    function ReadUInt64ByString(var AReturn: UInt64): TcrIntfResult; stdcall;
    function ReadUInt64ByStringNullable(var AReturn: UInt64): TcrIntfResult; stdcall;
    function ReadUInt64Ex(const ATypeId: Cardinal;
      var AReturn: UInt64): TcrIntfResult; stdcall;

    function ReadBigInteger(var AReturn: IcrBigInteger): TcrIntfResult; stdcall;

    function ReadDecimalD1(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadDecimalD2(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadDecimalD3(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadDecimalD4(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadDecimalD5(var AReturn: Int64): TcrIntfResult; stdcall;
    function ReadDecimalD6(var AReturn: Int64): TcrIntfResult; stdcall;

    function ReadDouble(var AReturn: Double): TcrIntfResult; stdcall;
    function ReadDoubleNullable(var AReturn: Double): TcrIntfResult; stdcall;
    function ReadDoubleByString(var AReturn: Double): TcrIntfResult; stdcall;
    function ReadDoubleByStringNullable(
      var AReturn: Double): TcrIntfResult; stdcall;
    function ReadDoubleEx(const ATypeId: Cardinal;
      var AReturn: Double): TcrIntfResult; stdcall;

    function ReadCurrency(var AReturn: Currency): TcrIntfResult; stdcall;
    function ReadCurrencyNullable(
      var AReturn: Currency): TcrIntfResult; stdcall;
    function ReadCurrencyByString(
      var AReturn: Currency): TcrIntfResult; stdcall;
    function ReadCurrencyByStringNullable(
      var AReturn: Currency): TcrIntfResult; stdcall;
    function ReadCurrencyEx(const ATypeId: Cardinal;
      var AReturn: Currency): TcrIntfResult; stdcall;

    function ReadBigDecimal(
      var ABigDecimal: IcrBigDecimal): TcrIntfResult; stdcall;

    function ReadString(const AStringType: TcrIntfStringType;
      var AReturn: TcrIntfStringData): TcrIntfResult; stdcall;
    function ReadStringNullable(const AStringType: TcrIntfStringType;
      var AReturn: TcrIntfStringData): TcrIntfResult; stdcall;
    function ReadStringEx(const ATypeId: Cardinal;
      const AStringType: TcrIntfStringType;
      var AReturn: TcrIntfStringData): TcrIntfResult; stdcall;

    function ReadDataAsString(const AStringType: TcrIntfStringType;
      var AReturn: TcrIntfStringData): TcrIntfResult; stdcall;

    function ReadUUID(var AReturn: TGUID): TcrIntfResult; stdcall;
    function ReadBytes(var AReturn: Pointer;
      const AAllocMethod: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;
    function ReadBytesBase64(var AReturn: Pointer;
      const AAllocMethod: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;

    function ReadTimeStamp(var AReturn: TcrTimeStamp): TcrIntfResult; stdcall;
    function ReadTimeStampOrZeroIfNull(
      var AReturn: TcrTimeStamp): TcrIntfResult; stdcall;
    function ReadTimeStampUTC(
      var AReturn: TcrTimeStampUTC): TcrIntfResult; stdcall;

    function ReadAsBjsnBytes(var AReturn: Pointer;
      const AAllocMethod: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;
    function ReadAsJsonBytes(const AIsPretty: WordBool; var AReturn: Pointer;
      const AAllocMethod: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;

    function TryReadNull(var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadBooleanElseNull(var AValue: WordBool;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadUInt8ElseNull(var AValue: Byte;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadUInt16ElseNull(var AValue: Word;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadUInt32ElseNull(var AValue: Cardinal;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadUInt64ElseNull(var AValue: UInt64;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadSInt8ElseNull(var AValue: ShortInt;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadSInt16ElseNull(var AValue: SmallInt;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadSInt32ElseNull(var AValue: Integer;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadSInt64ElseNull(var AValue: Int64;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadCurrencyElseNull(var AValue: Currency;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadReal32ElseNull(var AValue: Single;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadReal64ElseNull(var AValue: Double;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function TryReadStringElseNull(
      const AStringType: TcrIntfStringType; var AValue: TcrIntfStringData;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function TryReadBytesElseNull(var ABytes: Pointer;
      const ABytesAlloc: TcrIntfBytesAllocMethod;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function ReadAsBjsnBuffer(const AWritePrefix: Boolean; var ABytes: Pointer;
      const ABytesAlloc: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;

    function ReadName(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData): TcrIntfResult; stdcall;
    function ReadID(var AId: UInt64): TcrIntfResult; stdcall;

    function ReadAt(const AStringType: TcrIntfStringType;
      const AFieldName: TcrIntfStringData;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function IgnoreFields: TcrIntfResult; stdcall;

    function ReadArrayBegin: TcrIntfResult; stdcall;
    function ReadArrayEnd: TcrIntfResult; stdcall;

    function ReadObjBegin: TcrIntfResult; stdcall;
    function ReadObjEnd: TcrIntfResult; stdcall;

    function IgnoreValue: TcrIntfResult; stdcall;

    function ToJsonBytes(const AIsPretty: WordBool; var ABytes: Pointer;
      const ABytesAlloc: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;

    function ToJsonFile(const AStringType: TcrIntfStringType;
      const AFieldName: TcrIntfStringData;
      const AIsPretty: WordBool): TcrIntfResult; stdcall;

    function GetCursor(var ACursor: PByte): TcrIntfResult; stdcall;
    function GetOutOf(var AOutOf: PByte): TcrIntfResult; stdcall;
    function SetCursor(const ACursor: PByte): TcrIntfResult; stdcall;
    function GetBuffer(var ABuffer: Pointer;
      const ABytesAlloc: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;

    function Restart: TcrIntfResult; stdcall;

    function TryScrollToField(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function GetIsEof(var AIsEof: WordBool): TcrIntfResult; stdcall;
  end;

  IcrBjsnWriter = interface(IcrUnknown)
  ['{B4C0927A-9BA2-4D39-92CD-FD331C848603}']

    function SetHeaderSize(const AHeaderSize: Integer): TcrIntfResult; stdcall;
    function Reset: TcrIntfResult; stdcall;
    function Flush(const AWritePrefix: Boolean; var ABytesPtr: PByte;
      var ABytesCount: NativeInt): TcrIntfResult; stdcall;

    function WriteByInnerObjJsonBytes(const ABytesPtr: PByte;
      const ABytesCount: NativeInt): TcrIntfResult; stdcall;

    function WriteItemByReader(const AReader: IcrBjsnReader;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function WriteItemsByReaderBeforeUntilObjEnd(const AReader: IcrBjsnReader;
      var AReturn: Integer): TcrIntfResult; stdcall;
    function WriteItemsByReaderBeforeUntilArrayEnd(const AReader: IcrBjsnReader;
      var AReturn: Integer): TcrIntfResult; stdcall;

    function PushStoreOffset(
      var AOffsetField: TcrBjsnStoreOffset): TcrIntfResult; stdcall;
    function WriteOffset(
      var AOffsetField: TcrBjsnStoreOffset): TcrIntfResult; stdcall;

    function WriteThreeState(const AValue: Cardinal): TcrIntfResult; stdcall;
    function WriteBoolean(const AValue: WordBool): TcrIntfResult; stdcall;
    function WriteNull: TcrIntfResult; stdcall;
    function WriteArrayBegin: TcrIntfResult; stdcall;
    function WriteArrayEnd: TcrIntfResult; stdcall;
    function WriteObjBegin: TcrIntfResult; stdcall;
    function WriteObjEnd: TcrIntfResult; stdcall;
    function WriteUInt32(const AValue: UInt32): TcrIntfResult; stdcall;
    function WriteUInt64(const AValue: UInt64): TcrIntfResult; stdcall;
    function WriteInt32(const AValue: Int32): TcrIntfResult; stdcall;
    function WriteInt64(const AValue: Int64): TcrIntfResult; stdcall;
    function WriteBigInteger(const AValue: IcrBigInteger): TcrIntfResult; stdcall;

    function WriteDouble(const AValue: Double): TcrIntfResult; stdcall;
    function WriteDecimal(const AIntPart, AFracPart: UInt64;
      const ADigits: Integer; const AIsNegative: WordBool): TcrIntfResult; stdcall;
    function WriteDecimalByString(const AStringType: TcrIntfStringType;
      const ADecimal: TcrIntfStringData): TcrIntfResult; stdcall;
    function WriteBigDecimal(const AValue: IcrBigDecimal): TcrIntfResult; stdcall;
    function WriteCurrency(const AValue: Currency): TcrIntfResult; stdcall;

    function WriteString(const AStringType: TcrIntfStringType;
      const AStringValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function WriteStringDate(const AValue: TcrDate): TcrIntfResult; stdcall;
    function WriteStringTime(const AValue: TcrTime): TcrIntfResult; stdcall;
    function WriteStringTimeUTC(const AValue: TcrTimeUTC): TcrIntfResult; stdcall;
    function WriteStringTimeStamp(const AValue: TcrTimeStamp): TcrIntfResult; stdcall;
    function WriteStringTimeStampUTC(
      const AValue: TcrTimeStampUTC): TcrIntfResult; stdcall;

    function WriteStringMoney2DecInv(const AValue: Currency): TcrIntfResult; stdcall;
    function WriteStringCurrency(const AValue: Currency): TcrIntfResult; stdcall;
    function WriteStringCurrency2(const AValue: Currency;
      const ADigits: Integer): TcrIntfResult; stdcall;
    function WriteStringDouble(const AValue: Double): TcrIntfResult; stdcall;

    function WriteCStr(const ACStrPtr: PWideChar;
      const ACStrLength: NativeInt): TcrIntfResult; stdcall;

    function WriteFieldName(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData): TcrIntfResult; stdcall;
    function WriteFieldId(const AFieldId: UInt64): TcrIntfResult; stdcall;

    function WriteDataByReader(
      const AReader: IcrBjsnReader): TcrIntfResult; stdcall;

    function WriteBytesArray(const ABytesPtr: PByte;
      const ABytesCount: NativeInt): TcrIntfResult; stdcall;
    function WriteBytesHexUpper(const ABytesPtr: PByte;
      const ABytesCount: NativeInt): TcrIntfResult; stdcall;
    function WriteBytesHexLower(const ABytesPtr: PByte;
      const ABytesCount: NativeInt): TcrIntfResult; stdcall;
    function WriteBytesBase64(const ABytesPtr: PByte;
      const ABytesCount: NativeInt): TcrIntfResult; stdcall;

    function WriteUUID(const AUUID: TGUID): TcrIntfResult; stdcall;

    function WriteValueByJsonBytes(const AJsonBytesPtr: PByte;
      const AJsonBytesCount: NativeInt): TcrIntfResult; stdcall;
    function WriteValueByBjsnBytes(const ABjsnBytesPtr: PByte;
      const ABjsnBytesCount: NativeInt): TcrIntfResult; stdcall;

    function WriteFieldThreeState(
      const AStringType: TcrIntfStringType; const AName: TcrIntfStringData;
      const AValue: Cardinal): TcrIntfResult; stdcall;

    function WriteFieldBool(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      const AValue: WordBool): TcrIntfResult; stdcall;

    function WriteFieldBoolEx(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData; const AValue: WordBool;
      const ATypeId: Cardinal): TcrIntfResult; stdcall;

    function WriteFieldNull(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData): TcrIntfResult; stdcall;


//    procedure WriteFieldNull(const AName: UString);
//    procedure WriteFieldUInt32(const AName: UString; const AValue: UInt32);
//    procedure WriteFieldUInt32Ex(const AName: UString; const AValue: UInt32;
//      const ATypeId: TcrBjsnMapFieldTypeId);
//    procedure WriteFieldUInt64(const AName: UString; const AValue: UInt64);
//    procedure WriteFieldUInt64Ex(const AName: UString; const AValue: UInt64;
//      const ATypeId: TcrBjsnMapFieldTypeId);
//
//    procedure WriteFieldUIntPtr(const AName: UString; const AValue: UIntPtr); inline;
//    procedure WriteFieldInt32(const AName: UString; const AValue: Int32);
//    procedure WriteFieldInt32Ex(const AName: UString; const AValue: Int32;
//      const ATypeId: TcrBjsnMapFieldTypeId);
//    procedure WriteFieldInt32IfNotZero(const AName: UString; const AValue: Int32);
//    procedure WriteFieldInt64(const AName: UString; const AValue: Int64);
//    procedure WriteFieldInt64Ex(const AName: UString; const AValue: Int64;
//      const ATypeId: TcrBjsnMapFieldTypeId);
//    procedure WriteFieldInt64IfNotZero(const AName: UString; const AValue: Int64);
//    procedure WriteFieldNativeInt(const AName: UString;
//      const AValue: NativeInt);
//    procedure WriteFieldBigInteger(const AName: UString;
//      const AValue: BigInteger);
//    procedure WriteFieldDouble(const AName: UString; const AValue: Double);
//    procedure WriteFieldDoubleEx(const AName: UString; const AValue: Double;
//      const ATypeId: TcrBjsnMapFieldTypeId);
//    procedure WriteFieldNumeric(const AName: UString; const AValue: TcrNumeric);
//    procedure WriteFieldDecimal(const AName: UString;
//      const AIntPart, AFracPart: UInt64;
//      const ADigits: Integer; const AIsNegative: Boolean); overload;
//    procedure WriteFieldDecimal(const AName: UString; const ABCDString: UString); overload;
//    procedure WriteFieldDecimal(const AName: UString; const ADecimal: TcrDecimal); overload;
//    procedure WriteFieldBigDecimal(const AName: UString; const AValue: BigDecimal);
//    procedure WriteFieldCurrency(const AName: UString; const AValue: Currency); overload;
//    procedure WriteFieldCurrency(const AName: UString; const AValue: Currency;
//      const ADigits: Integer); overload;
//    procedure WriteFieldCurrencyEx(const AName: UString; const AValue: Currency;
//      const ATypeId: TcrBjsnMapFieldTypeId); overload;
//    procedure WriteFieldCurrencyEx(const AName: UString; const AValue: Currency;
//      const ADigits: Integer; const ATypeId: TcrBjsnMapFieldTypeId); overload;
//    procedure WriteFieldCurrencyIfNotZero(const AName: UString;
//      const AValue: Currency);
//    procedure WriteFieldString(const AName, AValue: LString); overload;
//    procedure WriteFieldString(const AName: UString; const AValue: LString); overload;
//    procedure WriteFieldString(const AName, AValue: UString); overload;
//    procedure WriteFieldStringFmt(const AName, AValueFmt: UString;
//      const AValueArgs: array of const); overload;
//    procedure WriteFieldStringBool(const AName: UString; const AValue: Boolean);
//    procedure WriteFieldStringNullable(const AName, AValue: UString);
//    procedure WriteFieldStringDate(const AName: UString; const AValue: TDate);
//    procedure WriteFieldStringTime(const AName: UString; const AValue: TTime);
//    procedure WriteFieldStringDateTime(const AName: UString;
//      const AValue: TDateTime);
//    procedure WriteFieldStringTimeStamp(const AName: UString;
//      const AValue: TcrTimeStamp);
//    procedure WriteFieldStringTimeStampNullIfZero(const AName: UString;
//      const AValue: TcrTimeStamp);
//    procedure WriteFieldStringTimeStampIfNotZero(const AName: UString;
//      const AValue: TcrTimeStamp);
//    procedure WriteFieldStringIfNotEmpty(const AName, AValue: UString);
//    procedure WriteFieldStringMoney2DecInv(const AName: UString;
//      const AValue: Currency);
//    procedure WriteFieldStringCurrency(const AName: UString; const AValue: Currency); overload;
//    procedure WriteFieldStringCurrency(const AName: UString; const AValue: Currency;
//      const ADigits: Integer); overload;
//    procedure WriteFieldStringDouble(const AName: UString; const AValue: Double);
//    procedure WriteFieldStringInt32(const AName: UString; const AValue: Integer);
//    procedure WriteFieldStringInt64(const AName: UString; const AValue: Int64);
//    procedure WriteFieldStringUInt32(const AName: UString; const AValue: UInt32);
//    procedure WriteFieldStringUInt64(const AName: UString; const AValue: UInt64);
//    procedure WriteFieldStringBool01(const AName: UString; const AValue: Boolean);
//    procedure WriteFieldBytesArray(const AName: UString; const AValue: TBytes); overload;
//    procedure WriteFieldBytesBase64(const AName: UString; const AValue: TBytes); overload;
//    procedure WriteFieldBytesBase64(const AName: UString; const ABytesPtr: PByte;
//      const ABytesCount: Integer); overload;
//    procedure WriteFieldUUID(const AName: UString; const AUUID: TcrUUID);
//    procedure WriteFieldArrayBegin(const AName: UString);
//    procedure WriteFieldObjBegin(const AName: UString);
//    procedure WriteFieldOffset(const AName: UString;
//      var AOffsetField: TcrBjsnStoreOffset);
//
//    procedure WriteFieldTime(const AName: UString; const AValue: TcrTime);
//    procedure WriteFieldTimeTZ(const AName: UString; const AValue: TcrTimeUTC);
//    procedure WriteFieldDate(const AName: UString; const AValue: TcrDate);
//    procedure WriteFieldTimeStamp(const AName: UString;
//      const AValue: TcrTimeStamp);
//    procedure WriteFieldTimeStampTZ(const AName: UString;
//      const AValue: TcrTimeStampUTC);
//
//    function WriteFieldValueByJsonBytes(const AName: UString;
//      const AJsonBytes: TBytes): Boolean;
//    function WriteFieldValueByBjsnBytes(const AName: UString;
//      const ABjsnBytes: TBytes): Boolean;
//
//    { xData }
//    procedure WriteFieldxBool(const AName: UString; const AValue: TxBoolean);
//    procedure WriteFieldxUInt8(const AName: UString; const AValue: TxUInt8);
//    procedure WriteFieldxUInt16(const AName: UString; const AValue: TxUInt16);
//    procedure WriteFieldxUInt32(const AName: UString; const AValue: TxUInt32);
//    procedure WriteFieldxUInt64(const AName: UString; const AValue: TxUInt64);
//    procedure WriteFieldxSInt8(const AName: UString; const AValue: TxSInt8);
//    procedure WriteFieldxSInt16(const AName: UString; const AValue: TxSInt16);
//    procedure WriteFieldxSInt32(const AName: UString; const AValue: TxSInt32);
//    procedure WriteFieldxSInt64(const AName: UString; const AValue: TxSInt64);
//    procedure WriteFieldxBigInteger(const AName: UString;
//      const AValue: TxBigInteger);
//    procedure WriteFieldxCurrency(const AName: UString;
//      const AValue: TxCurrency);
//    procedure WriteFieldxBigDecimal(const AName: UString;
//      const AValue: TxBigDecimal);
//    procedure WriteFieldxReal32(const AName: UString; const AValue: TxReal32);
//    procedure WriteFieldxReal64(const AName: UString; const AValue: TxReal64);
//    procedure WriteFieldxNumeric(const AName: UString; const AValue: TxNumeric);
//    procedure WriteFieldxChar(const AName: UString; const AValue: TxChar);
//    procedure WriteFieldxString(const AName: UString; const AValue: TxText);
//    procedure WriteFieldxBytes(const AName: UString; const AValue: TxBinary);
//    procedure WriteFieldxUUID(const AName: UString; const AValue: TxUUID);
//    procedure WriteFieldxTime(const AName: UString; const AValue: TxTime);
//    procedure WriteFieldxTimeTZ(const AName: UString; const AValue: TxTimeTZ);
//    procedure WriteFieldxDate(const AName: UString; const AValue: TxDate);
//    procedure WriteFieldxTimeStamp(const AName: UString;
//      const AValue: TxTimeStamp);
//    procedure WriteFieldxTimeStampTZ(const AName: UString;
//      const AValue: TxTimeStampTZ);
//
//    procedure WriteFieldIPv4DynArray(const AName: UString;
//      const AValue: TcrIPv4DynArray);
//    procedure WriteFieldIPv6DynArray(const AName: UString;
//      const AValue: TcrIPv6DynArray);
//    procedure WriteFieldStringDynArray(const AName: UString;
//      const AValue: array of UString);
//
//    procedure WriteFieldInt64DynArray(const AName: UString;
//      var AList: TcrInt64List); overload;
//    procedure WriteFieldInt64DynArray(const AName: UString;
//      const AValue: TcrInt64DynArray); overload;
//
//    procedure WriteIdxFieldBool(var AIndex: Integer; const AName: UString;
//      const AValue: Boolean);
//    procedure WriteIdxFieldNull(var AIndex: Integer; const AName: UString);
//    procedure WriteIdxFieldUInt32(var AIndex: Integer; const AName: UString;
//      const AValue: UInt32);
//    procedure WriteIdxFieldUInt64(var AIndex: Integer; const AName: UString;
//      const AValue: UInt64);
//    procedure WriteIdxFieldUIntPtr(var AIndex: Integer; const AName: UString;
//      const AValue: NativeUInt); inline;
//    procedure WriteIdxFieldInt32(var AIndex: Integer; const AName: UString;
//      const AValue: Int32);
//    procedure WriteIdxFieldInt64(var AIndex: Integer; const AName: UString;
//      const AValue: Int64);
//    procedure WriteIdxFieldDouble(var AIndex: Integer; const AName: UString;
//      const AValue: Double);
//    procedure WriteIdxFieldDecimal(var AIndex: Integer; const AName: UString;
//      const AIntPart, AFracPart: UInt64; const ADigits: Integer;
//      const AIsNegative: Boolean); overload;
//    procedure WriteIdxFieldDecimal(var AIndex: Integer; const AName: UString;
//      const ABCDString: UString); overload;
//    procedure WriteIdxFieldCurrency(var AIndex: Integer; const AName: UString;
//      const AValue: Currency); overload;
//    procedure WriteIdxFieldCurrency(var AIndex: Integer; const AName: UString;
//      const AValue: Currency; const ADigits: Integer); overload;
//    procedure WriteIdxFieldString(var AIndex: Integer; const AName: UString;
//      const AValue: LString); overload;
//    procedure WriteIdxFieldString(var AIndex: Integer; const AName, AValue: UString); overload;
//    procedure WriteIdxFieldStringDate(var AIndex: Integer; const AName: UString;
//      const AValue: TDate);
//    procedure WriteIdxFieldStringTime(var AIndex: Integer; const AName: UString;
//      const AValue: TTime);
//    procedure WriteIdxFieldStringDateTime(var AIndex: Integer; const AName: UString;
//      const AValue: TDateTime);
//    procedure WriteIdxFieldBytesBase64(var AIndex: Integer; const AName: UString;
//      const AValue: TBytes); overload;
//    procedure WriteIdxFieldBytesBase64(var AIndex: Integer; const AName: UString;
//      const ABytesPtr: PByte; const ABytesCount: Integer); overload;
//    procedure WriteIdxFieldArrayBegin(var AIndex: Integer; const AName: UString);
//    procedure WriteIdxFieldObjBegin(var AIndex: Integer; const AName: UString);
//
//    { fci }
//    procedure fciWriteField(const AIndex: Cardinal);
//    procedure fciWriteFieldBool(const AIndex: Cardinal; const AValue: Boolean);
//    procedure fciWriteFieldNull(const AIndex: Cardinal);
//    procedure fciWriteFieldUInt32(const AIndex: Cardinal; const AValue: UInt32);
//    procedure fciWriteFieldUInt64(const AIndex: Cardinal; const AValue: UInt64);
//    procedure fciWriteFieldUIntPtr(const AIndex: Cardinal; const AValue: UIntPtr); inline;
//    procedure fciWriteFieldInt32(const AIndex: Cardinal; const AValue: Int32);
//    procedure fciWriteFieldInt64(const AIndex: Cardinal; const AValue: Int64);
//    procedure fciWriteFieldString(const AIndex: Cardinal; const AValue: UString);
//    procedure fciWriteFieldBytes(const AIndex: Cardinal; const AValue: TBytes);
//    procedure fciWriteFieldCurrency(const AIndex: Cardinal; const AValue: Currency);
//    procedure fciWriteFieldDouble(const AIndex: Cardinal; const AValue: Double);
//    procedure fciWriteFieldArrayBegin(const AIndex: Cardinal);
//    procedure fciWriteFieldObjBegin(const AIndex: Cardinal);
//
//    { nji }
//    procedure njiWrite(var ANji: Cardinal; const AIndex: Cardinal);
//    procedure njiWriteIf(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Boolean);
//    procedure njiWriteFieldBoolIfFalse(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Boolean);
//    procedure njiWriteFieldBoolIfTrue(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Boolean);
//    procedure njiWriteFieldDateRef(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue, ARef: TDate);
//    procedure njiWriteFieldInt32(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Int32);
//    procedure njiWriteFieldInt32IfNotNeg(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Int32);
//    procedure njiWriteFieldInt64IfNotNeg(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Int64);
//    procedure njiWriteFieldInt32IfNotZero(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Int32);
//    procedure njiWriteFieldInt64IfNotZero(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Int64);
//    procedure njiWriteFieldInt32DynArrayIfNotEmpty(var ANji: Cardinal;
//      const AIndex: Cardinal; const AValue: array of Integer);
//    procedure njiWriteFieldCurrIfNotZero(var ANji: Cardinal;
//      const AIndex: Cardinal; const AValue: Currency);
//    procedure njiWriteFieldDouble(var ANji: Cardinal; const AIndex: Cardinal;
//      const AValue: Double);
//    procedure njiWriteFieldStringNotEmpty(var ANji: Cardinal;
//      const AIndex: Cardinal; const AValue: LString); overload;
//    procedure njiWriteFieldStringNotEmpty(var ANji: Cardinal;
//      const AIndex: Cardinal; const AValue: UString); overload;
//    procedure njiWriteFieldBytesIfNotEmpty(var ANji: Cardinal;
//      const AIndex: Cardinal; const AValue: TBytes); overload;
//    procedure njiWriteFieldArrayBegin(var ANji: Cardinal; const AIndex: Cardinal);
//
//    function dfNull(var AIndex: Integer; const AName: UString): TcrBjsnWriter; inline;
//    function dfBCD(var AIndex: Integer; const AName: UString;
//      const ABCDString: UString): TcrBjsnWriter; overload; inline;
//    function dfArrayBegin(var AIndex: Integer;
//      const AName: UString): TcrBjsnWriter; inline;
//    function dfObjBegin(var AIndex: Integer;
//      const AName: UString): TcrBjsnWriter; inline;
//
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: Boolean): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: UInt32): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: UInt64): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: NativeUInt): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: Int32): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: Int64): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: Double): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AIntPart, AFracPart: UInt64;
//      const ADigits: Integer; const AIsNegative: Boolean): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: Currency): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      const AValue: Currency; const ADigits: Integer): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      const AValue: LString): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName, AValue: UString): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: TDate): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: TTime): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      var AValue: TDateTime): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      const AValue: TBytes): TcrBjsnWriter; overload; inline;
//    function df(var AIndex: Integer; const AName: UString;
//      const ABytesPtr: PByte; const ABytesCount: Integer): TcrBjsnWriter; overload; inline;
//
//    procedure AddByBjsnBytes(const ABytes: TBytes);
//
//    procedure AddByTextJsonBytesPtr(const ABytesPtr: PByte;
//      const ABytesCount: Integer);
//    procedure AddByTextJsonBytes(const ABytes: TBytes);
//    procedure AddByTextJsonFile(const AFileName: UString);
//
//    function SaveToBytesCodec: TBytes; overload;
//    function SaveToBytesCodec(const AIdCodecSeed: Byte): TBytes; overload;
//
//    procedure SaveToFileCodec(const AFileName: UString); overload;
//    procedure SaveToFileCodec(const AFileName: UString;
//      const AIdCodecSeed: Byte); overload;
//
//    function SaveToBytes(const AWritePrefix: Boolean): TBytes;
//    function SaveToBytesLzma2(const AWritePrefix: Boolean): TBytes;
//
//    procedure SaveToFile(const AFileName: UString; const AWritePrefix: Boolean);
//    procedure SaveToFileWithPrefix(const AFileName: UString);
//
//    procedure SaveToFileWithSign(const AFileName: UString;
//      const AWritePrefix: Boolean); overload; inline;
//    procedure SaveToFileWithSign(const AFileName: UString;
//      const AX509Certificate: TObject; const AWritePrefix: Boolean); overload;
//
//    function SaveToJsonBytes(const AIsPretty: Boolean): TBytes; overload; inline;
//    function SaveToJsonBytes(const AIsPretty: Boolean;
//      const ALineBreak: TcrLineBreak): TBytes; overload;
//    function SaveToJsonBytesLzma2(const AIsPretty: Boolean): TBytes;
//
//    function SaveToJsonString(const AIsPretty: Boolean): UString;
//
//    function Sign(const AX509Certificate: TObject): TcrBjsnWriter;
//
//    function SaveToBytesWithSign(
//      const AWritePrefix: Boolean): TBytes; overload; inline;
//    function SaveToBytesWithSign(const AX509Certificate: TObject;
//      const AWritePrefix: Boolean): TBytes; overload;
//
//    function SaveToJsonBytesWithSign(
//      const AIsPretty: Boolean): TBytes; overload; inline;
//    function SaveToJsonBytesWithSign(const AX509Certificate: TObject;
//      const AIsPretty: Boolean): TBytes; overload;
//
//    procedure SaveToJsonFile(const AFileName: UString; const AIsPretty: Boolean);
//    procedure SaveToJsonFileWithSign(const AFileName: UString;
//      const AIsPretty: Boolean); overload; inline;
//    procedure SaveToJsonFileWithSign(const AFileName: UString;
//      const AX509Certificate: TObject; const AIsPretty: Boolean); overload;
//
//    function SaveToBytesEx(const AFormatId: TcrBjsnFileFormatTypeId): TBytes;


  end;

type
  IcrValue = interface;
  IcrValues = interface;

  IcrValue = interface(IcrUnknown)
  ['{34D3DBBD-DB45-41D7-AAFD-F8DD8B374042}']

    function GetOwner(var AOwner: IcrValues): TcrIntfResult; stdcall;
    function GetName(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetDataTypeId(var ADataTypeId: Cardinal): TcrIntfResult; stdcall;

    function GetIsNull(var AIsNull: WordBool): TcrIntfResult; stdcall;
    function Clear: TcrIntfResult; stdcall;

    function GetAsBoolean(var AValue: WordBool): TcrIntfResult; stdcall;
    function SetAsBoolean(const AValue: WordBool): TcrIntfResult; stdcall;

    function GetAsUInt8(var AValue: UInt8): TcrIntfResult; stdcall;
    function SetAsUInt8(const AValue: UInt8): TcrIntfResult; stdcall;
    function GetAsUInt16(var AValue: UInt16): TcrIntfResult; stdcall;
    function SetAsUInt16(const AValue: UInt16): TcrIntfResult; stdcall;
    function GetAsUInt32(var AValue: UInt32): TcrIntfResult; stdcall;
    function SetAsUInt32(const AValue: UInt32): TcrIntfResult; stdcall;
    function GetAsUInt64(var AValue: UInt64): TcrIntfResult; stdcall;
    function SetAsUInt64(const AValue: UInt64): TcrIntfResult; stdcall;

    function GetAsSInt8(var AValue: Int8): TcrIntfResult; stdcall;
    function SetAsSInt8(const AValue: Int8): TcrIntfResult; stdcall;
    function GetAsSInt16(var AValue: Int16): TcrIntfResult; stdcall;
    function SetAsSInt16(const AValue: Int16): TcrIntfResult; stdcall;
    function GetAsSInt32(var AValue: Int32): TcrIntfResult; stdcall;
    function SetAsSInt32(const AValue: Int32): TcrIntfResult; stdcall;
    function GetAsSInt64(var AValue: Int64): TcrIntfResult; stdcall;
    function SetAsSInt64(const AValue: Int64): TcrIntfResult; stdcall;

    function GetAsBigInteger(var AValue: IcrBigInteger): TcrIntfResult; stdcall;
    function SetAsBigInteger(const AValue: IcrBigInteger): TcrIntfResult; stdcall;

    function GetAsCurrency(var AValue: Currency): TcrIntfResult; stdcall;
    function SetAsCurrency(const AValue: Currency): TcrIntfResult; stdcall;
    function GetAsReal32(var AValue: Single): TcrIntfResult; stdcall;
    function SetAsReal32(const AValue: Single): TcrIntfResult; stdcall;
    function GetAsReal64(var AValue: Double): TcrIntfResult; stdcall;
    function SetAsReal64(const AValue: Double): TcrIntfResult; stdcall;
    function GetAsBigDecimal(var AValue: IcrBigDecimal): TcrIntfResult; stdcall;
    function SetAsBigDecimal(const AValue: IcrBigDecimal): TcrIntfResult; stdcall;

    function GetAsChar(var AValue: WideChar): TcrIntfResult; stdcall;
    function SetAsChar(const AValue: WideChar): TcrIntfResult; stdcall;
    function GetAsString(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetAsString(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetAsBytes(var ABytes: Pointer;
      const ABytesAllocMethod: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;
    function SetAsBytes(const ABytesPtr: Pointer;
      const ABytesCount: Integer): TcrIntfResult; stdcall;

    function GetAsUUID(var AValue: TGUID): TcrIntfResult; stdcall;
    function SetAsUUID(const AValue: TGUID): TcrIntfResult; stdcall;

    function GetAsTime(var ATime: TcrTime): TcrIntfResult; stdcall;
    function SetAsTime(const ATime: TcrTime): TcrIntfResult; stdcall;
    function GetAsTimeUTC(var ATimeUTC: TcrTimeUTC): TcrIntfResult; stdcall;
    function SetAsTimeUTC(const ATimeUTC: TcrTimeUTC): TcrIntfResult; stdcall;
    function GetAsDate(var ADate: TcrDate): TcrIntfResult; stdcall;
    function SetAsDate(const ADate: TcrDate): TcrIntfResult; stdcall;
    function GetAsTimeStamp(
      var ATimeStamp: TcrTimeStamp): TcrIntfResult; stdcall;
    function SetAsTimeStamp(
      const ATimeStamp: TcrTimeStamp): TcrIntfResult; stdcall;
    function GetAsTimeStampUTC(
      var ATimeStampUTC: TcrTimeStampUTC): TcrIntfResult; stdcall;
    function SetAsTimeStampUTC(
      const ATimeStampUTC: TcrTimeStampUTC): TcrIntfResult; stdcall;
  end;

  IcrValues = interface(IcrUnknown)
  ['{FAE3F64E-5AA5-4752-9767-DDF42D3BB69E}']
    function GetCount(var ACount: NativeInt): TcrIntfResult; stdcall;
    function GetItem(const AIndex: NativeInt;
      var AItem: IcrValue): TcrIntfResult; stdcall;

    function Find(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AItem: IcrValue): TcrIntfResult; stdcall;
    function ByName(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AItem: IcrValue): TcrIntfResult; stdcall;
  end;

  IcrDataSet = interface(IcrUnknown)
  ['{4548DC6E-BA7A-46ED-AC1D-0667B4E61768}']
    function AddRow: TcrIntfResult; stdcall;
    function MoveFirst(var AIsMoved: WordBool): TcrIntfResult; stdcall;
    function MoveNext(var AIsMoved: WordBool): TcrIntfResult; stdcall;

    function GetIsEmpty(var AIsEmpty: WordBool): TcrIntfResult; stdcall;
    function GetValues(var AValues: IcrValues): TcrIntfResult; stdcall;
  end;

  IcrBioHandle = interface(IcrUnknown)
  ['{7B8684D7-1610-4DC6-8142-D21B8DEE6812}']
    function Peek(ABytesPtr: PByte; ABytesCount: Integer;
      var AReturnBytesCount: Integer): TcrIntfResult; stdcall;
    function Recv(ABytesPtr: PByte; ABytesCount: Integer;
      var AReturnBytesCount: Integer): TcrIntfResult; stdcall;
    function Send(ABytesPtr: PByte; ABytesCount: Integer;
      var AReturnBytesCount: Integer): TcrIntfResult; stdcall;
    function SendFlush: TcrIntfResult; stdcall;
    function RecvLength(var ABytesCount: Integer): TcrIntfResult; stdcall;
    function WaitForRecv(const ATimeout: TcrTimeStamp;
      var AWaitResult: Cardinal): TcrIntfResult; stdcall;
    function WaitForSend(const ATimeout: TcrTimeStamp;
      var AWaitResult: Cardinal): TcrIntfResult; stdcall;
    function RecvBuffer(ABytesPtr: PByte;
      ABytesCount: Integer): TcrIntfResult; stdcall;
    function SendBuffer(ABytesPtr: PByte;
      ABytesCount: Integer): TcrIntfResult; stdcall;
    function SendFilePart(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData;
      const AOffset, ABytesCount: Int64): TcrIntfResult; stdcall;

    function ForceWaitRecvDef: TcrIntfResult; stdcall;
    function ForceWaitSendDef: TcrIntfResult; stdcall;
    function ForceWaitRecv(
      const ATimeout: TcrTimeStamp): TcrIntfResult; stdcall;
    function ForceWaitSend(
      const ATimeout: TcrTimeStamp): TcrIntfResult; stdcall;
  end;

type
  IcrHttpField = interface;
  IcrHttpHeader = interface;
  IcrHttpCookie = interface;
  IcrHttpCookies = interface;

  IcrHttpField = interface(IcrUnknown)
  ['{6435A104-1A6E-4EA7-9FC9-A9C1E362464F}']
    function GetName(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetContent(const AStringType: TcrIntfStringType;
      var AContent: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetContent(const AStringType: TcrIntfStringType;
      const AContent: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

  IcrHttpHeader = interface(IcrUnknown)
  ['{469BF7E0-D8FD-45FB-A944-F455AD7B90C4}']
    function GetMethodId(var AMethodId: Cardinal): TcrIntfResult; stdcall;
    function SetMethodId(const AMethodId: Cardinal): TcrIntfResult; stdcall;

    function GetVersionId(var AVersionId: Cardinal): TcrIntfResult; stdcall;
    function SetVersionId(const AVersionId: Cardinal): TcrIntfResult; stdcall;

    function GetStatusCode(var AStatusCode: Cardinal): TcrIntfResult; stdcall;
    function SetStatusCode(const AStatusCode: Cardinal): TcrIntfResult; stdcall;

    function GetStatusMessage(const AStringType: TcrIntfStringType;
      var AStatusMessage: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetStatusMessage(const AStringType: TcrIntfStringType;
      const AStatusMessage: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetURI(const AStringType: TcrIntfStringType;
      var AURI: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetURI(const AStringType: TcrIntfStringType;
      const AURI: TcrIntfStringData): TcrIntfResult; stdcall;

    function FindField(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AHttpField: IcrHttpField): TcrIntfResult; stdcall;
    function AddOrSetField(const AStringType: TcrIntfStringType;
      const AName, AContent: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

  IcrHttpCookie = interface(IcrUnknown)
  ['{C790D59E-31B8-4BB1-B9E0-9020F8DB0157}']
    function GetName(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetContent(const AStringType: TcrIntfStringType;
      var AContent: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetExpires(var AExpires: TcrTimeStamp): TcrIntfResult; stdcall;
    function GetMaxAge(var AMaxAge: Int64): TcrIntfResult; stdcall;
    function GetDomain(const AStringType: TcrIntfStringType;
      var ADomain: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetPath(const AStringType: TcrIntfStringType;
      var APath: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetHttpOnly(var AHttpOnly: WordBool): TcrIntfResult; stdcall;
    function GetSameSite(const AStringType: TcrIntfStringType;
      var ASameSite: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

  IcrHttpCookies = interface(IcrUnknown)
  ['{3EDFF0BC-479D-44CA-8A1B-8509906A0FFF}']
  end;

const
  crDbConnectParams_DbTypeId_Firebird = 1;
  crDbConnectParams_DbTypeId_PostgreSql = 2;

  crDbConnectParams_Option_DbNoTrigger = 1;
  crDbConnectParams_Option_Root = 2;

type
  IcrDbConnectParams = interface;
  IcrDbConnectionPool = interface;
  IcrDbConnection = interface;
  IcrDbQuery = interface;
  IcrDbFactory = interface;

  IcrDbConnectParams = interface(IcrUnknown)
  ['{9874BE73-11A0-4132-81E9-F4C2DF778007}']
    function GetDbTypeId(var AValue: Cardinal): TcrIntfResult; stdcall;
    function SetDbTypeId(const AValue: Cardinal): TcrIntfResult; stdcall;

    function GetServerPort(var AValue: Word): TcrIntfResult; stdcall;
    function SetServerPort(const AValue: Word): TcrIntfResult; stdcall;
    function GetServerHost(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetServerHost(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetDatabase(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetDatabase(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetSchemaName(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetSchemaName(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetLoginUserName(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetLoginUserName(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetLoginPassword(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetLoginPassword(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetRoleName(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetRoleName(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetCharSetName(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetCharSetName(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetCollateName(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetCollateName(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetOptions(var AValue: NativeUInt): TcrIntfResult; stdcall;
    function SetOptions(const AValue: NativeUInt): TcrIntfResult; stdcall;

    function GetIPVersion(var AValue: Cardinal): TcrIntfResult; stdcall;
    function SetIPVersion(const AValue: Cardinal): TcrIntfResult; stdcall;

    function LoadFromJsonFile(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;
    function SaveToJsonFile(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData): TcrIntfResult; stdcall;
  end;

  IcrDbConnectionPool = interface(IcrUnknown)
  ['{459BCD0E-6D53-4AAF-8A2C-306D1FD977BF}']
    function GetDbConnectParams(
      var ADbConnectParams: IcrDbConnectParams): TcrIntfResult; stdcall;
    function SetDbConnectParams(
      const ADbConnectParams: IcrDbConnectParams): TcrIntfResult; stdcall;

    function Enter(var AConnection: IcrDbConnection): TcrIntfResult; stdcall;
  end;

  IcrDbConnection = interface(IcrUnknown)
  ['{F0891803-1D58-405F-AD0E-AD00C8D28A6A}']
    function GetOwnerPool(
      var AOwnerPool: IcrDbConnectionPool): TcrIntfResult; stdcall;
    function Leave: TcrIntfResult; stdcall;

    function CreateQuery(var AQuery: IcrDbQuery): TcrIntfResult; stdcall;

    function GetConnectionCrypt(var AValue: Cardinal): TcrIntfResult; stdcall;
    function GetSupportsTimeZone(var AValue: WordBool): TcrIntfResult; stdcall;

    function GetTransactionType(var AValue: Cardinal): TcrIntfResult; stdcall;
    function GetInTransaction(var AValue: WordBool): TcrIntfResult; stdcall;
    function BeginTransaction(
      const AReadOnly: WordBool): TcrIntfResult; stdcall;
    function Rollback: TcrIntfResult; stdcall;
    function Commit: TcrIntfResult; stdcall;

    function BeginTransactionIf(const AReadOnly: WordBool;
      var AReturn: WordBool): TcrIntfResult; stdcall;
    function RollbackIf(var AReturn: WordBool): TcrIntfResult; stdcall;
    function CommitIf(var AReturn: WordBool): TcrIntfResult; stdcall;

    function GetIsConnected(var AReturn: WordBool): TcrIntfResult; stdcall;
    function Connect(const AParams: IcrDbConnectParams): TcrIntfResult; stdcall;
    function Disconnect: TcrIntfResult; stdcall;

    function GetConnectParams(
      var AParams: IcrDbConnectParams): TcrIntfResult; stdcall;

    function Execute(const AStringType: TcrIntfStringType;
      const ACommandText: TcrIntfStringData): TcrIntfResult; stdcall;
    function ExecuteWthRowsCount(const AStringType: TcrIntfStringType;
      const ACommandText: TcrIntfStringData;
      var ARowsCount: Int64): TcrIntfResult; stdcall;
  end;

  IcrDbQuery = interface(IcrUnknown)
  ['{C7B05F4D-28DD-4359-832D-E05DE5A0A66C}']

    function GetDbConnection(
      var ADbConnection: IcrDbConnection): TcrIntfResult; stdcall;
    function GetRowsAffected(var AValue: Int64): TcrIntfResult; stdcall;
    function GetCommandText(const AStringType: TcrIntfStringType;
      var AValue: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetCommandText(const AStringType: TcrIntfStringType;
      const AValue: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetParams(var AParams: IcrValues): TcrIntfResult; stdcall;
    function GetFields(var AFields: IcrValues): TcrIntfResult; stdcall;

    function FindParam(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AParam: IcrValue): TcrIntfResult; stdcall;
    function ParamByName(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AParam: IcrValue): TcrIntfResult; stdcall;

    function FindField(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AField: IcrValue): TcrIntfResult; stdcall;
    function FieldByName(const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AField: IcrValue): TcrIntfResult; stdcall;

    function Execute: TcrIntfResult; stdcall;
    function Open: TcrIntfResult; stdcall;
    function Close: TcrIntfResult; stdcall;

    function OpenData(var AReturn: WordBool): TcrIntfResult; stdcall;

    function GetIsEmpty(var AIsEmpty: WordBool): TcrIntfResult; stdcall;
    function MoveNext(var AIsMoved: WordBool): TcrIntfResult; stdcall;

    function DumpAllToJson(const AStringType: TcrIntfStringType;
      var AJsonContent: TcrIntfStringData;
      const AIsPretty: WordBool): TcrIntfResult; stdcall;
  end;

  IcrDbFactory = interface(IcrUnknown)
  ['{F7359B47-2D9D-423D-943F-9E8968CF5945}']

    function CreateDbConnectParams(
      var ADbConnectParams: IcrDbConnectParams): TcrIntfResult; stdcall;

    function CreateDbConnectionPool(const AFlwCtrl: IcrFlwCtrl;
      var ADbConnectionPool: IcrDbConnectionPool): TcrIntfResult; stdcall;
  end;

type
  IcrXmlItem = interface;
  IcrXmlAttribute = interface;
  IcrXmlNode = interface;
  IcrXmlNodes = interface;
  IcrXmlDocument = interface;

  IcrXmlSchema = interface;
  IcrXmlSchemaName = interface;
  IcrXmlSchemaFile = interface;
  IcrXmlSchemaItem = interface;
  IcrXmlSchemaItemDataType = interface;
  IcrXmlSchemaItemDataSimpleType = interface;
  IcrXmlSchemaItemDataComplexType = interface;
  IcrXmlSchemaItemDataPrimitiveType = interface;
  IcrXmlSchemaItemDataComplexTypeSimpleContent = interface;
  IcrXmlSchemaItemDataComplexTypeComplexContent = interface;
  IcrXmlSchemaItemDataComplexTypeContent = interface;
  IcrXmlSchemaItemData = interface;
  IcrXmlSchemaItemSequence = interface;
  IcrXmlSchemaItemChoice = interface;
  IcrXmlSchemaItemElement = interface;
  IcrXmlSchemaItemAttribute = interface;
  IcrXmlSchemaEnumerationItem = interface;
  IcrXmlSchemaEnumeration = interface;
  IcrXmlSchemaItemRoot = interface;
  IcrXmlSchemaAnnotation = interface;
  IcrXmlSchemaValidateReport = interface;
  IcrXmlSchemaValidateReportItem = interface;
  IcrXmlSvriAttributeRequiredNotFound = interface;
  IcrXmlSvriAttributeUnexpected = interface;
  IcrXmlSvriNodeRequiredNotFound = interface;
  IcrXmlSvriNodesRequiredNotFound = interface;
  IcrXmlSvriNodeUnexpected = interface;
  IcrXmlSvriItemPrimitiveTypeFail = interface;
  IcrXmlSvriItemPatternFail = interface;
  IcrXmlSvriItemMinLengthFail = interface;
  IcrXmlSvriItemMaxLengthFail = interface;
  IcrXmlSvriItemLengthFail = interface;
  IcrXmlSvriItemEnumFail = interface;

  IcrXmlFactory = interface;

  IcrXmlItem = interface(IcrUnknown)
  ['{EEA8FDEC-C9FB-4C72-AB3D-4D09A3A7351C}']

    function GetItemType(var AItemType: Cardinal): TcrIntfResult; stdcall;
    function GetParent(var AParent: IcrXmlNode): TcrIntfResult; stdcall;

    function GetPrefixAndName(const AStringType: TcrIntfStringType;
      var APrefixAndName: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetName(const AStringType: TcrIntfStringType;
      var AName: TcrIntfStringData): TcrIntfResult; stdcall;
    function GetPrefix(const AStringType: TcrIntfStringType;
      var APrefix: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetContent(const AStringType: TcrIntfStringType;
      var AContent: TcrIntfStringData): TcrIntfResult; stdcall;
    function SetContent(const AStringType: TcrIntfStringType;
      const AContent: TcrIntfStringData): TcrIntfResult; stdcall;

    function IsAncestral(const AXmlNode: IcrXmlNode;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function GetSourcePosLen(var APosition, ALength: Integer): TcrIntfResult; stdcall;

    function GetFullPath(const AStringType: TcrIntfStringType;
      var AFullPath: TcrIntfStringData): TcrIntfResult; stdcall;

    function GetFullPath2(const AStringType: TcrIntfStringType;
      const ADivisor: TcrIntfStringData;
      var AFullPath: TcrIntfStringData): TcrIntfResult; stdcall;

    function DigestToBytes(const ARootNode: IcrXmlNode;
      const ACharset: IcrCharset; const AEncoding: IcrEncoding;
      AXmlDigestTypeId: Cardinal; var ABytes: Pointer;
      const ABytesAlloc: TcrIntfBytesAllocMethod): TcrIntfResult; stdcall;
    function DigestToString(const ARootNode: IcrXmlNode;
      const ACharset: IcrCharset; const AEncoding: IcrEncoding;
      AXmlDigestTypeId, ADigestEncoding: Cardinal;
      const AStringType: TcrIntfStringType;
      var AContent: TcrIntfStringData): TcrIntfResult; stdcall;

    function SaveToString(const ACharset: IcrCharset;
      const AOptions: NativeUInt; const AStringType: TcrIntfStringType;
      var AContent: TcrIntfStringData): TcrIntfResult; stdcall;
    function SaveToFile(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData;
      const AEncoding: IcrEncoding; const ACharset: IcrCharset;
      const AOptions: NativeUInt): TcrIntfResult; stdcall;
  end;

  IcrXmlAttribute = interface(IcrXmlItem)
  ['{125F7799-CF7D-4D40-8D2A-A19F57995876}']
    function GetPrevious(var APrevious: IcrXmlAttribute): TcrIntfResult; stdcall;
    function GetNext(var ANext: IcrXmlAttribute): TcrIntfResult; stdcall;
  end;

  IcrXmlNode = interface(IcrXmlItem)
  ['{36DDC1D7-FAF0-4C40-8319-06E25BA0674D}']
    function GetPrevious(var APrevious: IcrXmlNode): TcrIntfResult; stdcall;
    function GetNext(var ANext: IcrXmlNode): TcrIntfResult; stdcall;
    function GetPreviousData(var APrevious: IcrXmlNode): TcrIntfResult; stdcall;
    function GetNextData(var ANext: IcrXmlNode): TcrIntfResult; stdcall;

    function GetFirstNodeChild(var ANode: IcrXmlNode): TcrIntfResult; stdcall;
    function GetLastNodeChild(var ANode: IcrXmlNode): TcrIntfResult; stdcall;
    function GetFirstNodeChildData(var ANode: IcrXmlNode): TcrIntfResult; stdcall;
    function GetLastNodeChildData(var ANode: IcrXmlNode): TcrIntfResult; stdcall;

    function GetFirstAttribute(
      var AAttribute: IcrXmlAttribute): TcrIntfResult; stdcall;
    function GetLastAttribute(
      var AAttribute: IcrXmlAttribute): TcrIntfResult; stdcall;

    function AttributesClear: TcrIntfResult; stdcall;
    function NodesClear: TcrIntfResult; stdcall;

    function AddWhiteSpace(const AStringType: TcrIntfStringType;
      const AStringData: TcrIntfStringData;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;
    function AddComment(const AStringType: TcrIntfStringType;
      const AStringData: TcrIntfStringData;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;
    function AddElement(const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;

    function AddData(const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;
    function AddCData(const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;

    function AddOrSetAttribute(const AStringType: TcrIntfStringType;
      const APrefix, AName, AContent: TcrIntfStringData;
      var AAttribute: IcrXmlAttribute): TcrIntfResult; stdcall;
    function RemoveAttribute(const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      var AReturn: WordBool): TcrIntfResult; stdcall;

    function AddOrSetParseAttributes(
      const AStringType: TcrIntfStringType;
      const ASource: TcrIntfStringData): TcrIntfResult; stdcall;

    function FindAttribute(
      const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      var AAttribute: IcrXmlAttribute): TcrIntfResult; stdcall;

    function FindAttributeIgnorePrefix(
      const AStringType: TcrIntfStringType;
      const AName: TcrIntfStringData;
      var AAttribute: IcrXmlAttribute): TcrIntfResult; stdcall;

    function FindNode(
      const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      const ASearchTree: WordBool;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;

    function FindNodes(
      const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      const ASearchTree: WordBool;
      const AMaxCount: NativeInt;
      var ANodes: IcrXmlNodes): TcrIntfResult; stdcall;

    function FindNext(
      const AStringType: TcrIntfStringType;
      const APrefix, AName: TcrIntfStringData;
      var ANode: IcrXmlNode): TcrIntfResult; stdcall;

    function FindAttributeEx(
      const AStringType: TcrIntfStringType;
      const APrefix, AName, AContent: TcrIntfStringData;
      var AAttribute: IcrXmlAttribute): TcrIntfResult; stdcall;

//    function fgAttribute(const APrefix, AName: TcrXmlString): TcrXmlAttribute;
//    function fgAttributeIgnorePrefix(const AName: TcrXmlString): TcrXmlAttribute;
//
//    function fgNode(const APrefix, AName: TcrXmlString;
//      const ASearchTree: Boolean): TcrXmlNode;
//
//    function AttributesSaveToString(const ACharset: TcrCharset;
//      const AOptions: TcrXmlOptions): TcrXmlString; overload;
//    procedure AttributesSaveToString(var ASB: TcrUStringBuilder;
//      const ACharset: TcrCharset; const AOptions: TcrXmlOptions); overload;
  end;

  IcrXmlNodes = interface(IcrUnknown)
  ['{DC7D5871-EE52-4095-9045-4314B0935892}']

    function GetCount(var ACount: NativeInt): TcrIntfResult; stdcall;
    function GetItem(const AIndex: NativeInt;
      var AXmlNode: IcrXmlNode): TcrIntfResult; stdcall;
  end;

  IcrXmlDocument = interface(IcrXmlNode)
  ['{BE0B53F4-7835-44E5-BCAB-BD6603FBC16C}']

    function LoadFromUCStr(const AUCStrPtr: PWideChar;
      const AUCStrLength: NativeInt;
      const AOptions: NativeUInt): TcrIntfResult; stdcall;

    function LoadFromString(const AStringType: TcrIntfStringType;
      const AStringData: TcrIntfStringData;
      const AOptions: NativeUInt): TcrIntfResult; stdcall;

    function LoadFromFile(const AStringType: TcrIntfStringType;
      const AFileName: TcrIntfStringData;
      const AStringEncoding: IcrEncoding;
      const AOptions: NativeUInt): TcrIntfResult; stdcall;

    function LoadFromBuffer(const ABytesPtr: PByte;
      const ABytesCount: NativeInt;
      const AStringEncoding: IcrEncoding;
      const AOptions: NativeUInt): TcrIntfResult; stdcall;
  end;

  IcrXmlSchema = interface(IcrUnknown)
  ['{1D0566EF-44D0-4A25-B246-8576F8B9261B}']
  end;

  IcrXmlSchemaName = interface(IcrUnknown)
  ['{1D0566EF-44D0-4A25-B246-8576F8B9261B}']
  end;

  IcrXmlSchemaFile = interface(IcrUnknown)
  ['{3D4A2D29-664D-4E6B-AB14-3BD9A3BDE7E9}']
  end;

  IcrXmlSchemaItem = interface(IcrUnknown)
  ['{2FB75C9F-EF89-433C-BDDE-91B720DB2716}']
  end;

  IcrXmlSchemaItemDataType = interface(IcrUnknown)
  ['{ECF85BF3-DCAE-4426-BC1B-778EE231CD92}']
  end;

  IcrXmlSchemaItemDataSimpleType = interface(IcrUnknown)
  ['{EFA765C3-2803-42FC-9539-567659C459B2}']
  end;

  IcrXmlSchemaItemDataComplexType = interface(IcrUnknown)
  ['{20FBE00F-0A38-4C40-9239-2D2E214D2055}']
  end;

  IcrXmlSchemaItemDataPrimitiveType = interface(IcrUnknown)
  ['{BBB0B332-0B33-49E7-99A7-030754073493}']
  end;

  IcrXmlSchemaItemDataComplexTypeSimpleContent = interface(IcrUnknown)
  ['{8C87C2B3-5865-44D4-853C-25CCDD0E39B4}']
  end;

  IcrXmlSchemaItemDataComplexTypeComplexContent = interface(IcrUnknown)
  ['{8951BA24-8AC2-4AEC-94B2-93506207D876}']
  end;

  IcrXmlSchemaItemDataComplexTypeContent = interface(IcrUnknown)
  ['{B224E00B-C587-4725-B828-E3CC9C7169F5}']
  end;

  IcrXmlSchemaItemData = interface(IcrUnknown)
  ['{9BAC56B5-108A-4344-BEDC-900FD6583A89}']
  end;

  IcrXmlSchemaItemSequence = interface(IcrUnknown)
  ['{F02A1D1E-0CC0-4842-9018-38703A99F163}']
  end;

  IcrXmlSchemaItemChoice = interface(IcrUnknown)
  ['{0F9E1384-C5C2-4A60-8B2F-4A2B04610919}']
  end;

  IcrXmlSchemaItemElement = interface(IcrUnknown)
  ['{39A529F0-3230-4187-AE0D-BA7E551F5AB4}']
  end;

  IcrXmlSchemaItemAttribute = interface(IcrUnknown)
  ['{826C75EB-60DE-4067-B0DF-754561B10CA8}']
  end;

  IcrXmlSchemaEnumerationItem = interface(IcrUnknown)
  ['{EFB6CDD6-6748-46A3-9C0F-B61D50FA5F6F}']
  end;

  IcrXmlSchemaEnumeration = interface(IcrUnknown)
  ['{459EE907-1112-4AC8-ADDD-CC4E2F3DEBC2}']
  end;

  IcrXmlSchemaItemRoot = interface(IcrUnknown)
  ['{12ADA348-843A-4F9F-989A-0591719406BD}']
  end;

  IcrXmlSchemaAnnotation = interface(IcrUnknown)
  ['{C17E7194-D473-452A-A4FA-7BF32BD3588A}']
  end;

  IcrXmlSchemaValidateReport = interface(IcrUnknown)
  ['{013E0D9F-531C-43FF-BD0B-B3283B1BB2C6}']
  end;

  IcrXmlSchemaValidateReportItem = interface(IcrUnknown)
  ['{3C2EE751-2296-483F-8B38-AAB6AFE86635}']
  end;

  IcrXmlSvriAttributeRequiredNotFound = interface(IcrUnknown)
  ['{7617DD37-C005-45B0-A2F8-E1D1767C7563}']
  end;

  IcrXmlSvriAttributeUnexpected = interface(IcrUnknown)
  ['{348C4117-8876-461C-8BC6-66EC10BF3A06}']
  end;

  IcrXmlSvriNodeRequiredNotFound = interface(IcrUnknown)
  ['{348C4117-8876-461C-8BC6-66EC10BF3A06}']
  end;

  IcrXmlSvriNodesRequiredNotFound = interface(IcrUnknown)
  ['{348C4117-8876-461C-8BC6-66EC10BF3A06}']
  end;

  IcrXmlSvriNodeUnexpected = interface(IcrUnknown)
  ['{93EC2934-711A-475F-A668-22CFB55E014A}']
  end;

  IcrXmlSvriItemPrimitiveTypeFail = interface(IcrUnknown)
  ['{06392837-EBF8-40E6-A678-86146D943F47}']
  end;

  IcrXmlSvriItemPatternFail = interface(IcrUnknown)
  ['{6788A305-09AA-4A15-89EE-B0C5B1A51AF5}']
  end;

  IcrXmlSvriItemMinLengthFail = interface(IcrUnknown)
  ['{BDFC6C9C-B23B-4FAB-82DE-344AE994E62F}']
  end;

  IcrXmlSvriItemMaxLengthFail = interface(IcrUnknown)
  ['{5A35C3F7-B9EE-4F45-BC45-43C18C4B3DBE}']
  end;

  IcrXmlSvriItemLengthFail = interface(IcrUnknown)
  ['{F64E1760-BC8B-4124-8889-6C0ACD2DC1A1}']
  end;

  IcrXmlSvriItemEnumFail = interface(IcrUnknown)
  ['{0AFD673F-4FCA-476D-97E0-5209F0F8FB4B}']
  end;

  IcrXmlFactory = interface(IcrUnknown)
  ['{F664071E-192F-4B3E-AD43-121493ADAED5}']

    function CreateXmlDocument(
      var AXmlDocument: IcrXmlDocument): TcrIntfResult; stdcall;
  end;

implementation

function crIntfResultIsError(const AValue: TcrIntfResult): Boolean;
begin
  Exit(AValue < 0);
end;

end.
