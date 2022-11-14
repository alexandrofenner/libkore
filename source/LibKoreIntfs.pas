unit LibKoreIntfs;

interface

type
  IcrObject = interface;
  IcrErrorInfo = interface;

  IcrObject = interface
  ['{A3A61077-137B-447F-99CC-7C664BB532F2}']
    function GetErrorInfo(var AErrorInfo: IcrErrorInfo): WordBool; stdcall;
  end;

  IcrErrorInfo = interface
  ['{7D0E797F-C219-4167-A618-D31470D0A8F2}']
    function GetMessage(var AMessage: WideString): WordBool; stdcall;
    function GetStatusCode(var AStatusCode: Cardinal): WordBool; stdcall;
    function GetOsError(var AOsError: Cardinal): WordBool; stdcall;
    function GetCodeLocalId(var ACodeLocalId: PAnsiChar): WordBool; stdcall;
  end;

type
  IcrStream = interface
  ['{C2395A79-0018-49F0-9615-C54A55987198}']

    function GetSize(var ASize: Int64): WordBool; stdcall;
    function SetSize(const ASize: Int64): WordBool; stdcall;

    function Read(ABytesPtr: PByte; ABytesCount: NativeInt;
      var ABytesRead: NativeInt): WordBool; stdcall;
    function Write(ABytesPtr: PByte; ABytesCount: NativeInt;
      var ABytesWritten: NativeInt): WordBool; stdcall;
    function Seek(AOffset: Int64; AOrigin: Integer;
      var AReturn: Int64): WordBool; stdcall;

    function Flush: WordBool; stdcall;
  end;

const
  ClassID_ConsoleFactory: TGUID = '{5242C0B6-BFC6-4D91-B7F2-ED6D61CFD938}';

type
  IcrConsole = interface;
  IcrConsoleFactory = interface;

  TcrConsoleAddLineMethod =
    procedure(const AUserData: Pointer; const S: WideString); stdcall;

  IcrConsole = interface
  ['{4AC82DD8-F944-43C8-9ED4-C2B9812AF766}']

    function AlternativeBuffer(AOnOff: WordBool): WordBool; stdcall;
    { Apagar a linha. Se AIdType
        = 0 -> Da posição atual até o fim da linha
        = 1 -> Do inicio até a posição atual
        = 2 -> Toda a linha }
    function ClearLine(const AIdType: Byte): WordBool; stdcall;
    { Apagar a tela. Se AIdType
        = 0 -> Da posição atual até o fim da tela
        = 1 -> Do ínicio até a posição atual
        = 2 -> Toda a tela }
    function ClearScreen(const AIdType: Byte): WordBool; stdcall;

    function CursorUp(const ACount: Cardinal): WordBool; stdcall;
    function CursorDown(const ACount: Cardinal): WordBool; stdcall;
    function CursorForward(const ACount: Cardinal): WordBool; stdcall;
    function CursorBack(const ACount: Cardinal): WordBool; stdcall;
    function CursorNextLine(const ACount: Cardinal): WordBool; stdcall;
    function CursorPreviousLine(const ACount: Cardinal): WordBool; stdcall;
    function CursorHorzAbsolute(const ACount: Cardinal): WordBool; stdcall;

    function DeviceStatusReport: WordBool; stdcall;
    function RestoreCurrentCursorPosition: WordBool; stdcall;
    function RestoreScreen: WordBool; stdcall;

    function SaveCurrentCursorPosition: WordBool; stdcall;
    function SaveScreen: WordBool; stdcall;

    function SetBracketPasteMode(const AOnOff: WordBool): WordBool; stdcall;
    function SetEnabledAuxPort(const AOnOff: WordBool): WordBool; stdcall;

    function SetCursorPosition(const AX, AY: Cardinal): WordBool; stdcall;
    function SetCursorVisible(const AOnOff: WordBool): WordBool; stdcall;

    function SetTitle(const ATitle: WideString): WordBool; stdcall;

    function ScrollUp(const ACount: Cardinal): WordBool; stdcall;
    function ScrollDown(const ACount: Cardinal): WordBool; stdcall;

    function SelectGraphicRenditionRGB(const AId, ARGB: Cardinal): WordBool; stdcall;

    function Text(const S: WideString): WordBool; stdcall;
  end;

  IcrConsoleFactory = interface
  ['{C2909600-8297-4897-A592-1594833A108B}']

    function CreateByAddLine(const AAddLineCode: TcrConsoleAddLineMethod;
      const AAddLineData: Pointer; var AConsole: IcrConsole): WordBool; stdcall;
    function CreateByOutHandle(const AOutHandle: THandle;
      var AConsole: IcrConsole): WordBool; stdcall;
    function CreateCacheByWnd(const AWnd: THandle;
      const AInnerConsole: IcrConsole;
      var AConsole: IcrConsole): WordBool; stdcall;
  end;

const
  ClassID_WinPseudoConsoleFactory: TGUID = '{1BD1C8C4-F253-409D-AB83-977B403763FB}';

type
  IcrWinPseudoConsole = interface;
  IcrWinPseudoConsoleFactory = interface;

  TcrWinPseudoConsoleParams = record
    ExitCode: Cardinal;
    OSError: Cardinal;
    IsOkay: Boolean;
    IsTerminated: Boolean;
  end;

  IcrWinPseudoConsole = interface
  ['{48D7005F-7D43-4C77-AFED-E191FF7C78B2}']

    function Execute: WordBool; stdcall;
    function IsTerminated(var AIsTerminated: WordBool): WordBool; stdcall;
    function BeginExecute: WordBool; stdcall;
    function GetExitCode(var AExitCode: Cardinal): WordBool; stdcall;
  end;

  IcrWinPseudoConsoleFactory = interface
  ['{3A07BF1C-539C-4C32-A6CA-BE4D8BF88344}']

    { RemoveAdministrativePermissions ->
        Remove os privilégios de administrador
        É útil quando o aplicativo 'pai' estiver sendo executado com
        privilégios de administrador e se deseja executar um processo
        'filho' somente com privilégios de usuário normal }
    function RemoveAdministrativePermissions: WordBool; stdcall;

    { SetOsLogin ->
        Reservado para versões futuras }
    function SetOsLogin(const AUserName, ADomain, APassword: WideString): WordBool; stdcall;

    { SetCommandLine ->
        Indicar a linha de comando que deverá ser executada }
    function SetCommandLine(const ACommandLine: WideString): WordBool; stdcall;
    { SetDirectory ->
        Indica qual será o diretório atual do processo 'filho' }
    function SetDirectory(const ADirectory: WideString): WordBool; stdcall;
    { SetVariable ->
        Alterar o valor de uma variável de ambiente.
        Este valor alterado estará disponível apenas para o processo 'filho' }
    function SetVariable(const AName, AValue: WideString): WordBool; stdcall;

    { SetRecordingStream ->
        Indica uma stream para 'gravar' o conteúdo de saída do console }
    function SetRecordingStream(const AStream: IcrStream): WordBool; stdcall;
    { SetRecordingFile ->
        Indica um arquivo para 'gravar' o conteúdo de saída do console }
    function SetRecordingFileName(
      const AFileName: WideString): WordBool; stdcall;
    { SetConsole ->
        Indica um console sobre o qual o processo filho será criado }
    function SetConsole(const AConsole: IcrConsole): WordBool; stdcall;
    { Create ->
        Cria um objeto 'IcrWinPseudoConsole' }
    function Create(var AIntf: IcrWinPseudoConsole): WordBool; stdcall;
  end;

const
  LibKoreDLL = 'LibKore32.dll';
//  LibKoreDLL = 'LibKore64.dll';

function CreateFactory(const AClassName: WideString;
  var AFactory: IUnknown): WordBool; stdcall;
  external LibKoreDLL name 'CreateFactory';

implementation

end.
