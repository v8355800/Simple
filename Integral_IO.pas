unit Integral_IO;

interface

uses
  Classes;

//type
//  TK10XResult = record
//    Good  : Boolean;     // Результат "Годен"/"Брак"
//    OverP : Boolean;     // Наличие перегрузок "+"
//    OverM : Boolean;     // Наличие перегрузок "-"
//    Gener : Boolean;     // Наличие паразитной генерации
//    Pult  : Byte;        // Номер плана
//  end;
type
  TBoardID = (INEJ_ID = $1, ISTINA_ID = $2, INTEGRAL_ID = $3, T4502_ID = $4);
  TBoardParams = record
    IOPort: Word;
    Name: string;
  end;

type
  TReleOnOff = (trON, trOFF);

  PIntegralIO_Base = ^TIntegralIO_Base;
  TIntegralIO_Base = class(TObject)
  private
    fName: string;
    fEmulation: Boolean;
  protected

  public
//    function TESTER_TRA: Boolean;         virtual; abstract;
    function PROPUSK: Boolean;            virtual; abstract;
    procedure TESTER_RESET;
    procedure Command(OCT: String);        virtual; abstract;
    procedure Value(OCT: String);          virtual; abstract;
    procedure Matrix(OCT: String);         virtual; abstract;
    function K100: Word; virtual; abstract;
    function K101: Word; virtual; abstract;

    procedure _Kommand(K: Word); virtual; abstract;
    procedure _Value(V: Word);   virtual; abstract;

    { Работа с РЕЛЕ }
    procedure TESTER_RELE_1_16(Status: TReleOnOff);   overload; virtual; abstract;
    procedure TESTER_RELE_1_16(W: Word);              overload; virtual; abstract;
    procedure TESTER_RELE_17_32(Status: TReleOnOff);  overload; virtual; abstract;
    procedure TESTER_RELE_17_32(W: Word);             overload; virtual; abstract;

    procedure TESTER_RELE(W: Word);       virtual; abstract;

    property Name: String read fName;
    property Emulation: Boolean read fEmulation;
  end;

//------------------------------------------------------------------------------
// LPT - SP
//------------------------------------------------------------------------------
type
  TDD = (DD1, DD2, DD3, DD4, DD5);
  TR  = (DD1_L, DD1_H, DD2_L, DD2_H, DD3_L, DD3_H, DD4_L, DD4_H, DD5_L, DD5_H);

  TIntegralIO_LPT = class(TIntegralIO_Base)
  private
    function GetSP: Word;
    function GetCP: Word;
  private
    fBaseAddr: Word;

    procedure INIT(Command: Byte);
    procedure STROBE(Data: Byte);
    function _R: Word;


    procedure DATA8(OCT: String);
    function Check(SendData: Word; ShowError: Boolean = True): Boolean;

    procedure Write_DD(const DD: TDD; Data: Byte);
    procedure Read_Half_DD(const R: TR; var Data: Byte);
    procedure Read_DD(const DD: TDD; var Data: Byte);

    property DP: Word read fBaseAddr;
    property SP: Word read GetSP;
    property CP: Word read GetCP;

  public
    constructor Create(const Name: String; const BaseAddr: Word; Emulation: Boolean = False);
//    function TESTER_TRA: Boolean; override;
    function PROPUSK: Boolean; override;
    procedure Command(OCT: String); override;
    procedure Value(OCT: String); override;
    procedure Matrix(OCT: String); override;
    function K100: Word; override;
    function K101: Word; override;

    procedure DATA(D: Word);
    procedure _Kommand(K: Word); override;
    procedure _Value(V: Word);   override;

    procedure TESTER_RELE_1_16(Status: TReleOnOff);   overload; override;
    procedure TESTER_RELE_1_16(W: Word);              overload; override;
    procedure TESTER_RELE_17_32(Status: TReleOnOff);  overload; override;
    procedure TESTER_RELE_17_32(W: Word);             overload; override;

    class function FindBoard(BoardID: TBoardID; var Params: TBoardParams): Boolean;
  end;

{ Функции для работы с портами ввода/вывода под Windows }
function Inp32(PortAdr: Word): Byte; stdcall; external 'inpout32.dll';
function Out32(wAddr: Word; bOut: Byte): Byte; stdcall; external 'inpout32.dll';

var
  fIO: TIntegralIO_Base;

implementation

uses
//  Forms, StdCtrls, ExtCtrls, Controls,
//  uMain,
  SysUtils, Dialogs,
  CONVUNIT, JvStrUtils, JclLogic,
  unitportdetect;
//  {CONVUNIT, uDevices,{ unitportdetect,} JclLogic, JvstrUtils,
//  {Integral_Globals, Integral_Functions};

{ TIntegralIO_LPT }

constructor TIntegralIO_LPT.Create(const Name: String; const BaseAddr: Word; Emulation: Boolean = False);
begin
  fName := Name;
  fBaseAddr := BaseAddr;
  fEmulation := Emulation;
end;

procedure TIntegralIO_LPT.INIT(Command: Byte);
begin
  Out32(DP, Command);
  Out32(CP, 4);
  Out32(CP, 0);
end;

procedure TIntegralIO_LPT.STROBE(Data: Byte);
begin
  Out32(DP, Data);
  Out32(CP, 1);
  Out32(CP, 0);
end;

function TIntegralIO_LPT._R: Word;
var
  P1, P2: Byte;
  D: Word;
begin
  Read_DD(DD1, P1);
  Read_DD(DD2, P2);
  D := P1 + (P2 shl 8);
  D := not D;
  D := D and $FFF;

  Result := D;
end;

procedure TIntegralIO_LPT.DATA8(OCT: String);
var
  C: Word;
begin

  C := OCT2DEC(OCT);
  C := not C;
  C := ReverseBits(C) shr 4;

  Write_DD(DD5, Lo(C));
  Write_DD(DD4, Hi(C));
end;

function TIntegralIO_LPT.Check(SendData: Word; ShowError: Boolean): Boolean;
var
  ReadData: Word;
begin
  SendData := SendData and $FFF;
  ReadData := _R;
  if ReadData <> SendData then
  begin
    Result := False;
    if ShowError then
      ShowMessage(IntToBin(SendData, 12, 3) + ' <> ' + IntToBin(ReadData, 12, 3));
  end
  else
    Result := True;
end;


procedure TIntegralIO_LPT.Write_DD(const DD: TDD; Data: Byte);
begin
  INIT(1 + Ord(DD));
  STROBE(Data);
end;

procedure TIntegralIO_LPT.Read_Half_DD(const R: TR; var Data: Byte);
begin
  Out32(DP, 11 + Ord(R));
  Out32(CP, 4);
  Out32(CP, 0);
  Out32(CP, 1);
  Out32(CP, 0);
  Data := Inp32(SP) shr 3;
end;

procedure TIntegralIO_LPT.Read_DD(const DD: TDD; var Data: Byte);
var
  P1, P2: Byte;
begin
  case DD of
    DD1:
      begin
        Read_Half_DD(DD1_L, P1);
        Read_Half_DD(DD1_H, P2);
        Data := (P1 and $F) + ((P2 and $F) shl 4);
      end;
    DD2:
      begin
        Read_Half_DD(DD2_L, P1);
        Read_Half_DD(DD2_H, P2);
        Data := (P1 and $F) + ((P2 and $F) shl 4);
      end;
  else
    Data := 0;
  end;
end;

function TIntegralIO_LPT.PROPUSK: Boolean;
var
  Data: Byte;
begin
  if fEmulation then
  begin
    PROPUSK := True;
    Exit;
  end;

  INIT($34);
  Data := Inp32(SP) shr 3;

  Result := (Data and 1) <> 0;
end;

//function TIntegralIO_LPT.TESTER_TRA: Boolean;
//var
//  Data: Byte;
//begin
//  if DebugMode then
//  begin
//    Result := True;
//    Exit;
//  end;
//
//  Read_Half_DD(DD3_H, Data);
//  Result := (Data and (1 shl 0)) = 0;
//end;


function TIntegralIO_LPT.GetSP: Word;
begin
  Result := fBaseAddr + 1;
end;

function TIntegralIO_LPT.GetCP: Word;
begin
  Result := fBaseAddr + 2;
end;

procedure TIntegralIO_Base.TESTER_RESET;
begin
//	Command(pos8(0,2,1,0)); // сброс оценки результата сравнения
//	Command(pos8(0,2,1,1)); // сброс вентилей источников
//	Command(pos8(0,2,1,2)); // сброс полярности, д-на значения и пред. ток
//	Command(pos8(0,2,1,4)); // сброс соединений в матрице
  Command('0217');
end;

procedure TIntegralIO_LPT.Command(OCT: String);
var
  i: Integer;
  D, D1: Word;
begin
  if fEmulation then
    Exit;
    
  DATA8(OCT);
  INIT($22);

  INIT($21);
  while (PROPUSK = False) do
    ;
  INIT($20);

  INIT($23);

  D := OCT2DEC(OCT);

  if ((OCT = '0100') or (OCT = '0101')) then
  begin
    ;
  end
  else
  begin
    Check(D);
  end;
end;

procedure TIntegralIO_LPT.Value(OCT: String);
var
  D, D1: Word;
begin
  if fEmulation then
    Exit;

  DATA8(OCT);

  INIT($24);

  INIT($21);
  while (PROPUSK = False) do
    ;
  INIT($20);

  INIT($25);

  D := OCT2DEC(OCT);
  Check(D);
end;

procedure TIntegralIO_LPT.Matrix(OCT: String);
var
  D, D1: Word;
begin
  if fEmulation then
    Exit;

  DATA8(OCT);

  INIT($22);
  INIT($24);

  INIT($21);
  while (PROPUSK = False) do
    ;
  INIT($20);

  INIT($23);
  INIT($25);

  D := OCT2DEC(OCT);
  Check(D);
end;

function TIntegralIO_LPT.K100: Word;
var
  R: Word;
begin
  if fEmulation then
  begin
    Result := 0;
    Exit;
  end;

  Command('0100');
  R := _R;
  Result := R and $FFF;
end;

function TIntegralIO_LPT.K101: Word;
var
  R: Word;
begin
  if fEmulation then
  begin
    Result := 0;
    Exit;
  end;

  Command('0101');
  R := _R;
  Result := R and $FFF;
end;


procedure TIntegralIO_LPT.DATA(D: Word);
begin
  D := not D;
  D := ReverseBits(D) shr 4;

  Write_DD(DD5, Lo(D));
  Write_DD(DD4, Hi(D));
end;

procedure TIntegralIO_LPT._Kommand(K: Word);
var
  i: Integer;
  D, D1: Word;
begin
  if fEmulation then
    Exit;

  DATA(K);

  INIT($22);

  INIT($21);
  while (PROPUSK = False) do
    ;
  INIT($20);

  INIT($23);

  if (K <> 64) or
     (K <> 65) then
  else
    Check(D);
end;

procedure TIntegralIO_LPT._Value(V: Word);
begin
  if fEmulation then
    Exit;

  DATA(V);

  INIT($24);

  INIT($21);
  while (PROPUSK = False) do
    ;
  INIT($20);

  INIT($25);

  //  D := OCT2DEC(Format('%d%d%d%d', [B1, B2, B3, B4]));
  Check(V);
end;


//function FindBoard(Tester: PIntegralIO_Base): Boolean;
//const
//  LPT_COUNT = 10;
//var
//  DevList: TDeviceList;
//begin
//  DevList := TDeviceList.Create;
//  { MosChip PCI Parallel Port }
//  Result := FindPCIDevices('9710', '9865', @DevList);
////  { Advantech PCI-1751 }
////  Result := FindPCIDevices('13FE', '1751', @DevList);
////  Out32(DP, 20);
////  Out32(CP, 4); Out32(CP, 0);  // INIT
////  Out32(CP, 1); Out32(CP, 0);  // STROBE
////  Data := Inp32(SP) shr 3;
//
//  if Result then
//    Tester^ := TIntegralIO_LPT.Create( Format('[$%.4x] %s', [DevList[2].BaseAddr, DevList[2].Name]), DevList[2].BaseAddr);
////    Tester^ := TIstinaIO_EPP.Create( Format('[$%.4x; $%.4x] %s', [DevList[0].BaseAddr, DevList[0].BaseAddr - $10, DevList[0].Name]), DevList[0].BaseAddr, DevList[0].BaseAddr - $10);
//
//  DevList.Free;
//end;


procedure TIntegralIO_LPT.TESTER_RELE_1_16(Status: TReleOnOff);
begin
  inherited;

  case Status of
    trON:  INIT($42);
    trOFF: INIT($40);
  end;
end;

procedure TIntegralIO_LPT.TESTER_RELE_1_16(W: Word);
begin
  inherited;

  INIT($46);
  STROBE(Lo(W));
  INIT($47);
  STROBE(Hi(W));

  INIT($44);
end;

procedure TIntegralIO_LPT.TESTER_RELE_17_32(Status: TReleOnOff);
begin
  inherited;

  case Status of
    trON:  INIT($43);
    trOFF: INIT($41);
  end;
end;

procedure TIntegralIO_LPT.TESTER_RELE_17_32(W: Word);
begin
  inherited;

  INIT($46);
  STROBE(Lo(W));
  INIT($47);
  STROBE(Hi(W));

  INIT($45);
end;

class function TIntegralIO_LPT.FindBoard(BoardID: TBoardID; var Params: TBoardParams): Boolean;
const
  MaxPorts = 5;
var
  Data: Byte;

  LPTPorts: array[0..MaxPorts-1] of TDetectedPort;
  i: Byte;
begin
  Params.IOPort := 0;
  Params.Name   := '';
  Result        := False;

  if getLPTPorts(LPTPorts) then
  begin
    for i := 0 to MaxPorts - 1 do
    begin
      if (LPTPorts[i].friendlyName <> '') and (LPTPorts[i].portStart <> 0) then
      begin
        { Проверка - подкючена ли плата к данному порту }
        Out32(LPTPorts[i].portStart, $50);
        Out32(LPTPorts[i].portStart+2, 4);
        Out32(LPTPorts[i].portStart+2, 0);
        Data := Inp32(LPTPorts[i].portStart+1) shr 3;
        Data := Data and $F;
        if Data = Ord(BoardID) then
        begin
          Params.IOPort := LPTPorts[i].portStart;
          Params.Name   := LPTPorts[i].friendlyName;
          Result        := True;
          Break;
        end;
      end;
    end;
  end;
end;

end.
