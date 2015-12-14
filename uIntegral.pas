unit uIntegral;

interface

uses
  Classes,
  Contnrs;

type
  TUnits = (nA, uA, mA, A, mV, V, mS, S);
const
//  TUnitsString : array[0..Integer(High(TUnits))] of string = ('нА', 'мкА', 'мА', 'А', 'мВ', 'В', 'мс', 'с');
  TUnitsString : array[TUnits] of string = ('нА', 'мкА', 'мА', 'А', 'мВ', 'В', 'мс', 'с');

{ Параметры разбраковки }
type
  TRegMode  = 0..5;
  TStopMode = 0..4;
  TRW       = set of 0..12;
var
  RegMode  : TRegMode;
  StopMode : TStopMode;
  RW       : TRW;

//------------------------------------------------------------------------------
// Тесты
//------------------------------------------------------------------------------
type
  TOCTValue = string[4];
  TTestType = (ttRegular, ttCharacteristic, ttDifferential);
  TTestResult = record
    fGroup     : TOCTValue;
    fValue     : Real;
    fBrak      : Boolean;
    fOSC       : Boolean;  // Генерация
    fOverloadP : Boolean;  // Перегрузка+
    fOverloadM : Boolean;  // Перегрузка-
  end;
  TValidityCriterion = (vcK100, vcK101);
  TTestNorm = record
    fValueOct: TOCTValue;
    ValidityCriterion: TValidityCriterion; 
    fValue: Real;
    fValueUnits: TUnits;
  end;

  TTest = class(TObject)
  private
    fTestN: Cardinal;
    fResult: TTestResult; //Real;

    function GetTestType: TTestType; virtual; abstract;
  protected

  public
    function Execute(const fADC: Boolean = False): Integer;{: TTestResult;} virtual; abstract;

    constructor Create;

    property TestType: TTestType read GetTestType;
    property TestN: Cardinal read fTestN write fTestN;
    property Result: TTestResult read fResult;
  end;

  { Обычный тест }
  TRegularTest = class(TTest)
  private
    fCommands: TStringList;
    fTestNorm: TTestNorm;

    function GetTestType: TTestType; override;
    function GetTestN(slTest: TStringList): Cardinal;
  public
    constructor Create(TestText: String);
    destructor Destroy; override;

    function Execute(const fADC: Boolean = False): Integer; override;

    property TestNorm: TTestNorm read fTestNorm;
  end;

  { Характеристический тест }
  TCharacteristicTest = class(TTest)
  private
    fCommands: TStringList;
    fParams: array[1..5] of TOCTValue;

    function GetTestType: TTestType; override;
    function GetTestN(slTest: TStringList): Cardinal;
    function GetParams(Item: Integer): TOCTValue;
  public
    constructor Create(TestText: String);
    function Execute(const fADC: Boolean = False): Integer; override;

    property Params[Item: Integer]: TOCTValue read GetParams;
  end;

  { Диффиренциальный тест }
  TDifferentialTest = class(TTest)
  private
    fTest1: TRegularTest;
    fTest2: TRegularTest;
    fDiffNorm: TOCTValue;
    fDiffNormValue: Real;
    fDiffNormUnits: TUnits;

    function GetTestType: TTestType; override;
  public
    constructor Create(TestText1, TestText2: String);
    destructor Destroy; override;

    function Execute(const fADC: Boolean = False): Integer; override;

    property Test1: TRegularTest read fTest1;
    property Test2: TRegularTest read fTest2;
    property DiffNorm: TOCTValue read fDiffNorm;
    property DiffNormValue: Real read fDiffNormValue;
    property DiffNormUnits: TUnits read fDiffNormUnits;
  end;

  { Список тестов }
  TTestList = class(TObjectList)
  private

  protected
    function GetItem(Index: Integer): TTest;

  public
    function CountAll: Integer;
    function Add(AObject: TTest): Integer;
    property Items[Index: Integer]: TTest read GetItem; default;
  end;

//------------------------------------------------------------------------------
// План
//------------------------------------------------------------------------------
type
  TOnTestEndEvent = procedure(Sender: TObject; TestNumber: Cardinal; TestResult: TTestResult) of object;

  TPlan = class(TObject)
  private
    fTests: TTestList;
    fClass: array[1..21] of TOCTValue;
    fOnTestEnd: TOnTestEndEvent;

    function TestType(slTest: String): TTestType;

    function LoadWP(const FileName: String): Boolean;
  protected

  public
    constructor Create(const FileName: String);
    destructor Destroy; override;

    function Execute(const fADC: Boolean): Int64;

    property Tests: TTestList read fTests;
  published
    property OnTestEnd: TOnTestEndEvent read fOnTestEnd write fOnTestEnd;
  end;

//    TOnErrorEvent = procedure(Sender: TObject; ErrorCode: Cardinal;
//    ErrorMsg: string) of object;



function ADC(K41, V41, K42, V42: String): Real;

implementation

uses
  SysUtils, StrUtils, Dialogs, Forms,
  Math, CONVUNIT, StopWatch,
  Integral_IO;

var
  K41, V41, K42, V42: string;

{ TTestList }

function TTestList.Add(AObject: TTest): Integer;
begin
  Result := inherited Add(AObject);
end;

function TTestList.CountAll: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if Items[i].TestType = ttDifferential then
      Result := Result + 2
    else
      Result := Result + 1;
end;

function TTestList.GetItem(Index: Integer): TTest;
begin
  Result := TTest(inherited Items[Index]);
end;

{ TPlan }

constructor TPlan.Create(const FileName: String);
begin
  inherited Create;

  fTests := TTestList.Create(True);
  LoadWP(FileName);
end;

destructor TPlan.Destroy;
begin
  fTests.Free;

  inherited;
end;

function TPlan.Execute(const fADC: Boolean): Int64;
var
  i: integer;
  SW: TStopWatch;
begin
//  for i := 0 to fTests.Count - 1 do
//  begin
//    fTests[i].fResult.fGroup     := '0000';
//    fTests[i].fResult.fValue     := 0.0;
//    fTests[i].fResult.fBrak      := False;
//    fTests[i].fResult.fOSC       := False;
//    fTests[i].fResult.fOverloadP := False;
//    fTests[i].fResult.fOverloadM := False;
//  end;
  SW := TStopWatch.Create(True);

  for i := 0 to fTests.Count - 1 do
  begin
    fTests[i].Execute(fADC);
    if Assigned(fOnTestEnd) then
    begin
      fOnTestEnd(Self, fTests[i].TestN, fTests[i].Result);
      Application.ProcessMessages;
    end;

    if fTests[i].fResult.fBrak then
    begin
      fIO.Command(fTests[i].fResult.fGroup);
      Break;
    end;

    if (i = (fTests.Count - 1)) and (not fTests[i].fResult.fBrak) then
      fIO.Command('0300');
  end;

  fIO.Command('0217');

  SW.Stop;
  Result := SW.ElapsedMilliseconds;
  SW.Free;
end;

function TPlan.LoadWP(const FileName: String): Boolean;
var
  slFile: TStringList;
  sl: TStringList;
  Row: Integer;
  Test: TTest;
  i: integer;
begin
  if not FileExists(FileName) then
  begin
    Result := False;
    Exit;
  end;

  slFile := TStringList.Create;
  try
    slFile.LoadFromFile(FileName);
    { Очистить от пустых строк и комментариев }
    for Row := slFile.Count - 1 downto 0 do
    begin
      if (Trim(slFile[Row]) = '') or
        (TrimLeft(slFile[Row])[1] = ';') then
        slFile.Delete(Row);
    end;

    { Перебираем строки }
    Row := 0;
    while(Row < slFile.Count) do
    begin
      { Если конец программы: код '7700' }
      if (Pos('7700', slFile[Row]) <> 0) then
      begin
        sl := TStringList.Create;
        try
          sl.Delimiter := #9;
          sl.DelimitedText := slFile[Row];
          sl.Delete(0);
          for i := 1 to sl.Count do
            fClass[i] := sl[i-1];
        finally
          sl.Free;
        end;

        Break;
      end;

      { Определяем тип теста }
      case TestType(slFile[Row]) of
        ttRegular        : fTests.Add( TRegularTest.Create(slFile[Row]) );
        ttCharacteristic : fTests.Add( TCharacteristicTest.Create(slFile[Row]) );
        ttDifferential   :
        begin
          fTests.Add( TDifferentialTest.Create(slFile[Row], slFile[Row+1]) );
          Row := Row + 1;
        end;
      else
        fTests.Add( TRegularTest.Create(slFile[Row]) );
      end;

      Inc(Row);
    end;

  finally
    slFile.Free;
  end;

end;

//------------------------------------------------------------------------------
// Возвращает тип теста по его тексту
//------------------------------------------------------------------------------
function TPlan.TestType(slTest: String): TTestType;
  function ReversePos(const SubString, S: String): Integer;
  begin
    Result := Length(S) - Pos(AnsiReverseString(SubString), AnsiReverseString(S));
  end;
begin
  case Copy(slTest, ReversePos(#9'76', slTest), 4)[4] of
    '2', '3': Result := ttCharacteristic;
    '4', '5': Result := ttDifferential;
  else
    Result := ttRegular;
  end;
end;

{ TRegularTest }

constructor TRegularTest.Create(TestText: String);
begin
  inherited Create;

  fCommands := TStringList.Create;
  fCommands.Delimiter := #9;
  fCommands.DelimitedText := TestText;  // разбираем строку на комманды

  TestN := GetTestN(fCommands);
end;

destructor TRegularTest.Destroy;
begin
  fCommands.Destroy;

  inherited;
end;

function TRegularTest.Execute(const fADC: Boolean = False): Integer;
var
  R: Word;
  i, Item: Integer;
begin
  Item := 0;
  while (Item <= fCommands.Count - 1) do
  begin                               
    case (StrToInt(fCommands[Item]) div 100) of
      { Ошибочные коды: 1000-3777; 4700-5077; 5400-7277 }
//      10..37, 47..50, 54..72:
//      begin
//
//      end;
      00,01,04..07:
        begin
          fIO.Command(fCommands[Item]);
          Item := Item + 1;
        end;

      { 02 - Сброс ранее введенной программы
        03 - Включение индикации поста оператора и
             отключение измерительных цепей пульта }
      02, 03:
        begin
          fIO.Command(fCommands[Item]);
          Item := Item + 1;
        end;

      { Останов автоматической работы ЭВМ }
      73:
        begin
          ShowMessage('Остановка по коду "7300". Нажмите "ПРОБЕЛ" для продолжения');
          Item := Item + 1;
        end;  

      { Программирование коммутации вблоке матрицы М201 }
      74:
        begin
          i := Item + 1;
          while (i <= fCommands.Count - 2) and
            (not ((StrToInt(fCommands[i]) div 100) in [40..47, 51, 76])) do
          begin
            case fCommands[i][3] of
              '2': fIO.Matrix(fCommands[i][1] + fCommands[i][2] + '0' + fCommands[i][4]);
              '6': fIO.Matrix(fCommands[i][1] + fCommands[i][2] + '4' + fCommands[i][4]);
            else
              fIO.Matrix(fCommands[i]);
            end;

            i := i + 1;
          end;
          Item := i;

          Sleep(3);  // задержка на коммутацию
        end;

      { 40         - включение вентилей источников в блоках И203 и В202
        41, 42     - программирование цифрового измерительного блока В202
        43..47, 51 - программирование блоков источников И203                  }
      40..47, 51:
        begin
          if (StrToInt(fCommands[Item]) div 100 = 41) then
          begin
            K41 := fCommands[Item];
            V41 := fCommands[Item + 1];
          end;
          if (StrToInt(fCommands[Item]) div 100 = 42) then
          begin
            K42 := fCommands[Item];
            V42 := fCommands[Item + 1];
          end;

          fIO.Command(fCommands[Item]);
          fIO.Value(fCommands[Item + 1]);

          Item := Item + 2;
        end;

      { Временная пауза }
      75:
      begin
        { Если длительность 0 - то пауза 4096 мс }
        if ( StrToInt(fCommands[Item][3] + fCommands[Item][4]) = 0 ) then
          Sleep(4096)
        else
          Sleep(StrToInt(fCommands[Item][3] + fCommands[Item][4]));

        Item := Item + 1;
      end;

      { код "критерия годности" - конец теста }
      76:
        begin
          { Задержка перед измерением }
          if StrToInt(fCommands[Item + 1]) = 0 then
            Sleep(4096)
          else
            Sleep(StrToInt(fCommands[Item + 1]));

          { код "критерия годности" }
          case fCommands[Item][4] of
            '0': R := fIO.K100;
            '1': R := fIO.K101;
          end;

          fResult.fGroup := fCommands[Item + 2];
          { наличие Перегрузок и Генерации }
          fResult.fOSC       := Boolean(((R shr  8) and $1));
          fResult.fOverloadM := Boolean(((R shr  9) and $1));
          fResult.fOverloadP := Boolean(((R shr 10) and $1));
          { ГОДЕН/БРАК }
          fResult.fBrak := Boolean(((R shr 11) and $1));

          if fADC then
          begin
            fResult.fValue := ADC(K41, V41, K42, V42);
            fIO.Command(K41);
            fIO.Value(V41);
          end;

          Break;
        end;
    else
      Item := Item + 1;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Возвращает номер теста по его тексту
//------------------------------------------------------------------------------
function TRegularTest.GetTestN(slTest: TStringList): Cardinal;
begin
  if UpperCase(slTest[0][1]) = 'T' then
  begin
    Result := StrToInt( StrUtils.RightStr(slTest[0], Length(slTest[0])-1) );
    slTest.Delete(0);
  end
  else
    Result := 0;
end;

function TRegularTest.GetTestType: TTestType;
begin
  Result := ttRegular;
end;

{ TTest }

constructor TTest.Create;
begin
  fTestN := 0;
  with fResult do
  begin
    fValue     := 0.0;
    fBrak      := False;
    fOSC       := False;
    fOverloadP := False;
    fOverloadM := False;
  end;
end;

{ TCharacteristicTest }

constructor TCharacteristicTest.Create(TestText: String);
var
  i: Integer;
begin
  inherited Create;

  fCommands := TStringList.Create;
  fCommands.Delimiter := #9;
  fCommands.DelimitedText := TestText;  // разбираем строку на комманды

  for i:= 1 to Length(fParams) do
    fParams[i] := fCommands[fCommands.Count - Length(fParams) + i - 1];
  for i:= 1 to Length(fParams) do
    fCommands.Delete(fCommands.Count - Length(fParams) + i - 1);

  TestN := GetTestN(fCommands);
end;

function TCharacteristicTest.Execute(const fADC: Boolean = False): Integer;
begin

end;

function TCharacteristicTest.GetParams(Item: Integer): TOCTValue;
begin
  if Item in [1..5] then
    Result := fParams[Item]
  else
    Result := '';
end;

function TCharacteristicTest.GetTestN(slTest: TStringList): Cardinal;
begin
  if UpperCase(slTest[0][1]) = 'T' then
  begin
    Result := StrToInt( StrUtils.RightStr(slTest[0], Length(slTest[0])-1) );
    slTest.Delete(0);
  end
  else
    Result := 0;
end;

function TCharacteristicTest.GetTestType: TTestType;
begin
  Result := ttCharacteristic;
end;

{ TDifferentialTest }

constructor TDifferentialTest.Create(TestText1, TestText2: String);
var
  l_r: Real;
  K41, K42: TOCTValue;
begin
  inherited Create;

  fDiffNorm := Copy(TestText1, Pos(#9'03', TestText1)+6, 4);

  TestText1 := LeftStr(TestText1, Length(TestText1)-5);
  case Copy(TestText1, Pos(#9'76', TestText1)+1, 4)[4] of
    '4': TestText1[Pos(#9'76', TestText1)+4] := '0';
    '5': TestText1[Pos(#9'76', TestText1)+4] := '1';
  end;

  fTest1 := TRegularTest.Create(TestText1);
  fTest2 := TRegularTest.Create(TestText2);

  l_r := StrToInt('$'+IntToHex(OCT2DEC(fDiffNorm),3)[1]) +
         StrToInt('$'+IntToHex(OCT2DEC(fDiffNorm),3)[2])*0.1 +
         StrToInt('$'+IntToHex(OCT2DEC(fDiffNorm),3)[3])*0.01;

  K41 := Copy(TestText1, Pos(#9'41', TestText1)+1, 4);
  K42 := Copy(TestText1, Pos(#9'42', TestText1)+1, 4);
  case K41[3] of
    '4': begin fDiffNormUnits := A; fDiffNormValue := l_r * Power(10, -StrToInt(K41[4])); end;     // Current -> A
    '6': begin fDiffNormUnits := A; fDiffNormValue := -(l_r * Power(10, -StrToInt(K41[4]))); end;  // Current -> A
  else
    fDiffNormUnits := V;
    case K42[4] of  // Voltage -> V
      '1': fDiffNormValue := l_r * 2;  // x2
      '2': fDiffNormValue := l_r * 1;  // x1
      '3': fDiffNormValue := l_r * 0.1;  // x0.1

      '5': fDiffNormValue := -(l_r * 2);  // x2
      '6': fDiffNormValue := -(l_r * 1);  // x1
      '7': fDiffNormValue := -(l_r * 0.1);  // x0.1
    end;
  end;  
end;

destructor TDifferentialTest.Destroy;
begin
  fTest1.Free;
  fTest2.Free;

  inherited;
end;

function TDifferentialTest.Execute(const fADC: Boolean = False): Integer;
var
  K76: TOCTValue;
begin
  fTest1.Execute(True);
  fTest2.Execute(True);

  fResult.fValue     := Abs(fTest2.fResult.fValue - fTest1.fResult.fValue);
  fResult.fOSC       := fTest1.fResult.fOSC or fTest2.fResult.fOSC;
  fResult.fOverloadP := fTest1.fResult.fOverloadP or fTest2.fResult.fOverloadP;
  fResult.fOverloadM := fTest1.fResult.fOverloadM or fTest2.fResult.fOverloadM;

  K76 := Copy(fTest1.fCommands.Text, Pos(#$A'76', fTest1.fCommands.Text)+1, 4);
  case K76[4] of
    '0': fResult.fBrak := fResult.fValue > Abs(fDiffNormValue);   // ГОДЕН, если абсолютная величина разности равна или менее абсолютной величины уставки
    '1': fResult.fBrak := fResult.fValue <= Abs(fDiffNormValue);  // ГОДЕН, если абсолютная величина разности измеренных параметров более абсолюной величины уставки
  end;
end;

function TDifferentialTest.GetTestType: TTestType;
begin
  Result := ttDifferential;
end;

function ADC(K41, V41, K42, V42: String{; DownNorm, UPNorm: Real; NormUnits: TUnits}): Real;
  type
    TPolarity = (pPositive, pNegative);
    TMeasureMode = (mmVoltage, mmCurrent);
    s_bit  = set of 0..15;
  var
    R: Real;
    l1, l2, l: Word;
    l_r: Real;
    Res: Word;
    Res_b: s_bit absolute res;
    i: Integer;

    Polarity: TPolarity;
    MeasureMode: TMeasureMode;
    MeasureDiap: Byte;
begin
  if fIO.Emulation then
  begin
    Result := RandomRange(-3000, 3000)*0.01;
    Exit;
  end;

  { Режим измерения }
  if K41[3] in ['4', '6'] then
    MeasureMode := mmCurrent
  else
    MeasureMode := mmVoltage;

  { Определяем полярность }
  fIO._Kommand(OCT2DEC(K41)); fIO._Value(0);
  Res := fIO.K100;
  if (11 in Res_b) then
    Polarity := pPositive
  else
    Polarity := pNegative;

  { Определяем диапазон }
  case MeasureMode of
    mmCurrent:
    begin
      for i := 7 downto 1 do
      begin
        if Polarity = pPositive then
          fIO._Kommand( OCT2DEC('41' + '4'{K41[3]} + IntToStr(i)))
        else
          fIO._Kommand( OCT2DEC('41' + '6'{K41[3]} + IntToStr(i)));

        if i = 7 then
          fIO._Value(OCT2DEC('7631'));

//        Sleep(120);
        Sleep(5);

        if Polarity = pPositive then
          Res := fIO.K100
        else
          Res := fIO.K101;

        if not (11 in Res_b) then
        begin
          MeasureDiap := i;
          Break;
        end;
      end;
    end;

    mmVoltage:
    begin
      for i := 3 downto 1 do
      begin
        if i = 3 then
        begin
          fIO._Kommand(OCT2DEC(K41));
          fIO._Value(OCT2DEC('7631'));
        end;
        if Polarity = pPositive then
          fIO._Kommand( OCT2DEC('42' + K42[3] + IntToStr(i)))
        else
          fIO._Kommand( OCT2DEC('42' + K42[3] + IntToStr(4 + i)));

//        Sleep(50);
        Sleep(5);

        if Polarity = pPositive then
          Res := fIO.K100
        else
          Res := fIO.K101;

        if not (11 in Res_b) then
        begin
          MeasureDiap := i;
          Break;
        end;
      end;
    end;
  end;

  { Поиск значения }
  l1 := 0;
  l2 := OCT2DEC('7631');
  case MeasureMode of
    mmCurrent:
    begin
      if Polarity = pPositive then
        fIO._Kommand( OCT2DEC('41' + '4'{K41[3]} + IntToStr(MeasureDiap)))
      else
        fIO._Kommand( OCT2DEC('41' + '6'{K41[3]} + IntToStr(MeasureDiap)));
    end;

    mmVoltage:
    begin
      if Polarity = pPositive then
        fIO._Kommand( OCT2DEC('42' + K42[3] + IntToStr(MeasureDiap)))
      else
        fIO._Kommand( OCT2DEC('42' + K42[3] + IntToStr(4 + MeasureDiap)));
      fIO._Kommand(OCT2DEC(K41));        
    end;
  end;

  repeat
    l := (l1 + l2) div 2;
    l_r := StrToInt('$'+IntToHex(l,3)[1]) + StrToInt('$'+IntToHex(l,3)[2])*0.1 + StrToInt('$'+IntToHex(l,3)[3])*0.01;
    fIO._Value(l);

    if Polarity = pPositive then
      Res := fIO.K100
    else
      Res := fIO.K101;

    if (11 in Res_b) then
    begin //' > '
      l1 := l;
    end
    else
    begin //' < '
      l2 := l;
    end;
  until (l2 - l1) <= 1;

  if Polarity = pNegative then
    l_r := -l_r;

  case MeasureMode of
    mmCurrent:
    begin
      R := l_r * Power(10, -MeasureDiap);  // -> A
//      case NormUnits of
//        nA: R := R * Power(10, 9);         // -> nA
//        uA: R := R * Power(10, 6);         // -> uA
//        mA: R := R * Power(10, 3);         // -> mA
//      end;
    end;

    mmVoltage:
    begin
      { -> V }
      case MeasureDiap of
        1: R := l_r * 2;  // x2
        2: R := l_r * 1;  // x1
        3: R := l_r * 0.1;  // x0.1
      end;
      
//      case NormUnits of
//        mV: R := R * Power(10, 3);         // -> mV
//      end;
    end;
  end;

  Result := R;
end;

end.
