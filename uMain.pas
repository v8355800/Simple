unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OoMisc, ADTrmEmu, StdCtrls, ComCtrls, Grids,
  uIntegral, JvExExtCtrls, JvRadioGroup, NxScrollControl,
  NxCustomGridControl, NxCustomGrid, NxGrid, NxEdit, NxColumns,
  NxColumnClasses, Spin, NxThemesSupport;

type
  TForm1 = class(TForm)
    tmrPUSK: TTimer;
    AdVT100Emulator: TAdVT100Emulator;
    StatusBar: TStatusBar;
    PageControl3: TPageControl;
    tsParams: TTabSheet;
    grpStopMode: TGroupBox;
    rbStopMode0: TRadioButton;
    rbStopMode1: TRadioButton;
    rbStopMode2: TRadioButton;
    rbStopMode3: TRadioButton;
    rbStopMode4: TRadioButton;
    TabSheet8: TTabSheet;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    Terminal: TAdTerminal;
    TabSheet6: TTabSheet;
    ListBox1: TListBox;
    TabSheet7: TTabSheet;
    Grid: TStringGrid;
    grpRegMode: TGroupBox;
    rbRegMode0: TRadioButton;
    rbRegMode1: TRadioButton;
    rbRegMode2: TRadioButton;
    rbRegMode3: TRadioButton;
    rbRegMode4: TRadioButton;
    rbRegMode5: TRadioButton;
    grpRW: TGroupBox;
    chkRW0: TCheckBox;
    chkRW1: TCheckBox;
    chkRW2: TCheckBox;
    chkRW3: TCheckBox;
    chkRW4: TCheckBox;
    chkRW5: TCheckBox;
    chkRW6: TCheckBox;
    chkRW8: TCheckBox;
    chkRW9: TCheckBox;
    chkRW10: TCheckBox;
    chkRW11: TCheckBox;
    chkRW12: TCheckBox;
    TabSheet1: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    nxGrid: TNextGrid;
    Label3: TLabel;
    edtColNumber: TNxSpinEdit;
    Button1: TButton;
    Label4: TLabel;
    edtGridFontSize: TNxSpinEdit;
    pnlClient: TPanel;
    pnlTop: TPanel;
    PageControl1: TPageControl;
    tsMeasure: TTabSheet;
    GroupBox1: TGroupBox;
    cbWP: TComboBox;
    btnShowWP: TButton;
    pnlCRC: TPanel;
    GroupBox2: TGroupBox;
    lblMeasureTime: TLabel;
    lblProgressBar: TLabel;
    cbADC: TCheckBox;
    btnStart: TButton;
    ProgressBar: TProgressBar;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    pnlRegMode: TPanel;
    lblRegMode: TLabel;
    pnlStopMode: TPanel;
    lblStopMode: TLabel;
    chkRW7: TCheckBox;
    pnlRW: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrPUSKTimer(Sender: TObject);
    procedure cbADCClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnShowWPClick(Sender: TObject);
    procedure rbRegModeClick(Sender: TObject);
    procedure rbStopModeClick(Sender: TObject);
    procedure chkRWClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure edtColNumberChange(Sender: TObject);
    procedure edtGridFontSizeChange(Sender: TObject);
    procedure nxGridCellColoring(Sender: TObject; ACol, ARow: Integer;
      var CellColor, GridColor: TColor; CellState: TCellState);
  private
    { Private declarations }
    procedure TermWrite(const S: String);
    procedure TermWriteLn(const S: String = '');
    procedure FillComboBox(const ComboBox: TComboBox);
    procedure ReadParams;
    procedure CreateGrid(const Columns: Byte);
    procedure FillGrid;

    procedure Measure(const WPFileName: String);
    procedure Plan1OnTestEnd(Sender: TObject; TestNumber: Cardinal; TestResult: TTestResult);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  fADC: Boolean;

//function ADC(K41, V41, K42, V42: String{; DownNorm, UPNorm: Real; NormUnits: TUnits}): Real;
function ValueToString(const Val: Real; Mode: Byte): string;

implementation

{$R *.dfm}

uses
  Math, ShellAPI,
  JvStrUtils, JclLogic,
  CONVUNIT, uCRC32,
  Integral_IO;

const
  ColumnN = 3;

var
  CurrPlan: Byte;
  OSC, OverloadP, OverloadM: Boolean;
  Plan1: TPlan;

procedure TForm1.TermWrite(const S: String);
begin
  if Terminal.Font.Charset = OEM_CHARSET then
    Terminal.WriteString(StrToOem(S))
  else
    Terminal.WriteString(S);
end;

procedure TForm1.TermWriteLn(const S: String = '');
begin
  if Terminal.Font.Charset = OEM_CHARSET then
    Terminal.WriteString(StrToOem(S)+#13#10)
  else
    Terminal.WriteString(S+#13#10);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Params: TBoardParams;
begin
  DecimalSeparator := '.';

  FillComboBox(cbWP);

  if TIntegralIO_LPT.FindBoard(INTEGRAL_ID, Params) then
  begin
    fIO := TIntegralIO_LPT.Create( Format('[$%.4x] %s', [Params.IOPort, Params.Name]), Params.IOPort, False);
    StatusBar.SimpleText := Format('[$%.4x] %s', [Params.IOPort, Params.Name] );
  end
  else
  begin
    fIO := TIntegralIO_LPT.Create( Format('[$%.4x] %s', [$0, 'LPT1']), $0, True);
    StatusBar.SimpleText := 'Режим эмуляции';
  end;

  tmrPUSK.Enabled := not fIO.Emulation;
  if tmrPUSK.Enabled then
  begin
    fIO.Command('0217');
    fIO.Command('0300');
  end;

  fADC := cbADC.Checked;
  Terminal.Columns := 120;
  CurrPlan := 0;
  edtGridFontSize.Value := nxGrid.Font.Size;

  { Интегрировать надпись в progressbar }
  lblProgressBar.Parent      := ProgressBar;
  lblProgressBar.AutoSize    := False;
  lblProgressBar.Transparent := True;
  lblProgressBar.Top         := 0;
  lblProgressBar.Left        := 0;
  lblProgressBar.Width       := ProgressBar.ClientWidth;
  lblProgressBar.Height      := ProgressBar.ClientHeight;
  lblProgressBar.Alignment   := taCenter;
  lblProgressBar.Layout      := tlCenter;

  { Считать Параметры разбраковки }
  ReadParams;
  CreateGrid(edtColNumber.AsInteger);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fIO.Free;
end;

procedure TForm1.tmrPUSKTimer(Sender: TObject);
var
  PlanN: Byte;
begin
  PlanN := (fIO.K100 and $F);

  if PlanN > 0 then
  begin
    TTimer(Sender).Enabled := False;

//    CurrPlan := PlanN;
//    MEASURE('..\WP\112LD1.txt');
    btnStartClick(btnStart);

    TTimer(Sender).Enabled := True;
  end;
end;

procedure TForm1.Measure(const WPFileName: String);
type
  TTestType = (ttRegular, ttDiff, ttChar, ttClass);
  TDiffTest = record
    V1: Real;
    V2: Real;
    Norm: Real;
  end;
var
  slFile: TStringList;
  slRow: TStringList;
  i, Row, Item: Integer;
  R: Word;
  Brak: Boolean;
  S: string;

  K41, V41, K42, V42: String;
  RES_ADC: array[1..1000] of Real;

  LastTestType, CurrTestType: TTestType;

  DiffTest: TDiffTest;
begin
  if not FileExists(WPFileName) then
  begin
    fIO.Command('0317');

    ShowMessage('Файл с РП ' + QuotedStr(WPFileName) + ' не найден!');
    Exit;
  end;

  TermWriteLn('ПЛАН №' + IntToStr(CurrPlan));
  TermWriteLn;
  TermWriteLn('START_PROGRAM');

  slFile := TStringList.Create;
  slRow := TStringList.Create;
  slRow.Delimiter := #9;
  try
    slFile.LoadFromFile(WPFileName);
    { Очистить от пустых строк и комментариев }
    for Row := slFile.Count - 1 downto 0 do
    begin
      if (Trim(slFile[Row]) = '') or
        (TrimLeft(slFile[Row])[1] = ';') then
        slFile.Delete(Row);
    end;

    for Row := 0 to slFile.Count - 1 do
    begin
//      TESTinc;
      Application.ProcessMessages;

      if ((Row+0) mod ColumnN) = 0 then
        TermWriteLn;
      TermWrite(Format(#9'T%.3d'#9, [Row+1]));

      Brak := False;

      slRow.DelimitedText := slFile[Row];

      Item := 1;
      //      S := '';
      while (Item <= slRow.Count - 1) do
      begin
        //        S := S + slRow[Item] + ' ';
        case (StrToInt(slRow[Item]) div 100) of
          74:
            begin
              //            MatrixStr := '';
              i := Item + 1;
              while (i <= slRow.Count - 2) and
                (not ((StrToInt(slRow[i]) div 100) in [40..47, 51, 76])) do
              begin
                if slRow[i][3] = '2' then
                  slRow[i] := slRow[i][1] + slRow[i][2] + '0' + slRow[i][4];
                if slRow[i][3] = '6' then
                  slRow[i] := slRow[i][1] + slRow[i][2] + '4' + slRow[i][4];

                fIO.Matrix(slRow[i]);
                //              MatrixStr := MatrixStr + slRow[Matrix] + ' ';
                //              Matrix := Matrix + 1;
                i := i + 1;
              end;
              Sleep(10);
              Item := i;
            end;
        end;

        case (StrToInt(slRow[Item]) div 100) of
          40..47, 51:
            begin
              //            Writeln( GetCommandText(slRow[Item], slRow[Item+1]) );
              //            Writeln;
              if (StrToInt(slRow[Item]) div 100 = 41) then
              begin
                K41 := slRow[Item];
                V41 := slRow[Item + 1];
              end;
              if (StrToInt(slRow[Item]) div 100 = 42) then
              begin
                K42 := slRow[Item];
                V42 := slRow[Item + 1];
              end;

              fIO.Command(slRow[Item]);
              fIO.Value(slRow[Item + 1]);

              Item := Item + 2;
            end;

          75:
          begin
            Sleep(StrToInt(slRow[Item][3]+slRow[Item][4]));
            Item := Item + 1;            
          end;

          76:
            begin
              Sleep(StrToInt(slRow[Item + 1]));
              if slRow[Item][4] = '0' then
              begin
                R := fIO.K100;
              end
              else
              begin
                R := fIO.K101;
              end;

//              if (((R shr 8) and 7) <> 0) then
//              begin
////                smenagr := True;
////                teststop;
//              end;

              if (((R shr 11) and 1) <> 0) then
              begin
//                fIO.Command(slRow[Item + 2]);
                Brak := True;
              end;

//              if TESTNO in ostanov then
//                teststop;

              case slRow[Item][4] of
                '2', '3': CurrTestType := ttChar;
                '4', '5': CurrTestType := ttDiff;
              else
                CurrTestType := ttRegular;
              end;

              if fADC then
                RES_ADC[Row+1] := ADC(K41, V41, K42, V42);

              if CurrTestType = ttDiff then
              begin
                DiffTest.Norm := StrToInt(slRow[Item + 3]);
                LastTestType := CurrTestType;
                Break;
              end;

              if LastTestType = ttDiff then
              begin
                  if K41[3] in ['4', '6'] then
                    TermWrite( ValueToString(RES_ADC[Row+1]-RES_ADC[Row], 0) )
                  else
                    TermWrite( ValueToString(RES_ADC[Row+1]-RES_ADC[Row], 1) );
              end
              else
                if fADC then
                begin
                  if K41[3] in ['4', '6'] then
                    TermWrite( ValueToString(RES_ADC[Row+1], 0) )
                  else
                    TermWrite( ValueToString(RES_ADC[Row+1], 1) );

  //                TermWrite( Format('%10.3e', [RES_ADC[Row+1]]) );
  //                { Режим измерения }
  //                if K41[3] in ['4', '6'] then
  //                  TermWrite(' A')
  //                else
  //                  TermWrite(' V');
                end;

              if RegMode = 2 then
              begin
                if Brak then
                begin
                  if TestBit(R, 8) or TestBit(R, 9) or TestBit(R, 10) then
                    TermWrite(';')
                  else
                    TermWrite('*');
                end
                else
                begin
                  if TestBit(R, 8) or TestBit(R, 9) or TestBit(R, 10) then
                    TermWrite('!');
//                  else
//                    TermWrite('.');
                end;
              end;

              fIO.Command(K41);
              fIO.Value(V41);
              LastTestType := CurrTestType;
              Break;
            end;

//            77:
//            begin
//              SKIP := 1;
//            end;
        else
          fIO.Command(slRow[Item]);
          //            Writeln( GetCommandText(slRow[Item], '') );
          //            Writeln;
          Item := Item + 1;
        end;
      end;
      { Тест разобран }

      { Get test number }
      if UpperCase(slRow[0][1]) = 'T' then
        S := slRow[0];

      if Brak then
      begin
//        lbOUT.Items.Append(S + ' - <> - ' + FloatToStr(ADC));
//        GRIEF := 1;
//        TOTALBRAK := True;
//        ENDTEST;
//        if not Integral_Globals.ignore then
//          Break;
      end
      else
      begin
//        GRIEF := 0;
//        ENDTEST;
      end;
//        lbOUT.Items.Append(S + ' - OK - ' + FloatToStr(ADC));
    end;

    fIO.Command('0217');
  finally
    slFile.Free;
    fIO.Command('0300');
  end;

  TermWriteLn;
  TermWriteLn('END_PROGRAM');
end;

function ValueToString(const Val: Real; Mode: Byte): string;
begin
  if Mode = 0 then
  begin
    { A }
//    if Abs(Val) > 1E-9 then
//      Result := Format('%10.3f nA', [Val * Power(10, 9)]);

    if Abs(Val) > 1E-9 then
      Result := Format('%10.3f uA', [Val * Power(10, 6)]);

    if Abs(Val) > 1E-6 then
      Result := Format('%10.3f mA', [Val * Power(10, 3)]);

//    if Abs(Val) > 1E-3 then
//      Result := Format('%10.3f A', [Val]);

  end
  else
  begin
    { V }
    if Abs(Val) > 1E-3 then
      Result := Format('%10.3f mV', [Val * Power(10, 3)]);

    if Abs(Val) > 1    then
      Result := Format('%10.3f  V', [Val]);
  end;  
end;  

procedure TForm1.cbADCClick(Sender: TObject);
begin
  fADC := cbADC.Checked;
end;

procedure TForm1.btnStartClick(Sender: TObject);
var
  i: Integer;
  Time: Int64;
begin
  Randomize;
  btnStart.Enabled := False;
  tsParams.Enabled := False;
  ListBox1.Items.Clear;

//  Plan1 := TPlan.Create('..\WP\112LD1.txt');
  if cbWP.ItemIndex > -1 then
  begin
    Plan1 := TPlan.Create(cbWP.Text);
    pnlCRC.Caption := Format('Контрольная сумма: %8x', [FileCRC32(cbWP.Text)]);
  end
  else
  begin
    Plan1 := TPlan.Create('..\WP\112LD1.txt');
    pnlCRC.Caption := Format('Контрольная сумма: %8x', [FileCRC32('..\WP\112LD1.txt')]);
  end;

  Plan1.OnTestEnd := Plan1OnTestEnd;
  ProgressBar.Visible := True;
  lblProgressBar.Visible := True;
  ProgressBar.Max := Plan1.Tests.CountAll;
  ProgressBar.Position := 0;
//  Plan1 := TPlan.Create('..\WP\155LA3.txt');
  try
    Time := Plan1.Execute(cbADC.Checked);
//    lblMeasureTime.Caption := Format( 'Время измерения: %.3f с (%d мс)', [ Time / 1000, Time] );
    lblMeasureTime.Caption := Format( 'Время измерения: %d мс', [Time] );
    for i := 0 to Plan1.Tests.Count - 1 do
    begin
      if Plan1.Tests[i].TestType = ttDifferential then
        ListBox1.Items.Append( Format('D %d, %d: %s (%.3e) %s   ABS(%.3e - %3.e) = %.3e', [
          TDifferentialTest(Plan1.Tests[i]).Test1.TestN,
          TDifferentialTest(Plan1.Tests[i]).Test2.TestN,
          TDifferentialTest(Plan1.Tests[i]).DiffNorm,
          TDifferentialTest(Plan1.Tests[i]).DiffNormValue,
          TUnitsString[TDifferentialTest(Plan1.Tests[i]).DiffNormUnits],
          TDifferentialTest(Plan1.Tests[i]).Test1.Result.fValue,
          TDifferentialTest(Plan1.Tests[i]).Test2.Result.fValue,
          TDifferentialTest(Plan1.Tests[i]).Result.fValue
          ]) )
      else if Plan1.Tests[i].TestType = ttCharacteristic then
        ListBox1.Items.Append( Format('H %d: %s', [TCharacteristicTest(Plan1.Tests[i]).TestN,
        TCharacteristicTest(Plan1.Tests[i]).Params[1] + ' ' +
        TCharacteristicTest(Plan1.Tests[i]).Params[2] + ' ' +
        TCharacteristicTest(Plan1.Tests[i]).Params[3] + ' ' +
        TCharacteristicTest(Plan1.Tests[i]).Params[4] + ' ' +
        TCharacteristicTest(Plan1.Tests[i]).Params[5]
        ]) )
      else
        ListBox1.Items.Append( Format('%d: %.3e', [Plan1.Tests[i].TestN, Plan1.Tests[i].Result.fValue]) );
    end;

    { Grid }
    Grid.RowCount := Plan1.Tests.Count + 1;
    for i := 0 to Plan1.Tests.Count - 1 do
    begin
      case Plan1.Tests[i].TestType of
        ttRegular, ttCharacteristic: Grid.Cells[0, i+1] := Format('T%.3d', [Plan1.Tests[i].TestN]);
        ttDifferential: Grid.Cells[0, i+1] := Format('T%.3d, %.3d', [TDifferentialTest(Plan1.Tests[i]).Test1.TestN, TDifferentialTest(Plan1.Tests[i]).Test2.TestN]);
      end;

      Grid.Cells[1, i+1] := Format('%.3e', [Plan1.Tests[i].Result.fValue]);
    end;

    { NextGrid }
    FillGrid;

  finally
    Plan1.Free;
  end;

  ProgressBar.Visible := False;
  lblProgressBar.Visible := False;
  btnStart.Enabled := True;
  tsParams.Enabled := True;  
end;

procedure TForm1.Plan1OnTestEnd(Sender: TObject; TestNumber: Cardinal;
  TestResult: TTestResult);
begin
  ProgressBar.Position := TestNumber+1;
  lblProgressBar.Caption := Format('Тест №%d из %d', [TestNumber+1, TPlan(Sender).Tests.CountAll]);
end;

procedure TForm1.FillComboBox(const ComboBox: TComboBox);
  { Процедура для построения списка файлов }
  procedure FindFiles(FilesList: TStrings; StartDir, FileMask: string);
  var
    SR: TSearchRec;
    DirList: TStringList;
    IsFound: Boolean;
    i: integer;
  begin
    if StartDir[length(StartDir)] <> '\' then
      StartDir := StartDir + '\';

    { Build a list of the files in directory StartDir
       (not the directories!)                         }

    IsFound :=
      FindFirst(StartDir+FileMask, faAnyFile-faDirectory, SR) = 0;
    while IsFound do begin
      FilesList.Add(StartDir + SR.Name);
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);

    // Build a list of subdirectories
    DirList := TStringList.Create;
    IsFound := FindFirst(StartDir+'*.*', faAnyFile, SR) = 0;
    while IsFound do begin
      if ((SR.Attr and faDirectory) <> 0) and
           (SR.Name[1] <> '.') then
        DirList.Add(StartDir + SR.Name);
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);

    // Scan the list of subdirectories
    for i := 0 to DirList.Count - 1 do
      FindFiles(FilesList, DirList[i], FileMask);

    DirList.Free;
  end;
begin
  ComboBox.Items.Clear;

  FindFiles(ComboBox.Items, '..\WP', '*.txt');
end;

procedure TForm1.btnShowWPClick(Sender: TObject);
begin
  if cbWP.ItemIndex > -1 then
    ShellExecute(0, nil, 'explorer.exe', PChar('/open,' + cbWP.Text), nil, SW_SHOWNORMAL);
end;

procedure TForm1.rbRegModeClick(Sender: TObject);
begin
  RegMode := TComponent(Sender).Tag;
//  pnlRegMode.Caption := 'Mode: ' + IntToStr(Ord(RegMode));
  lblRegMode.Caption := TButton(Sender).Caption;
end;

procedure TForm1.rbStopModeClick(Sender: TObject);
begin
  StopMode := TComponent(Sender).Tag;
//  pnlStopMode.Caption := 'Mode: ' + IntToStr(Ord(StopMode));
  lblStopMode.Caption := TButton(Sender).Caption;
end;

procedure TForm1.chkRWClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
//    Include(RW, TCheckBox(Sender).Tag)
    RW := RW + [TCheckBox(Sender).Tag]
  else
    RW := RW - [TCheckBox(Sender).Tag];

  pnlRW.Caption := DEC2OCT(WORD(RW)) + ' (8)';
end;

//------------------------------------------------------------------------------
// Считываем параметры разбраковки
//------------------------------------------------------------------------------
procedure TForm1.ReadParams;
var
  i: Integer;
begin
  RW := [];
  for i := 0 to grpRW.ControlCount-1 do
    if grpRW.Controls[i] is TCheckBox then
      if TCheckBox(grpRW.Controls[i]).Checked then
        RW := RW + [grpRW.Controls[i].Tag];
  pnlRW.Caption := DEC2OCT(WORD(RW)) + ' (8)';

  for i := 0 to grpRegMode.ControlCount-1 do
    if TRadioButton(grpRegMode.Controls[i]).Checked then
    begin
      RegMode := grpRegMode.Controls[i].Tag;
      lblRegMode.Caption := TButton(grpRegMode.Controls[i]).Caption;
      Break;
    end;

  for i := 0 to grpStopMode.ControlCount-1 do
    if TRadioButton(grpStopMode.Controls[i]).Checked then
    begin
      StopMode := grpStopMode.Controls[i].Tag;
      lblStopMode.Caption := TButton(grpStopMode.Controls[i]).Caption;
      Break;
    end;
end;

procedure TForm1.CreateGrid(const Columns: Byte);
var
  i: integer;
begin
  nxGrid.Columns.Clear;

  for i := 1 to Columns do
  begin
//    with TNxNumberColumn(nxGrid.Columns.Add(TNxNumberColumn, '№')) do
//    begin
//      Header.Alignment := taCenter;
//      Alignment := taCenter;
//      Width := 50;
//      HideWhenEmpty := True;
//    end;

    with nxGrid.Columns.Add(TNxTextColumn, 'Тест') do
    begin
      Header.Alignment := taCenter;
      Alignment := taCenter;
    end;
    with TNxNumberColumn(nxGrid.Columns.Add(TNxNumberColumn, 'Результат')) do
    begin
      FormatMask := '#,##0.00';
      Header.Alignment := taCenter;
      Alignment := taRightJustify;
      HideWhenEmpty := True;
    end;
    with nxGrid.Columns.Add(TNxTextColumn, 'Ед. изм.') do
    begin
      Header.Alignment := taCenter;
//      Header.MultiLine := True;
      Alignment := taCenter;
      Width := 100;
    end;

    if i <> Columns then
      with nxGrid.Columns.Add(TNxTextColumn, '') do
      begin
        Color := clGray;
        Width := 10;
      end;
  end;

	for i := 0 to nxGrid.Columns.Count - 1 do
  begin
    nxGrid.Columns[i].Options := nxGrid.Columns[i].Options - [coCanSort];
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateGrid(edtColNumber.AsInteger);
end;

procedure TForm1.FillGrid;
var
  i: Integer;
  Row, Col: Integer;
begin
  nxGrid.ClearRows;
  nxGrid.AddRow( Ceil(Plan1.Tests.Count/edtColNumber.AsInteger) );
  nxGrid.BeginUpdate;
  Row := 0; Col := 0;
  for i := 0 to Plan1.Tests.Count - 1 do
  begin
//      nxGrid.Cell[Col + 0, Row].AsInteger := i+1;
    case Plan1.Tests[i].TestType of
      ttRegular, ttCharacteristic : nxGrid.Cell[Col + 0, Row].AsString := Format('T%.3d', [Plan1.Tests[i].TestN]);
      ttDifferential              : nxGrid.Cell[Col + 0, Row].AsString := Format('T%.3d, %.3d', [TDifferentialTest(Plan1.Tests[i]).Test1.TestN, TDifferentialTest(Plan1.Tests[i]).Test2.TestN]);
    end;

    nxGrid.Cell[Col + 1, Row].AsFloat := Plan1.Tests[i].Result.fValue; //Format('T%.3d', [Plan1.Tests[i].TestN]);
    if ((i+1) mod edtColNumber.AsInteger) = 0 then
    begin
      Row := Row + 1;
      Col := 0;
    end
    else
      Col := Col + 4;
  end;
  nxGrid.EndUpdate;
end;

procedure TForm1.edtColNumberChange(Sender: TObject);
begin
  CreateGrid(edtColNumber.AsInteger);
end;

procedure TForm1.edtGridFontSizeChange(Sender: TObject);
begin
  nxGrid.Font.Size := TSpinEdit(Sender).Value;
end;

procedure TForm1.nxGridCellColoring(Sender: TObject; ACol, ARow: Integer;
  var CellColor, GridColor: TColor; CellState: TCellState);
begin
//  if not (csEmpty in CellState) then
//    if (ARow mod 2) <> 0 then
//    begin
//      CellColor := clLtGray;
//    end;
end;

end.

