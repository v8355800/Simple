object Form1: TForm1
  Left = 170
  Top = 44
  Width = 969
  Height = 896
  Caption = 'fMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 839
    Width = 953
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 953
    Height = 225
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 4
      Top = 4
      Width = 945
      Height = 217
      ActivePage = tsMeasure
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      object tsMeasure: TTabSheet
        Caption = #1056#1072#1073#1086#1095#1072#1103' '#1087#1088#1086#1075#1088#1072#1084#1084#1072
        ImageIndex = 4
        object GroupBox1: TGroupBox
          Left = 8
          Top = 8
          Width = 441
          Height = 81
          Caption = #1056#1072#1073#1086#1095#1072#1103' '#1087#1088#1086#1075#1088#1072#1084#1084#1072':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object cbWP: TComboBox
            Left = 8
            Top = 20
            Width = 321
            Height = 21
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ItemHeight = 13
            ParentFont = False
            TabOrder = 0
          end
          object btnShowWP: TButton
            Left = 336
            Top = 20
            Width = 97
            Height = 21
            Caption = #1055#1088#1086#1089#1084#1086#1090#1088'...'
            TabOrder = 1
            OnClick = btnShowWPClick
          end
          object pnlCRC: TPanel
            Left = 8
            Top = 52
            Width = 425
            Height = 21
            BevelOuter = bvLowered
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
          end
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 96
          Width = 441
          Height = 89
          Caption = #1048#1079#1084#1077#1088#1077#1085#1080#1077':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object lblMeasureTime: TLabel
            Left = 8
            Top = 32
            Width = 425
            Height = 16
            Alignment = taCenter
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Transparent = False
          end
          object lblProgressBar: TLabel
            Left = 136
            Top = 16
            Width = 31
            Height = 13
            AutoSize = False
            Caption = 'lblProgressBar'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object cbADC: TCheckBox
            Left = 8
            Top = 16
            Width = 97
            Height = 17
            Caption = 'ADC'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 0
            OnClick = cbADCClick
          end
          object btnStart: TButton
            Left = 6
            Top = 56
            Width = 427
            Height = 25
            Caption = #1055#1059#1057#1050
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = btnStartClick
          end
          object ProgressBar: TProgressBar
            Left = 8
            Top = 32
            Width = 425
            Height = 17
            Smooth = True
            Step = 1
            TabOrder = 2
            Visible = False
          end
        end
        object GroupBox3: TGroupBox
          Left = 456
          Top = 8
          Width = 473
          Height = 177
          Caption = #1056#1077#1078#1080#1084' '#1080#1079#1084#1077#1088#1077#1085#1080#1103':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          object Label1: TLabel
            Left = 8
            Top = 32
            Width = 103
            Height = 13
            AutoSize = False
            Caption = #1056#1077#1078#1080#1084' '#1088#1077#1075#1080#1089#1090#1088#1072#1094#1080#1080':'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label2: TLabel
            Left = 8
            Top = 93
            Width = 103
            Height = 13
            AutoSize = False
            Caption = #1056#1077#1078#1080#1084' '#1086#1089#1090#1072#1085#1086#1074#1072':'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object pnlRegMode: TPanel
            Left = 120
            Top = 12
            Width = 345
            Height = 54
            BevelOuter = bvLowered
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            object lblRegMode: TLabel
              Left = 1
              Top = 1
              Width = 343
              Height = 52
              Align = alClient
              Alignment = taCenter
              AutoSize = False
              Caption = 'lblRegMode'
              Layout = tlCenter
              WordWrap = True
            end
          end
          object pnlStopMode: TPanel
            Left = 120
            Top = 72
            Width = 345
            Height = 54
            BevelOuter = bvLowered
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            object lblStopMode: TLabel
              Left = 1
              Top = 1
              Width = 343
              Height = 52
              Align = alClient
              Alignment = taCenter
              AutoSize = False
              Caption = 'Label3'
              Layout = tlCenter
              WordWrap = True
            end
          end
        end
      end
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 225
    Width = 953
    Height = 614
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object PageControl3: TPageControl
      Left = 4
      Top = 4
      Width = 945
      Height = 606
      ActivePage = TabSheet8
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object TabSheet8: TTabSheet
        Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090#1099
        ImageIndex = 2
        object PageControl2: TPageControl
          Left = 0
          Top = 0
          Width = 937
          Height = 578
          ActivePage = TabSheet1
          Align = alClient
          Style = tsButtons
          TabOrder = 0
          object TabSheet5: TTabSheet
            Caption = #1058#1077#1088#1084#1080#1085#1072#1083
            object Terminal: TAdTerminal
              Left = 0
              Top = 0
              Width = 929
              Height = 550
              BorderStyle = bsNone
              CaptureFile = 'APROTERM.CAP'
              Columns = 120
              Rows = 40
              Scrollback = True
              UseLazyDisplay = False
              Align = alClient
              Color = clBlack
              Emulator = AdVT100Emulator
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clSilver
              Font.Height = -13
              Font.Name = 'Lucida Console'
              Font.Style = []
              ParentColor = False
              ParentFont = False
              TabOrder = 0
            end
          end
          object TabSheet6: TTabSheet
            Caption = #1057#1087#1080#1089#1086#1082
            ImageIndex = 1
            object ListBox1: TListBox
              Left = 0
              Top = 0
              Width = 929
              Height = 550
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
          end
          object TabSheet7: TTabSheet
            Caption = #1057#1090#1088#1080#1085#1075#1043#1088#1080#1076
            ImageIndex = 2
            object Grid: TStringGrid
              Left = 0
              Top = 0
              Width = 929
              Height = 550
              Align = alClient
              DefaultRowHeight = 18
              FixedCols = 0
              TabOrder = 0
            end
          end
          object TabSheet1: TTabSheet
            Caption = 'NextGrid'
            ImageIndex = 3
            object Panel1: TPanel
              Left = 0
              Top = 0
              Width = 929
              Height = 41
              Align = alTop
              BevelOuter = bvLowered
              TabOrder = 0
              object Label3: TLabel
                Left = 8
                Top = 14
                Width = 109
                Height = 13
                Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1082#1086#1083#1086#1085#1086#1082':'
              end
              object Label4: TLabel
                Left = 288
                Top = 14
                Width = 82
                Height = 13
                Caption = #1056#1072#1079#1084#1077#1088' '#1096#1088#1080#1092#1090#1072':'
              end
              object edtColNumber: TNxSpinEdit
                Left = 128
                Top = 10
                Width = 41
                Height = 21
                TabOrder = 0
                Text = '2'
                OnChange = edtColNumberChange
                Max = 10.000000000000000000
                Min = 1.000000000000000000
                Value = 2.000000000000000000
                Increment = 1.000000000000000000
              end
              object Button1: TButton
                Left = 200
                Top = 8
                Width = 75
                Height = 25
                Caption = 'Button1'
                TabOrder = 1
                OnClick = Button1Click
              end
              object edtGridFontSize: TNxSpinEdit
                Left = 376
                Top = 10
                Width = 41
                Height = 21
                TabOrder = 2
                Text = '2'
                OnChange = edtGridFontSizeChange
                Max = 12.000000000000000000
                Min = 1.000000000000000000
                Value = 2.000000000000000000
                Increment = 1.000000000000000000
              end
            end
            object Panel2: TPanel
              Left = 0
              Top = 41
              Width = 929
              Height = 506
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 1
              object nxGrid: TNextGrid
                Left = 4
                Top = 4
                Width = 921
                Height = 498
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -13
                Font.Name = 'Tahoma'
                Font.Style = []
                GridLinesColor = clMoneyGreen
                GridLinesStyle = lsActiveRows
                HeaderSize = 24
                Options = [goGrid, goHeader]
                ReadOnly = True
                ParentFont = False
                TabOrder = 0
                TabStop = True
                OnCellColoring = nxGridCellColoring
              end
            end
          end
        end
      end
      object tsParams: TTabSheet
        Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1088#1072#1079#1073#1088#1072#1082#1086#1074#1082#1080
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 2
        ParentFont = False
        object grpStopMode: TGroupBox
          Left = 456
          Top = 240
          Width = 457
          Height = 145
          Caption = #1058#1072#1073#1083'. 4.4. '#1056#1077#1078#1080#1084' '#1086#1089#1090#1072#1085#1086#1074#1072':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object rbStopMode0: TRadioButton
            Left = 8
            Top = 24
            Width = 441
            Height = 17
            Caption = '0 - '#1048#1089#1087#1086#1083#1085#1077#1085#1080#1077' '#1048#1055' '#1073#1077#1079' '#1086#1089#1090#1072#1085#1086#1074#1086#1074
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            TabStop = True
            OnClick = rbStopModeClick
          end
          object rbStopMode1: TRadioButton
            Tag = 1
            Left = 8
            Top = 45
            Width = 441
            Height = 17
            Caption = '1 - '#1054#1089#1090#1072#1085#1086#1074' '#1087#1086#1089#1083#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1082#1072#1078#1076#1086#1075#1086' '#1090#1077#1089#1090#1072' '#1048#1055
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = rbStopModeClick
          end
          object rbStopMode2: TRadioButton
            Tag = 2
            Left = 8
            Top = 66
            Width = 441
            Height = 17
            Caption = 
              '2 - '#1054#1089#1090#1072#1085#1086#1074' '#1087#1086#1089#1083#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1082#1072#1078#1076#1086#1075#1086' '#1090#1077#1089#1090#1072' '#1089' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1084' '#1089#1088#1072#1074#1085#1077#1085 +
              #1080#1103' '#1041#1056#1040#1050
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = rbStopModeClick
          end
          object rbStopMode3: TRadioButton
            Tag = 3
            Left = 8
            Top = 87
            Width = 441
            Height = 17
            Caption = 
              '3 - '#1054#1089#1090#1072#1085#1086#1074' '#1087#1086#1089#1083#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1082#1072#1078#1076#1086#1075#1086' '#1090#1077#1089#1090#1072' '#1089' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1084' '#1089#1088#1072#1074#1085#1077#1085 +
              #1080#1103' '#1043#1054#1044#1045#1053
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = rbStopModeClick
          end
          object rbStopMode4: TRadioButton
            Tag = 4
            Left = 8
            Top = 108
            Width = 441
            Height = 29
            Caption = 
              '4 - '#1054#1089#1090#1072#1085#1086#1074' '#1087#1086#1089#1083#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1082#1072#1078#1076#1086#1075#1086' '#1090#1077#1089#1090#1072', '#1074' '#1084#1086#1084#1077#1085#1090' '#1089#1088#1072#1074#1085#1077#1085#1080#1103', ' +
              #1087#1086' '#1087#1088#1086#1075#1088#1072#1084#1084#1077' '#1082#1086#1090#1086#1088#1086#1075#1086' '#1080#1084#1077#1083#1080#1089#1100' '#1087#1088#1080#1079#1085#1072#1082#1080' '#1043#1045#1053#1045#1056#1040#1062#1048#1071' '#1080#1083#1080' '#1055#1045#1056#1045#1043#1056#1059#1047#1050#1040
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            WordWrap = True
            OnClick = rbStopModeClick
          end
        end
        object grpRegMode: TGroupBox
          Left = 456
          Top = 8
          Width = 457
          Height = 225
          Caption = #1058#1072#1073#1083'. 4.3. '#1056#1077#1078#1080#1084' '#1088#1077#1075#1080#1089#1090#1088#1072#1094#1080#1080':'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          object rbRegMode0: TRadioButton
            Left = 8
            Top = 24
            Width = 441
            Height = 25
            Caption = 
              '0 - '#1041#1077#1079' '#1087#1077#1095#1072#1090#1080' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1080#1079#1084#1077#1088#1077#1085#1080#1103' ('#1089#1088#1072#1074#1085#1077#1085#1080#1103') '#1087#1086' '#1090#1077#1089#1090#1072#1084'. '#1054#1082#1086#1085 +
              #1095#1072#1085#1080#1077' '#1080#1089#1087#1099#1090#1072#1085#1080#1081' '#1087#1088#1080' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1077' '#1089#1088#1072#1074#1085#1077#1085#1080#1103' '#1087#1086' '#1087#1088#1086#1075#1088#1072#1084#1084#1077' '#1080#1089#1087#1086#1083#1085#1103#1077#1084#1086 +
              #1075#1086' '#1090#1077#1089#1090#1072' '#1041#1056#1040#1050
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            WordWrap = True
            OnClick = rbRegModeClick
          end
          object rbRegMode1: TRadioButton
            Tag = 1
            Left = 8
            Top = 52
            Width = 441
            Height = 21
            Caption = '1 - '#1055#1077#1095#1072#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1080#1079#1084#1077#1088#1077#1085#1080#1081' '#1087#1086' '#1082#1072#1078#1076#1086#1084#1091' '#1090#1077#1089#1090#1091' '#1048#1055
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            TabStop = True
            WordWrap = True
            OnClick = rbRegModeClick
          end
          object rbRegMode2: TRadioButton
            Tag = 2
            Left = 8
            Top = 77
            Width = 441
            Height = 21
            Caption = 
              '2 - '#1055#1077#1095#1072#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1080#1079#1084#1077#1088#1077#1085#1080#1081' '#1087#1086' '#1090#1077#1089#1090#1072#1084' '#1089' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1084#1080' '#1089#1088#1072#1074#1085#1077 +
              #1085#1080#1103' '#1041#1056#1040#1050
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            WordWrap = True
            OnClick = rbRegModeClick
          end
          object rbRegMode3: TRadioButton
            Tag = 3
            Left = 8
            Top = 102
            Width = 441
            Height = 39
            Caption = 
              '3 - '#1055#1077#1095#1072#1090#1100' '#1079#1085#1072#1082#1072' "'#1061'", '#1077#1089#1083#1080' '#1090#1077#1089#1090' '#1093#1072#1088#1072#1082#1090#1077#1088#1080#1089#1090#1080#1095#1077#1089#1082#1080#1081' '#1080#1083#1080' '#1087#1077#1095#1072#1090#1100' '#1088#1077 +
              #1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1089#1088#1072#1074#1085#1077#1080#1103' '#1087#1086' '#1082#1072#1078#1076#1086#1084#1091' '#1090#1077#1089#1090#1091' ("." - '#1043#1054#1044#1045#1053', "'#1053'" - '#1041#1056#1040#1050', "!' +
              '" - '#1043#1054#1044#1045#1053' '#1080' '#1043#1045#1053#1045#1056#1040#1062#1048#1071' '#1080#1083#1080' '#1055#1045#1056#1045#1043#1056#1059#1047#1050#1040', ";" - '#1041#1056#1040#1050' '#1080' '#1043#1045#1053#1045#1056#1040#1062#1048#1071' '#1080#1083#1080 +
              ' '#1055#1045#1056#1045#1043#1056#1059#1047#1050#1040')'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            WordWrap = True
            OnClick = rbRegModeClick
          end
          object rbRegMode4: TRadioButton
            Tag = 4
            Left = 8
            Top = 147
            Width = 441
            Height = 39
            Caption = 
              '4 - '#1048#1089#1087#1086#1083#1085#1077#1085#1080#1077' '#1048#1055' '#1073#1077#1079' '#1087#1077#1095#1072#1090#1080' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074' '#1080#1079#1084#1077#1088#1077#1085#1080#1103' ('#1089#1088#1072#1074#1085#1077#1085#1080#1103') '#1087 +
              #1086' '#1090#1077#1089#1090#1072#1084' '#1080' '#1073#1077#1079' '#1086#1082#1086#1085#1095#1072#1085#1080#1103' '#1080#1089#1087#1099#1090#1072#1085#1080#1081' '#1087#1088#1080' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1077' '#1089#1088#1072#1074#1085#1077#1085#1080#1103' '#1087#1086' '#1087 +
              #1088#1086#1075#1088#1072#1084#1084#1077' '#1080#1089#1087#1086#1083#1085#1103#1077#1084#1086#1075#1086' '#1090#1077#1089#1090#1072' '#1041#1056#1040#1050
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            WordWrap = True
            OnClick = rbRegModeClick
          end
          object rbRegMode5: TRadioButton
            Tag = 5
            Left = 8
            Top = 192
            Width = 441
            Height = 25
            Caption = 
              '5 - '#1055#1077#1095#1072#1090#1100' '#1074' '#1082#1086#1085#1094#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1048#1055' '#1089#1087#1080#1089#1082#1072' '#1085#1086#1084#1077#1088#1086#1074' '#1090#1077#1089#1090#1086#1074' '#1089' '#1088#1077#1079#1091#1083#1100#1090 +
              #1072#1090#1072#1084#1080' '#1089#1088#1072#1074#1085#1077#1085#1080#1103' '#1041#1056#1040#1050
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 5
            WordWrap = True
            OnClick = rbRegModeClick
          end
        end
        object grpRW: TGroupBox
          Left = 8
          Top = 72
          Width = 441
          Height = 313
          Caption = #1058#1072#1073#1083'. 5.1. RW:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object chkRW0: TCheckBox
            Left = 8
            Top = 48
            Width = 425
            Height = 17
            Caption = '0 - '#1064#1072#1075#1086#1074#1099#1081' '#1088#1077#1078#1080#1084' '#1087#1088#1086#1075#1088#1072#1084#1084#1080#1088#1086#1074#1072#1085#1080#1103' '#1089#1090#1086#1081#1082#1080
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = chkRWClick
          end
          object chkRW1: TCheckBox
            Tag = 1
            Left = 8
            Top = 64
            Width = 425
            Height = 17
            Caption = '1 - '#1062#1080#1082#1083#1080#1088#1086#1074#1072#1085#1080#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1090#1077#1082#1091#1097#1077#1075#1086' '#1090#1077#1089#1090#1072' '#1048#1055
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = chkRWClick
          end
          object chkRW2: TCheckBox
            Tag = 2
            Left = 8
            Top = 80
            Width = 425
            Height = 17
            Caption = '2 - '#1062#1080#1082#1083#1080#1088#1086#1074#1072#1085#1080#1077' '#1080#1089#1087#1086#1083#1085#1077#1085#1080#1103' '#1073#1083#1086#1082#1072' '#1090#1077#1089#1090#1086#1074' '#1048#1055
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = chkRWClick
          end
          object chkRW3: TCheckBox
            Tag = 3
            Left = 8
            Top = 96
            Width = 425
            Height = 17
            Caption = '3 - '#1054#1089#1090#1072#1085#1086#1074' '#1069#1042#1052' '#1087#1077#1088#1077#1076' '#1079#1072#1087#1088#1086#1089#1086#1084' '#1088#1072#1073#1086#1090#1099' '#1089' '#1087#1091#1083#1100#1090#1086#1074' '#1086#1087#1077#1088#1072#1090#1086#1088#1086#1074
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = chkRWClick
          end
          object chkRW4: TCheckBox
            Tag = 4
            Left = 8
            Top = 112
            Width = 425
            Height = 34
            Caption = 
              '4 - '#1062#1080#1082#1083#1080#1088#1086#1074#1072#1085#1080#1077' '#1087#1088#1086#1094#1077#1089#1089#1072' '#1086#1073#1084#1077#1085#1072' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1077#1081' '#1084#1077#1078#1076#1091' '#1089#1090#1086#1081#1082#1086#1081' '#1080' '#1069#1042#1052 +
              ' '#1087#1088#1080' '#1087#1088#1086#1075#1088#1072#1084#1084#1080#1088#1086#1074#1072#1085#1080#1080' '#1089#1090#1086#1081#1082#1080' '#1089' '#1082#1083#1072#1074#1080#1072#1090#1091#1088#1099' '#1076#1080#1089#1087#1083#1077#1103
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            WordWrap = True
            OnClick = chkRWClick
          end
          object chkRW5: TCheckBox
            Tag = 5
            Left = 8
            Top = 144
            Width = 425
            Height = 34
            Caption = 
              '5 - '#1041#1083#1086#1082#1080#1088#1086#1074#1082#1072' '#1080#1079#1084#1077#1088#1077#1085#1080#1103' '#1080' '#1088#1072#1089#1087#1077#1095#1072#1090#1082#1080' '#1092#1091#1085#1082#1094#1080#1086#1085#1072#1083#1100#1085#1086#1081' '#1079#1072#1074#1080#1089#1080#1084#1086#1089#1090#1080 +
              ' '#1087#1088#1086' '#1087#1088#1086#1075#1088#1072#1084#1084#1077' '#1092#1091#1085#1082#1094#1080#1086#1085#1072#1083#1100#1085#1086#1075#1086' '#1090#1077#1089#1090#1072
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 5
            WordWrap = True
            OnClick = chkRWClick
          end
          object chkRW6: TCheckBox
            Tag = 6
            Left = 8
            Top = 176
            Width = 425
            Height = 17
            Caption = '6 - '#1047#1072#1087#1088#1077#1090' '#1072#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1086#1075#1086' '#1087#1086#1076#1073#1086#1088#1072' '#1096#1082#1072#1083#1099' '#1087#1088#1080' '#1080#1079#1084#1077#1088#1077#1085#1080#1080
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 6
            OnClick = chkRWClick
          end
          object chkRW8: TCheckBox
            Tag = 8
            Left = 8
            Top = 208
            Width = 425
            Height = 17
            Caption = '8 - '#1059#1095#1077#1090' '#1085#1072#1083#1080#1095#1080#1103' '#1074' '#1084#1086#1084#1077#1085#1090' '#1089#1088#1072#1074#1085#1077#1085#1080#1103' '#1089#1080#1075#1085#1072#1083#1072' "'#1043#1045#1053#1045#1056#1040#1062#1048#1071'"'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 8
            OnClick = chkRWClick
          end
          object chkRW9: TCheckBox
            Tag = 9
            Left = 8
            Top = 224
            Width = 425
            Height = 17
            Caption = '9 - '#1059#1095#1077#1090' '#1085#1072#1083#1080#1095#1080#1103' '#1074' '#1084#1086#1084#1077#1085#1090' '#1089#1088#1072#1074#1085#1077#1085#1080#1103' '#1089#1080#1075#1085#1072#1083#1072' "'#1055#1045#1056#1045#1043#1056#1059#1047#1050#1040' -"'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 9
            OnClick = chkRWClick
          end
          object chkRW10: TCheckBox
            Tag = 10
            Left = 8
            Top = 240
            Width = 425
            Height = 17
            Caption = '10 - '#1059#1095#1077#1090' '#1085#1072#1083#1080#1095#1080#1103' '#1074' '#1084#1086#1084#1077#1085#1090' '#1089#1088#1072#1074#1085#1077#1085#1080#1103' '#1089#1080#1075#1085#1072#1083#1072' "'#1055#1045#1056#1045#1043#1056#1059#1047#1050#1040' +"'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 10
            OnClick = chkRWClick
          end
          object chkRW11: TCheckBox
            Tag = 11
            Left = 8
            Top = 256
            Width = 425
            Height = 34
            Caption = 
              '11 - '#1062#1080#1082#1083#1080#1088#1086#1074#1072#1085#1080#1077' '#1074#1082#1083#1102#1095#1077#1085#1080#1103' '#1080' '#1074#1099#1082#1083#1102#1095#1077#1085#1080#1103' '#1074#1077#1085#1090#1080#1083#1077#1081' '#1087#1088#1086#1075#1088#1072#1084#1084#1080#1088#1091#1077#1084#1099 +
              #1093' '#1080#1089#1090#1086#1095#1085#1080#1082#1086#1074' '#1089' '#1080#1079#1084#1077#1088#1077#1085#1080#1077#1084
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 11
            WordWrap = True
            OnClick = chkRWClick
          end
          object chkRW12: TCheckBox
            Tag = 12
            Left = 8
            Top = 288
            Width = 425
            Height = 17
            Caption = 
              '12 - '#1062#1080#1082#1083#1080#1088#1086#1074#1072#1085#1080#1077' '#1074#1082#1083#1102#1095#1077#1085#1080#1103' '#1080' '#1074#1099#1082#1083#1102#1095#1077#1085#1080#1103' '#1074#1077#1085#1090#1080#1083#1077#1081' '#1087#1077#1088#1077#1076' '#1080#1079#1084#1077#1088#1077#1085#1080 +
              #1077#1084
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 12
            WordWrap = True
            OnClick = chkRWClick
          end
          object chkRW7: TCheckBox
            Tag = 7
            Left = 8
            Top = 192
            Width = 425
            Height = 17
            Caption = '7 - '
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 7
            OnClick = chkRWClick
          end
          object pnlRW: TPanel
            Left = 8
            Top = 16
            Width = 425
            Height = 25
            BevelOuter = bvLowered
            Caption = 'pnlRW'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 13
          end
        end
      end
    end
  end
  object tmrPUSK: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrPUSKTimer
    Left = 885
    Top = 782
  end
  object AdVT100Emulator: TAdVT100Emulator
    Terminal = Terminal
    Answerback = 'APROterm'
    DisplayUpperASCII = True
    Left = 885
    Top = 742
  end
end
