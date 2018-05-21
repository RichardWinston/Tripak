object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 344
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 0
    Top = 225
    Width = 535
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 169
    ExplicitWidth = 175
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 535
    Height = 81
    Align = alTop
    TabOrder = 0
    object btnTripackFortran: TButton
      Left = 32
      Top = 10
      Width = 115
      Height = 25
      Caption = 'Tripack Fortran'
      TabOrder = 0
      OnClick = btnTripackFortranClick
    end
    object btnTripackPascal: TButton
      Left = 189
      Top = 10
      Width = 97
      Height = 25
      Caption = 'Tripack Pascal'
      TabOrder = 1
      OnClick = btnTripackPascalClick
    end
    object btnTripackTest: TButton
      Left = 415
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Tripack Test'
      TabOrder = 2
      OnClick = btnTripackTestClick
    end
    object btnSfrpackFortran: TButton
      Left = 32
      Top = 41
      Width = 115
      Height = 25
      Caption = 'Sfrpack Fortran'
      TabOrder = 3
      OnClick = btnSfrpackFortranClick
    end
    object btnSfrpackPascal: TButton
      Left = 189
      Top = 41
      Width = 97
      Height = 25
      Caption = 'Sfrpack Pascal'
      TabOrder = 4
      OnClick = btnSfrpackPascalClick
    end
    object btnSfrpackTest: TButton
      Left = 311
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Sfrpack Test'
      TabOrder = 5
      OnClick = btnSfrpackTestClick
    end
    object sfrButton2: TButton
      Left = 400
      Top = 41
      Width = 75
      Height = 25
      Caption = 'sfr test 2'
      TabOrder = 6
      OnClick = sfrButton2Click
    end
    object btn1: TButton
      Left = 292
      Top = 10
      Width = 97
      Height = 25
      Caption = 'Modified Tripack Pascal'
      TabOrder = 7
      WordWrap = True
      OnClick = btn1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 81
    Width = 535
    Height = 144
    Align = alTop
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 281
      Top = 1
      Width = 5
      Height = 142
    end
    object memoFortran: TMemo
      Left = 1
      Top = 1
      Width = 280
      Height = 142
      Align = alLeft
      TabOrder = 0
      WordWrap = False
    end
    object memoPascal: TMemo
      Left = 286
      Top = 1
      Width = 248
      Height = 142
      Align = alClient
      TabOrder = 1
      WordWrap = False
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 228
    Width = 535
    Height = 116
    Align = alClient
    TabOrder = 2
    object Splitter3: TSplitter
      Left = 281
      Top = 1
      Width = 5
      Height = 114
      ExplicitHeight = 154
    end
    object memoFortranEPS: TMemo
      Left = 1
      Top = 1
      Width = 280
      Height = 114
      Align = alLeft
      TabOrder = 0
      WordWrap = False
    end
    object memoPascalEPS: TMemo
      Left = 286
      Top = 1
      Width = 248
      Height = 114
      Align = alClient
      TabOrder = 1
      WordWrap = False
    end
  end
end
