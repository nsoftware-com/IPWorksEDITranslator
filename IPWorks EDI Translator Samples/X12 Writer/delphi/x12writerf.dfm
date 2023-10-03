object FormX12writer: TFormX12writer
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'X12Writer Demo'
  ClientHeight = 413
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 754
    Height = 26
    Caption = 
      'This demo shows how to use X12Writer to create an X12 document. ' +
      'Several X12 documents can be created with this demo. Please see ' +
      'the code for details.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 120
    Width = 31
    Height = 13
    Caption = 'Status'
  end
  object Label5: TLabel
    Left = 399
    Top = 120
    Width = 72
    Height = 13
    Caption = 'X12 Document:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 776
    Height = 74
    Caption = 'Input X12 Data'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 29
      Width = 52
      Height = 13
      Caption = 'Document:'
    end
    object cbFileType: TComboBox
      Left = 74
      Top = 29
      Width = 113
      Height = 21
      TabOrder = 0
      Items.Strings = (
        '810'
        '850'
        '855'
        '856')
    end
    object btnWriteX12: TButton
      Left = 193
      Top = 29
      Width = 81
      Height = 25
      Caption = '&Generate X12'
      TabOrder = 1
      OnClick = btnWriteX12Click
    end
  end
  object txtLog: TMemo
    Left = 8
    Top = 136
    Width = 385
    Height = 242
    HelpType = htKeyword
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object txtFile: TMemo
    Left = 399
    Top = 136
    Width = 385
    Height = 242
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnSave: TButton
    Left = 704
    Top = 384
    Width = 81
    Height = 25
    Caption = '&Save...'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 688
    Top = 40
  end
  object editranslatorX12Writer1: TeditranslatorX12Writer
    Left = 752
    Top = 48
  end
end


