object FormEdifactwriter: TFormEdifactwriter
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'EDIFACTWriter Demo'
  ClientHeight = 413
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 741
    Height = 26
    Caption = 
      'This demo shows how to use EDIFACTWriter to create an EDI docume' +
      'nt. Several example X12 and EDIFACT demos can be created with th' +
      'is demo. Please see the code for details.'
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
    Caption = 'EDI Document:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 776
    Height = 65
    Caption = 'Input EDI Data'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 32
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
      Text = 'DESADV'
      Items.Strings = (
        'DESADV'
        'INVOIC'
        'ORDERS'
        'ORDRSP')
    end
    object btnWriteEDI: TButton
      Left = 193
      Top = 26
      Width = 81
      Height = 25
      Caption = '&Generate EDI'
      TabOrder = 1
      OnClick = btnWriteEDIClick
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
    Left = 80
    Top = 168
  end
  object editranslatorEDIFACTWriter1: TeditranslatorEDIFACTWriter
    Left = 536
    Top = 224
  end
end


