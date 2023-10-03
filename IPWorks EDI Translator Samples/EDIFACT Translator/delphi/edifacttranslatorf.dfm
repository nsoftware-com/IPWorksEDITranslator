object FormEdifacttranslator: TFormEdifacttranslator
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'EDIFACTTranslatorDemo'
  ClientHeight = 429
  ClientWidth = 878
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 822
    Height = 13
    Caption = 
      'This demo shows how to convert EDI data to XML and vice versa. T' +
      'o begin specify EDI data from file or string. Optionally specify' +
      ' an EDI schema before converting to XML. '
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 8
    Top = 19
    Width = 262
    Height = 13
    Caption = 'To translate from XML to EDI a schema is not required.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 38
    Width = 398
    Height = 366
    Caption = 'EDI Data'
    TabOrder = 12
    object Label3: TLabel
      Left = 16
      Top = 70
      Width = 155
      Height = 13
      Caption = 'EDIFACT Schema File (optional):'
    end
    object btnEdifactFile: TRadioButton
      Left = 16
      Top = 18
      Width = 113
      Height = 17
      Caption = 'EDIFACT File:'
      TabOrder = 0
    end
    object btnEdifactString: TRadioButton
      Left = 16
      Top = 43
      Width = 113
      Height = 17
      Caption = 'EDIFACT String:'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object GroupBox2: TGroupBox
    Left = 473
    Top = 38
    Width = 397
    Height = 366
    Caption = 'XML'
    TabOrder = 13
    object btnXmlFile: TRadioButton
      Left = 12
      Top = 18
      Width = 113
      Height = 17
      Caption = 'XML File:'
      TabOrder = 0
    end
    object btnXmlString: TRadioButton
      Left = 12
      Top = 43
      Width = 113
      Height = 17
      Caption = 'XML String:'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object btnSelectEdifactFile: TButton
    Left = 375
    Top = 52
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = btnSelectEdifactFileClick
  end
  object btnSelectSchemaFile: TButton
    Left = 375
    Top = 102
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = btnSelectSchemaFileClick
  end
  object btnSelectXmlFile: TButton
    Left = 836
    Top = 52
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 9
    OnClick = btnSelectXmlFileClick
  end
  object btnToEdi: TButton
    Left = 412
    Top = 167
    Width = 58
    Height = 25
    Caption = '<- To EDI'
    TabOrder = 7
    OnClick = btnToEdiClick
  end
  object btnToXml: TButton
    Left = 412
    Top = 136
    Width = 58
    Height = 25
    Caption = 'To XML ->'
    TabOrder = 6
    OnClick = btnToXmlClick
  end
  object chkEdifactOverwrite: TCheckBox
    Left = 304
    Top = 79
    Width = 97
    Height = 17
    Caption = 'Overwrite'
    TabOrder = 3
  end
  object chkXmlOverwrite: TCheckBox
    Left = 773
    Top = 81
    Width = 97
    Height = 17
    Caption = 'Overwrite'
    TabOrder = 10
  end
  object tMemoEdifactString: TMemo
    Left = 24
    Top = 136
    Width = 369
    Height = 257
    Lines.Strings = (
      'UNA:+.?*'#39
      'UNB+UNOB:1+WAYNE_TECH+ACME+160707:1547+000000002+'
      '+1234++++1'#39
      'UNH+509010117+INVOIC:D:97A:UN'#39
      'BGM+380:::TAX INVOICE+0013550417+9'#39
      'DTM+3:20070926:102'#39
      'DTM+4:20061123:102'#39
      'FTX+AAI+1'#39
      'TAX+7+VAT+++:::10072.14+S'#39
      'CUX+2:EUR:4++0.67529'#39
      'PAT+1'#39
      'DTM+10:20070926:102'#39
      'PCD+2:0:13'#39
      'LIN+000030+'#39
      'PIA+1+2265S13:BP::92'#39
      'PIA+1+5029766832002:UP::92'#39
      'IMD+F+'#39
      'QTY+47:50.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:19150.00'#39
      'PRI+INV:383.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:19150.45'#39
      'ALC+C+0.45+++FC'#39
      'MOA+8:0.45'#39
      'LIN+000040+'#39
      'PIA+1+2269F22:BP::92'#39
      'PIA+1+5051254078241:UP::92'#39
      'IMD+F+'#39
      'QTY+47:20.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:21060.00'#39
      'PRI+INV:1053.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:21060.50'#39
      'ALC+C+0.50+++FC'#39
      'MOA+8:0.50'#39
      'LIN+000170+'#39
      'PIA+1+2269F10:BP::92'#39
      'PIA+1+5051254078326:UP::92'#39
      'IMD+F+'#39
      'QTY+47:10.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:6950.00'#39
      'PRI+INV:695.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:6950.16'#39
      'ALC+C+0.16+++FC'#39
      'MOA+8:0.16'#39
      'LIN+000190+'#39
      'PIA+1+2269F26:BP::92'#39
      'PIA+1+5051254051190:UP::92'#39
      'IMD+F+'#39
      'QTY+47:5.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:2375.00'#39
      'PRI+INV:475.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:2375.06'#39
      'ALC+C+0.06+++FC'#39
      'MOA+8:0.06'#39
      'LIN+000200+'#39
      'PIA+1+2265S24:BP::92'#39
      'PIA+1+5029766000685:UP::92'#39
      'IMD+F+'#39
      'QTY+47:3.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:957.00'#39
      'PRI+INV:319.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:957.02'#39
      'ALC+C+0.02+++FC'#39
      'MOA+8:0.02'#39
      'LIN+000210+'#39
      'PIA+1+2263T95:BP::92'#39
      'PIA+1+5029766699575:UP::92'#39
      'IMD+F+'#39
      'QTY+47:3.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:2085.00'#39
      'PRI+INV:695.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:2085.05'#39
      'ALC+C+0.05+++FC'#39
      'MOA+8:0.05'#39
      'LIN+000250+'#39
      'PIA+1+2269F15:BP::92'#39
      'PIA+1+5051254080091:UP::92'#39
      'IMD+F+'#39
      'QTY+47:3.000:EA'#39
      'DTM+11:20070926:102'#39
      'MOA+203:4977.00'#39
      'PRI+INV:1659.00:TU'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:4977.12'#39
      'ALC+C+0.12+++FC'#39
      'MOA+8:0.12'#39
      'UNS+S'#39
      'CNT+4:7'#39
      'MOA+9:67627.50'#39
      'MOA+79:57554.00'#39
      'TAX+7+VAT+++:::17.500+S'#39
      'MOA+125:57555.36:EUR:3'#39
      'MOA+124:10072.14:EUR:3'#39
      'ALC+C+1.36+++FC'#39
      'MOA+8:1.36'#39
      'UNT+104+509010117'#39
      'UNZ+1+000000002'#39)
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object tMemoXmlString: TMemo
    Left = 485
    Top = 104
    Width = 372
    Height = 289
    ScrollBars = ssBoth
    TabOrder = 11
  end
  object txtEdifactFile: TEdit
    Left = 120
    Top = 54
    Width = 249
    Height = 21
    TabOrder = 0
    Text = '..\..\INVOIC.edi'
  end
  object txtSchema: TEdit
    Left = 185
    Top = 104
    Width = 184
    Height = 21
    TabOrder = 4
    Text = '..\..\RSSBus_D97A_INVOIC.json'
  end
  object txtXmlFile: TEdit
    Left = 568
    Top = 54
    Width = 262
    Height = 21
    TabOrder = 8
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 410
    Width = 878
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object OpenDialog1: TOpenDialog
    Left = 408
    Top = 48
  end
  object OpenDialog2: TOpenDialog
    Left = 408
    Top = 96
  end
  object OpenDialog3: TOpenDialog
    Left = 840
    Top = 8
  end
  object editranslatorEDIFACTTranslator1: TeditranslatorEDIFACTTranslator
    Left = 424
    Top = 208
  end
end


