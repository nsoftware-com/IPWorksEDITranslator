object FormEdifactparser: TFormEdifactparser
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'EDIFACT Parser Demo'
  ClientHeight = 652
  ClientWidth = 791
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
    Width = 629
    Height = 13
    Caption = 
      'This demo shows how to parse EDI data via the events or by using' +
      ' the Select method.  Select from either file input or string inp' +
      'ut.  '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 317
    Width = 66
    Height = 13
    Caption = 'Event Results'
  end
  object Label3: TLabel
    Left = 419
    Top = 317
    Width = 77
    Height = 13
    Caption = 'Select Traversal'
  end
  object Label4: TLabel
    Left = 429
    Top = 559
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object Label5: TLabel
    Left = 417
    Top = 586
    Width = 42
    Height = 13
    Caption = 'Element:'
  end
  object Label6: TLabel
    Left = 429
    Top = 613
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object Label7: TLabel
    Left = 437
    Top = 532
    Width = 22
    Height = 13
    Caption = 'Tag:'
  end
  object Label8: TLabel
    Left = 413
    Top = 505
    Width = 46
    Height = 13
    Caption = 'Segment:'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 27
    Width = 765
    Height = 213
    Caption = 'Input EDI Data'
    TabOrder = 0
  end
  object rbtnFile: TRadioButton
    Left = 16
    Top = 46
    Width = 113
    Height = 17
    Caption = 'Input from File'
    TabOrder = 1
  end
  object rbtnString: TRadioButton
    Left = 16
    Top = 69
    Width = 113
    Height = 17
    Caption = 'Input from String'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object txtFile: TEdit
    Left = 135
    Top = 42
    Width = 589
    Height = 21
    TabOrder = 3
    Text = '..\..\INVOIC.edi'
  end
  object memoString: TMemo
    Left = 16
    Top = 94
    Width = 745
    Height = 108
    Lines.Strings = (
      'UNA:+.?*'#39
      'UNB+UNOB:1+WAYNE_TECH+ACME+160707:1547+000000002++1234++++1'#39
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
    TabOrder = 4
  end
  object btnParse: TButton
    Left = 16
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 5
    OnClick = btnParseClick
  end
  object btnSelectFile: TButton
    Left = 730
    Top = 42
    Width = 31
    Height = 25
    Caption = '...'
    TabOrder = 6
    OnClick = btnSelectFileClick
  end
  object memoEvents: TMemo
    Left = 8
    Top = 336
    Width = 393
    Height = 298
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object txtValue: TEdit
    Left = 465
    Top = 559
    Width = 308
    Height = 21
    TabOrder = 8
  end
  object txtElement: TEdit
    Left = 465
    Top = 586
    Width = 308
    Height = 21
    TabOrder = 9
  end
  object txtElementValue: TEdit
    Left = 465
    Top = 613
    Width = 308
    Height = 21
    TabOrder = 10
  end
  object tvwXPath: TTreeView
    Left = 419
    Top = 336
    Width = 354
    Height = 163
    Indent = 19
    TabOrder = 11
    OnClick = tvwXPathClick
  end
  object txtTag: TEdit
    Left = 465
    Top = 532
    Width = 308
    Height = 21
    TabOrder = 12
  end
  object txtSegment: TEdit
    Left = 465
    Top = 505
    Width = 308
    Height = 21
    TabOrder = 13
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 246
    Width = 765
    Height = 65
    Caption = 'Input Schema'
    TabOrder = 14
    object Label10: TLabel
      Left = 23
      Top = 32
      Width = 60
      Height = 13
      Caption = 'Schema File:'
    end
    object txtSchema: TEdit
      Left = 89
      Top = 27
      Width = 627
      Height = 21
      TabOrder = 0
      Text = '..\..\RSSBus_D97A_INVOIC.json'
    end
    object btnSelectSchema: TButton
      Left = 722
      Top = 23
      Width = 31
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectSchemaClick
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 608
    Top = 200
  end
  object OpenDialog2: TOpenDialog
    Left = 648
    Top = 200
  end
  object editranslatorEDIFACTReader1: TeditranslatorEDIFACTReader
    OnEndFunctionalGroup = editranslatorEDIFACTReader1EndFunctionalGroup
    OnEndInterchange = editranslatorEDIFACTReader1EndInterchange
    OnEndLoop = editranslatorEDIFACTReader1EndLoop
    OnEndTransaction = editranslatorEDIFACTReader1EndTransaction
    OnError = editranslatorEDIFACTReader1Error
    OnResolveSchema = editranslatorEDIFACTReader1ResolveSchema
    OnSegment = editranslatorEDIFACTReader1Segment
    OnStartFunctionalGroup = editranslatorEDIFACTReader1StartFunctionalGroup
    OnStartInterchange = editranslatorEDIFACTReader1StartInterchange
    OnStartLoop = editranslatorEDIFACTReader1StartLoop
    OnStartTransaction = editranslatorEDIFACTReader1StartTransaction
    OnWarning = editranslatorEDIFACTReader1Warning
    Left = 320
    Top = 208
  end
  object editranslatorEDIFACTWriter1: TeditranslatorEDIFACTWriter
    Left = 360
    Top = 208
  end
end


