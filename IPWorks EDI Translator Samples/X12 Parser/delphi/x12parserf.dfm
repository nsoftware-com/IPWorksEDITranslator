object FormX12parser: TFormX12parser
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'X12 Parser Demo'
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
    Text = '..\..\x12.txt'
  end
  object memoString: TMemo
    Left = 16
    Top = 94
    Width = 745
    Height = 108
    Lines.Strings = (
      
        'ISA*00*          *00*          *ZZ*ACME           *ZZ*WAYNE_TECH' +
        '     *160707*1544*U*00401*000000006*0*T*>~'
      'GS*IN*ACME*WAYNE_TECH*20160707*1544*6*T*004010~'
      'ST*810*0001~'
      'BIG*20150708*3003014445**0476553272***DR~'
      'CUR*SE*USD~'
      'REF*8M*0056~'
      'N1*BY*Company*92*544380~'
      'N3*Address~'
      'N4*City*CA*Postal Code~'
      'N1*ST*Name*92*0607047800010~'
      'N3*Address~'
      'N4*City**200131*US~'
      'N1*RE*Name*92*5095956~'
      'N3*Address~'
      'N4*City*IL*Postal Code~'
      'IT1*20*2500*EA*36.96**BP*335S0594~'
      'REF*KK*0099778154~'
      'REF*PO*0476553272*20~'
      'TDS*9240000~'
      'CTT*1~'
      'SE*19*0001~'
      'GE*1*6~'
      'IEA*1*000000006~')
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
      Text = '..\..\RSSBus_00401_810.json'
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
  object editranslatorX12Reader1: TeditranslatorX12Reader
    OnEndFunctionalGroup = x12X12Reader1EndFunctionalGroup
    OnEndInterchange = x12X12Reader1EndInterchange
    OnEndLoop = x12X12Reader1EndLoop
    OnEndTransaction = x12X12Reader1EndTransaction
    OnError = x12X12Reader1Error
    OnResolveSchema = x12X12Reader1ResolveSchema
    OnSegment = x12X12Reader1Segment
    OnStartFunctionalGroup = x12X12Reader1StartFunctionalGroup
    OnStartInterchange = x12X12Reader1StartInterchange
    OnStartLoop = x12X12Reader1StartLoop
    OnStartTransaction = x12X12Reader1StartTransaction
    OnWarning = x12X12Reader1Warning
    Left = 264
    Top = 208
  end
  object editranslatorX12Writer1: TeditranslatorX12Writer
    Left = 304
    Top = 208
  end
end


