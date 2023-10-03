object FormX12translator: TFormX12translator
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'X12TranslatorDemo'
  ClientHeight = 429
  ClientWidth = 878
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 825
    Height = 13
    Caption = 
      'This demo shows how to convert X12 data to XML and vice versa. T' +
      'o begin specify X12 data from file or string. Optionally specify' +
      ' an X12 schema before converting to XML. '
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
    Width = 263
    Height = 13
    Caption = 'To translate from XML to X12 a schema is not required.'
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
    Caption = 'X12 Data'
    TabOrder = 12
    object Label3: TLabel
      Left = 16
      Top = 70
      Width = 130
      Height = 13
      Caption = 'X12 Schema File (optional):'
    end
    object btnX12File: TRadioButton
      Left = 16
      Top = 18
      Width = 113
      Height = 17
      Caption = 'X12 File:'
      TabOrder = 0
    end
    object btnX12String: TRadioButton
      Left = 16
      Top = 43
      Width = 113
      Height = 17
      Caption = 'X12 String:'
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
  object btnSelectX12File: TButton
    Left = 375
    Top = 52
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = btnSelectX12FileClick
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
  object btnToX12: TButton
    Left = 412
    Top = 167
    Width = 58
    Height = 25
    Caption = '<- To X12'
    TabOrder = 7
    OnClick = btnToX12Click
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
  object chkX12Overwrite: TCheckBox
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
  object tMemoX12String: TMemo
    Left = 24
    Top = 136
    Width = 369
    Height = 257
    Lines.Strings = (
      
        'ISA*00*          *00*          *ZZ*ACME           *ZZ*WAYNE_TECH' +
        '     '
      '*160707*1544*U*00401*000000006*0*T*>~'
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
  object txtX12File: TEdit
    Left = 120
    Top = 54
    Width = 249
    Height = 21
    TabOrder = 0
    Text = '..\..\x12.txt'
  end
  object txtSchema: TEdit
    Left = 185
    Top = 104
    Width = 184
    Height = 21
    TabOrder = 4
    Text = '..\..\RSSBus_00401_810.json'
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
  object editranslatorX12Translator1: TeditranslatorX12Translator
    Left = 424
    Top = 216
  end
end


