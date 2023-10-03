(*
 * IPWorks EDI Translator 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks EDI Translator in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworkseditranslator
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit edifactwriterf;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  editranslatorcore, editranslatortypes, editranslatoredifactwriter;

type
    TFormEdifactwriter = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cbFileType: TComboBox;
    btnWriteEDI: TButton;
    Label4: TLabel;
    txtLog: TMemo;
    Label5: TLabel;
    txtFile: TMemo;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    editranslatorEDIFACTWriter1: TeditranslatorEDIFACTWriter;
    procedure btnWriteEDIClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure writeFile();
    procedure writeFile_EDIFACT_DESADV();
    procedure writeFile_EDIFACT_INVOIC();
    procedure writeFile_EDIFACT_ORDERS();
    procedure writeFile_EDIFACT_ORDRSP();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEdifactwriter: TFormEdifactwriter;

implementation

{$R *.dfm}

procedure TFormEdifactwriter.btnSaveClick(Sender: TObject);
var
dir : AnsiString;
begin
  SaveDialog1.Filter := 'All Files (*.*)|*.*';
  dir:=GetCurrentDir;
  if (SaveDialog1.Execute() = True) then
    // write to file
    editranslatorEDIFACTWriter1.OutputFile := SaveDialog1.FileName;
    editranslatorEDIFACTWriter1.FileWriteMode := TeditranslatorEDIFACTWriterFileWriteModes(fwmOverwrite);
  writeFile();
  editranslatorEDIFACTWriter1.OutputFile := '';
  SetCurrentDir(dir) ;
end;

procedure TFormEdifactwriter.writeFile();
begin
  txtLog.Text := '';

  if (cbFileType.Text = 'DESADV') then
  begin
    writeFile_EDIFACT_DESADV();
  end
  else if (cbFileType.Text = 'INVOIC') then
  begin
    writeFile_EDIFACT_INVOIC();
  end
  else if (cbFileType.Text = 'ORDERS') then
  begin
    writeFile_EDIFACT_ORDERS();
  end
  else if (cbFileType.Text = 'ORDRSP') then
  begin
    writeFile_EDIFACT_ORDRSP();
  end;

  txtLog.Text := txtLog.Text + #13#10 + #13#10 + #13#10 +
    'Write Complete' + #13#10;

  txtFile.Text := editranslatorEDIFACTWriter1.OutputData;
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_DESADV();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartInterchangeHeader('D97A');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('UNOB');
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ACME');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('160707');
  editranslatorEDIFACTWriter1.WriteComponentString('1547');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('000000001');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('1234');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartTransactionHeader('DESADV');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('DESADV');
  editranslatorEDIFACTWriter1.WriteComponentString('D');
  editranslatorEDIFACTWriter1.WriteComponentString('97 A');
  editranslatorEDIFACTWriter1.WriteComponentString('UN');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('BGM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('351');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2014/10093');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('9');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('137');
  editranslatorEDIFACTWriter1.WriteComponentString('201404192036');
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('201404192036');
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MEA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MEA');
  editranslatorEDIFACTWriter1.WriteElementString('AAX');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('SQ');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('C62');
  editranslatorEDIFACTWriter1.WriteComponentString('17');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  editranslatorEDIFACTWriter1.WriteElementString('ST');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0018');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  editranslatorEDIFACTWriter1.WriteElementString('SU');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2019813');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TDTLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TDT' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TDTLoop1/TDT');
  editranslatorEDIFACTWriter1.WriteElementString('12');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('M');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('CARRIER');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('86');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: EQDLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: EQD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('EQDLoop1/EQD');
  editranslatorEDIFACTWriter1.WriteElementString('TE');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('X');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CPSLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CPS' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/CPS');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PACLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PAC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/PAC');
  editranslatorEDIFACTWriter1.WriteElementString('4');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('BOX - 001');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('52');
  editranslatorEDIFACTWriter1.WriteComponentString('50');
  editranslatorEDIFACTWriter1.WriteComponentString('C62');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CPSLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CPS' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/CPS');
  editranslatorEDIFACTWriter1.WriteElementString('2');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PACLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PAC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/PAC');
  editranslatorEDIFACTWriter1.WriteElementString('2');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('BOX - 002');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('52');
  editranslatorEDIFACTWriter1.WriteComponentString('100');
  editranslatorEDIFACTWriter1.WriteComponentString('C62');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('9001');
  editranslatorEDIFACTWriter1.WriteComponentString('IN');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CPSLoop1/LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('12');
  editranslatorEDIFACTWriter1.WriteComponentString('400');
  editranslatorEDIFACTWriter1.WriteComponentString('C62');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ON');
  editranslatorEDIFACTWriter1.WriteComponentString('N55109001');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  editranslatorEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  editranslatorEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_INVOIC();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartInterchangeHeader('D97A');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('UNOB');
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ACME');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('160707');
  editranslatorEDIFACTWriter1.WriteComponentString('1547');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('000000002');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('1234');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartTransactionHeader('INVOIC');
  editranslatorEDIFACTWriter1.WriteElementString('509010117');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INVOIC');
  editranslatorEDIFACTWriter1.WriteComponentString('D');
  editranslatorEDIFACTWriter1.WriteComponentString('97A');
  editranslatorEDIFACTWriter1.WriteComponentString('UN');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('BGM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('380');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('TAX INVOICE');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0013550417');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('9');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('3');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('4');
  editranslatorEDIFACTWriter1.WriteComponentString('20061123');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: FTX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('FTX');
  editranslatorEDIFACTWriter1.WriteElementString('AAI');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('10072.14');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CUXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CUX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CUXLoop1/CUX');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('EUR');
  editranslatorEDIFACTWriter1.WriteComponentString('4');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('0.67529');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PATLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PAT' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('PATLoop1/PAT');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('PATLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('10');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PCD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('PATLoop1/PCD');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('0');
  editranslatorEDIFACTWriter1.WriteComponentString('13');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000030');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2265S13');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5029766832002');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('50.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('19150.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('383.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('19150.45');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.45');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.45');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000040');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2269F22');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5051254078241');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('20.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('21060.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('1053.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('21060.50');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.50');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.50');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000170');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2269F10');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5051254078326');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('10.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('6950.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('695.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('6950.16');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.16');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.16');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000190');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2269F26');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5051254051190');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('5.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('2375.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('475.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('2375.06');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.06');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.06');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000200');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2265S24');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5029766000685');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('3.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('957.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('319.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('957.02');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.02');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.02');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000210');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2263T95');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5029766699575');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('3.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('2085.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('695.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('2085.05');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.05');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.05');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('000250');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2269F15');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('5051254080091');
  editranslatorEDIFACTWriter1.WriteComponentString('UP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('47');
  editranslatorEDIFACTWriter1.WriteComponentString('3.000');
  editranslatorEDIFACTWriter1.WriteComponentString('EA');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('11');
  editranslatorEDIFACTWriter1.WriteComponentString('20070926');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('203');
  editranslatorEDIFACTWriter1.WriteComponentString('4977.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INV');
  editranslatorEDIFACTWriter1.WriteComponentString('1659.00');
  editranslatorEDIFACTWriter1.WriteComponentString('TU');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('4977.12');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0.12');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('0.12');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: UNS' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('UNS');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: CNT' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CNT');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('4');
  editranslatorEDIFACTWriter1.WriteComponentString('7');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('9');
  editranslatorEDIFACTWriter1.WriteComponentString('67627.50');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('79');
  editranslatorEDIFACTWriter1.WriteComponentString('57554.00');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  editranslatorEDIFACTWriter1.WriteElementString('7');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VAT');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('17.500');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('125');
  editranslatorEDIFACTWriter1.WriteComponentString('57555.36');
  editranslatorEDIFACTWriter1.WriteComponentString('EUR');
  editranslatorEDIFACTWriter1.WriteComponentString('3');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('124');
  editranslatorEDIFACTWriter1.WriteComponentString('10072.14');
  editranslatorEDIFACTWriter1.WriteComponentString('EUR');
  editranslatorEDIFACTWriter1.WriteComponentString('3');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  editranslatorEDIFACTWriter1.WriteElementString('C');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('1.36');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FC');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('8');
  editranslatorEDIFACTWriter1.WriteComponentString('1.36');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  editranslatorEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  editranslatorEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_ORDERS();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartInterchangeHeader('D97A');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('UNOB');
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ACME');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('160707');
  editranslatorEDIFACTWriter1.WriteComponentString('1547');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('000000003');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('1234');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartTransactionHeader('ORDERS');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ORDERS');
  editranslatorEDIFACTWriter1.WriteComponentString('D');
  editranslatorEDIFACTWriter1.WriteComponentString('97A');
  editranslatorEDIFACTWriter1.WriteComponentString('UN');
  editranslatorEDIFACTWriter1.WriteComponentString('ED17A1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('BGM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('105');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('K12345');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('9');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('137');
  editranslatorEDIFACTWriter1.WriteComponentString('19980626');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: FTX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('FTX');
  editranslatorEDIFACTWriter1.WriteElementString('GEN');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('FREE TEXT');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('PS');
  editranslatorEDIFACTWriter1.WriteComponentString('10501');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.WriteComponentString('NO');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  editranslatorEDIFACTWriter1.WriteElementString('BY');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop2' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/RFFLoop2/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('VA');
  editranslatorEDIFACTWriter1.WriteComponentString('GB107328000');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  editranslatorEDIFACTWriter1.WriteElementString('SE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CUXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CUX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CUXLoop1/CUX');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('GBP');
  editranslatorEDIFACTWriter1.WriteComponentString('9');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('001');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0000057G3454');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('DESCRIPTION');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('21');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INF');
  editranslatorEDIFACTWriter1.WriteComponentString('27.54');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('9829');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('JB');
  editranslatorEDIFACTWriter1.WriteComponentString('JOB NO');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('21');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('19980717');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('002');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0000057G3454');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('DESCRIPTION');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('21');
  editranslatorEDIFACTWriter1.WriteComponentString('4000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INF');
  editranslatorEDIFACTWriter1.WriteComponentString('27.54');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('9830');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('JB');
  editranslatorEDIFACTWriter1.WriteComponentString('JOB NO');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('21');
  editranslatorEDIFACTWriter1.WriteComponentString('4000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('19980724');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('003');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('0000057G3454');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  editranslatorEDIFACTWriter1.WriteElementString('F');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('DESCRIPTION');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('21');
  editranslatorEDIFACTWriter1.WriteComponentString('3000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('INF');
  editranslatorEDIFACTWriter1.WriteComponentString('27.54');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('9831');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('JB');
  editranslatorEDIFACTWriter1.WriteComponentString('JOB NO');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('21');
  editranslatorEDIFACTWriter1.WriteComponentString('3000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('19980731');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: UNS' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('UNS');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  editranslatorEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  editranslatorEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_ORDRSP();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartInterchangeHeader('D97A');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('UNOB');
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ACME');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('160707');
  editranslatorEDIFACTWriter1.WriteComponentString('1547');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('000000004');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('1234');
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.SkipElement();
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  editranslatorEDIFACTWriter1.StartTransactionHeader('ORDRSP');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ORDRSP');
  editranslatorEDIFACTWriter1.WriteComponentString('D');
  editranslatorEDIFACTWriter1.WriteComponentString('97A');
  editranslatorEDIFACTWriter1.WriteComponentString('UN');
  editranslatorEDIFACTWriter1.WriteComponentString('EDOR04');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('BGM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('231');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('1109706372/3');
  editranslatorEDIFACTWriter1.EndElement();
  editranslatorEDIFACTWriter1.WriteElementString('9');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('137');
  editranslatorEDIFACTWriter1.WriteComponentString('20150708');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('ON');
  editranslatorEDIFACTWriter1.WriteComponentString('INCG14040002');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  editranslatorEDIFACTWriter1.WriteElementString('SE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  editranslatorEDIFACTWriter1.WriteElementString('BY');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CUXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CUX' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('CUXLoop1/CUX');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('USD');
  editranslatorEDIFACTWriter1.WriteComponentString('4');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  editranslatorEDIFACTWriter1.WriteComponentString('VP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('91');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('800');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('AAA');
  editranslatorEDIFACTWriter1.WriteComponentString('0.9600');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('800');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20140401');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150729');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('2');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  editranslatorEDIFACTWriter1.WriteComponentString('VP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('91');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('AAA');
  editranslatorEDIFACTWriter1.WriteComponentString('0.9600');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20141020');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150729');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('3');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  editranslatorEDIFACTWriter1.WriteComponentString('VP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('91');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('AAA');
  editranslatorEDIFACTWriter1.WriteComponentString('0.9600');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('3');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20141120');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150809');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('4');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  editranslatorEDIFACTWriter1.WriteComponentString('VP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('91');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('4000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('AAA');
  editranslatorEDIFACTWriter1.WriteComponentString('0.1000');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('4');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('4000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20140605');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150810');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('5');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  editranslatorEDIFACTWriter1.WriteComponentString('VP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('91');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('12000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('AAA');
  editranslatorEDIFACTWriter1.WriteComponentString('0.1000');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('5');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('12000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20140705');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150801');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.WriteElementString('6');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  editranslatorEDIFACTWriter1.WriteComponentString('VP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('91');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  editranslatorEDIFACTWriter1.WriteComponentString('BP');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('92');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('12000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('AAA');
  editranslatorEDIFACTWriter1.WriteComponentString('0.1000');
  editranslatorEDIFACTWriter1.WriteComponentString('CT');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('1');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('LI');
  editranslatorEDIFACTWriter1.SkipComponent();
  editranslatorEDIFACTWriter1.WriteComponentString('6');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('10000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20140805');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150805');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  editranslatorEDIFACTWriter1.WriteElementString('1');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('113');
  editranslatorEDIFACTWriter1.WriteComponentString('2000');
  editranslatorEDIFACTWriter1.WriteComponentString('PCE');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('2');
  editranslatorEDIFACTWriter1.WriteComponentString('20140805');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  editranslatorEDIFACTWriter1.StartElement();
  editranslatorEDIFACTWriter1.WriteComponentString('67');
  editranslatorEDIFACTWriter1.WriteComponentString('20150815');
  editranslatorEDIFACTWriter1.WriteComponentString('102');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: UNS' + #13#10;
  editranslatorEDIFACTWriter1.StartSegment('UNS');
  editranslatorEDIFACTWriter1.WriteElementString('S');
  editranslatorEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  editranslatorEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  editranslatorEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.btnWriteEDIClick(Sender: TObject);
begin
  try

    editranslatorEDIFACTWriter1.Reset();
    txtLog.Text := '';
    txtFile.Text := '';

    editranslatorEDIFACTWriter1.LoadSchema('..\..\RSSBus_D97A_' + cbFileType.Text +
    '.json');

    editranslatorEDIFACTWriter1.Suffix := TeditranslatorEDIFACTWriterSuffixes(3); // suffixCRLF

    editranslatorEDIFACTWriter1.Config('Encoding=iso-8859-1');
    writeFile();

  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
end;


end.



