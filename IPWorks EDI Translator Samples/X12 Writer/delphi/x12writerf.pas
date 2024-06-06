(*
 * IPWorks EDI Translator 2024 Delphi Edition - Sample Project
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
unit x12writerf;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, editranslatorcore,
  editranslatortypes, editranslatorx12writer;

type
  TFormX12writer = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cbFileType: TComboBox;
    btnWriteX12: TButton;
    Label4: TLabel;
    txtLog: TMemo;
    Label5: TLabel;
    txtFile: TMemo;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    editranslatorX12Writer1: TeditranslatorX12Writer;
    procedure cbStandardChange(Sender: TObject);
    procedure btnWriteX12Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure writeFile();
    procedure writeFile_X12_810();
    procedure writeFile_X12_850();
    procedure writeFile_X12_855();
    procedure writeFile_X12_856();
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormX12writer: TFormX12writer;

implementation

{$R *.dfm}

procedure TFormX12writer.btnSaveClick(Sender: TObject);
var
  dir: AnsiString;
begin
  SaveDialog1.Filter := 'All Files (*.*)|*.*';
  dir := GetCurrentDir;
  if (SaveDialog1.Execute() = True) then
    // write to file
    editranslatorX12Writer1.OutputFile := SaveDialog1.FileName;
  editranslatorX12Writer1.FileWriteMode := TeditranslatorX12WriterFileWriteModes.fwmOverwrite;
  writeFile();
  editranslatorX12Writer1.OutputFile := '';
  SetCurrentDir(dir);
end;

procedure TFormX12writer.writeFile();
begin
  txtLog.Text := '';

    if (cbFileType.ItemIndex = 0) then // 810
    begin
      writeFile_X12_810();
    end
    else if (cbFileType.ItemIndex = 1) then // 850
    begin
      writeFile_X12_850();
    end
    else if (cbFileType.ItemIndex = 2) then // 855
    begin
      writeFile_X12_855();
    end
    else if (cbFileType.ItemIndex = 3) then // 856
    begin
      writeFile_X12_856();
    end;

  txtLog.Lines.Add('Write Complete' + #13#10);

  txtFile.Text := editranslatorX12Writer1.OutputData;
end;

procedure TFormX12writer.writeFile_X12_810();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  editranslatorX12Writer1.StartInterchangeHeader('004010');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('U');
  editranslatorX12Writer1.WriteElementString('00401');
  editranslatorX12Writer1.WriteElementString('000000006');
  editranslatorX12Writer1.WriteElementString('0');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('>');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  editranslatorX12Writer1.StartFunctionalGroupHeader();
  editranslatorX12Writer1.WriteElementString('IN');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('20160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('6');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('004010');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  editranslatorX12Writer1.StartTransactionHeader('810');
  editranslatorX12Writer1.WriteElementString('810');
  editranslatorX12Writer1.WriteElementString('0001');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BIG');
  editranslatorX12Writer1.StartSegment('BIG');
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.WriteElementString('3003014445');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('0476553272');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('DR');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: CUR');
  editranslatorX12Writer1.StartSegment('CUR');
  editranslatorX12Writer1.WriteElementString('SE');
  editranslatorX12Writer1.WriteElementString('USD');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('REF');
  editranslatorX12Writer1.WriteElementString('8M');
  editranslatorX12Writer1.WriteElementString('0056');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('BY');
  editranslatorX12Writer1.WriteElementString('Company');
  editranslatorX12Writer1.WriteElementString('92');
  editranslatorX12Writer1.WriteElementString('544380');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  editranslatorX12Writer1.StartSegment('N1Loop1/N3');
  editranslatorX12Writer1.WriteElementString('Address');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  editranslatorX12Writer1.StartSegment('N1Loop1/N4');
  editranslatorX12Writer1.WriteElementString('City');
  editranslatorX12Writer1.WriteElementString('CA');
  editranslatorX12Writer1.WriteElementString('Postal Code');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('ST');
  editranslatorX12Writer1.WriteElementString('Name');
  editranslatorX12Writer1.WriteElementString('92');
  editranslatorX12Writer1.WriteElementString('0607047800010');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  editranslatorX12Writer1.StartSegment('N1Loop1/N3');
  editranslatorX12Writer1.WriteElementString('Address');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  editranslatorX12Writer1.StartSegment('N1Loop1/N4');
  editranslatorX12Writer1.WriteElementString('City');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('200131');
  editranslatorX12Writer1.WriteElementString('Country');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('RE');
  editranslatorX12Writer1.WriteElementString('Name');
  editranslatorX12Writer1.WriteElementString('92');
  editranslatorX12Writer1.WriteElementString('5095956');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  editranslatorX12Writer1.StartSegment('N1Loop1/N3');
  editranslatorX12Writer1.WriteElementString('Address');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  editranslatorX12Writer1.StartSegment('N1Loop1/N4');
  editranslatorX12Writer1.WriteElementString('City');
  editranslatorX12Writer1.WriteElementString('IL');
  editranslatorX12Writer1.WriteElementString('Postal Code');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: IT1Loop1');
  txtLog.Lines.Add('Segment: IT1');
  editranslatorX12Writer1.StartSegment('IT1Loop1/IT1');
  editranslatorX12Writer1.WriteElementString('20');
  editranslatorX12Writer1.WriteElementString('2500');
  editranslatorX12Writer1.WriteElementString('EA');
  editranslatorX12Writer1.WriteElementString('36.96');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('BP');
  editranslatorX12Writer1.WriteElementString('335S0594');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF_3');
  editranslatorX12Writer1.StartSegment('IT1Loop1/REF_3');
  editranslatorX12Writer1.WriteElementString('KK');
  editranslatorX12Writer1.WriteElementString('0099778154');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF_3\');
  editranslatorX12Writer1.StartSegment('IT1Loop1/REF_3');
  editranslatorX12Writer1.WriteElementString('PO');
  editranslatorX12Writer1.WriteElementString('0476553272');
  editranslatorX12Writer1.WriteElementString('20');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('Segment: TDS');
  editranslatorX12Writer1.StartSegment('TDS');
  editranslatorX12Writer1.WriteElementString('9240000');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: CTT');
  editranslatorX12Writer1.StartSegment('CTT');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('EndTransaction');
  editranslatorX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  editranslatorX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  editranslatorX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.writeFile_X12_850();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  editranslatorX12Writer1.StartInterchangeHeader('004010');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('U');
  editranslatorX12Writer1.WriteElementString('00401');
  editranslatorX12Writer1.WriteElementString('000000007');
  editranslatorX12Writer1.WriteElementString('0');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('>');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  editranslatorX12Writer1.StartFunctionalGroupHeader();
  editranslatorX12Writer1.WriteElementString('PO');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('20160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('7');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('004010');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  editranslatorX12Writer1.StartTransactionHeader('850');
  editranslatorX12Writer1.WriteElementString('850');
  editranslatorX12Writer1.WriteElementString('0001');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BEG');
  editranslatorX12Writer1.StartSegment('BEG');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.WriteElementString('DS');
  editranslatorX12Writer1.WriteElementString('0476696888');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('REF');
  editranslatorX12Writer1.WriteElementString('SB');
  editranslatorX12Writer1.WriteElementString('ZZ11');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('REF');
  editranslatorX12Writer1.WriteElementString('6P');
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('REF');
  editranslatorX12Writer1.WriteElementString('8M');
  editranslatorX12Writer1.WriteElementString('0056');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('REF');
  editranslatorX12Writer1.WriteElementString('CR');
  editranslatorX12Writer1.WriteElementString('1070335099');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('REF');
  editranslatorX12Writer1.WriteElementString('CO');
  editranslatorX12Writer1.WriteElementString('7109790082');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: PER');
  editranslatorX12Writer1.StartSegment('PER');
  editranslatorX12Writer1.WriteElementString('CN');
  editranslatorX12Writer1.WriteElementString('name');
  editranslatorX12Writer1.WriteElementString('TE');
  editranslatorX12Writer1.WriteElementString('Number');

  txtLog.Lines.Add('Segment: CSH');
  editranslatorX12Writer1.StartSegment('CSH');
  editranslatorX12Writer1.WriteElementString('BK');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: SACLoop1');
  txtLog.Lines.Add('Segment: SAC');
  editranslatorX12Writer1.StartSegment('SACLoop1/SAC');
  editranslatorX12Writer1.WriteElementString('C');
  editranslatorX12Writer1.WriteElementString('ZZZZ');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('06');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('Segment: TD5');
  editranslatorX12Writer1.StartSegment('TD5');
  editranslatorX12Writer1.WriteElementString('Z');
  editranslatorX12Writer1.WriteElementString('2');
  editranslatorX12Writer1.WriteElementString('Code');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: N9Loop1');
  txtLog.Lines.Add('Segment: N9');
  editranslatorX12Writer1.StartSegment('N9Loop1/N9');
  editranslatorX12Writer1.WriteElementString('PD');
  editranslatorX12Writer1.WriteElementString('ZCOF');
  editranslatorX12Writer1.EndElement();

  editranslatorX12Writer1.StartSegment('N9Loop1/MSG');
  editranslatorX12Writer1.WriteElementString('Thanks!');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1n');
  editranslatorX12Writer1.StartSegment('N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('BY');
  editranslatorX12Writer1.WriteElementString('Name');
  editranslatorX12Writer1.WriteElementString('92');
  editranslatorX12Writer1.WriteElementString('5601');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('EN');
  editranslatorX12Writer1.WriteElementString('Name');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('ST');
  editranslatorX12Writer1.WriteElementString('OEM NAME');
  editranslatorX12Writer1.WriteElementString('92');
  editranslatorX12Writer1.WriteElementString('0000505462');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N2');
  editranslatorX12Writer1.StartSegment('N1Loop1/N2');
  editranslatorX12Writer1.WriteElementString('additional name');
  editranslatorX12Writer1.WriteElementString(''); // not skipped because last element
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  editranslatorX12Writer1.StartSegment('N1Loop1/N3');
  editranslatorX12Writer1.WriteElementString('Address');
  editranslatorX12Writer1.WriteElementString('Address');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  editranslatorX12Writer1.StartSegment('N1Loop1/N4');
  editranslatorX12Writer1.WriteElementString('City');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('201613');
  editranslatorX12Writer1.WriteElementString('CN');
  editranslatorX12Writer1.WriteElementString('SP');
  editranslatorX12Writer1.WriteElementString('020');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: PO1Loop1');
  txtLog.Lines.Add('Segment: PO1');
  editranslatorX12Writer1.StartSegment('PO1Loop1/PO1');
  editranslatorX12Writer1.WriteElementString('00010');
  editranslatorX12Writer1.WriteElementString('500000');
  editranslatorX12Writer1.WriteElementString('EA');
  editranslatorX12Writer1.WriteElementString('495');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('BP');
  editranslatorX12Writer1.WriteElementString('337S3744');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: PID_2');
  editranslatorX12Writer1.StartSegment('PO1Loop1/PIDLoop1/PID_2');
  editranslatorX12Writer1.WriteElementString('F');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('Thanks!');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('EN');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('Segment: REF_7');
  editranslatorX12Writer1.StartSegment('PO1Loop1/REF_7');
  editranslatorX12Writer1.WriteElementString('CO');
  editranslatorX12Writer1.WriteElementString('7109790082');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF_7');
  editranslatorX12Writer1.StartSegment('PO1Loop1/REF_7');
  editranslatorX12Writer1.WriteElementString('LI');
  editranslatorX12Writer1.WriteElementString('000010');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: SCHLoop1');
  txtLog.Lines.Add('Segment: SCH');
  editranslatorX12Writer1.StartSegment('PO1Loop1/SCHLoop1/SCH');
  editranslatorX12Writer1.WriteElementString('500000');
  editranslatorX12Writer1.WriteElementString('EA');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('002');
  editranslatorX12Writer1.WriteElementString('20180708');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: CTTLoop1');
  txtLog.Lines.Add('Segment: CTT');
  editranslatorX12Writer1.StartSegment('CTTLoop1/CTT');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.WriteElementString('500000');
  editranslatorX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('EndTransaction');
  editranslatorX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  editranslatorX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  editranslatorX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.writeFile_X12_855();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  editranslatorX12Writer1.StartInterchangeHeader('004010');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('U');
  editranslatorX12Writer1.WriteElementString('00401');
  editranslatorX12Writer1.WriteElementString('000000008');
  editranslatorX12Writer1.WriteElementString('0');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('>');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  editranslatorX12Writer1.StartFunctionalGroupHeader();
  editranslatorX12Writer1.WriteElementString('PR');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('20160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('8');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('004010');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  editranslatorX12Writer1.StartTransactionHeader('855');
  editranslatorX12Writer1.WriteElementString('855');
  editranslatorX12Writer1.WriteElementString('0013');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BAK');
  editranslatorX12Writer1.StartSegment('BAK');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.WriteElementString('AT');
  editranslatorX12Writer1.WriteElementString('0476553696');
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('4900043704');
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: PO1Loop1');
  txtLog.Lines.Add('Segment: PO1');
  editranslatorX12Writer1.StartSegment('PO1Loop1/PO1');
  editranslatorX12Writer1.WriteElementString('000010');
  editranslatorX12Writer1.WriteElementString('1100');
  editranslatorX12Writer1.WriteElementString('EA');
  editranslatorX12Writer1.WriteElementString('14.00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('BP');
  editranslatorX12Writer1.WriteElementString('335S0548');
  editranslatorX12Writer1.WriteElementString('VP');
  editranslatorX12Writer1.WriteElementString('Product');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('PO1Loop1/REF');
  editranslatorX12Writer1.WriteElementString('PO');
  editranslatorX12Writer1.WriteElementString('0476553696');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('PO1Loop1/REF');
  editranslatorX12Writer1.WriteElementString('VN');
  editranslatorX12Writer1.WriteElementString('0025009879');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: ACKLoop1');
  txtLog.Lines.Add('Segment: ACK');
  editranslatorX12Writer1.StartSegment('PO1Loop1/ACKLoop1/ACK');
  editranslatorX12Writer1.WriteElementString('IA');
  editranslatorX12Writer1.WriteElementString('1100');
  editranslatorX12Writer1.WriteElementString('EA');
  editranslatorX12Writer1.WriteElementString('067');
  editranslatorX12Writer1.WriteElementString('20150709');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: CTTLoop1');
  txtLog.Lines.Add('Segment: CTT');
  editranslatorX12Writer1.StartSegment('CTTLoop1/CTT');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.WriteElementString('1100');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('EndTransaction');
  editranslatorX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  editranslatorX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  editranslatorX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.writeFile_X12_856();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  editranslatorX12Writer1.StartInterchangeHeader('004010');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('ZZ');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('U');
  editranslatorX12Writer1.WriteElementString('00401');
  editranslatorX12Writer1.WriteElementString('000000009');
  editranslatorX12Writer1.WriteElementString('0');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('>');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  editranslatorX12Writer1.StartFunctionalGroupHeader();
  editranslatorX12Writer1.WriteElementString('SH');
  editranslatorX12Writer1.WriteElementString('ACME');
  editranslatorX12Writer1.WriteElementString('WAYNE_TECH');
  editranslatorX12Writer1.WriteElementString('20160707');
  editranslatorX12Writer1.WriteElementString('1544');
  editranslatorX12Writer1.WriteElementString('9');
  editranslatorX12Writer1.WriteElementString('T');
  editranslatorX12Writer1.WriteElementString('004010');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  editranslatorX12Writer1.StartTransactionHeader('856');
  editranslatorX12Writer1.WriteElementString('856');
  editranslatorX12Writer1.WriteElementString('0029');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BSN');
  editranslatorX12Writer1.StartSegment('BSN');
  editranslatorX12Writer1.WriteElementString('00');
  editranslatorX12Writer1.WriteElementString('0403734501');
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.WriteElementString('162859');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: DTM');
  editranslatorX12Writer1.StartSegment('DTM');
  editranslatorX12Writer1.WriteElementString('011');
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: HLLoop1');
  txtLog.Lines.Add('Segment: HL');
  editranslatorX12Writer1.StartSegment('HLLoop1/HL');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('S');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: PRF');
  editranslatorX12Writer1.StartSegment('HLLoop1/PRF');
  editranslatorX12Writer1.WriteElementString('0476553696');
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.SkipElement();
  editranslatorX12Writer1.WriteElementString('20150708');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: TD1');
  editranslatorX12Writer1.StartSegment('HLLoop1/TD1');
  editranslatorX12Writer1.WriteElementString('CNT90');
  editranslatorX12Writer1.WriteElementString('0');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: TD5');
  editranslatorX12Writer1.StartSegment('HLLoop1/TD5');
  editranslatorX12Writer1.WriteElementString('O');
  editranslatorX12Writer1.WriteElementString('2');
  editranslatorX12Writer1.WriteElementString('FEDX');
  editranslatorX12Writer1.WriteElementString('A');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('HLLoop1/REF');
  editranslatorX12Writer1.WriteElementString('BM');
  editranslatorX12Writer1.WriteElementString('EDITEST403734501');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  editranslatorX12Writer1.StartSegment('HLLoop1/REF');
  editranslatorX12Writer1.WriteElementString('CR');
  editranslatorX12Writer1.WriteElementString('4900043704');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: HLLoop1');
  txtLog.Lines.Add('Segment: HL');
  editranslatorX12Writer1.StartSegment('HLLoop1/HL');
  editranslatorX12Writer1.WriteElementString('2');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.WriteElementString('O');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('HLLoop1/N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('ST');
  editranslatorX12Writer1.WriteElementString('Name');
  editranslatorX12Writer1.WriteElementString('92');
  editranslatorX12Writer1.WriteElementString('0042001808');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N1');
  editranslatorX12Writer1.StartSegment('HLLoop1/N1Loop1/N1');
  editranslatorX12Writer1.WriteElementString('SF');
  editranslatorX12Writer1.WriteElementString('NameT');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  editranslatorX12Writer1.StartSegment('HLLoop1/N1Loop1/N3');
  editranslatorX12Writer1.WriteElementString('Address');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  editranslatorX12Writer1.StartSegment('HLLoop1/N1Loop1/N4');
  editranslatorX12Writer1.WriteElementString('City');
  editranslatorX12Writer1.WriteElementString('SG');
  editranslatorX12Writer1.WriteElementString('339942');
  editranslatorX12Writer1.WriteElementString('SG');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: HLLoop1');
  txtLog.Lines.Add('Segment: HL');
  editranslatorX12Writer1.StartSegment('HLLoop1/HL');
  editranslatorX12Writer1.WriteElementString('3');
  editranslatorX12Writer1.WriteElementString('2');
  editranslatorX12Writer1.WriteElementString('I');
  editranslatorX12Writer1.WriteElementString('0');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: LIN');
  editranslatorX12Writer1.StartSegment('HLLoop1/LIN');
  editranslatorX12Writer1.WriteElementString('10');
  editranslatorX12Writer1.WriteElementString('BP');
  editranslatorX12Writer1.WriteElementString('335S0548');
  editranslatorX12Writer1.WriteElementString('VP');
  editranslatorX12Writer1.WriteElementString('Product');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: SN1');
  editranslatorX12Writer1.StartSegment('HLLoop1/SN1');
  editranslatorX12Writer1.WriteElementString('10');
  editranslatorX12Writer1.WriteElementString('1100');
  editranslatorX12Writer1.WriteElementString('EA');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: MAN');
  editranslatorX12Writer1.StartSegment('HLLoop1/MAN');
  editranslatorX12Writer1.WriteElementString('CP');
  editranslatorX12Writer1.WriteElementString('Marks');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: CTT');
  editranslatorX12Writer1.StartSegment('CTT');
  editranslatorX12Writer1.WriteElementString('1');
  editranslatorX12Writer1.EndElement();

  txtLog.Lines.Add('EndTransaction');
  editranslatorX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  editranslatorX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  editranslatorX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.btnWriteX12Click(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    try
      editranslatorX12Writer1.Reset();
      txtLog.Text := '';
      txtFile.Text := '';

      // load proper schema for file type
      editranslatorX12Writer1.LoadSchema('..\..\RSSBus_00401_' + cbFileType.Text + '.json');

      editranslatorX12Writer1.Suffix := TeditranslatorX12WriterSuffixes(3); // suffixCRLF

      editranslatorX12Writer1.Config('Encoding=iso-8859-1');
      writeFile();
      Screen.Cursor := crDefault;
    except
      on E: Exception do
        MessageDlg('Exception ' + E.Message, mtInformation, [mbOk], 0);
    end;
  finally
    Screen.Cursor := crDefault
  end;

end;

procedure TFormX12writer.cbStandardChange(Sender: TObject);
begin

  cbFileType.Items.Clear();
  cbFileType.AddItem('810', nil);
  cbFileType.AddItem('850', nil);
  cbFileType.AddItem('855', nil);
  cbFileType.AddItem('856', nil);
  cbFileType.ItemIndex := 0;

end;

procedure TFormX12writer.FormCreate(Sender: TObject);
begin
  cbFileType.ItemIndex := 0;
end;

end.


