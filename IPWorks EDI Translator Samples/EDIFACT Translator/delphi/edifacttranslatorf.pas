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
unit edifacttranslatorf;

interface



uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, editranslatorcore,  StrUtils,
  editranslatoredifacttranslator, ComCtrls, editranslatortypes;

type
  TFormEdifacttranslator = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    btnEdifactFile: TRadioButton;
    btnEdifactString: TRadioButton;
    txtEdifactFile: TEdit;
    btnSelectEdifactFile: TButton;
    tMemoEdifactString: TMemo;
    chkEdifactOverwrite: TCheckBox;
    txtSchema: TEdit;
    btnSelectSchemaFile: TButton;
    btnToXml: TButton;
    btnToEdi: TButton;
    btnXmlFile: TRadioButton;
    btnXmlString: TRadioButton;
    txtXmlFile: TEdit;
    btnSelectXmlFile: TButton;
    chkXmlOverwrite: TCheckBox;
    tMemoXmlString: TMemo;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    OpenDialog3: TOpenDialog;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    editranslatorEDIFACTTranslator1: TeditranslatorEDIFACTTranslator;
    procedure btnSelectEdifactFileClick(Sender: TObject);
    procedure btnSelectSchemaFileClick(Sender: TObject);
    procedure btnSelectXmlFileClick(Sender: TObject);
    procedure btnToXmlClick(Sender: TObject);
    procedure btnToEdiClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEdifacttranslator: TFormEdifacttranslator;

implementation

{$R *.dfm}

procedure TFormEdifacttranslator.btnSelectEdifactFileClick(Sender: TObject);
begin
  OpenDialog1 := TOpenDialog.Create(self);
  OpenDialog1.InitialDir := GetCurrentDir;
  OpenDialog1.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog1.Execute) Then
    txtEdifactFile.Text := openDialog1.FileName;

  OpenDialog1.Free();
end;

procedure TFormEdifacttranslator.btnSelectSchemaFileClick(Sender: TObject);
begin
  OpenDialog2 := TOpenDialog.Create(self);
  OpenDialog2.InitialDir := GetCurrentDir;
  OpenDialog2.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog2.Execute) Then
    txtSchema.Text := openDialog2.FileName;

  OpenDialog2.Free();
end;

procedure TFormEdifacttranslator.btnSelectXmlFileClick(Sender: TObject);
begin
  OpenDialog3 := TOpenDialog.Create(self);
  OpenDialog3.InitialDir := GetCurrentDir;
  OpenDialog3.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog3.Execute) Then
    txtXmlFile.Text := openDialog3.FileName;

  OpenDialog3.Free();
end;

procedure TFormEdifacttranslator.btnToXmlClick(Sender: TObject);
var
inputString:String;
i: Integer;
begin
  StatusBar1.Panels[0].Text := '';

  try
    editranslatorEDIFACTTranslator1.Reset();

    if(txtSchema.Text<>'') then
      editranslatorEDIFACTTranslator1.LoadSchema(txtSchema.Text);

    if(btnEdifactFile.Checked) then
      editranslatorEDIFACTTranslator1.InputFile := txtEdifactFile.Text
    else
      begin
        inputString := '';
      for i := 0 to tMemoEdifactString.Lines.Count -1 do
        begin
          inputString := inputString + tMemoEdifactString.Lines[i];
        end;
        editranslatorEDIFACTTranslator1.InputData:=inputString;
      end;

    if(btnXmlFile.Checked) then
    begin
      editranslatorEDIFACTTranslator1.OutputFile := txtXmlFile.Text;
      editranslatorEDIFACTTranslator1.Overwrite := chkXmlOverwrite.Checked;
    end;

    editranslatorEDIFACTTranslator1.Translate;

    tMemoXmlString.Text := editranslatorEDIFACTTranslator1.OutputData;

  finally

  end;

  StatusBar1.Panels[0].Text := 'Translated To XML';
end;

procedure TFormEdifacttranslator.btnToEdiClick(Sender: TObject);
var
inputString:String;
i: Integer;
begin
  StatusBar1.Panels[0].Text := '';

  try
    editranslatorEDIFACTTranslator1.Reset();

    editranslatorEDIFACTTranslator1.InputFormat := TeditranslatorEDIFACTTranslatorInputFormats.eifXML;
    editranslatorEDIFACTTranslator1.OutputFormat := TeditranslatorEDIFACTTranslatorOutputFormats.eofEDIFACT;

    if(btnXmlFile.Checked) then
      editranslatorEDIFACTTranslator1.InputFile := txtXmlFile.Text
    else
      begin
        inputString := '';
      for i := 0 to tMemoXmlString.Lines.Count -1 do
        begin
          inputString := inputString + tMemoXmlString.Lines[i];
        end;
        editranslatorEDIFACTTranslator1.InputData:=inputString;
      end;

    if(btnEdifactFile.Checked) then
    begin
      editranslatorEDIFACTTranslator1.OutputFile := txtEdifactFile.Text;
      editranslatorEDIFACTTranslator1.Overwrite := chkEdifactOverwrite.Checked;
    end;

    editranslatorEDIFACTTranslator1.Translate;

    tMemoEdifactString.Text := editranslatorEDIFACTTranslator1.OutputData;

  finally

  end;

  StatusBar1.Panels[0].Text := 'Translated To EDIFACT';
end;

end.


