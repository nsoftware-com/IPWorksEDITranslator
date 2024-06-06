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
unit x12translatorf;

interface



uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, StrUtils,
  ComCtrls, editranslatorcore, editranslatortypes,
  editranslatorx12translator;

type
  TFormX12translator = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    btnX12File: TRadioButton;
    btnX12String: TRadioButton;
    txtX12File: TEdit;
    btnSelectX12File: TButton;
    tMemoX12String: TMemo;
    chkX12Overwrite: TCheckBox;
    txtSchema: TEdit;
    btnSelectSchemaFile: TButton;
    btnToXml: TButton;
    btnToX12: TButton;
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
    editranslatorX12Translator1: TeditranslatorX12Translator;
    procedure btnSelectX12FileClick(Sender: TObject);
    procedure btnSelectSchemaFileClick(Sender: TObject);
    procedure btnSelectXmlFileClick(Sender: TObject);
    procedure btnToXmlClick(Sender: TObject);
    procedure btnToX12Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormX12translator: TFormX12translator;

implementation

{$R *.dfm}

procedure TFormX12translator.btnSelectX12FileClick(Sender: TObject);
begin
  OpenDialog1 := TOpenDialog.Create(self);
  OpenDialog1.InitialDir := GetCurrentDir;
  OpenDialog1.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog1.Execute) Then
    txtX12File.Text := openDialog1.FileName;

  OpenDialog1.Free();
end;

procedure TFormX12translator.btnSelectSchemaFileClick(Sender: TObject);
begin
  OpenDialog2 := TOpenDialog.Create(self);
  OpenDialog2.InitialDir := GetCurrentDir;
  OpenDialog2.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog2.Execute) Then
    txtSchema.Text := openDialog2.FileName;

  OpenDialog2.Free();
end;

procedure TFormX12translator.btnSelectXmlFileClick(Sender: TObject);
begin
  OpenDialog3 := TOpenDialog.Create(self);
  OpenDialog3.InitialDir := GetCurrentDir;
  OpenDialog3.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog3.Execute) Then
    txtXmlFile.Text := openDialog3.FileName;

  OpenDialog3.Free();
end;

procedure TFormX12translator.btnToXmlClick(Sender: TObject);
var
inputString:String;
i: Integer;
begin
  StatusBar1.Panels[0].Text := '';

  try
    editranslatorX12Translator1.Reset();

    if(txtSchema.Text<>'') then
      editranslatorX12Translator1.LoadSchema(txtSchema.Text);

    if(btnX12File.Checked) then
      editranslatorX12Translator1.InputFile := txtX12File.Text
    else
      begin
        inputString := '';
      for i := 0 to tMemoX12String.Lines.Count -1 do
        begin
          inputString := inputString + tMemoX12String.Lines[i];
        end;
        editranslatorX12Translator1.InputData:=inputString;
      end;

    if(btnXmlFile.Checked) then
    begin
      editranslatorX12Translator1.OutputFile := txtXmlFile.Text;
      editranslatorX12Translator1.Overwrite := chkXmlOverwrite.Checked;
    end;

    editranslatorX12Translator1.Translate;

    tMemoXmlString.Text := editranslatorX12Translator1.OutputData;

  finally

  end;

  StatusBar1.Panels[0].Text := 'Translated To XML';
end;




procedure TFormX12translator.btnToX12Click(Sender: TObject);
var
inputString:String;
i: Integer;
begin
  StatusBar1.Panels[0].Text := '';

  try
    editranslatorX12Translator1.Reset();

    editranslatorX12Translator1.InputFormat := TeditranslatorX12TranslatorInputFormats.xifXML;
    editranslatorX12Translator1.OutputFormat := TeditranslatorX12TranslatorOutputFormats.xofX12;

    if(btnXmlFile.Checked) then
      editranslatorX12Translator1.InputFile := txtXmlFile.Text
    else
      begin
        inputString := '';
      for i := 0 to tMemoXmlString.Lines.Count -1 do
        begin
          inputString := inputString + tMemoXmlString.Lines[i];
        end;
        editranslatorX12Translator1.InputData:=inputString;
      end;

    if(btnX12File.Checked) then
    begin
      editranslatorX12Translator1.OutputFile := txtX12File.Text;
      editranslatorX12Translator1.Overwrite := chkX12Overwrite.Checked;
    end;

    editranslatorX12Translator1.Translate;

    tMemoX12String.Text := editranslatorX12Translator1.OutputData;

  finally

  end;

  StatusBar1.Panels[0].Text := 'Translated To X12';
end;

end.






