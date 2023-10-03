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
unit edifactparserf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, editranslatorcore, StrUtils,
  editranslatortypes, editranslatoredifactwriter, editranslatoredifactreader;

type
  TFormEdifactparser = class(TForm)
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    rbtnFile: TRadioButton;
    rbtnString: TRadioButton;
    txtFile: TEdit;
    memoString: TMemo;
    btnParse: TButton;
    btnSelectFile: TButton;
    Label2: TLabel;
    memoEvents: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtSegment: TEdit;
    txtTag: TEdit;
    txtValue: TEdit;
    txtElement: TEdit;
    txtElementValue: TEdit;
    tvwXPath: TTreeView;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    txtSchema: TEdit;
    Label10: TLabel;
    OpenDialog2: TOpenDialog;
    btnSelectSchema: TButton;
    editranslatorEDIFACTReader1: TeditranslatorEDIFACTReader;
    editranslatorEDIFACTWriter1: TeditranslatorEDIFACTWriter;
    procedure btnSelectFileClick(Sender: TObject);
    procedure tvwXPathClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure btnSelectSchemaClick(Sender: TObject);
    procedure buildtree( Node: TTreeNode; XPath: String);
    procedure editranslatorEDIFACTReader1EndFunctionalGroup(Sender: TObject;
      const Tag, ControlNumber: string; Count: Integer;
      const FullSegment: string);
    procedure editranslatorEDIFACTReader1EndInterchange(Sender: TObject; const Tag,
      ControlNumber, FullSegment: string);
    procedure editranslatorEDIFACTReader1EndLoop(Sender: TObject);
    procedure editranslatorEDIFACTReader1StartTransaction(Sender: TObject; const Tag,
      ControlNumber, Code, FullSegment: string);
    procedure editranslatorEDIFACTReader1EndTransaction(Sender: TObject; const Tag,
      ControlNumber: string; Count: Integer; const FullSegment: string);
    procedure editranslatorEDIFACTReader1Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
    procedure editranslatorEDIFACTReader1ResolveSchema(Sender: TObject;
      const TransactionCode, StandardVersion: string);
    procedure editranslatorEDIFACTReader1Segment(Sender: TObject; const Tag, Name,
      LoopName, FullSegment: string);
    procedure editranslatorEDIFACTReader1StartFunctionalGroup(Sender: TObject;
      const Tag, ControlNumber, FullSegment: string);
    procedure editranslatorEDIFACTReader1StartInterchange(Sender: TObject; const Tag,
      ControlNumber, FullSegment: string);
    procedure editranslatorEDIFACTReader1StartLoop(Sender: TObject;
      const Name: string);
    procedure editranslatorEDIFACTReader1Warning(Sender: TObject; WarnCode: Integer;
      const Message: string; SegmentNumber: Integer; const SegmentTag,
      TechnicalErrorCode, SegmentErrorCode, ElementErrorCode: string;
      ElementPosition: Integer);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEdifactparser: TFormEdifactparser;

implementation

{$R *.dfm}



procedure TFormEdifactparser.btnParseClick(Sender: TObject);
       var
inputString: String;
root: String;
i: Integer;
begin
   Try
    editranslatorEDIFACTReader1.Reset;

    memoEvents.Clear;
    tvwXPath.Items.Clear;
    txtSegment.Clear;
    txtTag.Clear;
    txtValue.Clear;
    txtElement.Clear;
    txtElementValue.Clear;

    if(txtSchema.Text<>'') then
    begin
     editranslatorEDIFACTReader1.LoadSchema(txtSchema.Text);
    end;

    if(rbtnFile.Checked) Then
    begin
      editranslatorEDIFACTReader1.InputFile := txtFile.Text;
    end
    Else if rbtnString.Checked then
    begin
      inputString := '';
      for i := 0 to memoString.Lines.Count -1 do
        begin
          inputString := inputString + memoString.Lines[i];
        end;
        editranslatorEDIFACTReader1.InputData := inputString;
    end;

    editranslatorEDIFACTReader1.Parse();

    editranslatorEDIFACTReader1.XPath := '/';
    root := 'IX';
    buildtree(tvwXPath.Items.AddChild(nil, root), editranslatorEDIFACTReader1.XPath);
    tvwXPath.Selected:=tvwXPath.Items.GetFirstNode;
    tvwXPath.Selected.Expand(false);


  Finally

  End;
end;

Function GetXPath(Node : TTreeNode) : string;
var
ParentNode : TTreeNode;
str : string;
begin
    str:='/'+Node.Text;
    ParentNode:=Node;
    while ParentNode.Parent<>nil  do
    begin
        ParentNode :=  ParentNode.Parent;
        str:='/'+ParentNode.Text+str;
    end;
    Result := str;
end;

 procedure TFormEdifactparser.buildtree( Node: TTreeNode; XPath: string);
 var
 parentXPath : string;
 numChildren, i : Integer;
 parent : TTreeNode;
 begin
    parentXPath:= GetXPath(Node);
    numChildren:=  editranslatorEDIFACTReader1.XChildren;
    editranslatorEDIFACTReader1.XPath := XPath;

    if(editranslatorEDIFACTReader1.ElementCount > 0) then
    begin
      parent := tvwXPath.Items.AddChild(Node, 'Elements');
      for  i:=0 to editranslatorEDIFACTReader1.ElementCount-1 do
      begin
         tvwXPath.Items.AddChild(parent, editranslatorEDIFACTReader1.ElementName[i]+' = '+editranslatorEDIFACTReader1.ElementValue[i]);
      end;
    end;

    for i:=1 to numChildren  do
    begin
        editranslatorEDIFACTReader1.XPath := parentXPath+'/['+InttoStr(i)+']';
        buildtree(tvwXPath.Items.AddChild(node,editranslatorEDIFACTReader1.XSegment), editranslatorEDIFACTReader1.XPath);
    end;
 end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1EndFunctionalGroup(
  Sender: TObject; const Tag, ControlNumber: string; Count: Integer;
  const FullSegment: string);
begin
  memoEvents.Lines.Add('End Functional Group: ' + Tag);
end;

procedure TFormEdifactparser.btnSelectFileClick(Sender: TObject);
begin
  OpenDialog1 := TOpenDialog.Create(self);
  OpenDialog1.InitialDir := GetCurrentDir;
  OpenDialog1.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog1.Execute) Then
    txtFile.Text := openDialog1.FileName;

  OpenDialog1.Free();
end;

procedure TFormEdifactparser.btnSelectSchemaClick(Sender: TObject);
begin
  OpenDialog2 := TOpenDialog.Create(self);
  OpenDialog2.InitialDir := GetCurrentDir;
  OpenDialog2.Options := [ofFileMustExist, ofNoChangeDir];

  if(OpenDialog2.Execute) Then
    txtSchema.Text := openDialog2.FileName;

  OpenDialog2.Free();
end;



procedure TFormEdifactparser.editranslatorEDIFACTReader1EndInterchange(
  Sender: TObject; const Tag, ControlNumber, FullSegment: string);
begin
  memoEvents.Lines.Add('End Interchange: ' + Tag);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1EndLoop(Sender: TObject);
begin
  memoEvents.Lines.Add('End Loop');
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1EndTransaction(
  Sender: TObject; const Tag, ControlNumber: string; Count: Integer;
  const FullSegment: string);
begin
  memoEvents.Lines.Add('End Transaction: ' + Tag);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1Error(Sender: TObject;
  ErrorCode: Integer; const Description: string);
begin
  memoEvents.Lines.Add('ERROR:' + IntToStr(ErrorCode)+':'+Description);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1ResolveSchema(Sender: TObject;
  const TransactionCode, StandardVersion: string);
begin
    memoEvents.Lines.Add('Resolve Schema: ' + TransactionCode);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1Segment(Sender: TObject;
  const Tag, Name, LoopName, FullSegment: string);
begin
    memoEvents.Lines.Add('Segment: ' + Name);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1StartFunctionalGroup(
  Sender: TObject; const Tag, ControlNumber, FullSegment: string);
begin
  memoEvents.Lines.Add('Start Functional Group: ' + Tag);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1StartInterchange(
  Sender: TObject; const Tag, ControlNumber, FullSegment: string);
begin
   memoEvents.Lines.Add('Start Interchange: ' + Tag);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1StartLoop(Sender: TObject;
  const Name: string);
begin
   memoEvents.Lines.Add('Start Loop: ' + Name);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1StartTransaction(
  Sender: TObject; const Tag, ControlNumber, Code, FullSegment: string);
begin
  memoEvents.Lines.Add('Start Transaction: ' + Tag);
end;

procedure TFormEdifactparser.editranslatorEDIFACTReader1Warning(Sender: TObject;
  WarnCode: Integer; const Message: string; SegmentNumber: Integer;
  const SegmentTag, TechnicalErrorCode, SegmentErrorCode,
  ElementErrorCode: string; ElementPosition: Integer);
begin
  memoEvents.Lines.Add('WARNING:' + IntToStr(WarnCode)+':'+Message);
end;

procedure TFormEdifactparser.tvwXPathClick(Sender: TObject);
var
elementText,elementXPath : string;
begin
   if (AnsiEndsStr('Elements',GetXPath(tvwXPath.Selected)))  then
   begin
      //elements node selected
      editranslatorEDIFACTReader1.XPath := GetXPath(tvwXPath.Selected.Parent);
      txtSegment.Text :=  editranslatorEDIFACTReader1.XPath;
      txtTag.Text := editranslatorEDIFACTReader1.XSegment;
      txtValue.Text := editranslatorEDIFACTReader1.XTag;
      txtElement.Text := '';
      txtElementValue.Text  := '';
   end else
   begin
      elementXPath := GetXPath(tvwXPath.Selected);
      if (tvwXPath.Selected.Parent<>nil) And (tvwXPath.Selected.Parent.Text='Elements')  then
       begin
         elementXPath := GetXPath(tvwXPath.Selected)+''']';
         elementXPath := AnsiReplaceStr(elementXpath, '/Elements/','[1][@');
         elementXPath := AnsiReplaceStr(elementXPath,' = ','=''');
    end;
     editranslatorEDIFACTReader1.XPath:=elementXPath;
     txtSegment.Text :=  editranslatorEDIFACTReader1.XPath;
     txtTag.Text := editranslatorEDIFACTReader1.XSegment;
     txtValue.Text := editranslatorEDIFACTReader1.XTag;
     txtElement.Text := '';
     txtElementValue.Text  := '';

     if (tvwXPath.Selected.Parent<>nil)  then
      begin
       if (tvwXPath.Selected.Parent.Text='Elements')  then
       begin
          elementText := tvwXPath.Selected.Text;
          txtElement.Text := AnsiLeftStr(elementText, Pos(' = ', elementText));
          txtElementValue.Text := AnsiRightStr(elementText, Length(elementText) - 2 - Pos(' = ', elementText));
       end;
     end;

   end;

end;

end.

