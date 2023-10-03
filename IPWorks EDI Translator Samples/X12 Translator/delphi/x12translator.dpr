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

program x12translator;

uses
  Forms,
  x12translatorf in 'x12translatorf.pas' {FormX12translator};

begin
  Application.Initialize;

  Application.CreateForm(TFormX12translator, FormX12translator);
  Application.Run;
end.


         
