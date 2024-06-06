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

program x12parser;

uses
  Forms,
  x12parserf in 'x12parserf.pas' {FormX12parser};

begin
  Application.Initialize;

  Application.CreateForm(TFormX12parser, FormX12parser);
  Application.Run;
end.


         
