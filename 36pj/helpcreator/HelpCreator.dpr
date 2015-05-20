(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Project file }
program HelpCreator;

uses
  Forms,
  HCMain in 'HCMain.pas' {frmMain},
  HCEngine in 'HCEngine.pas',
  HCLexAnalyser in 'HCLexAnalyser.pas',
  HCInput in 'HCInput.pas',
  HCSymTable in 'HCSymTable.pas',
  HCUnitStack in 'HCUnitStack.pas',
  HCBackEnd in 'HCBackEnd.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Delphi Component Help Creator';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
