program TSP;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  Genetics in 'Genetics.pas',
  Data in 'Data.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

