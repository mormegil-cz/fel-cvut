program ATCSys;

uses
  Forms,
  AddPlan in 'AddPlan.pas' {frmNewPlan},
  Splash in 'Splash.pas' {SplashForm},
  AddPilot in 'AddPilot.pas' {frmAddPilot},
  MainMenu in 'MainMenu.pas' {MainForm},
  FindPlan in 'FindPlan.pas' {frmFindPlan},
  ListPilots in 'ListPilots.pas' {frmPilotList};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
