unit MainMenu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, Buttons;

type
  TMainForm = class(TForm)
    Database: TDatabase;
    boxPilot: TGroupBox;
    btnNewFlightPlan: TBitBtn;
    boxService: TGroupBox;
    btnNewPilot: TBitBtn;
    boxATC: TGroupBox;
    btnViewPlan: TBitBtn;
    btnPilots: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnNewFlightPlanClick(Sender: TObject);
    procedure btnNewPilotClick(Sender: TObject);
    procedure btnViewPlanClick(Sender: TObject);
    procedure btnPilotsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

  DB_Name:  string;

function ShowPosition(const Msg: string): Boolean;
  
implementation

uses Splash, AddPlan, AddPilot, FindPlan, ListPilots;

{$R *.DFM}

function ShowPosition(const Msg: string): Boolean;
begin
     OutputDebugString(PChar('ATCSys: '+Msg+#13#10));
     Result:=False;
     Exit;
     if MessageDlg(Msg, mtInformation, [mbOK, mbAbort], 0)=ID_ABORT then
     begin
          Result:=True;
          Application.Terminate;
     end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
   Screen.Cursor:=crSQLWait;
   try
     SplashForm:=TSplashForm.Create(Self);
     SplashForm.Show;
     Application.ProcessMessages;

     DB_Name:=InputBox('Open database', 'Enter database name', 'IBLocal');
     Application.ProcessMessages;

     try
       Database.Connected:=False; 
       if ShowPosition('Opening database') then Exit;
       Database.DatabaseName:=DB_Name;
       Database.AliasName:=DB_Name;
       Database.Open;
     except
       on E: Exception do begin
                               MessageDlg(Format('%s'#13#10'[%s]', [E.Message,E.ClassName]), mtError, [mbOk], 0);
                               Close;
                               Application.Terminate;
                               Exit;
                          end;
     end;
   finally
     SplashForm.Free;
     Screen.Cursor:=crDefault;
   end;
   ShowPosition('MainForm.FormCreate finished');
end;

procedure TMainForm.btnNewFlightPlanClick(Sender: TObject);
begin
     frmNewPlan:=TfrmNewPlan.Create(Self);
     frmNewPlan.ShowModal;
end;

procedure TMainForm.btnNewPilotClick(Sender: TObject);
begin
     frmAddPilot:=TfrmAddPilot.Create(Self);
     frmAddPilot.ShowModal;
end;

procedure TMainForm.btnViewPlanClick(Sender: TObject);
begin
     frmFindPlan:=TfrmFindPlan.Create(Self);
     try
      frmFindPlan.ShowModal;
      //if frmFindPlan.
     finally
      frmFindPlan.Free;
     end;
end;

procedure TMainForm.btnPilotsClick(Sender: TObject);
begin
     frmPilotList:=TfrmPilotList.Create(Self);
     frmPilotList.ShowModal;
end;

end.
