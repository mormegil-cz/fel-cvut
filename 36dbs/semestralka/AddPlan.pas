unit AddPlan;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Mask, DBCtrls, ExtCtrls, StdCtrls, Db, DBTables, BDE;

type
  TfrmNewPlan = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    editEquipment: TEdit;
    Label8: TLabel;
    comboSSR: TComboBox;
    Label9: TLabel;
    Label10: TLabel;
    Panel1: TPanel;
    lblNumber: TLabel;
    Panel2: TPanel;
    DBtxtAircraftType: TDBText;
    Panel3: TPanel;
    DBtxtTurbulence: TDBText;
    Label11: TLabel;
    editDepartTime: TMaskEdit;
    comboSpeedUnits: TComboBox;
    editSpeed: TMaskEdit;
    Label12: TLabel;
    boxLevelUnits: TComboBox;
    editLevel: TMaskEdit;
    Label13: TLabel;
    boxRoute: TListBox;
    btnAddNavPoint: TButton;
    btnRemoveNavPoint: TButton;
    Label14: TLabel;
    Label15: TLabel;
    editEET: TMaskEdit;
    Label16: TLabel;
    Label17: TLabel;
    memoOtherInfo: TMemo;
    Label18: TLabel;
    Label19: TLabel;
    btnAcknowledge: TButton;
    btnCancel: TButton;
    DBComboPilot: TDBLookupComboBox;
    Panel4: TPanel;
    DBtxtPilotInfo: TDBText;
    srcAircftType: TDataSource;
    srcRegAircft: TDataSource;
    srcAirport: TDataSource;
    srcPilot: TDataSource;
    qAircftType: TQuery;
    qAirport: TQuery;
    qPilot: TQuery;
    qNavPoint: TQuery;
    srcNavPoint: TDataSource;
    qRegAircft: TQuery;
    srcPlan: TDataSource;
    tPlan: TTable;
    DBComboAircraftID: TDBLookupComboBox;
    comboRules: TDBComboBox;
    comboFlightType: TDBComboBox;
    DBComboDepart: TDBLookupComboBox;
    DBComboDestin: TDBLookupComboBox;
    DBComboAltn: TDBLookupComboBox;
    DBComboAltn2: TDBLookupComboBox;
    boxNavPoints: TListBox;
    qMaxSchvalCislo: TQuery;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    tPlanPresBod: TTable;
    procedure boxLevelUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcknowledgeClick(Sender: TObject);
    procedure boxNavPointsClick(Sender: TObject);
    procedure boxRouteClick(Sender: TObject);
    procedure btnAddNavPointClick(Sender: TObject);
    procedure btnRemoveNavPointClick(Sender: TObject);
    procedure boxNavPointsDblClick(Sender: TObject);
    procedure boxRouteDblClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure RouteSwap(const I1, I2: Integer);
  public
    { Public declarations }
  end;

var
  frmNewPlan: TfrmNewPlan;

implementation

uses MainMenu;

type TNavPoint = class
                       Nazev: string;
                       ID:    string[5];
                 end;

{$R *.DFM}

procedure ErrorFound(const Comp: TWinControl; const Msg: String);
begin
     MessageDlg(Msg, mtError, [mbOk], 0);
     Comp.SetFocus;
end;

function CheckDateTime(const S, DFmt, TFmt: string): Boolean;
var DT: string;
    P:  Integer;
begin
     ShortDateFormat:=DFmt;
     ShortTimeFormat:=TFmt;
     DT:=S;
     repeat
           P:=Pos('_', DT);
           if P>0 then Delete(DT, P, 1);
     until P=0;
     
     P:=Pos(', ', DT);
     if P>0 then Delete(DT, P, 1);
    
     try
       Result:=StrToDateTime(DT)>Now;
     except
       on EConvertError do Result:=False;
     end;
end;

function GetDateTime(const S, DFmt, TFmt: string): TDateTime;
var DT: string;
    P:  Integer;
begin
     ShortDateFormat:=DFmt;
     ShortTimeFormat:=TFmt;
     DT:=S;
     repeat
           P:=Pos('_', DT);
           if P>0 then Delete(DT, P, 1);
     until P=0;

     P:=Pos(', ', DT);
     if P>0 then Delete(DT, P, 1);
     
     Result:=StrToDateTime(DT);
end;

procedure TfrmNewPlan.boxLevelUnitsChange(Sender: TObject);
begin
     case boxLevelUnits.ItemIndex of
          0, 1: editLevel.EditMask:='000;1; ';  // F, A
          2, 3: editLevel.EditMask:='0000;1; '; // S, M
     end;
end;

procedure TfrmNewPlan.FormCreate(Sender: TObject);
var I: Integer;
    P: TNavPoint;
begin
   Screen.Cursor:=crSQLWait;
   try
     try
       for I:=0 to ComponentCount-1 do
         if Components[I] is TDBDataSet then
            with Components[I] as TDBDataSet do
            begin
                 if ShowPosition('Opening "'+Name+'"') then Exit;
                 DatabaseName:=DB_Name;
                 Active:=True;
            end;

       if ShowPosition('Loading NavPoints') then Exit;
       // load NavPoints list
       qNavPoint.First;
       while not qNavPoint.Eof do
       begin
            Application.ProcessMessages;
            P:=TNavPoint.Create;
            with P do
            begin
                 Nazev:=qNavPoint.FieldByName('NAZEV').AsString;
                 ID:=qNavPoint.FieldByName('ID').AsString;
            end;
            boxNavPoints.Items.AddObject(P.Nazev, P);
            qNavPoint.Next;
       end;
       qNavPoint.Active:=False;

       if ShowPosition('Getting plan#') then Exit;
       frmNewPlan.Caption:='New flight plan #'+IntToStr(qMaxSchvalCislo.Fields[0].AsInteger+1);

       if ShowPosition('Switching to insert mode') then Exit;
       tPlan.Insert;
     except
       on E: Exception do begin
                               MessageDlg(Format('%s'#13#10'[%s]', [E.Message,E.ClassName]), mtError, [mbOk], 0);
                               Close;
                               Application.Terminate;
                               Exit;
                          end;
     end;
   finally
     Screen.Cursor:=crDefault;
   end;
   ShowPosition('frmNewPlan.FormCreate finished');
end;

procedure TfrmNewPlan.btnCancelClick(Sender: TObject);
begin
     Close;
end;

procedure TfrmNewPlan.btnAcknowledgeClick(Sender: TObject);
var I: Integer;
begin
     // check for validity
     if DBComboAircraftID.KeyValue=Null then ErrorFound(DBComboAircraftID, 'Aircraft ID required')
     else if comboRules.ItemIndex=-1 then ErrorFound(comboRules, 'Choose flight rules')
     else if DBComboDepart.KeyValue=Null then ErrorFound(DBComboDepart, 'Specify departure aerodrome')
     else if not CheckDateTime(editDepartTime.Text, 'd.m.yyyy','hh:mm') then ErrorFound(editDepartTime, 'Specify departure time')
     else if DBComboDestin.KeyValue=Null then ErrorFound(DBComboDestin, 'Specify destination aerodrome')
     else if DBComboPilot.KeyValue=Null then ErrorFound(DBComboPilot, 'Enter pilot in command')
     else
     begin
          if ShowPosition('Filling CasOdletu') then Exit;
          tPlan.FieldByName('CASODLETU').AsDateTime:=GetDateTime(editDepartTime.Text, 'd.m.yyyy', 'hh:mm');
          if ShowPosition('Filling SchvalCislo') then Exit;
          tPlan.FieldByName('SCHVALCISLO').AsInteger:=qMaxSchvalCislo.Fields[0].AsInteger+1;

          if ShowPosition('Posting record') then Exit;
          tPlan.Post;
          if ShowPosition('Closing table') then Exit;
          tPlan.Close;

          if boxRoute.Items.Count>0 then
          begin
               if ShowPosition('Creating PlanPresBod records...') then Exit;
               tPlanPresBod.Open;
               for I:=0 to boxRoute.Items.Count-1 do
               begin
                    if ShowPosition('...Append') then Exit;
                    tPlanPresBod.Append;
                    tPlanPresBod.FieldByName('PORADIVPLANU').AsInteger:=I;
                    tPlanPresBod.FieldByName('LETOVYPL_SCHVALCISLO').AsInteger:=qMaxSchvalCislo.Fields[0].AsInteger+1;
                    tPlanPresBod.FieldByName('NAVIGACN_NAVOBJEK_ID').AsString:=(boxRoute.Items.Objects[I] as TNavPoint).ID;
                    if ShowPosition('...Post') then Exit;
                    tPlanPresBod.Post;
               end;
               if ShowPosition('Closing PlanPresBod') then Exit;
               tPlanPresBod.Close;
          end;

          if ShowPosition('Finished, closing') then Exit;
          Close;
     end;
end;

procedure TfrmNewPlan.boxNavPointsClick(Sender: TObject);
begin
     btnAddNavPoint.Enabled:=boxNavPoints.ItemIndex<>-1;
end;

procedure TfrmNewPlan.boxRouteClick(Sender: TObject);
begin
     btnRemoveNavPoint.Enabled:=boxRoute.ItemIndex<>-1;
     btnMoveUp.Enabled:=boxRoute.ItemIndex>0;
     btnMoveDown.Enabled:=(boxRoute.ItemIndex<>-1) and (boxRoute.ItemIndex<boxRoute.Items.Count-1);
end;

procedure TfrmNewPlan.btnAddNavPointClick(Sender: TObject);
begin
     boxRoute.ItemIndex:=boxRoute.Items.AddObject(boxNavPoints.Items[boxNavPoints.ItemIndex], boxNavPoints.Items.Objects[boxNavPoints.ItemIndex]);
     boxRoute.OnClick(boxRoute);
end;

procedure TfrmNewPlan.btnRemoveNavPointClick(Sender: TObject);
var OldII: Integer;
begin
     OldII:=boxRoute.ItemIndex;
     boxRoute.Items.Delete(boxRoute.ItemIndex);
     boxRoute.ItemIndex:=OldII;
     if boxRoute.ItemIndex=-1 then boxRoute.ItemIndex:=boxRoute.Items.Count-1;
     boxRoute.OnClick(boxRoute);
end;

procedure TfrmNewPlan.boxNavPointsDblClick(Sender: TObject);
begin
     boxNavPoints.OnClick(boxNavPoints);
     if btnAddNavPoint.Enabled then btnAddNavPoint.Click;
end;

procedure TfrmNewPlan.boxRouteDblClick(Sender: TObject);
begin
     boxRoute.OnClick(boxRoute);
     if btnRemoveNavPoint.Enabled then btnRemoveNavPoint.Click;
end;

procedure TfrmNewPlan.btnMoveUpClick(Sender: TObject);
begin
     RouteSwap(boxRoute.ItemIndex, boxRoute.ItemIndex-1);
end;

procedure TfrmNewPlan.btnMoveDownClick(Sender: TObject);
begin
     RouteSwap(boxRoute.ItemIndex, boxRoute.ItemIndex+1);
end;

procedure TfrmNewPlan.RouteSwap(const I1, I2: Integer);
var Title: string;
    Obj:   TObject;
begin
     Title:=boxRoute.Items.Strings[I1];
     Obj:=boxRoute.Items.Objects[I1];

     boxRoute.Items.Strings[I1]:=boxRoute.Items.Strings[I2];
     boxRoute.Items.Objects[I1]:=boxRoute.Items.Objects[I2];

     boxRoute.Items.Strings[I2]:=Title;
     boxRoute.Items.Objects[I2]:=Obj;

     boxRoute.ItemIndex:=I2;
     boxRoute.OnClick(boxRoute);
end;

procedure TfrmNewPlan.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action:=caFree;
end;

end.
