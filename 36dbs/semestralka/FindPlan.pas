unit FindPlan;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Grids, DBGrids, StdCtrls, DBCtrls, Mask, ExtCtrls;

type
  TfrmFindPlan = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    srcPilot: TDataSource;
    qPilot: TQuery;
    qNavPoint: TQuery;
    srcNavPoint: TDataSource;
    srcPlan: TDataSource;
    tPlan: TTable;
    DBNavigator: TDBNavigator;
    DBComboPilot: TDBText;
    DBComboAircraftID: TDBText;
    DBComboDepart: TDBText;
    DBComboDestin: TDBText;
    editDepartTime: TDBText;
    comboRules: TDBText;
    txtPlanNum: TDBText;
    tPlanCASODLETU: TDateTimeField;
    tPlanPREDPIS: TStringField;
    tPlanSCHVALCISLO: TIntegerField;
    tPlanLETADLOP_ID: TStringField;
    tPlanODLETZ_NAVOBJEK_ID: TStringField;
    tPlanPRISTANI_NAVOBJEK_ID: TStringField;
    tPlanODLETZ_NAVOBJEK_ID2: TStringField;
    tPlanPRISTANI_NAVOBJEK_ID2: TStringField;
    tPlanKAPITANL_CISLOLETPRUKAZU2: TIntegerField;
    DBText1: TDBText;
    tPlanKapitan_Jmeno: TStringField;
    qRoute: TQuery;
    srcRoute: TDataSource;
    gridRoute: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure tPlanAfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFindPlan: TfrmFindPlan;

implementation
uses MainMenu;

{$R *.DFM}

procedure TfrmFindPlan.FormCreate(Sender: TObject);
var I: Integer;
begin
     for I:=0 to ComponentCount-1 do
         if Components[I] is TDBDataSet then
            with Components[I] as TDBDataSet do
            begin
                 if ShowPosition('Opening "'+Name+'"') then Exit;
                 DatabaseName:=DB_Name;
                 Active:=True;
            end;
end;

procedure TfrmFindPlan.tPlanAfterScroll(DataSet: TDataSet);
begin
     qRoute.Active:=False;
     qRoute.ParamByName('PlanNum').Value:=tPlan.FieldByName('SchvalCislo').AsInteger;
     qRoute.Active:=True;
end;

end.
