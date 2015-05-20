unit ListPilots;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, Grids, DBGrids;

type
  TfrmPilotList = class(TForm)
    gridPilots: TDBGrid;
    srcPilots: TDataSource;
    tblPilots: TTable;
    tblPilotsRODCISLO: TStringField;
    tblPilotsCISLOLETPRUKAZU: TIntegerField;
    tblPilotsJMENO: TStringField;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPilotList: TfrmPilotList;

implementation
uses MainMenu;

{$R *.DFM}

procedure TfrmPilotList.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action:=caFree;
end;

procedure TfrmPilotList.FormCreate(Sender: TObject);
begin
     if ShowPosition('Opening table of pilots') then Exit;
     tblPilots.DatabaseName:=DB_Name;
     tblPilots.Open;
end;

end.
