unit AddPilot;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, Mask, DBCtrls;

type
  TfrmAddPilot = class(TForm)
    srcPilots: TDataSource;
    tblPilots: TTable;
    Label1: TLabel;
    editName: TDBEdit;
    Label2: TLabel;
    Label3: TLabel;
    editLicense: TDBEdit;
    btnOK: TButton;
    btnCancel: TButton;
    editID: TDBEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAddPilot: TfrmAddPilot;

implementation

{$R *.DFM}

procedure TfrmAddPilot.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action:=caFree;
end;

procedure TfrmAddPilot.FormCreate(Sender: TObject);
begin
     tblPilots.Open;
     tblPilots.Insert;
end;

procedure TfrmAddPilot.btnOKClick(Sender: TObject);
begin
     tblPilots.Post;
     tblPilots.Close;
     ModalResult:=mrOk;
end;

end.
