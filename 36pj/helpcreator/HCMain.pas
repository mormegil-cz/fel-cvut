(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Main program + dialog }
unit HCMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ToolEdit, ComCtrls;

type
  TfrmMain = class(TForm)
    PageControl: TPageControl;
    tabFiles: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnStart: TButton;
    tabSearchPath: TTabSheet;
    boxSearchPaths: TListBox;
    btnAdd: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    btnDelete: TButton;
    editSource: TEdit;
    editRTF: TEdit;
    editHPJ: TEdit;
    editDirectory: TEdit;
    tabAbout: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure editSourceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure boxSearchPathsClick(Sender: TObject);
    procedure editDirectoryChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    procedure LoadDefaultSearchPath;
    function  CompilerProgress(const UnitName, SymbolName: string): Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses Registry, HCInput, HCLexAnalyser, HCEngine, HCSymTable;

{$R *.DFM}

function ExpandPath(const S: string): string;
begin
     if (S = '') or (S[Length(S)] = '\') then Result := S
                                         else Result := S + '\';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
     case ParamCount of
          0: ;
          1: begin
                  editSource.Text := ParamStr(1);
                  editRTF.Text    := ChangeFileExt(editSource.Text, '.RTF');
                  editHPJ.Text    := ChangeFileExt(editSource.Text, '.HPJ');
             end;
          2: begin
                  editSource.Text := ParamStr(1);
                  editRTF.Text    := ParamStr(2);
                  editHPJ.Text    := ChangeFileExt(editRTF.Text, '.HPJ');
             end;
          3: begin
                  editSource.Text := ParamStr(1);
                  editRTF.Text    := ParamStr(2);
                  editHPJ.Text    := ParamStr(3);
             end;
          else MessageDlg('Invalid parameters.', mtError, [mbIgnore], 0);
     end;

     LoadDefaultSearchPath;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  SearchPath := boxSearchPaths.Items;
  HCSymTable.OnProgress := CompilerProgress;

  btnStart.Enabled:=False;
  Enabled := False;
  Screen.Cursor := crHourGlass;
  try
     try
       Compile(editSource.Text, editRTF.Text, editHPJ.Text);
     except
       on E: ESyntaxError do
       begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
       end;
     end;
     MessageDlg('OK.', mtInformation, [mbOK], 0);
  finally
     Screen.Cursor := crDefault;
     Enabled := True;
     btnStart.Enabled:=True;
     Caption := 'Delphi Component Help Creator';
     Application.Terminate;
  end;
end;

procedure TfrmMain.editSourceChange(Sender: TObject);
begin
     btnStart.Enabled := FileExists(editSource.Text);
end;

procedure TfrmMain.boxSearchPathsClick(Sender: TObject);
begin
     btnMoveUp.Enabled := boxSearchPaths.ItemIndex > 0;
     btnMoveDown.Enabled := (boxSearchPaths.ItemIndex <> -1) and (boxSearchPaths.ItemIndex < boxSearchPaths.Items.Count-1);
     btnDelete.Enabled := boxSearchPaths.ItemIndex <> -1;
end;

procedure TfrmMain.editDirectoryChange(Sender: TObject);
begin
     btnAdd.Enabled := editDirectory.Text <> '';
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
begin
     boxSearchPaths.ItemIndex := boxSearchPaths.Items.Add(ExpandPath(editDirectory.Text));
     boxSearchPathsClick(boxSearchPaths);
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
begin
     boxSearchPaths.Items.Delete(boxSearchPaths.ItemIndex);
     boxSearchPathsClick(boxSearchPaths);
end;

procedure TfrmMain.btnMoveUpClick(Sender: TObject);
begin
     boxSearchPaths.Items.Move(boxSearchPaths.ItemIndex, boxSearchPaths.ItemIndex-1);
     boxSearchPathsClick(boxSearchPaths);
end;

procedure TfrmMain.btnMoveDownClick(Sender: TObject);
begin
     boxSearchPaths.Items.Move(boxSearchPaths.ItemIndex, boxSearchPaths.ItemIndex+1);
     boxSearchPathsClick(boxSearchPaths);
end;

procedure TfrmMain.LoadDefaultSearchPath;
var Reg: TRegistry;
    DelphiVersions: TStringList;
    DelphiRootDir,
    KeyPath,
    Entry,
    Path: string;
    P:    Integer;
begin
     Reg := TRegistry.Create;
     try
       Reg.RootKey := HKEY_CURRENT_USER;
       if not Reg.OpenKey('\SOFTWARE\Borland\Delphi', False) then Exit;
       DelphiVersions := TStringList.Create;
       try
         Reg.GetKeyNames(DelphiVersions);
         DelphiVersions.Sort;
         if DelphiVersions[DelphiVersions.Count-1] = 'Locales' then
            DelphiVersions.Delete(DelphiVersions.Count-1);
         if not Reg.OpenKey(DelphiVersions[DelphiVersions.Count-1], False) then Exit;
         KeyPath := Reg.CurrentPath;
         if not Reg.OpenKey('Library', False) then Exit;
       finally
         DelphiVersions.Free;
       end;

       //Path := Reg.ReadString('Search Path');
       Path := Reg.ReadString('Browsing Path');

       Reg.RootKey := HKEY_LOCAL_MACHINE;
       if Reg.OpenKey(KeyPath, False) then
          DelphiRootDir := Reg.ReadString('RootDir');
     finally
       Reg.Free;
     end;

     while Path<>'' do
     begin
          P := Pos(';', Path);
          if P = 0 then P := Length(Path) + 1;
          Entry := Copy(Path, 1, P-1);
          Delete(Path, 1, P);
          P := Pos('$(DELPHI)', Entry);
          if P<>0 then
          begin
               Delete(Entry, P, 9);
               Insert(DelphiRootDir, Entry, P);
          end;
          boxSearchPaths.Items.Add(ExpandPath(Entry));
     end;
end;

function TfrmMain.CompilerProgress(const UnitName,
  SymbolName: string): Boolean;
const Cnt: Integer = 0;
begin
     Inc(Cnt);
     if (Cnt < 500) and (UnitName<>'') then
     begin
          Result := not Application.Terminated;
          Exit;
     end;
     Cnt := 0;

     if UnitName='' then Caption := SymbolName
                    else Caption := UnitName+'.'+SymbolName;
     Application.ProcessMessages;

     Result := not Application.Terminated;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
     OnPaint := nil;

     if ParamCount = 3 then
     begin
          Invalidate;
          Application.ProcessMessages;
          btnStart.Click;
     end;
end;

end.
