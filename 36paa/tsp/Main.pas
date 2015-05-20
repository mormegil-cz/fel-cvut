unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TeeProcs, TeEngine, Chart, Series, ComCtrls;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    lblGeneration: TLabel;
    Label2: TLabel;
    lblValue: TLabel;
    Label3: TLabel;
    lblTime: TLabel;
    lblLastChange: TLabel;
    Graf: TPaintBox;
    Label4: TLabel;
    lblElite: TLabel;
    Label5: TLabel;
    lblInvMutation: TLabel;
    Label7: TLabel;
    lblSwapMutation: TLabel;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure GrafPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses Data, Genetics;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var FN: string;
begin
     if ParamCount <> 1 then
     begin
          MessageDlg('Missing param (data file)', mtError, [mbOk], 0);
          Application.Terminate;
          Abort;
     end;
     FN := ParamStr(1);
     if Pos('.tsp', FN) <> 0 then LoadTSP(FN)
                             else LoadTXT(FN);
     Caption := 'Genetics -- TSP [' + ExtractFileName(FN) + ']';
end;

var
    GenHist: array of Integer;
    History: array of Integer;
    HL: Integer;
    AdjHist: array of Integer;
    AHL: Integer;

{
const
 Solution: array[0..80] of Integer =
}

procedure TfrmMain.FormActivate(Sender: TObject);
var P: TPopulation;
    C: TChromozome;
    I: Integer;
    Start: TDateTime;
    F: TextFile;
    LT: Cardinal;
    Curr, Best: TValue;
    LastAdjustment,
    LastChange,
    Generation: Integer;
    PMC: Int64;
begin
     OnActivate := nil;
     Application.ProcessMessages;

     StatusBar.Panels[0].Text := 'Initializing...';
     Application.ProcessMessages;
     if Application.Terminated then Exit;

     QueryPerformanceCounter(PMC);
     RandSeed := Integer(PMC);
     //Randomize;
     {
     RandSeed := Random(MaxInt);
     }

     EliteCount := 20;
     SwapMutationProb := 5;
     InvMutationProb := 30;
     //CrossOverProb := 50;

     SetLength(History, 100000);
     SetLength(GenHist, 100000);
     SetLength(AdjHist, 1000);

     AssignFile(F, 'TSP.LOG');
     FileMode := 2;
     Rewrite(F);

     P := TPopulation.Create(100);
     try
       Screen.Cursor := crHourglass;
       //Generation := 0;
       for I := 1 to P.PopSize do
       begin
          StatusBar.Panels[0].Text := Format('Generating initial population...%d/%d', [I, P.PopSize]);
          Application.ProcessMessages;
          if Application.Terminated then Exit;

          C := TChromozome.CreateValid;
          C.Eval;
          //if FitnessToLength(C.Value) < NO_PATH_LIMIT then Inc(Generation);
          P.Inject(C);
          C.Free;
       end;
       //Writeln(F, '# ', Generation, ' valid chromozomes');
     finally
       Screen.Cursor := crDefault;
     end;
     C := TChromozome.CreateRoundTrip;
     C.Eval;
     P.Inject(C);
     Writeln(F, '# RoundTrip length: ', Round(FitnessToLength(C.Value)));
     C.Free;

     {
     C := TChromozome.CreateFromData(Solution);
     P.Inject(C);
     C.Free;
     }

     C := TChromozome.CreateGreedy;
     C.Eval;
     P.Inject(C);
     Writeln(F, '# Greedy length: ', Round(FitnessToLength(C.Value)));
     C.Free;

     StatusBar.Panels[0].Text := 'Running...';
     Application.ProcessMessages;

     Generation := 0;
     LastChange := 0;
     LastAdjustment := 0;
     Start := Now;
     LT := GetTickCount;
     HL := 0;
     AHL := 0;
     Best := -1;

     while not Application.Terminated do
     begin
          P.Step;

          Curr := P.BestValue;
          if Curr <> Best then
          begin
               History[HL] := Round(FitnessToLength(Curr));
               GenHist[HL] := Generation;
               Writeln(F, Generation, #9, History[HL]);
               HL := (HL + 1) mod Length(History);
               Best := Curr;
               LastChange := Generation;

               LastAdjustment := Generation;
               if EliteCount < 20 then Inc(EliteCount);
               if SwapMutationProb > 5 then Dec(SwapMutationProb, 1);
               if InvMutationProb > 30 then Dec(InvMutationProb, 1);
          end;

          if GetTickCount - LT >= 800 then
          begin
               lblGeneration.Caption := IntToStr(Generation);
               lblLastChange.Caption := '(' + IntToStr(LastChange) + ')';
               lblTime.Caption := TimeToStr(Now - Start);
               lblValue.Caption := IntToStr(Round(FitnessToLength(Curr)));
               lblElite.Caption := IntToStr(EliteCount);
               lblInvMutation.Caption := IntToStr(InvMutationProb);
               lblSwapMutation.Caption := IntToStr(SwapMutationProb);
               Graf.Invalidate;

               LT := GetTickCount;
          end;
          Inc(Generation);
          if Generation and $3F = 0 then
          begin
             C := nil;
             case Random(4) of
                  0: C := TChromozome.CreateRandom;
                  1: C := TChromozome.CreateValid;
                  2: C := TChromozome.CreateRoundTrip;
                  3: C := TChromozome.CreateGreedy;
                  4: C := TChromozome.CreateRandomGreedy;
             end;
             P.Inject(C);
             C.Free;
          end;
          if Generation = LastAdjustment + 400 then
          begin
               AdjHist[AHL] := Generation;
               AHL := (AHL + 1) mod Length(AdjHist);

               if SwapMutationProb < 40 then Inc(SwapMutationProb, 5);
               if InvMutationProb < 97 then Inc(InvMutationProb, 3);
               if EliteCount > 1 then Dec(EliteCount);
               LastAdjustment := Generation;

               C := TChromozome.CreateRandom;
               P.Inject(C);
               C.Free;
          end;
          if (InvMutationProb >= 95) and (Generation > LastChange + 3000) then
          begin
               lblGeneration.Caption := IntToStr(Generation);
               lblLastChange.Caption := '(' + IntToStr(LastChange) + ')';
               lblTime.Caption := 'Stopped at ' + TimeToStr(Now - Start);
               lblValue.Caption := IntToStr(Round(FitnessToLength(Curr)));
               lblElite.Caption := IntToStr(EliteCount);
               lblInvMutation.Caption := IntToStr(InvMutationProb);
               lblSwapMutation.Caption := IntToStr(SwapMutationProb);
               Graf.Invalidate;
               Break;
          end;

          Application.ProcessMessages;
     end;

     StatusBar.Panels[0].Text := 'Done...';
     Application.ProcessMessages;

     Writeln(F, '# ----- END OF HISTORY -----');
     Write(F, '# ');
     P.BestChromozome.PrintTo(F);
     Writeln(F, '#');
     Writeln(F, '# Last generation: ', Generation);
     Writeln(F, '# Last change at ', LastChange);
     Writeln(F, '# Total time: ', TimeToStr(Now - Start));
     CloseFile(F);
     P.Free;
end;

procedure TfrmMain.GrafPaint(Sender: TObject);
const LEFT_B = 50;
      RIGHT_B = 0;
      TOP_B = 0;
      BOTTOM_B = 50;
var Wf, Hf: Single;
    MaxG: Integer;
    MaxL: Integer;
    I:    Integer;
    S:    String;
begin
     if GenHist = nil then Exit;
     MaxG := GenHist[HL-1];
     MaxL := History[0];
     if MaxG = 0 then MaxG := 1;
     if MaxL = 0 then Exit;
     Wf := (Graf.ClientWidth - LEFT_B - RIGHT_B) / MaxG;
     Hf := (Graf.ClientHeight - TOP_B - BOTTOM_B) / MaxL;
     with Graf.Canvas do
     begin
          Brush.Style := bsSolid;
          Brush.Color := clWhite;
          FillRect(Graf.BoundsRect);

          TextOut(0, 0, IntToStr(MaxL));
          S := IntToStr(MaxG);
          TextOut(Graf.ClientWidth - RIGHT_B - TextWidth(S), Graf.ClientHeight - TextHeight(S), S);

          Pen.Color := clBlack;
          Pen.Style := psSolid;
          MoveTo(LEFT_B, Graf.ClientHeight - BOTTOM_B - Round(Hf * MaxL));
          for I := 1 to HL-1 do
          begin
               LineTo(LEFT_B + Round(GenHist[I] * Wf), Graf.ClientHeight - BOTTOM_B - Round(Hf * History[I]));
          end;

          Pen.Style := psDot;
          Pen.Color := clMoneyGreen;
          for I := 0 to AHL-1 do
          begin
               MoveTo(LEFT_B + Round(AdjHist[I] * Wf), 0);
               LineTo(LEFT_B + Round(AdjHist[I] * Wf), Graf.ClientHeight-1);
          end;
     end;
end;

end.

