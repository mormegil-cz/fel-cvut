program KnapsackBatch;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Knapsack in 'Knapsack.pas';

var
   Items: TKnapsackItemList;
   I,
   N,
   ID, Cdp, Ch1, Ch2, 
   MaxWeight: Integer;
   Solution: TKnapsackItemSet;
   Start, Stop, Freq,
   StatesSumBF, StatesSumBB, StatesSumH1, StatesSumH2, StatesSumDP,
   TimeSumBF, TimeSumBB, TimeSumH1, TimeSumH2, TimeSumDP: Int64;
   Err,
   MaxErrH1,
   SumErrH1,
   MaxErrH2,
   SumErrH2: Double;
   Count: Integer;

   DoBf: Boolean;

   ProgStart, ProgStop: TDateTime;

begin
     Count := 0;
     TimeSumBF := 0;
     TimeSumH1 := 0;
     TimeSumH2 := 0;
     TimeSumDP := 0;
     TimeSumBB := 0;
     SumErrH1 := 0;
     MaxErrH1 := -1;
     SumErrH2 := 0;
     MaxErrH2 := -1;
     StatesSumBF := 0;
     StatesSumBB := 0;
     StatesSumH1 := 0;
     StatesSumH2 := 0;
     StatesSumDP := 0;

     DoBf := ParamStr(1) <> '-nobf';

     ProgStart := Now;
     while not EOF do
     begin
          if EOLN then
          begin
               Readln;
               Continue;
          end;
          Read(ID);
          Read(N);
          if N > High(TKnapsackItemId) then
          begin
               Writeln('*ERROR* Too big problem instance');
               Halt(1);
          end;
          Read(MaxWeight);
          SetLength(Items, N);
          for I := 0 to N-1 do
              with Items[I] do Read(Weight, Cost);

          if DoBf then
          begin
               QueryPerformanceCounter(Start);
               Solution := SolveKnapsack_BruteForce3(MaxWeight, Items, ID);
               QueryPerformanceCounter(Stop);
               Inc(TimeSumBF, Stop - Start);
               ComputeCost(Solution, Items);
               Inc(StatesSumBF, Steps_Counter);
          end;

          QueryPerformanceCounter(Start);
          Solution := SolveKnapsack_BranchBound(MaxWeight, Items, ID);
          QueryPerformanceCounter(Stop);
          Inc(TimeSumBB, Stop - Start);
          ComputeCost(Solution, Items);
          Inc(StatesSumBB, Steps_Counter);

          QueryPerformanceCounter(Start);
          Solution := SolveKnapsack_Heur_CbyW(MaxWeight, Items, ID);
          QueryPerformanceCounter(Stop);
          Inc(TimeSumH1, Stop - Start);
          Ch1 := ComputeCost(Solution, Items);
          Inc(StatesSumH1, Steps_Counter);

          QueryPerformanceCounter(Start);
          Solution := SolveKnapsack_Heur_CbyW_test(MaxWeight, Items, ID);
          QueryPerformanceCounter(Stop);
          Inc(TimeSumH2, Stop - Start);
          Ch2 := ComputeCost(Solution, Items);
          Inc(StatesSumH2, Steps_Counter);

          QueryPerformanceCounter(Start);
          Solution := SolveKnapsack_DynaProg(MaxWeight, Items, ID);
          QueryPerformanceCounter(Stop);
          Inc(TimeSumDP, Stop - Start);
          Cdp := ComputeCost(Solution, Items);
          Inc(StatesSumDP, Steps_Counter);

          if Cdp = 0 then
          begin
               Writeln('*ERROR* Problem with no solution!');
               Halt(1);
          end else
          begin
               Err := 1-Ch1/Cdp;
               SumErrH1 := SumErrH1 + Err;
               if Err > MaxErrH1 then MaxErrH1 := Err;

               Err := 1-Ch2/Cdp;
               SumErrH2 := SumErrH2 + Err;
               if Err > MaxErrH2 then MaxErrH2 := Err;
          end;

          Inc(Count);
          Write(ErrOutput, Count,^M);
     end;
     ProgStop := Now;

     if Count <> 0 then
     begin
          QueryPerformanceFrequency(Freq);
          if DoBf then
             Writeln('Avg BF time: ',TimeSumBF/Count/Freq * 1E3:0:5,' ms');
          Writeln('Avg BB time: ',TimeSumBB/Count/Freq * 1E3:0:5,' ms');
          Writeln('Avg H1 time: ',TimeSumH1/Count/Freq * 1E3:0:5,' ms');
          Writeln('Avg H2 time: ',TimeSumH2/Count/Freq * 1E3:0:5,' ms');
          Writeln('Avg DP time: ',TimeSumDP/Count/Freq * 1E3:0:5,' ms');
          if DoBf then
             Writeln('Avg BF steps: ',StatesSumBF/Count:0:2);
          Writeln('Avg BB steps: ',StatesSumBB/Count:0:2);
          Writeln('Avg H1 steps: ',StatesSumH1/Count:0:2);
          Writeln('Avg H2 steps: ',StatesSumH2/Count:0:2);
          Writeln('Avg DP steps: ',StatesSumDP/Count:0:2);
          Writeln('Avg H1 error: ', 100*SumErrH1/Count:0:5, '%');
          Writeln('Avg H2 error: ', 100*SumErrH2/Count:0:5, '%');
          Writeln('Max H1 error: ', 100*MaxErrH1:0:5, '%');
          Writeln('Max H2 error: ', 100*MaxErrH2:0:5, '%');
     end;
     Writeln('Total running time: ', (ProgStop-ProgStart)*86400:0:3,' s');
end.

