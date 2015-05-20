unit Genetics;

interface

type
  TValue = Double;

const
  NO_PATH_LIMIT = 1E5;

var
  DistanceMatrix: array of array of TValue;

function LengthToFitness(L: TValue): TValue;
function FitnessToLength(F: TValue): TValue;

{ ---- GA params ---- }
{$DEFINE ALWAYS_CROSSOVER}              // Crossover probability = 100% ?
var
  ChromozomeLength: Integer;

  EliteCount:       Integer;
{$IFNDEF ALWAYS_CROSSOVER}
  CrossOverProb:    Integer;
{$ENDIF}
  SwapMutationProb: Integer;
  InvMutationProb:  Integer;
{ ---- }

type
  TChromozome = class
  private
    Genes: array of Integer;
    FValue: TValue;
  public
    constructor CreateRandom;
    constructor CreateEmpty;
    constructor CreateValid;
    constructor CreateRoundTrip;
    constructor CreateGreedy;
    constructor CreateRandomGreedy;
    constructor CreateFromData(const D);

    function    DeepCopy: TChromozome;
    procedure   Assign(Source: TChromozome);
    function    Compare(Op: TChromozome): Boolean;

    procedure   Normalize;     // make the permutation start by 0

    procedure   Mutate_RandomSwap;
    procedure   Mutate_RandomInvert;
    procedure   BeAnOffspring_Basic(P1, P2: TChromozome);
    procedure   BeAnOffspring_Greedy(P1, P2: TChromozome);
    procedure   BeAnOffspring_ERX(P1, P2: TChromozome);
    procedure   DoMutations;

    procedure   Eval;
    property    Value: TValue read FValue;

    procedure   PrintTo(var F: TextFile);
  end;

  TPopulationGenome = array of TChromozome;

  TPopulation = class
    PopSize: Integer;
    S1, S2,
    CurrPop, NextPop: TPopulationGenome;
    ValueSum: TValue;

    procedure EvalAndSort;
    function  ChooseParent: Integer;
    function  ChromozomeIsInNextPop(C: TChromozome; NextPopLen: Integer): Boolean;
    procedure Swap;
  private
    function GetBestChromozome: TChromozome;
    function GetBestValue: TValue;
  public
    constructor Create(PopSize: Integer);

    procedure   Step;

    procedure   Inject(C: TChromozome);

    property    BestValue: TValue read GetBestValue;
    property    BestChromozome: TChromozome read GetBestChromozome;
  end;

implementation
uses SysUtils, Math, Sort;

function LengthToFitness(L: TValue): TValue;
begin
     Result := 1.0 / L;
     //Result := Power(L, -0.5);
     //Result := 1E10 - L;
     //Result := 1.0 / Ln(L);
end;

function FitnessToLength(F: TValue): TValue;
begin
     Result := 1.0 / F;
     //Result := Power(F, -2.0);
     //Result := 1E10 - F;
     //Result := Exp(1.0/F);
end;

{ TChromozome }

procedure TChromozome.Assign(Source: TChromozome);
begin
  Move(Source.Genes[0], Genes[0], SizeOf(Genes[0]) * ChromozomeLength);
  FValue := Source.FValue;
end;

procedure TChromozome.BeAnOffspring_Basic(P1, P2: TChromozome);
var I1, I2, I, J: Integer;
    Used: array of Boolean;
    T: TChromozome;
begin
     I1 := Random(ChromozomeLength);
     I2 := Random(ChromozomeLength);
     SetLength(Used, ChromozomeLength);
     FillChar(Used[0], SizeOf(Used[0]) * ChromozomeLength, 0);
     if I1 > I2 then
     begin
          T := P1;
          P1 := P2;
          P2 := T;
          I := I1;
          I1 := I2;
          I2 := I;
     end;

     for I := I1 to I2 do
     begin
          Genes[I] := P1.Genes[I];
          Used[Genes[I]] := True;
     end;
     if I1 = 0 then I := I2 + 1
               else I := 0;
     for J := 0 to ChromozomeLength-1 do
     begin
          if not Used[P2.Genes[J]] then
          begin
               Assert(I < ChromozomeLength);
               Genes[I] := P2.Genes[J];
               Used[Genes[I]] := True;
               Inc(I);
               if I = I1 then I := I2 + 1;
          end;
     end;
     {
     while I < ChromozomeLength do
     begin
          while Used[P2.Genes[J]] do
          begin
               Inc(J);
               Assert(J < chromozomelength);
          end;
          Genes[I] := P2.Genes[J];
          Used[Genes[I]] := True;
          Inc(I);
          Inc(J);
          if I = I1 then I := I2 + 1;
     end;
     }

     Normalize;   // - not required -- should be already normalized
end;

procedure TChromozome.BeAnOffspring_ERX(P1, P2: TChromozome);
var Connected: array of array of Boolean;
    ConnCnt:   array of Integer;
    I, J:      Integer;
    BI:        Integer;
    BC:        Integer;
begin
     // \todo !!!!!!!!!! THIS IS TOTALLY (!) BAD !!!!!!!!!!!!!!!!
     SetLength(Connected, ChromozomeLength, ChromozomeLength);
     SetLength(ConnCnt, ChromozomeLength);
     FillChar(ConnCnt[0], SizeOf(ConnCnt[0]) * ChromozomeLength, 0);
     for I := ChromozomeLength-1 downto 0 do
      for J := ChromozomeLength-1 downto 0 do
      begin
           Connected[J][I] := DistanceMatrix[J][I] < NO_PATH_LIMIT;
           if Connected[J][I] then Inc(ConnCnt[J]);
      end;

     for I := 0 to ChromozomeLength-1 do
     begin
          BI := Random(ChromozomeLength);
          while ConnCnt[BI] = 0 do BI := (BI + 1) mod ChromozomeLength;
          BC := ConnCnt[BI];
          for J := 0 to ChromozomeLength-1 do
            if (ConnCnt[J] > 0) and (J <> BC) and (ConnCnt[J] < BC) then
            begin
                 BI := J;
                 BC := ConnCnt[J];
            end;

          for J := 0 to ChromozomeLength-1 do
          begin
               if Connected[BI][J] then
               begin
                    Connected[BI][J] := False;
                    Connected[J][BI] := False;
                    Dec(ConnCnt[J]);
               end;
          end;
          ConnCnt[BI] := 0;
          Genes[I] := BI;
     end;

     Normalize;
end;

procedure TChromozome.BeAnOffspring_Greedy(P1, P2: TChromozome);
var Used: array of Boolean;
    CC: Integer;
    I, J: Integer;
    I1, I2: Integer;
    //BD: TValue;
begin
     SetLength(Used, ChromozomeLength);
     FillChar(Used[0], SizeOf(Used[0]) * ChromozomeLength, 0);
     CC := Random(ChromozomeLength);
     Genes[0] := CC;
     Used[CC] := True;
     for I := 1 to ChromozomeLength-1 do
     begin
          I1 := -1;
          for J := ChromozomeLength-1 downto 0 do
              if P1.Genes[J] = CC then
              begin
                   I1 := J;
                   Break;
              end;
          Assert(I1 >= 0);
          I2 := -1;
          for J := ChromozomeLength-1 downto 0 do
              if P2.Genes[J] = CC then
              begin
                   I2 := J;
                   Break;
              end;
          Assert(I2 >= 0);
          I1 := (I1 + 1) mod ChromozomeLength;
          I2 := (I2 + 1) mod ChromozomeLength;
          if Used[P1.Genes[I1]] then
          begin
               if Used[P2.Genes[I2]] then
               begin
                    // both used
                    {
                    I1 := Random(ChromozomeLength);
                    while Used[I1] do I1 := (I1 + 1) mod ChromozomeLength;
                    BD := DistanceMatrix[CC][I1];

                    for J := I1 + 1 to ChromozomeLength-1 do
                        if (not Used[J]) and (DistanceMatrix[CC][J] < BD) then
                        begin
                             I1 := J;
                             BD := DistanceMatrix[CC][J];
                        end;
                    CC := I1;
                    }
                    CC := Random(ChromozomeLength);
                    while Used[CC] do CC := (CC + 1) mod ChromozomeLength;
               end else
               begin
                    // I1 used, I2 not
                    CC := P2.Genes[I2];
               end;
          end else
          begin
               if Used[P2.Genes[I2]] then
               begin
                    // I2 used, I1 not
                    CC := P1.Genes[I1];
               end else
               begin
                    // neither of I1,I2 used => choose the nearer
                    if DistanceMatrix[CC][P1.Genes[I1]] <= DistanceMatrix[CC][P2.Genes[I2]] then CC := P1.Genes[I1]
                                                                                            else CC := P2.Genes[I2];
               end;
          end;
          Used[CC] := True;
          Genes[I] := CC;
     end;

     Normalize;
end;

function TChromozome.Compare(Op: TChromozome): Boolean;
begin
  Result := (FValue = Op.Value) and CompareMem(@Genes[0], @Op.Genes[0], SizeOf(Genes[0]) * ChromozomeLength);
end;

constructor TChromozome.CreateEmpty;
begin
  SetLength(Genes, ChromozomeLength);
end;

constructor TChromozome.CreateFromData(const D);
begin
  SetLength(Genes, ChromozomeLength);
  Move(D, Genes[0], SizeOf(Genes[0]) * ChromozomeLength);
end;

constructor TChromozome.CreateGreedy;
var Used: array of Boolean;
    I, C, LC, J: Integer;
    BD: TValue;
begin
  SetLength(Genes, ChromozomeLength);
  SetLength(Used, ChromozomeLength);
  FillChar(Used[0], SizeOf(Used[0]) * ChromozomeLength, 0);
  C := Random(ChromozomeLength);
  Used[C] := True;
  Genes[0] := C;
  for I := 1 to ChromozomeLength-1 do
  begin
       LC := C;
       J := ChromozomeLength-1;
       while (J >= 0) and (Used[J] or (DistanceMatrix[LC][J] >= NO_PATH_LIMIT)) do Dec(J);
       if J >= 0 then
       begin
            BD := DistanceMatrix[LC][J];
            C := J;
            Dec(J);
            while J >= 0 do
            begin
                 if not Used[J] and (DistanceMatrix[LC][J] < BD) then
                 begin
                      BD := DistanceMatrix[LC][J];
                      C := J;
                 end;
                 Dec(J);
            end;
       end else
       begin
            C := Random(ChromozomeLength);
            while Used[C] do C := (C + 1) mod ChromozomeLength;
       end;
       Genes[I] := C;
       Used[C] := True;
  end;

  Normalize;
end;

constructor TChromozome.CreateRandom;
var C: array of Integer;
    I: Integer;
    R: Integer;
begin
  SetLength(Genes, ChromozomeLength);
  SetLength(C, ChromozomeLength);
  for I := ChromozomeLength - 1 downto 0 do
      C[I] := I;
  for I := ChromozomeLength - 1 downto 0 do
  begin
       R := Random(I+1);
       Genes[I] := C[R];
       C[R] := C[I];
  end;

  Normalize;
end;

constructor TChromozome.CreateRandomGreedy;
var Used: array of Boolean;
    I, C, LC: Integer;
    Retry,
    Rem: Integer;
begin
  SetLength(Genes, ChromozomeLength);
  SetLength(Used, ChromozomeLength);
  FillChar(Used[0], SizeOf(Used[0]) * ChromozomeLength, 0);
  Rem := 0;
  C := Random(ChromozomeLength);
  Used[C] := True;
  Genes[0] := C;
  for I := 1 to ChromozomeLength-1 do
  begin
       LC := C;
       C := Random(ChromozomeLength);
       Retry := 0;
       while Retry < Rem do
       begin
            while Used[C] do C := (C + 1) mod ChromozomeLength;
            if DistanceMatrix[LC][C] >= NO_PATH_LIMIT then
            begin
                 C := (C + 1) mod ChromozomeLength;
                 Inc(Retry);
            end else Break;
       end;
       if Retry >= Rem then
       begin
            C := Random(ChromozomeLength);
            while Used[C] do C := (C + 1) mod ChromozomeLength;
       end;
       Genes[I] := C;
       Used[C] := True;
       Dec(Rem);
  end;

  Normalize;
end;

constructor TChromozome.CreateRoundTrip;
var I: Integer;
begin
  SetLength(Genes, ChromozomeLength);
  for I := ChromozomeLength - 1 downto 0 do
      Genes[I] := I;
end;

constructor TChromozome.CreateValid;
var I, IC, LC, NC, GC, T: Integer;
    Cnt,
    Restart,
    Rem: Integer;
    Used: array of Boolean;
    Bad:  array of Boolean;
begin
     CreateEmpty;

     SetLength(Used, ChromozomeLength);
     SetLength(Bad, ChromozomeLength);
     FillChar(Used[0], SizeOf(Used[0]) * ChromozomeLength, 0);
     FillChar(Bad[0], SizeOf(Bad[0]) * ChromozomeLength, 0);
     IC := Random(ChromozomeLength);
     Genes[0] := IC;
     Used[IC] := True;
     Cnt := 1;
     Rem := ChromozomeLength - 1;
     NC := IC;
     Restart := 0;
     while Rem > 0 do
     begin
          LC := NC;
          NC := Random(ChromozomeLength);
          GC := 0;
          while True do // Break inside
          begin
               while Used[NC] do NC := (NC + 1) mod ChromozomeLength;
               if DistanceMatrix[LC][NC] < NO_PATH_LIMIT then Break;
               NC := (NC + 1) mod ChromozomeLength;
               Inc(GC);
               if GC >= Rem then Break;
          end;
          if GC >= Rem then
          begin
               // unable to prolong
               Bad[LC] := True;
               while Restart < Cnt - 1 do
               begin
                    if (DistanceMatrix[LC][Genes[Restart]] < NO_PATH_LIMIT) and (not Bad[Genes[Restart + 1]]) then Break;
                    Inc(Restart);
               end;
               if Restart < Cnt - 1 then
               begin
                    for I := 1 to (Cnt - Restart - 1) div 2 do
                    begin
                         T := Genes[Restart + I];
                         Genes[Restart + I] := Genes[Cnt - I];
                         Genes[Cnt - I] := T;
                         NC := Genes[Cnt-1];
                    end;
               end else
               begin
                    // unable to find any path -- continue to any remaining city...
                    NC := Random(ChromozomeLength);
                    while Used[NC] do NC := (NC + 1) mod ChromozomeLength;
                    Genes[Cnt] := NC;
                    Used[NC] := True;
                    Inc(Cnt);
                    Dec(Rem);
               end;
          end else
          begin
               // prolong
               Genes[Cnt] := NC;
               Used[NC] := True;
               Inc(Cnt);
               Dec(Rem);
          end;
     end;
end;

function TChromozome.DeepCopy: TChromozome;
begin
  Result := TChromozome.CreateEmpty;
  Result.Assign(Self);
end;

procedure TChromozome.DoMutations;
begin
     if Random(100) < SwapMutationProb then Mutate_RandomSwap;
     if Random(100) < InvMutationProb  then Mutate_RandomInvert;
end;

procedure TChromozome.Eval;
var I: Integer;
    S: TValue;
begin
     S := DistanceMatrix[Genes[0]][Genes[ChromozomeLength-1]];
     for I := ChromozomeLength-2 downto 0 do
         S := S + DistanceMatrix[Genes[I]][Genes[I+1]];
     FValue := LengthToFitness(S);
end;

procedure TChromozome.Mutate_RandomInvert;
var I, It, I1, I2: Integer;
    T: Integer;
begin
     I1 := Random(ChromozomeLength);
     I2 := Random(ChromozomeLength-1);
     if I2 >= I1 then Inc(I2)
                 else begin
                           T := I1;
                           I1 := I2;
                           I2 := T;
                      end;
     // now, I1 < I2
     for I := (I2 - I1) div 2 downto 0 do
     begin
          It := I2 - I;
          T := Genes[It];
          Genes[It] := Genes[I1 + I];
          Genes[I1 + I] := T;
     end;

     Normalize;
end;

procedure TChromozome.Mutate_RandomSwap;
var I1, I2: Integer;
    T: Integer;
begin
     I1 := Random(ChromozomeLength);
     I2 := Random(ChromozomeLength-1);
     if I2 >= I1 then Inc(I2);
     T := Genes[I1];
     Genes[I1] := Genes[I2];
     Genes[I2] := T;

     Normalize;
end;

procedure TChromozome.Normalize;
var I: Integer;
    Old: TChromozome;
begin
     for I := 0 to ChromozomeLength-1 do
         if Genes[I] = 0 then
         begin
              if I = 0 then Exit;

              Old := DeepCopy;
              Move(Old.Genes[I], Genes[0], SizeOf(Genes[0]) * (ChromozomeLength - I));
              Move(Old.Genes[0], Genes[ChromozomeLength - I], SizeOf(Genes[0]) * I);
              Old.Free;
              Exit;
         end;

     // what the hell?
     Assert(False);
end;

procedure TChromozome.PrintTo(var F: TextFile);
var I: Integer;
begin
     Write(F, 'Chromozome: (');
     for I := 0 to ChromozomeLength-1 do
     begin
          if I <> 0 then Write(F, ', ');
          Write(F, Genes[I]);
     end;
     Writeln(F, '), value = ', Round(FitnessToLength(Value)));
end;

{ TPopulation }

function TPopulation.ChooseParent: Integer;
var I: Integer;
    R, S: TValue;
begin
     R := Random * ValueSum;
     S := 0;
     for I := 0 to PopSize-1 do
     begin
          S := S + CurrPop[I].Value;
          if R <= S then
          begin
               Result := I;
               Exit;
          end;
     end;
     Result := PopSize - 1;
end;

function TPopulation.ChromozomeIsInNextPop(C: TChromozome;
  NextPopLen: Integer): Boolean;
var I: Integer;
begin
     for I := NextPopLen-1 downto 0 do
          if NextPop[I].Compare(C) then
          begin
               Result := True;
               Exit;
          end;

     Result := False;
end;

constructor TPopulation.Create(PopSize: Integer);
var I: Integer;
begin
     Self.PopSize := PopSize;
     SetLength(S1, PopSize);
     SetLength(S2, PopSize);
     CurrPop := S1;
     NextPop := S2;

     for I := PopSize-1 downto 0 do
     begin
          CurrPop[I] := TChromozome.CreateRandom;
          NextPop[I] := TChromozome.CreateEmpty;
     end;

     EvalAndSort;
end;

function CompareChromos(const A, B): Integer; // A<B ==> <0, A=B ==> =0, A>B ==> >0
begin
     if TChromozome(A).Value < TChromozome(B).Value then Result := +1
     else if TChromozome(A).Value > TChromozome(B).Value then Result := -1
     else Result := 0;
end;

procedure TPopulation.EvalAndSort;
var I: Integer;
begin
     ValueSum := 0;
     for I := PopSize-1 downto 0 do
     begin
          CurrPop[I].Eval;
          ValueSum := ValueSum + CurrPop[I].Value;
     end;
     qsort(CurrPop[0], PopSize, SizeOf(CurrPop[0]), CompareChromos);
end;

function TPopulation.GetBestChromozome: TChromozome;
begin
     Result := CurrPop[0];
end;

function TPopulation.GetBestValue: TValue;
begin
     Result := CurrPop[0].Value;
end;

procedure TPopulation.Inject(C: TChromozome);
begin
     // replace the worst chromozome with C
     CurrPop[PopSize-1].Assign(C);
     Swap;
     while ChromozomeIsInNextPop(C, PopSize-1) do
     begin
          if Random(2) = 0 then CurrPop[PopSize-1].Mutate_RandomSwap
                           else CurrPop[PopSize-1].Mutate_RandomInvert;
     end;
     Swap;
     EvalAndSort;
end;

procedure TPopulation.Step;
var I: Integer;
    I1, I2: Integer;
    Retries: Integer;
begin
     // copy the elite
     for I := EliteCount-1 downto 0 do
         NextPop[I].Assign(CurrPop[I]);
     // make the rest using crossovers
     for I := EliteCount to PopSize-1 do
     begin
          Retries := 0;
          while True do  // Break inside
          begin
                {$IFNDEF ALWAYS_CROSSOVER}
                if Random(100) < CrossOverProb then
                {$ENDIF}
                begin
                     I1 := ChooseParent;
                     //I1 := Random(PopSize);
                     repeat
                           //I2 := ChooseParent;
                           I2 := Random(PopSize);
                     until I1 <> I2;
                     {if Random(3) = 0 then
                        NextPop[I].BeAnOffspring_Greedy(CurrPop[I1], CurrPop[I2])
                     else}
                        NextPop[I].BeAnOffspring_Basic(CurrPop[I1], CurrPop[I2]);
                     //NextPop[I].BeAnOffspring_ERX(CurrPop[I1], CurrPop[I2]);
                {$IFNDEF ALWAYS_CROSSOVER}
                end else
                begin
                     I1 := ChooseParent;
                     NextPop[I].Assign(CurrPop[I1]);
                {$ENDIF}
                end;
                if ChromozomeIsInNextPop(NextPop[I], I-1) then
                begin
                     // maybe some check/mutations/...
                     if Retries > 50 then
                     begin
                          Retries := 0;
                          while ChromozomeIsInNextPop(NextPop[I], I-1) do
                          begin
                               if Random(2) = 0 then NextPop[I].Mutate_RandomSwap
                                                else NextPop[I].Mutate_RandomInvert;
                               if Retries > 50 then
                               begin
                                    NextPop[I].Free;
                                    NextPop[I] := TChromozome.CreateRandom;
                               end else Inc(Retries);
                          end;
                          Break;
                     end else Inc(Retries);
                end else Break;
          end;
     end;
     // mutations
     for I := EliteCount to PopSize-1 do
         NextPop[I].DoMutations;

     // done -- now make the next population current, and eval+sort it
     Swap;
     EvalAndSort;

     {
     CurrPop[PopSize-1].Free;
     CurrPop[PopSize-1] := TChromozome.CreateRandom;
     CurrPop[PopSize-2].Free;
     CurrPop[PopSize-2] := TChromozome.CreateValid;
     CurrPop[PopSize-3].Free;
     CurrPop[PopSize-3] := TChromozome.CreateRandomGreedy;
     }

     EvalAndSort;
end;

procedure TPopulation.Swap;
var T: TPopulationGenome;
begin
     T := CurrPop;
     CurrPop := NextPop;
     NextPop := T;
end;

end.

