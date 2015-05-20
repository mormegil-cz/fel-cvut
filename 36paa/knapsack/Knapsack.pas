unit Knapsack;

interface

type
  TKnapsackItem = record
                        Weight: Integer;
                        Cost:   Integer;
                  end;

  TKnapsackItemList = array of TKnapsackItem;

  TKnapsackItemId = 0..62;
  TKnapsackItemSet = set of TKnapsackItemId;

function ComputeWeight(const Knapsack: TKnapsackItemSet; const Items: TKnapsackItemList): Integer;
function ComputeCost(const Knapsack: TKnapsackItemSet; const Items: TKnapsackItemList): Integer;

function SolveKnapsack_BruteForce(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;  // loop
function SolveKnapsack_BruteForce2(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet; // recursive
function SolveKnapsack_BruteForce3(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet; // recursive, in assembler

function SolveKnapsack_BranchBound(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;

function SolveKnapsack_Heur_CbyW(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
function SolveKnapsack_Heur_CbyW_test(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;

function SolveKnapsack_DynaProg(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;

var
 Steps_Counter: Integer;

implementation

var
 Log_BruteForce,
 Log_BranchBound,
 Log_Heur_CbyW,
 Log_Heur_CbyW_test,
 Log_DynaProg: TextFile;

function ComputeWeight(const Knapsack: TKnapsackItemSet; const Items: TKnapsackItemList): Integer;
var I: TKnapsackItemId;
begin
     Result := 0;
     for I := 0 to Length(Items)-1 do
         if I in Knapsack then Inc(Result, Items[I].Weight);
end;

function ComputeCost(const Knapsack: TKnapsackItemSet; const Items: TKnapsackItemList): Integer;
var I: TKnapsackItemId;
begin
     Result := 0;
     for I := 0 to Length(Items)-1 do
         if I in Knapsack then Inc(Result, Items[I].Cost);
end;

function SolveKnapsack_BruteForce(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
var I, Hi: Int64;
    S: TKnapsackItemSet absolute I;
    BS: TKnapsackItemSet;
    BC: Integer;
    //BW: Integer;
    W, C: Integer;
begin
     Steps_Counter := 0;

     //BW := 0;
     BC := 0;
     BS := [];
     I := 0;
     Hi := 1 shl Length(Items) - 1;
     while I < Hi do
     begin
          Inc(Steps_Counter);
          W := ComputeWeight(S, Items);
          C := ComputeCost(S, Items);
          if (W <= MaxWeight) and (C > BC) then
          begin
               //BW := W;
               BC := C;
               BS := S;
          end;
          Inc(I);
     end;
     Result := BS;

     Writeln(Log_BruteForce, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

var
 _Items: TKnapsackItemList;
 _MaxWeight: Integer;
 _N:         Integer;

 _CurrWeight: Integer;
 _CurrCost:   Integer;
 _CurrSol:    TKnapsackItemSet;

 _MaxCost:    Integer;
 _MaxSol:     TKnapsackItemSet;

procedure RecursiveBruteForce(Item: TKnapsackItemId);
var C, W: Integer;
begin
     Inc(Steps_Counter);
     if _CurrCost > _MaxCost then
     begin
          _MaxSol := _CurrSol;
          _MaxCost := _CurrCost;
     end;
     if (Item > _N) or (_CurrWeight = _MaxWeight) then Exit;
     
     RecursiveBruteForce(Item + 1);

     with _Items[Item] do
     begin
          C := Cost;
          W := Weight;
     end;

     Include(_CurrSol, Item);
     Inc(_CurrWeight, W);
     Inc(_CurrCost, C);

     if _CurrWeight <= _MaxWeight then
        RecursiveBruteForce(Item + 1);
        
     Exclude(_CurrSol, Item);
     Dec(_CurrWeight, W);
     Dec(_CurrCost, C);
end;

function SolveKnapsack_BruteForce2(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
begin
     Steps_Counter := 0;
     
     _Items := Items;
     _MaxWeight := MaxWeight;
     _N := Length(Items);

     _MaxCost := 0;
     _MaxSol  := [];

     _CurrWeight := 0;
     _CurrCost := 0;
     _CurrSol := [];

     RecursiveBruteForce(0);

     Result := _MaxSol;
     Writeln(Log_BruteForce, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

const
  MAXN = 64;   // MUST be a multiple of 4 !

var
  KnapsackItemUsed: array[0..MAXN-1] of Byte;  // output
  ItemUsed: array[0..MAXN-1] of Byte;   // temp

{$STACKFRAMES OFF}
procedure BruteForceCore(const Items; N, MaxWeight: Integer); // output in KnapsackItemUsed
// EAX <- @Items
// EDX <- N
// ECX <- MaxWeight
//
// @Items                  ... ESI
// MaxWeight - CurrWeight  ... ECX
// CurrItem                ... EAX
// CurrCost                ... EDX
// BestCost                ... EBX
asm
   PUSH ESI
   PUSH EDI
   PUSH EBX

   MOV  ESI, EAX
   MOV  EBX, ECX

   LEA  EDI, [ItemUsed]
   MOV  ECX, MAXN/4
   XOR  EAX, EAX
   REP  STOSD

   MOV  ECX, EBX
   MOV  EAX, EDX
   XOR  EBX, EBX
   XOR  EDX, EDX

   CALL @MainLoop

   POP  EBX
   POP  EDI
   POP  ESI
   RETN

@MainLoop:
   CMP  EDX, EBX
   JNA  @NotBest

   PUSH ECX
   PUSH ESI
   LEA  ESI, [ItemUsed]
   LEA  EDI, [KnapsackItemUsed]
   MOV  ECX, MAXN/4
   REP  MOVSD
   POP  ESI
   POP  ECX

   MOV  EBX, EDX
@NotBest:

   TEST ECX, ECX
   JZ   @Return

@MainLoop_Entry2:
   INC  DWORD PTR [Steps_Counter]

   DEC  EAX
   JS   @NoMoreItems

   CALL @MainLoop_Entry2       // CurrItem unused

   PUSH ECX
   SUB  ECX, [ESI + 8*EAX]
   JS   @CantUseItem
   PUSH EDX
   MOV  BYTE PTR [ItemUsed + EAX], 1
   ADD  EDX, [ESI + 8*EAX + 4]
   CALL @MainLoop              // CurrItem used
   POP  EDX
   MOV  BYTE PTR [ItemUsed + EAX], 0
@CantUseItem:
   POP  ECX

@NoMoreItems:
   INC  EAX
@Return:
   RETN        
end;

function SolveKnapsack_BruteForce3(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
type TUsedArray = packed array[0..20] of Byte;
var I: Integer;
    P: ^TUsedArray;
begin
     Steps_Counter := 0;
     Assert(Length(Items) <= MAXN);
     BruteForceCore(Items[0], Length(Items), MaxWeight);
     Result := [];
     P := @KnapsackItemUsed;
       for I := 0 to Length(Items)-1 do
           if P^[I] <> 0 then Include(Result, I);

     Writeln(Log_BruteForce, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

var
 _RemCost:    Integer;
 _RemToMax:   Integer;

procedure RecursiveBranchBound(Item: TKnapsackItemId);
var C, W: Integer;
begin
     Inc(Steps_Counter);
     if _CurrCost > _MaxCost then
     begin
          _MaxSol := _CurrSol;
          _MaxCost := _CurrCost;
          _RemToMax := 0;
     end;
     if (Item > _N) or (_CurrWeight = _MaxWeight) or (_RemCost <= _RemToMax) then Exit;

     with _Items[Item] do
     begin
          C := Cost;
          W := Weight;
     end;
     Dec(_RemCost, C);

     RecursiveBranchBound(Item + 1);

     Include(_CurrSol, Item);
     Inc(_CurrWeight, W);
     Inc(_CurrCost, C);
     Dec(_RemToMax, C);

     if _CurrWeight <= _MaxWeight then
        RecursiveBranchBound(Item + 1);

     Exclude(_CurrSol, Item);
     Dec(_CurrWeight, W);
     Dec(_CurrCost, C);
     Inc(_RemCost, C);
     Inc(_RemToMax, C);
end;

function SolveKnapsack_BranchBound(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
var I: Integer;
begin
     Steps_Counter := 0;

     _Items := Items;
     _MaxWeight := MaxWeight;
     _N := Length(Items);

     _MaxCost := 0;
     _MaxSol  := [];

     _CurrWeight := 0;
     _CurrCost := 0;
     _CurrSol := [];

     _RemToMax := 0;
     _RemCost := 0;
     for I := 0 to _N-1 do
         Inc(_RemCost, Items[I].Cost);

     RecursiveBranchBound(0);

     Result := _MaxSol;
     Writeln(Log_BranchBound, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

function SolveKnapsack_Heur_CbyW(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
var I, J, B: TKnapsackItemId;
    W:    Integer;
    BR:   Double;
    Rating: array of Double;
begin
     Steps_Counter := 0;
     SetLength(Rating, Length(Items));
     for I := 0 to Length(Items)-1 do
     begin
          Inc(Steps_Counter);
          with Items[I] do
               Rating[I] := Cost/Weight;
     end;

     Result := [];
     W := 0;
     for I := 0 to Length(Items)-1 do
     begin
          B := 0;
          BR := -1;
          for J := 0 to Length(Items)-1 do
              if Rating[J] > BR then
              begin
                   Inc(Steps_Counter);
                   B := J;
                   BR := Rating[J];
              end;

          Rating[B] := -2;
          if W + Items[B].Weight <= MaxWeight then
          begin
               Include(Result, B);

               Inc(W, Items[B].Weight);
               if W = MaxWeight then Exit;
          end;
     end;

     Writeln(Log_Heur_CbyW, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

function SolveKnapsack_Heur_CbyW_test(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
var I, J, B: TKnapsackItemId;
    W:    Integer;
    BR:   Double;
    Rating: array of Double;
    MC:   Integer;
    SC:   Integer;
begin
     Steps_Counter := 0;
     SetLength(Rating, Length(Items));
     MC := -1;
     for I := 0 to Length(Items)-1 do
         with Items[I] do
         begin
              Inc(Steps_Counter);
              Rating[I] := Cost/Weight;
              if Cost > MC then MC := Cost;
         end;

     Result := [];
     SC := 0;
     W := 0;
     for I := 0 to Length(Items)-1 do
     begin
          B := 0;
          BR := -1;
          for J := 0 to Length(Items)-1 do
              if Rating[J] > BR then
              begin
                   Inc(Steps_Counter);
                   B := J;
                   BR := Rating[J];
              end;

          Rating[B] := -2;
          if W + Items[B].Weight <= MaxWeight then
          begin
               Include(Result, B);

               Inc(W, Items[B].Weight);
               Inc(SC, Items[B].Cost);
               if W = MaxWeight then Break;
          end;
     end;

     // test nejdrazsi veci
     Inc(Steps_Counter);
     if MC > SC then
     begin
          // jenom nejdrazsi vec
          for I := 0 to Length(Items)-1 do
              if Items[I].Cost = MC then
              begin
                   Result := [I];
                   Break;
              end;
     end;
     
     Writeln(Log_Heur_CbyW_test, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

type
  TKnapsack_DynaProg = class
  private
    Wweight: array of array of Integer;
    Wset: array of array of TKnapsackItemSet;
    WKnown: array of array of Boolean;

    MI, MC: Integer;

    Items: TKnapsackItemList;

    procedure Solve(I, C: Integer);
  public
    constructor Create(MaxI, MaxC: Integer; const Items: TKnapsackItemList);

    function GetWweight(I, C: Integer): Integer;
    function GetWset(I, C: Integer): TKnapsackItemSet;
  end;

function SolveKnapsack_DynaProg(MaxWeight: Integer; const Items: TKnapsackItemList; ID: Integer): TKnapsackItemSet;
var I, CSum, N, W: Integer;
    DP: TKnapsack_DynaProg;
begin
  Steps_Counter := 0;
  
  CSum := 0;
  N := Length(Items);
  for I := 0 to N-1 do
      Inc(CSum, Items[I].Cost);

  DP := TKnapsack_DynaProg.Create(N, CSum, Items);

  Result := [];
  for I := CSum downto 0 do
  begin
       W := DP.GetWweight(N, I);
       if (W >= 0) and (W <= MaxWeight) then
       begin
            Result := DP.GetWset(N, I);
            Break;
       end;
  end;

  DP.Free;
  Writeln(Log_DynaProg, ID, #9, Steps_Counter, #9, ComputeCost(Result, Items));
end;

{ TKnapsack_DynaProg }

constructor TKnapsack_DynaProg.Create(MaxI, MaxC: Integer; const Items: TKnapsackItemList);
var C: Integer;
begin
     MI := MaxI;
     MC := MaxC;
     SetLength(Wweight, MaxI+1, MaxC+1);
     SetLength(Wset, MaxI+1, MaxC+1);
     SetLength(Wknown, MaxI+1, MaxC+1);
     for C := 1 to MaxC do
     begin
          Wweight[0][C] := -1;
          WKnown[0][C] := True;
     end;
     Wweight[0][0] := 0;
     WKnown[0][0] := True;
     Wset[0][0] := [];
     Self.Items := Items;
end;

function TKnapsack_DynaProg.GetWset(I, C: Integer): TKnapsackItemSet;
begin
     if (I < 0) or (C < 0) or (I > MI) or (C > MC) then
     begin
          Result := [];
          Exit;
     end;
     if not WKnown[I][C] then Solve(I,C);
     Result := Wset[I][C];
end;

function TKnapsack_DynaProg.GetWweight(I, C: Integer): Integer;
begin
     if (I < 0) or (C < 0) or (I > MI) or (C > MC) then
     begin
          Result := -1;
          Exit;
     end;
     if not WKnown[I][C] then Solve(I,C);
     Result := Wweight[I][C];
end;

procedure TKnapsack_DynaProg.Solve(I, C: Integer);
var W1, W2, W: Integer;
    S1, S2, S: TKnapsackItemSet;
begin
     Assert((I >= 0) and (C >= 0) and (I <= MI) and (C <= MC));
     
     Inc(Steps_Counter);

     W1 := GetWweight(I-1, C);
     S1 := GetWset(I-1, C);
     W2 := GetWweight(I-1, C-Items[I-1].Cost);
     S2 := GetWset(I-1, C-Items[I-1].Cost) + [I-1];
     if W2 >= 0 then Inc(W2, Items[I-1].Weight);
     if W1 < 0 then
     begin
          if W2 < 0 then
          begin
               W := -1;
               S := [];
          end else
          begin
                W := W2;
                S := S2;
          end;
     end else
     begin
          if W2 < 0 then
          begin
               W := W1;
               S := S1;
          end else
          begin
               if W1 <= W2 then
               begin
                    W := W1;
                    S := S1;
               end else
               begin
                    W := W2;
                    S := S2;
               end;
          end;
     end;
     Wweight[I][C] := W;
     Wset[I][C] := S;
     WKnown[I][C] := True;
end;

initialization
     FileMode := 1;
     AssignFile(Log_BruteForce, 'BF.LOG');
     Rewrite(Log_BruteForce);
     AssignFile(Log_BranchBound, 'BB.LOG');
     Rewrite(Log_BranchBound);
     AssignFile(Log_Heur_CbyW, 'H1.LOG');
     Rewrite(Log_Heur_CbyW);
     AssignFile(Log_Heur_CbyW_test, 'H2.LOG');
     Rewrite(Log_Heur_CbyW_test);
     AssignFile(Log_DynaProg, 'DP.LOG');
     Rewrite(Log_DynaProg);
finalization
     CloseFile(Log_DynaProg);
     CloseFile(Log_Heur_CbyW_test);
     CloseFile(Log_Heur_CbyW);
     CloseFile(Log_BranchBound);
     CloseFile(Log_BruteForce);
end.

