unit gassgsbeta;

interface

uses gassgsoc, projectdata, individual, helpers;

function RunGASSGSBeta1: TDblArr;
function RunGASSGSBeta2: TDblArr;
function RunGASSGSBeta3: TDblArr;
function RunGASSGSBeta4: TDblArr;
function RunGASSGSBeta5: TDblArr;
function RunGASSGSBeta6: TDblArr;
function RunGASSGSBeta7: TDblArr;
function RunGASSGSBeta8: TDblArr;

function GetBetaName(i: Integer): String;
function GetBetaTexName(i: Integer): String;

type TActivityListBetaIndividual = class(TActivityListIndividual)
  b: JobData;
  crossB: Boolean;
  class var linked, separateCrossover, upperSgs: Boolean;
  class procedure SetOptions(pLinked, pSeparateCrossover, pUpperSgs: Boolean);

  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure Swap(i1, i2: Integer); inline;
protected
  procedure InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer); override;
end;

implementation

uses globals, ssgsmod, profit;

function AllocateIndiv: IIndividual;
begin
  result := TActivityListBetaIndividual.Create;
end;

procedure FillNthPermutation(n: Integer; out digits: Array of Boolean);
var p: Integer;
begin
  for p := 0 to 2 do begin
    if n mod 2 = 1 then digits[p] := True else digits[p] := False;
    n := n div 2;
  end;
end;

function RunGASSGSBeta(i: Integer): TDblArr;
var digits: Array[0..2] of Boolean;
begin
  FillNthPermutation(i, digits);
  TActivityListBetaIndividual.SetOptions(digits[0], digits[1], digits[2]);
  result := TGACore.Run(AllocateIndiv, False);
end;

function B2S(b: Boolean): String; begin if b then result := 'true' else result := 'false'; end;

function GetBetaName(i: Integer): String;
var digits: Array[0..2] of Boolean;
begin
  FillNthPermutation(i, digits);
  result := '(linked=' + B2S(digits[0]) + ' sepcross=' + B2S(digits[1]) + ' upper=' + B2S(digits[2]) + ')';
end;

function GetBetaTexName(i: Integer): String;
var digits: Array[0..2] of Boolean;
begin
  FillNthPermutation(i, digits);
  result := '(\lambda|\beta^{';
  if digits[0] then result := result + 'L' else result := result + 'l';
  if digits[1] then result := result + 'S' else result := result + 's';
  if digits[2] then result := result + 'U' else result := result + 'u';
  result := result + '})';
end;

function RunGASSGSBeta1: TDblArr; begin result := RunGASSGSBeta(0); end;
function RunGASSGSBeta2: TDblArr; begin result := RunGASSGSBeta(1); end;
function RunGASSGSBeta3: TDblArr; begin result := RunGASSGSBeta(2); end;
function RunGASSGSBeta4: TDblArr; begin result := RunGASSGSBeta(3); end;
function RunGASSGSBeta5: TDblArr; begin result := RunGASSGSBeta(4); end;
function RunGASSGSBeta6: TDblArr; begin result := RunGASSGSBeta(5); end;
function RunGASSGSBeta7: TDblArr; begin result := RunGASSGSBeta(6); end;
function RunGASSGSBeta8: TDblArr; begin result := RunGASSGSBeta(7); end;

class procedure TActivityListBetaIndividual.SetOptions(pLinked, pSeparateCrossover, pUpperSgs: Boolean);
begin
  linked := pLinked; // if linked b_j=1 means OC for activity order[j] OTHERWISE for activity j
  separateCrossover := pSeparateCrossover; // different q vals for OPC of order and b
  upperSgs := pUpperSgs; // enforce OC usage on b_j=1 (von oben planung)
end;

procedure TActivityListBetaIndividual.InitializePopulation(var population: IndivArray);
var i, j: Integer;
begin
  inherited InitializePopulation(population);

  for i := 0 to Length(population) - 1 do
  begin
    SetLength(TActivityListBetaIndividual(population[i]).b, ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      TActivityListBetaIndividual(population[i]).b[j] := 0;
  end;
end;

procedure TActivityListBetaIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  if separateCrossover then begin
    crossB := False;
    TActivityListIndividual(daughter).OnePointCrossover(self, TActivityListIndividual(other));
    TActivityListIndividual(son).OnePointCrossover(TActivityListIndividual(other), self);
    crossB := True;
    TActivityListIndividual(daughter).OnePointCrossover(self, TActivityListIndividual(other));
    TActivityListIndividual(son).OnePointCrossover(TActivityListIndividual(other), self);
  end else begin
    TActivityListIndividual(daughter).OnePointCrossover(self, TActivityListIndividual(other));
    TActivityListIndividual(son).OnePointCrossover(TActivityListIndividual(other), self);
  end;
end;

procedure TActivityListBetaIndividual.Mutate;
var i, j: Integer;
begin
  // Mutate order&b
  for i := 2 to ps.numJobs - 1 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[order[i-1], order[i]] = 0) then
      Swap(i, i-1);

  // Mutate b
  for j := 0 to ps.numJobs - 1 do
    if THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE then
      b[j] := 1 - b[j];
end;

function TActivityListBetaIndividual.Fitness: Double;
begin
  TSSGSMod.Solve(order, b, sts, resRem, linked, upperSgs);
  result := TProfit.CalcProfit(sts, resRem);
end;

procedure TActivityListBetaIndividual.Swap(i1, i2: Integer);
var tmp: Integer;
begin
  tmp := order[i1];
  order[i1] := order[i2];
  order[i2] := tmp;

  if linked then begin
    tmp := b[i1];
    b[i1] := b[i2];
    b[i2] := tmp;
  end;
end;

procedure TActivityListBetaIndividual.InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer);
begin
  if not(separateCrossover) then begin
    order[childIndex] := parent.order[parentIndex];
    b[childIndex] := TActivityListBetaIndividual(parent).b[parentIndex];
  end else begin
    if crossB then
      b[childIndex] := TActivityListBetaIndividual(parent).b[parentIndex]
    else
      order[childIndex] := parent.order[parentIndex];
  end;
end;

end.

