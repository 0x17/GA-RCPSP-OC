unit gassgsbeta;

interface

uses gassgsoc, projectdata, individual;

function RunGASSGSBeta1: Double;
function RunGASSGSBeta2: Double;
function RunGASSGSBeta3: Double;
function RunGASSGSBeta4: Double;
function RunGASSGSBeta5: Double;
function RunGASSGSBeta6: Double;
function RunGASSGSBeta7: Double;
function RunGASSGSBeta8: Double;

type TActivityListBetaIndividual = class(TActivityListIndividual)
  b: JobData;
  class var linked, separateCrossover, upperSgs: Boolean;
  class procedure SetOptions(pLinked, pSeparateCrossover, pUpperSgs: Boolean);

  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure Swap(i1, i2: Integer); inline;
  procedure OPC(const mother, father: JobData; var daughter: JobData);
protected
  procedure InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer); override;
end;

implementation

uses globals, helpers, ssgsmod, profit;

function AllocateIndiv: IIndividual;
begin
  result := TActivityListBetaIndividual.Create;
end;

function RunGASSGSBeta(i: Integer): Double;
  procedure FillNthPermutation(n: Integer; out digits: Array of Boolean);
  var p: Integer;
  begin
    for p := 0 to 2 do begin
      if n mod 2 = 1 then digits[p] := True else digits[p] := False;
      n := n div 2;
    end;
  end;

var digits: Array[0..2] of Boolean;
begin
  TActivityListBetaIndividual.SetOptions(digits[0], digits[1], digits[2]);
  result := TGACore.Run(AllocateIndiv, False);
end;

function RunGASSGSBeta1: Double; begin result := RunGASSGSBeta(0); end;
function RunGASSGSBeta2: Double; begin result := RunGASSGSBeta(1); end;
function RunGASSGSBeta3: Double; begin result := RunGASSGSBeta(2); end;
function RunGASSGSBeta4: Double; begin result := RunGASSGSBeta(3); end;
function RunGASSGSBeta5: Double; begin result := RunGASSGSBeta(4); end;
function RunGASSGSBeta6: Double; begin result := RunGASSGSBeta(5); end;
function RunGASSGSBeta7: Double; begin result := RunGASSGSBeta(6); end;
function RunGASSGSBeta8: Double; begin result := RunGASSGSBeta(7); end;

class procedure TActivityListBetaIndividual.SetOptions(pLinked, pSeparateCrossover, pUpperSgs: Boolean);
begin
  linked := pLinked; // if linked b_j=1 means OC for activity order[j] OTHERWISE for activity j
  upperSgs := pUpperSgs; // enforce OC usage on b_j=1 (von oben planung)
  separateCrossover := pSeparateCrossover; // different q vals for OPC of order and b
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

procedure TActivityListBetaIndividual.OPC(const mother, father: JobData; var daughter: JobData);
var
  q, i, j, k, len: Integer;
  fromOther: Boolean;
begin
  len := Length(mother);

  q := THelper.RandomRangeIncl(1, len);
  for i := 0 to q - 1 do
    daughter[i] := mother[i];

  k := q;
  // Probiere alle von Elternteil
  for i := 0 to len - 1 do
  begin
    // Nehme nur, falls nicht bereits in 0,..,q-1 von anderem Elternteil
    fromOther := False;
    for j := 0 to q - 1 do
      if daughter[j] = mother[i] then
        fromOther := True;

    if not(fromOther) then begin
      daughter[k] := father[i];
      inc(k);
    end;
  end;

end;

procedure TActivityListBetaIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  if separateCrossover then begin
    OPC(self.order, TActivityListBetaIndividual(other).order, TActivityListBetaIndividual(daughter).order);
    OPC(self.b, TActivityListBetaIndividual(other).b, TActivityListBetaIndividual(daughter).b);

    OPC(TActivityListBetaIndividual(other).order, self.order, TActivityListBetaIndividual(son).order);
    OPC(TActivityListBetaIndividual(other).b, self.b, TActivityListBetaIndividual(son).b);
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
  order[childIndex] := parent.order[parentIndex];
  b[childIndex] := TActivityListBetaIndividual(parent).b[parentIndex];
end;

end.

