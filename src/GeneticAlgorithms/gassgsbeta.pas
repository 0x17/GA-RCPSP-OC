unit gassgsbeta;

interface

uses gassgsoc, projectdata, individual;

function RunGASSGSBeta: Double;

type TActivityListBetaIndividual = class(TActivityListIndividual)
  b: JobData;
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

uses globals, helpers, ssgsmod, profit, ssgsoc;

function AllocateIndiv: IIndividual;
begin
  result := TActivityListBetaIndividual.Create;
end;

function RunGASSGSBeta: Double;
begin
  result := TGACore.Run(AllocateIndiv, False);
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
  TActivityListIndividual(daughter).OnePointCrossover(self, TActivityListIndividual(other));
  TActivityListIndividual(son).OnePointCrossover(TActivityListIndividual(other), self);
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
var
  tau: JobData;
  j: Integer;
begin
  TSSGSMod.Solve(order, b, sts, resRem);
  result := TProfit.CalcProfit(sts, resRem);
  {SetLength(tau, ps.numJobs);
  for j := 0 to ps.numJobs - 1 do
    if b[j] = 1 then tau[j] := 0 else tau[j] := 99;
  result := TSSGSOC.SolveWithTau(order, tau, sts, resRem);}
end;

procedure TActivityListBetaIndividual.Swap(i1, i2: Integer);
var tmp: Integer;
begin
  tmp := order[i1];
  order[i1] := order[i2];
  order[i2] := tmp;

  tmp := b[i1];
  b[i1] := b[i2];
  b[i2] := tmp;
end;

procedure TActivityListBetaIndividual.InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer);
begin
  order[childIndex] := parent.order[parentIndex];
  b[childIndex] := TActivityListBetaIndividual(parent).b[parentIndex];
end;

end.
