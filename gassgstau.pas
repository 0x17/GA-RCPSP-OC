unit gassgstau;

interface

uses classes, sysutils, individual, projectdata, globals, ssgsoc, gassgsoc, helpers;

function RunGASSGSTau: Double;

type TActivityListTauIndividual = class(TActivityListIndividual)
  tau: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure SwapNeighborhood(var lambda, tau: JobData);
  procedure Swap(var lambda, tau: JobData; i1, i2: Integer); inline;
protected
  procedure InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer); override;
end;

implementation

procedure TActivityListTauIndividual.InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer);
begin
  order[childIndex] := parent.order[parentIndex];
  tau[childIndex] := TActivityListTauIndividual(parent).tau[parentIndex];    
end;

procedure TActivityListTauIndividual.Swap(var lambda, tau: JobData; i1, i2: Integer);
var
  tmp, tmp2: Integer;
begin
  tmp := lambda[i1];
  tmp2 := tau[i1];

  lambda[i1] := lambda[i2];
  tau[i1] := tau[i2];

  lambda[i2] := tmp;
  tau[i2] := tmp2;
end;

procedure TActivityListTauIndividual.SwapNeighborhood(var lambda, tau: JobData);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 2 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[lambda[i-1], lambda[i]] = 0) then
      Swap(lambda, tau, i, i-1);
end;

procedure TActivityListTauIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var otherT, daughterT, sonT: TActivityListIndividual;
begin
  otherT := TActivityListIndividual(other);
  daughterT := TActivityListIndividual(daughter);
  sonT := TActivityListIndividual(son);

  daughterT.OnePointCrossover(self, otherT);
  sonT.OnePointCrossover(otherT, self);
end;

procedure TActivityListTauIndividual.Mutate;
var j: Integer;	
begin
  SwapNeighborhood(order, tau);
  for j := 0 to ps.numJobs - 1 do
    if THelper.RandomRangeIncl(0, 99) <= PROB_MUTATE-1 then
      tau[j] := THelper.RandomRangeIncl(0, 99);
end;

function TActivityListTauIndividual.Fitness: Double;
var sts: JobData;
begin
  result := TSSGSOC.SolveWithTau(order, tau, sts);
end;

procedure TActivityListTauIndividual.InitializePopulation(var population: IndivArray);
var i, j: Integer;
begin
  inherited InitializePopulation(population);

  for i := 0 to POP_SIZE * 2 - 1 do
  begin
    SetLength(TActivityListTauIndividual(population[i]).tau, ps.numJobs);
    for j := 0 to ps.numJobs - 1 do
      TActivityListTauIndividual(population[i]).tau[j] := THelper.RandomRangeIncl(0, 99);
  end;
end;

function RunGASSGSTau: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListTauIndividual.Create;

  population[0].InitializePopulation(population);
  result := RunGA(population, bestIndiv, False);
  FreePopulation(population);
end;

end.

