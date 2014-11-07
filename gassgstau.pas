unit gassgstau;

interface

uses classes, sysutils, individual, projectdata, operators, globals, ssgsoc, topsort, gassgsoc, helpers;

type TALTauPair = record
  order, tau: JobData;
end;

function RunGASSGSTau(out best: TALTauPair): Double;

type TActivityListTauIndividual = class(TActivityListIndividual)
  tau: JobData;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
end;

procedure InitializeActivityListTauPopulation(var population: IndivArray);

implementation

procedure TActivityListTauIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var otherT, daughterT, sonT: TActivityListTauIndividual;
begin
  otherT := TActivityListTauIndividual(other);
  daughterT := TActivityListTauIndividual(daughter);
  sonT := TActivityListTauIndividual(son);

  OnePointCrossover(order, tau, otherT.order, otherT.tau, daughterT.order, daughterT.tau);
  OnePointCrossover(otherT.order, otherT.tau, order, tau, sonT.order, sonT.tau);
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

procedure InitializeActivityListTauPopulation(var population: IndivArray);
var
  i, j: Integer;
  prioRules: JobDataArray;
begin
  ProjData.InitPriorityRulesFromFile(ps, prioRules);
  for i := 0 to 12 do
  begin
    SetLength(TActivityListTauIndividual(population[i]).order, ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      TActivityListTauIndividual(population[i]).order[j] := prioRules[i, j];
  end;

  for i := 13 to POP_SIZE * 2 - 1 do
    TTopSort.RandomSort(TActivityListTauIndividual(population[i]).order);
	
	for i := 0 to POP_SIZE * 2 - 1 do
	begin
		SetLength(TActivityListTauIndividual(population[i]).tau, ps.numJobs);
		for j := 0 to ps.numJobs - 1 do
			TActivityListTauIndividual(population[i]).tau[j] := THelper.RandomRangeIncl(0, 99);
	end;
end;

function RunGASSGSTau(out best: TALTauPair): Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListTauIndividual.Create;

  InitializeActivityListTauPopulation(population);
  result := RunGA(population, bestIndiv, (*True*) False);

  best.order := (bestIndiv as TActivityListTauIndividual).order;
  best.tau := (bestIndiv as TActivityListTauIndividual).tau;

  FreePopulation(population);
end;

end.

