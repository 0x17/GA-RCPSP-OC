unit gassgstau;

interface

uses classes, sysutils, individual, projectdata, operators, globals, ssgsoc, gassgsoc, helpers;

type TALTauPair = record
  order, tau: JobData;
end;

function RunGASSGSTau: Double;

type TActivityListTauIndividual = class(TActivityListIndividual)
  tau: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
end;

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
  result := RunGA(population, bestIndiv, (*True*) False);
  FreePopulation(population);
end;

end.

