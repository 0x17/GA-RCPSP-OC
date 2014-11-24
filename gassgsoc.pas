unit gassgsoc;

interface

uses classes, sysutils, individual, projectdata, operators, globals, ssgsoc, topsort;

function RunGASSGSOC: Double;

type TActivityListIndividual = class(IIndividual)
  order: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
end;

implementation

procedure TActivityListIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  OnePointCrossover(order, TActivityListIndividual(other).order, TActivityListIndividual(daughter).order);
  OnePointCrossover(TActivityListIndividual(other).order, order , TActivityListIndividual(son).order);
end;

procedure TActivityListIndividual.Mutate;
begin
  SwapNeighborhood(order);
end;

function TActivityListIndividual.Fitness: Double;
var sts: JobData;
begin
  result := TSSGSOC.Solve(order, sts, False);
end;

procedure TActivityListIndividual.InitializePopulation(var population: IndivArray);
var
  i, j: Integer;
  prioRules: JobDataArray;
begin
  inherited InitializePopulation(population);

  ProjData.InitPriorityRulesFromFile(ps, prioRules);
  for i := 0 to 12 do
  begin
    SetLength(TActivityListIndividual(population[i]).order, ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      TActivityListIndividual(population[i]).order[j] := prioRules[i, j];
  end;

  for i := 13 to POP_SIZE * 2 - 1 do
    TTopSort.RandomSort(TActivityListIndividual(population[i]).order);
end;

function RunGASSGSOC: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListIndividual.Create;

  population[0].InitializePopulation(population);
  result := RunGA(population, bestIndiv, True);
  FreePopulation(population);
end;

end.

