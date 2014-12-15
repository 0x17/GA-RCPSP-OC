unit gassgsoc;

interface

uses classes, sysutils, individual, projectdata, globals, ssgsoc, topsort, helpers;

function RunGASSGSOC: Double;

type TActivityListIndividual = class(IIndividual)
  order: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
protected
  procedure Swap(var lambda: JobData; i1, i2: Integer); inline;
  procedure SwapNeighborhood(var lambda: JobData);
  procedure OnePointCrossover(const mother, father: JobData; var daughter: JobData);
end;

implementation

procedure TActivityListIndividual.OnePointCrossover(const mother, father: JobData; var daughter: JobData);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
begin
  len := Length(mother);
  q := THelper.RandomRangeIncl(1, len);

  // Ersten q1: 0,..,q-1 von Mutter
  for i := 0 to q-1 do
    daughter[i] := mother[i];

  // q,..,len-1 von Vater, falls nicht von Mutter
  k := q;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q-1
    fromMother := false;
    for j := 0 to q-1 do
      if daughter[j] = father[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q-1 übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughter[k] := father[i];
      inc(k);
    end;
  end;
end;

procedure TActivityListIndividual.Swap(var lambda: JobData; i1, i2: Integer);
var
  tmp: Integer;
begin
  tmp := lambda[i1];
  lambda[i1] := lambda[i2];
  lambda[i2] := tmp;
end;

procedure TActivityListIndividual.SwapNeighborhood(var lambda: JobData);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 2 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[lambda[i-1], lambda[i]] = 0) then
      Swap(lambda, i, i-1);
end;

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

