unit gassgsbeta;

interface

uses gassgsoc, individual, projectdata, globals, helpers, ssgsmod, profit;

function RunGASSGSBeta: Double;

type TActivityListBetaIndividual = class(TActivityListIndividual)
  b: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure Swap(i1, i2: Integer); inline;
  procedure OnePointCrossover(const mother, father: TActivityListBetaIndividual; var dghter: IIndividual);
end;

implementation

procedure TActivityListBetaIndividual.OnePointCrossover(const mother, father: TActivityListBetaIndividual; var dghter: IIndividual);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
  daughter: TActivityListBetaIndividual;
begin
  daughter := TActivityListBetaIndividual(dghter);

  len := Length(mother.order);
  q := THelper.RandomRangeIncl(1, len);

  // Ersten q: 0,..,q-1 von Mutter
  for i := 0 to q-1 do
  begin
    daughter.order[i] := mother.order[i];
    daughter.b[i] := mother.b[i];
  end;

  // q,..,len-1 von Vater, falls nicht von Mutter
  k := q;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q-1
    fromMother := false;
    for j := 0 to q-1 do
      if daughter.order[j] = father.order[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q-1 übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughter.order[k] := father.order[i];
      daughter.b[k] := father.b[i];
      inc(k);
    end;
  end;
end;

procedure TActivityListBetaIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  OnePointCrossover(self, TActivityListBetaIndividual(other), daughter);
  OnePointCrossover(TActivityListBetaIndividual(other), self, son);
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
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  TSSGSMod.Solve(ps, order, b, sts, resRemaining);
  result := CalcProfit(sts, resRemaining);
end;

procedure TActivityListBetaIndividual.InitializePopulation(var population: IndivArray);
var i, j: Integer;
begin
  inherited InitializePopulation(population);

  for i := 0 to POP_SIZE * 2 - 1 do
  begin
    SetLength(TActivityListBetaIndividual(population[i]).b, ps.numJobs);
    for j := 0 to ps.numJobs - 1 do
      TActivityListBetaIndividual(population[i]).b[j] := 0;
  end;
end;

function RunGASSGSBeta: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListBetaIndividual.Create;

  population[0].InitializePopulation(population);
  result := RunGA(population, bestIndiv, False);
  FreePopulation(population);
end;

end.

