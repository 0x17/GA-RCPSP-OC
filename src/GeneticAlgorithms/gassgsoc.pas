unit gassgsoc;

interface

uses classes, sysutils, individual, projectdata, globals, ssgsoc, helpers, algens;

function RunGASSGSOC: Double;

type TActivityListIndividual = class(IIndividual)
  order: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;

  function OnePointCrossover(const mother, father: TActivityListIndividual): Integer;

protected
  procedure Swap(var lambda: JobData; i1, i2: Integer); inline;
  procedure SwapNeighborhood(var lambda: JobData);

  procedure InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer); virtual;
  procedure InheritFirst(q: Integer; const parent: TActivityListIndividual); inline;
  procedure InheritRemaining(q: Integer; const parent: TActivityListIndividual); inline;
end;

implementation

procedure FillNeighbourhood(const origin: IIndividual; var population: IndivArray); forward;

function RunGASSGSOC: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE * 2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListIndividual.Create;

  population[0].InitializePopulation(population);
  {result := }RunGA(population, bestIndiv, True);

  // Populationsgröße halbieren
  for i := POP_SIZE to POP_SIZE * 2 - 1 do
    population[i].Free;
  SetLength(population, POP_SIZE);

  FillNeighbourhood(bestIndiv, population);
  result := RunGA(population, bestIndiv, True);

  FreePopulation(population);
end;

procedure FillNeighbourhood(const origin: IIndividual; var population: IndivArray);
var
  alg: IALGenerator;
  i: Integer;
begin
  alg := BBRSMNeighbourhood.Create(TActivityListIndividual(origin).order);
  TActivityListIndividual(population[0]).order := Copy(TActivityListIndividual(origin).order, 0, ps.numJobs);
  for i := 1 to Length(population) div 2 - 1 do
    alg.PickSample(TActivityListIndividual(population[i]).order);
  FreeAndNil(alg);
end;

procedure TActivityListIndividual.InitializePopulation(var population: IndivArray);
var
  i: Integer;
  alg: IALGenerator;
begin
  inherited InitializePopulation(population);

  alg := RBBRSGenerator.Create;
  for i := 0 to Length(population) - 1 do
    alg.PickSample(TActivityListIndividual(population[i]).order);
  FreeAndNil(alg);
end;

procedure TActivityListIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  TActivityListIndividual(daughter).OnePointCrossover(self, TActivityListIndividual(other));
  TActivityListIndividual(son).OnePointCrossover(TActivityListIndividual(other), self);
end;

procedure TActivityListIndividual.Mutate;
begin
  SwapNeighborhood(order);
end;

function TActivityListIndividual.Fitness: Double;
var sts: JobData;
begin
  result := TSSGSOC.Solve(order, sts);
end;

function TActivityListIndividual.OnePointCrossover(const mother, father: TActivityListIndividual): Integer;
begin
  result := THelper.RandomRangeIncl(1, Length(mother.order));
  // Ersten q: 0,..,q-1 von Mutter
  InheritFirst(result, mother);
  // q,..,len-1 von Vater, falls nicht von Mutter
  InheritRemaining(result, father);
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

procedure TActivityListIndividual.InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer);
begin
  order[childIndex] := parent.order[parentIndex];
end;

procedure TActivityListIndividual.InheritFirst(q: Integer; const parent: TActivityListIndividual);
var i: Integer;
begin
  for i := 0 to q - 1 do
    InheritGene(parent, i, i);
end;

procedure TActivityListIndividual.InheritRemaining(q: Integer; const parent: TActivityListIndividual);
var
   i, j, k, len: Integer;
   fromOther: Boolean;
begin
  len := Length(parent.order);
  k := q;
  // Probiere alle von Elternteil
  for i := 0 to len - 1 do
  begin
    // Nehme nur, falls nicht bereits in 0,..,q-1 von anderem Elternteil
    fromOther := False;
    for j := 0 to q - 1 do
      if order[j] = parent.order[i] then
        fromOther := True;

    if not(fromOther) then begin
      InheritGene(parent, i, k);
      inc(k);
    end;
  end;
end;

end.
