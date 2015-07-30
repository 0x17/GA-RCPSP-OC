unit gassgsoc;

interface

uses projectdata, helpers, individual;

function RunGASSGSOC: TDblArr;

type TActivityListIndividual = class(IIndividual)
  order, sts: JobData;
  resRem: ResourceProfile;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure FillNeighborhood(const origin: IIndividual; var population: IndivArray); override;
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

uses classes, sysutils, globals, ssgsoc, algenerator, bbrsm, rbbrsm;

function AllocateIndividual: IIndividual;
begin
  result := TActivityListIndividual.Create;
end;

function RunGASSGSOC: TDblArr;
begin
  result := TGACore.Run(AllocateIndividual, True);
end;

procedure TActivityListIndividual.FillNeighborhood(const origin: IIndividual; var population: IndivArray);
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
  for i := 0 to Length(population) div 2 - 1 do
    alg.PickSample(TActivityListIndividual(population[i]).order);
  FreeAndNil(alg);

  for i := Length(population) div 2 to Length(population)-1 do
    SetLength(TActivityListIndividual(population[i]).order, ps.numJobs);

  // HACK
  // Force add topological ordering to ensure at least zero profit everywhere -> profits now ratio scaled
  TActivityListIndividual(population[0]).order := Copy(ps.topOrder, 0, ps.numJobs);
  // ENDHACK
end;

procedure TActivityListIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var daughterC, sonC, otherC: TActivityListIndividual;
begin
  daughterC := TActivityListIndividual(daughter);
  sonC := TActivityListIndividual(son);
  otherC := TActivityListIndividual(other);

  daughterC.OnePointCrossover(self, otherC);
  sonC.OnePointCrossover(otherC, self);
end;

procedure TActivityListIndividual.Mutate;
begin
  SwapNeighborhood(order);
end;

function TActivityListIndividual.Fitness: Double;
begin
  result := TSSGSOC.Solve(order, sts, resRem);
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

