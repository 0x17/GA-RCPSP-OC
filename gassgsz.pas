unit gassgsz;

interface

uses classes, sysutils, individual, projectdata, operators, globals, ssgs, topsort, helpers, profit;

type TALZPair = record
  order: JobData;
  z: ResData;
end;

function RunGASSGSZ(out best: TALZPair): Double;

type TActivityListZIndividual = class(IIndividual)
  order: JobData;
  z: ResData;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
end;

procedure InitializeActivityListZPopulation(var population: IndivArray);

implementation

procedure CrossoverOC(const mother, father: ResData; var daughter: ResData);
var r, q: Integer;
begin
  for r := 0 to ps.numRes - 1 do
  begin
    q := Random(2);
    if q = 0 then
      daughter[r] := mother[r]
    else
      daughter[r] := father[r];
  end;
end;

procedure TActivityListZIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  OnePointCrossover(order, TActivityListZIndividual(other).order, TActivityListZIndividual(daughter).order);
  OnePointCrossover(TActivityListZIndividual(other).order, order, TActivityListZIndividual(son).order);
  CrossoverOC(z, TActivityListZIndividual(other).z, TActivityListZIndividual(daughter).z);
  CrossoverOC(TActivityListZIndividual(other).z, z, TActivityListZIndividual(son).z);
end;

procedure MutateOC(var z: ResData);
var
  r, q: Integer;
begin
  for r := 0 to ps.numRes-1 do
  begin
    q := THelper.RandomRangeIncl(1, 100);
    if q <= PROB_MUTATE then
    begin
      if Random(2) = 0 then
        inc(z[r])
      else
        dec(z[r]);

      if z[r] < 0 then z[r] := 0;
      if z[r] > ps.zmax[r] then z[r] := ps.zmax[r];
    end;
  end;
end;

procedure TActivityListZIndividual.Mutate;
begin
  SwapNeighborhood(order);
  MutateOC(z);
end;

function TActivityListZIndividual.Fitness: Double;
var
  sts: JobData;
  zfilled, resRemaining: ResourceProfile;
  r, t: Integer;
begin
  SetLength(zfilled, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      zfilled[r,t] := z[r];

  TSSGS.Solve(order, zfilled, sts, resRemaining);
  result := CalcProfit(sts, resRemaining);
end;

procedure InitializeActivityListZPopulation(var population: IndivArray);
var
  i, j: Integer;
  prioRules: JobDataArray;
  r: Integer;
begin
  ProjData.InitPriorityRulesFromFile(ps, prioRules);
  for i := 0 to 12 do
  begin
    SetLength(TActivityListZIndividual(population[i]).order, ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      TActivityListZIndividual(population[i]).order[j] := prioRules[i, j];
  end;

  for i := 13 to POP_SIZE * 2 - 1 do
    TTopSort.RandomSort(TActivityListZIndividual(population[i]).order);

  for i := 0 to POP_SIZE * 2 - 1 do
  begin
    SetLength(TActivityListZIndividual(population[i]).z, ps.numRes);
    for r := 0 to ps.numRes - 1 do
      TActivityListZIndividual(population[i]).z[r] := THelper.RandomRangeIncl(0, ps.zmax[r]);
  end;
end;

function RunGASSGSZ(out best: TALZPair): Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListZIndividual.Create;

  InitializeActivityListZPopulation(population);
  result := RunGA(population, bestIndiv, False);

  best.order := (bestIndiv as TActivityListZIndividual).order;
  best.z := (bestIndiv as TActivityListZIndividual).z;
  FreePopulation(population);
end;

end.
