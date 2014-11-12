unit gassgszt;

interface

uses classes, sysutils, individual, projectdata, operators, globals, ssgs, helpers, profit, gassgsoc;

type TALOCPair = record
  order: JobData;
  oc: ResourceProfile;
end;

function RunGASSGSZT: Double;

type TActivityListOCIndividual = class(TActivityListIndividual)
  oc: ResourceProfile;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
end;

implementation

procedure CrossoverOC(const mother, father: ResourceProfile; var daughter: ResourceProfile);
var r, t, q: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
    begin
      q := Random(2);
      if q = 0 then
        daughter[r,t] := mother[r,t]
      else
        daughter[r,t] := father[r,t];
    end;
end;

procedure TActivityListOCIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
begin
  OnePointCrossover(order, TActivityListOCIndividual(other).order, TActivityListOCIndividual(daughter).order);
  OnePointCrossover(TActivityListOCIndividual(other).order, order, TActivityListOCIndividual(son).order);
  CrossoverOC(oc, TActivityListOCIndividual(other).oc, TActivityListOCIndividual(daughter).oc);
  CrossoverOC(TActivityListOCIndividual(other).oc, oc, TActivityListOCIndividual(son).oc);
end;

procedure MutateOC(var oc: ResourceProfile);
var
  r, t: Integer;
begin
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods - 1 do
      if THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE then
        oc[r,t] := THelper.RandomRangeIncl(0, ps.zmax[r]);
end;

procedure TActivityListOCIndividual.Mutate;
begin
  SwapNeighborhood(order);
  MutateOC(oc);
end;

function TActivityListOCIndividual.Fitness: Double;
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  TSSGS.Solve(order, oc, sts, resRemaining);
  result := CalcProfit(sts, resRemaining);
end;

procedure TActivityListOCIndividual.InitializePopulation(var population: IndivArray);
var i, r, t: Integer;
begin
  inherited InitializePopulation(population);

  for i := 0 to POP_SIZE * 2 - 1 do
  begin
    SetLength(TActivityListOCIndividual(population[i]).oc, ps.numRes, ps.numPeriods);
    for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
        TActivityListOCIndividual(population[i]).oc[r, t] := THelper.RandomRangeIncl(0, ps.zmax[r]);
  end;
end;

function RunGASSGSZT: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListOCIndividual.Create;

  population[0].InitializePopulation(population);
  result := RunGA(population, bestIndiv, False);
  FreePopulation(population);
end;

end.
