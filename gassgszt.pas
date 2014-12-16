unit gassgszt;

interface

uses classes, sysutils, individual, projectdata, globals, ssgs, helpers, profit, gassgsoc;

function RunGASSGSZT: Double;

type TActivityListOCIndividual = class(TActivityListIndividual)
  oc: ResourceProfile;
  fts: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure RandomCrossoverOC(const mother, father: ResourceProfile; var daughter: ResourceProfile);
  procedure OnePointCrossoverOC(const mother, father: ResourceProfile; var daughter: ResourceProfile);
  procedure OnePointCrossoverSmart(const mother, father: TActivityListOCIndividual; var daughter: TActivityListOCIndividual);
end;

implementation

procedure TActivityListOCIndividual.RandomCrossoverOC(const mother, father: ResourceProfile; var daughter: ResourceProfile);
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

procedure TActivityListOCIndividual.OnePointCrossoverOC(const mother, father: ResourceProfile; var daughter: ResourceProfile);
var q, r, t: Integer;
begin
  q := THelper.RandomRangeIncl(0, ps.numPeriods-1);
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
      begin
        if t < q then
           daughter[r,t] := mother[r,t]
        else
           daughter[r,t] := father[r,t];
      end;
end;

procedure TActivityListOCIndividual.OnePointCrossoverSmart(const mother, father: TActivityListOCIndividual; var daughter: TActivityListOCIndividual);
var j, q, maxFt, r, t: Integer;
begin
  // Crossover von Aktivitätenliste
  q := TActivityListIndividual(daughter).OnePointCrossover(TActivityListIndividual(mother), TActivityListIndividual(father));

  // Crossover von Zrt
  maxFt := 0;
  for j := 0 to q-1 do
    if mother.fts[j] > maxFt then
      maxFt := mother.fts[j];

  for t := 0 to ps.numPeriods-1 do
    for r := 0 to ps.numRes-1 do
      if t <= maxFt then
        daughter.oc[r,t] := mother.oc[r,t]
      else
        daughter.oc[r,t] := father.oc[r,t];
end;

procedure TActivityListOCIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var otherC, daughterC, sonC: TActivityListOCIndividual;
begin
  otherC := TActivityListOCIndividual(other);
  daughterC := TActivityListOCIndividual(daughter);
  sonC := TActivityListOCIndividual(son);

  OnePointCrossoverSmart(self, otherC, daughterC);
  OnePointCrossoverSmart(otherC, self, sonC);
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
  j: Integer;
begin
  TSSGS.Solve(order, oc, sts, resRemaining);
  result := CalcProfit(sts, resRemaining);

  for j := 0 to ps.numJobs-1 do
    fts[j] := sts[j] + ps.durations[j];
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

    SetLength(TActivityListOCIndividual(population[i]).fts, ps.numJobs);
    population[i].Fitness;
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
