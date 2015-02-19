unit gassgszt;

interface

uses projectdata, individual, gassgsoc;

function RunGASSGSZT: Double;

type TActivityListOCIndividual = class(TActivityListIndividual)
  oc: ResourceProfile;
  fts: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure MutateOC;
  procedure OnePointCrossoverSmart(const mother, father: TActivityListOCIndividual; var daughter: TActivityListOCIndividual);
end;

implementation

uses classes, sysutils, globals, ssgs, helpers, profit;

function AllocateIndividual: IIndividual;
begin
  result := TActivityListOCIndividual.Create;
end;

function RunGASSGSZT: Double;
begin
  result := TGACore.Run(AllocateIndividual, False);
end;

procedure TActivityListOCIndividual.InitializePopulation(var population: IndivArray);
var i, r, t: Integer;
begin
  inherited InitializePopulation(population);

  // Initialize parents
  for i := 0 to Length(population) div 2 - 1 do
  begin
    SetLength(TActivityListOCIndividual(population[i]).oc, ps.numRes, ps.numPeriods);
    for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
        TActivityListOCIndividual(population[i]).oc[r, t] := THelper.RandomRangeIncl(0, ps.zmax[r]);

    SetLength(TActivityListOCIndividual(population[i]).fts, ps.numJobs);

    population[i].Fitness;
  end;

  // Initialize children without information
  for i := Length(population) div 2 to Length(population) - 1 do begin
    SetLength(TActivityListOCIndividual(population[i]).oc, ps.numRes, ps.numPeriods);
    SetLength(TActivityListOCIndividual(population[i]).fts, ps.numJobs);
  end
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

procedure TActivityListOCIndividual.Mutate;
begin
  SwapNeighborhood(order);
  MutateOC;
end;

function TActivityListOCIndividual.Fitness: Double;
var j: Integer;
begin
  TSSGS.Solve(order, oc, sts, resRem);
  result := TProfit.CalcProfit(sts, resRem);

  for j := 0 to ps.numJobs-1 do
    fts[j] := sts[j] + ps.durations[j];
end;

procedure TActivityListOCIndividual.MutateOC;
var r, t: Integer;
begin
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods - 1 do
      if THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE then
        oc[r,t] := THelper.RandomRangeIncl(0, ps.zmax[r]);
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

(*procedure RandomCrossoverOC(const mother, father: TActivityListOCIndividual; var daughter: TActivityListOCIndividual);
var r, t, q: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
    begin
      q := Random(2);
      if q = 0 then
        daughter.oc[r,t] := mother.oc[r,t]
      else
        daughter.oc[r,t] := father.oc[r,t];
    end;
end;

procedure OnePointCrossoverOC(const mother, father: TActivityListOCIndividual; var daughter: TActivityListOCIndividual);
var q, r, t: Integer;
begin
  q := THelper.RandomRangeIncl(0, ps.numPeriods-1);
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
        if t < q then
           daughter.oc[r,t] := mother.oc[r,t]
        else
           daughter.oc[r,t] := father.oc[r,t];
end;*)

end.
