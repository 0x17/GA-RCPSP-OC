unit gassgs;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit benutzung von Zusaktzpazitäten
// Verwendet als Repräsentation (lambda, z_rt)

interface

uses projectdata, gaactivitylist, classes, sysutils, math, parallelfitness, constants, ssgs, profit, helpers, operators, gacommon;

type TALOCPair = record
  order: JobData;
  oc: ResourceProfile;
end;

function RunGASSGS(const nps: ProjData; best: TALOCPair): Double;

implementation

var ps: ProjData;

procedure InitOC(out oc: ResourceProfile);
var r, t: Integer;
begin
  SetLength(oc, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
      oc[r,t] := RandomRange(0, ps.zmax[r]);
end;

procedure MutateOC(var oc: ResourceProfile);
var r, t: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
      if RandomRange(1, 100) <= 3 then
      begin
        if RandomRange(1,2) = 1 then
          inc(oc[r,t])
        else
          dec(oc[r,t]);

        if oc[r,t] < 0 then oc[r,t] := 0;
        if oc[r,t] > ps.zmax[r] then oc[r,t] := ps.zmax[r];
      end;
end;

procedure CrossoverOC(const order, other: ResourceProfile; var daughter, son: ResourceProfile);
begin
  OnePointCrossover(order, other, daughter, ps.numRes, ps.numPeriods);
  OnePointCrossover(other, order, son, ps.numRes, ps.numPeriods);
end;

function FitnessSSGS(const pair: TALOCPair): Double;
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  Solve(ps, pair.order, pair.oc, sts, resRemaining);
  result := CalcProfit(ps, sts, resRemaining);
end;

procedure InitializePopulation(var population: TPop<TALOCPair>); forward;

procedure CrossoverALOC(const mother, father: TALOCPair; var daughter, son: TALOCPair);
begin
    CrossoverAL(mother.order, father.order, daughter.order, son.order);
    CrossoverOC(mother.oc, father.oc, daughter.oc, son.oc);
end;

procedure MutateALOC(var individual: TALOCPair);
begin
  MutateAL(individual.order);
  MutateOC(individual.oc);
end;

function RunGASSGS(const nps: ProjData; best: TALOCPair): Double;
var core: TGACore<TALOCPair>;
    procs: TGAProcs<TALOCPair>;
begin
  ps := nps;
  procs.initProc := InitializePopulation;
  procs.crossProc := CrossoverALOC;
  procs.fitnessFunc := FitnessSSGS;
  procs.mutateProc := MutateALOC;
  core := TGACore<TALOCPair>.Create(procs);
  result := core.Run(best);
  FreeAndNil(core);
end;

procedure InitializePopulation(var population: TPop<TALOCPair>);
var
  i: Integer;
  prioRules: JobDataArray;
  j: Integer;
begin
  SetProjectStructureAL(ps);

  ProjData.InitPriorityRulesFromFile(ps, prioRules);
  for i := 0 to 12 do
  begin
    SetLength(population[i].order, ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      population[i].order[j] := prioRules[i, j];
  end;

  for i := 13 to POP_SIZE * 2 - 1 do
      InitAL(population[i].order);

  for i := 0 to POP_SIZE * 2 - 1 do
      InitOC(population[i].oc);
end;

end.
