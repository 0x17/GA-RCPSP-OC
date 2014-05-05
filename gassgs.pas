unit gassgs;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit benutzung von Zusaktzpazitäten
// Verwendet als Repräsentation (lambda, z_rt)

interface

uses projectdata, gaactivitylist, gaovercapacity, classes, sysutils, math, parallelfitness, constants, ssgs, profit, helpers, operators, gacommon;

function RunGASSGS(const nps: ProjData; out sts: JobData; best: TALOCPair): Double;

implementation

var ps: ProjData;
    population: TPop<TALOCPair>;

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

function RunGASSGS(const nps: ProjData; out sts: JobData; best: TALOCPair): Double;
var i: Integer;
    core: TGACore<TALOCPair>;
    procs: TGAProcs<TALOCPair>;
begin
  ps := nps;
  procs.initProc := InitializePopulation;
  procs.crossProc := CrossoverALOC;
  procs.fitnessFunc := FitnessSSGS;
  procs.mutateProc := MutateALOC;
  core := TGACore<TALOCPair>.Create(procs);
  result := core.Run(sts, best);
  FreeAndNil(core);
end;

procedure InitializePopulation(var population: TPop<TALOCPair>);
var
  i: Integer;
  prioRules: JobDataArray;
  j: Integer;
begin
  SetProjectStructureAL(ps);
  SetProjectStructureOC(ps);

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
