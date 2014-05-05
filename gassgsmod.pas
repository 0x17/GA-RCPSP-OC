unit gassgsmod;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit Benutzung von Zusatzkapazitäten
// Verwendet als Repräsentation (lambda, 01010010...)

interface

uses projectdata, gaactivitylist, gab, classes, sysutils, math, parallelfitness, constants, ssgsmod, profit, helpers, operators, gacommon;

function RunGASSGSMod(const nps: ProjData; out sts: JobData; best: TALBPair): Double;

implementation

var ps: ProjData;
    population: TPop<TALBPair>;

procedure InitializePopulation(var population: TPop<TALBPair>); forward;

procedure CrossoverALB(const mother, father: TALBPair; var daughter, son: TALBPair);
begin
    CrossoverAL(mother.order, father.order, daughter.order, son.order);
    CrossoverB(mother.b, father.b, daughter.b, son.b);
end;

procedure MutateALB(var individual: TALBPair);
begin
  MutateAL(individual.order);
  MutateB(individual.b);
end;

function RunGASSGSMod(const nps: ProjData; out sts: JobData; best: TALBPair): Double;
var i: Integer;
    core: TGACore<TALBPair>;
    procs: TGAProcs<TALBPair>;
begin
  ps := nps;
  procs.initProc := InitializePopulation;
  procs.crossProc := CrossoverALB;
  procs.fitnessFunc := FitnessSSGSMod;
  procs.mutateProc := MutateALB;
  core := TGACore<TALBPair>.Create(procs);
  result := core.Run(sts, best);
  FreeAndNil(core);
end;

procedure InitializePopulation(var population: TPop<TALBPair>);
var
  i: Integer;
  prioRules: JobDataArray;
  j: Integer;
begin
  SetProjectStructureAL(ps);
  SetProjectStructureB(ps);

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
    InitB(population[i].b);
end;

end.
