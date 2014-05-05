unit gassgsoc;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit Benutzung von Zusatzkapazitäten
// Verwendet als Repräsentation (lambda)

interface

uses projectdata, gaactivitylist, ssgsoc, classes, sysutils, math, parallelfitness, constants, helpers, operators, gacommon;

function RunGASSGSOC(const nps: ProjData; bestOrder: JobData): Double;

implementation

var ps: ProjData;

procedure InitializePopulation(var population: TPop<JobData>); forward;

function RunGASSGSOC(const nps: ProjData; bestOrder: JobData): Double;
var core: TGACore<JobData>;
    procs: TGAProcs<JobData>;
begin
  ps := nps;
  procs.initProc := InitializePopulation;
  procs.crossProc := CrossoverAL;
  procs.fitnessFunc := FitnessSSGSOC;
  procs.mutateProc := MutateAL;
  core := TGACore<JobData>.Create(procs);
  result := core.Run(bestOrder);
  FreeAndNil(core);
end;

procedure InitializePopulation(var population: TPop<JobData>);
var
  i: Integer;
  prioRules: JobDataArray;
  j: Integer;
begin
  SetProjectStructureAL(ps);

  ProjData.InitPriorityRulesFromFile(ps, prioRules);
  for i := 0 to 12 do
  begin
    SetLength(population[i], ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      population[i][j] := prioRules[i, j];
  end;

  for i := 13 to POP_SIZE * 2 - 1 do
    InitAL(population[i]);
end;

end.
