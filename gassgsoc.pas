unit gassgsoc;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit Benutzung von Zusatzkapazitäten
// Verwendet als Repräsentation (lambda)

interface

uses projectdata, gaactivitylist, ssgsoc, classes, sysutils, parallelfitness, constants, helpers, operators, gacommon;

function RunGASSGSOC(const nps: ProjData; out bestOrder: JobData; doRightShift: Boolean): Double;

implementation

var ps: ProjData;

procedure InitializePopulation(var population: TPop<JobData>); forward;

function RunGASSGSOC(const nps: ProjData; out bestOrder: JobData; doRightShift: Boolean): Double;
var core: TGACore<JobData>;
    procs: TGAProcs<JobData>;
begin
  ps := nps;
  procs.initProc := InitializePopulation;
  procs.crossProc := CrossoverAL;
  if doRightShift then
    procs.fitnessFunc := FitnessSSGSOCRS
  else
    procs.fitnessFunc := FitnessSSGSOC;
  procs.mutateProc := MutateAL;
  core := TGACore<JobData>.Create(procs);
  SetLength(bestOrder, ps.numJobs);
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
