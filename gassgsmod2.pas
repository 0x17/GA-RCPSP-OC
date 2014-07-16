unit gassgsmod2;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit Benutzung von Zusatzkapazitäten
// Verwendet als Repräsentation (lambda, 01010010...)

interface

uses projectdata, gaactivitylist, classes, sysutils, parallelfitness, constants, ssgsmod, profit, helpers, operators, gacommon, gassgsmod;

function RunGASSGSMod2(const nps: ProjData; out best: TALBPair): Double;

implementation

var ps: ProjData;

procedure InitializePopulation(var population: TPop<TALBPair>); forward;

procedure InitB(out b: JobData);
var j: Integer;
begin
  SetLength(b, ps.numJobs);
  for j := 0 to ps.numJobs - 1 do
    b[j] := 0;//RandomRangeIncl(0, 1);
end;

procedure MutateB(var b: JobData);
var j: Integer;
begin
  for j := 0 to ps.numJobs - 1 do
      if RandomRangeIncl(1, 100) <= PROB_MUTATE then
         b[j] := 1 - b[j];
end;

function FitnessSSGSMod(const individual: TALBPair): Double;
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  SolveMod(ps, individual.order, individual.b, sts, resRemaining);
  result := CalcProfit(ps, sts, resRemaining);
end;

procedure CrossoverALB(const mother, father: TALBPair; var daughter, son: TALBPair);
begin
  OnePointCrossover(mother, father, daughter);
  OnePointCrossover(father, mother, son);
  //TwoPointCrossover(mother, father, daughter);
  //TwoPointCrossover(father, mother, son);
end;

procedure SwapALB(var pair: TALBPair; i1, i2: Integer);
var
  tmp: Integer;
begin
  tmp := pair.order[i1];
  pair.order[i1] := pair.order[i2];
  pair.order[i2] := tmp;

  tmp := pair.b[i1];
  pair.b[i1] := pair.b[i2];
  pair.b[i2] := tmp;
end;

procedure SwapNeighborhoodALB(var pair: TALBPair);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 1 do
    if (RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[pair.order[i-1], pair.order[i]] = 0) then
      SwapALB(pair, i, i-1);
end;

procedure MutateALB(var individual: TALBPair);
begin
  SwapNeighborhoodALB(individual);
  MutateB(individual.b);
end;

function RunGASSGSMod2(const nps: ProjData; out best: TALBPair): Double;
var core: TGACore<TALBPair>;
    procs: TGAProcs<TALBPair>;
begin
  ps := nps;
  procs.initProc := InitializePopulation;
  procs.crossProc := CrossoverALB;
  procs.fitnessFunc := FitnessSSGSMod;
  procs.mutateProc := MutateALB;
  core := TGACore<TALBPair>.Create(procs);
  SetLength(best.order, ps.numJobs);
  SetLength(best.b, ps.numJobs);
  result := core.Run(best, true);
  FreeAndNil(core);
end;

procedure InitializePopulation(var population: TPop<TALBPair>);
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
    InitB(population[i].b);
end;

end.
