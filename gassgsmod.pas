unit gassgsmod;

// Genetischer Algorithmus zur Bestimmung eines optimalen Plans mit Benutzung von Zusatzkapazitäten
// Verwendet als Repräsentation (lambda, 01010010...)

interface

uses projectdata, gaactivitylist, classes, sysutils, parallelfitness, constants, ssgsmod, profit, helpers, operators, gacommon;

function RunGASSGSMod(const nps: ProjData; out best: TALBPair): Double;

implementation

var ps: ProjData;

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
      if THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE then
         b[j] := 1 - b[j];
end;

procedure OPC(const mother, father: JobData; var daughter: JobData);
var
  j, q: Integer;
begin
    q := THelper.RandomRangeIncl(0, ps.numJobs-1);
    for j := 0 to ps.numJobs-1 do
      if j <= q then
        daughter[j] := mother[j]
      else
        daughter[j] := father[j];
end;

procedure TPC(const mother, father: JobData; var daughter: JobData);
var
  j, q1, q2, tmp: Integer;
begin
    q1 := THelper.RandomRangeIncl(1, ps.numJobs);
    q2 := THelper.RandomRangeIncl(1, ps.numJobs);
    if q2 < q1 then
    begin
      tmp := q1;
      q1 := q2;
      q2 := tmp;
    end;

    for j := 0 to ps.numJobs-1 do
      if (j <= q1-1) or (j >= q2-1) then
        daughter[j] := mother[j]
      else
        daughter[j] := father[j];
end;

procedure CrossoverB(const b, other: JobData; var daughter, son: JobData);
begin
  OPC(b, other, daughter);
  OPC(other, b, son);
  //TPC(b, other, daughter);
  //TPC(other, b, son);
end;

function FitnessSSGSMod(const individual: TALBPair): Double;
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  SolveMod(ps, individual.order, individual.b, sts, resRemaining);
  result := CalcProfit(ps, sts, resRemaining);
end;

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

function RunGASSGSMod(const nps: ProjData; out best: TALBPair): Double;
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
