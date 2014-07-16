unit testing;

interface

uses operators, projectdata, printing, helpers, ssgsoc, ssgs, profit, topsort, classes, stsvis, gassgsoc, constants, sysutils;

procedure RunTests;

implementation

procedure TestCrossoverJobList;
const NJOBS = 32;
var
  i: Integer;
  mother, father, daughter: JobData;
begin
  SetLength(daughter, NJOBS);
  SetLength(mother, NJOBS);
  SetLength(father, NJOBS);

  for i := 0 to NJOBS-1 do
  begin
    mother[i] := i+1;
    father[i] := NJOBS-i;
  end;

  PrintActivityList(mother);
  PrintActivityList(father);

  OnePointCrossover(mother, father, daughter);
  PrintActivityList(daughter);

  TwoPointCrossover(mother, father, daughter);
  PrintActivityList(daughter);
end;

procedure TestCrossoverTALBPair;
const NJOBS = 32;
  procedure Init(var pair: TALBPair);
  begin
    SetLength(pair.order, NJOBS);
    SetLength(pair.b, NJOBS);
  end;
var
  mother, father, daughter: TALBPair;
  i: Integer;
begin
  Init(mother); Init(father); Init(daughter);
  for i := 0 to NJOBS-1 do
  begin
    mother.order[i] := i+1;
    mother.b[i] := RandomRangeIncl(0, 1);
    father.order[i] := NJOBS-i;
    father.b[i] := RandomRangeIncl(0, 1);
  end;
  PrintTALBPair(father);
  PrintTALBPair(mother);

  OnePointCrossover(mother, father, daughter);
  PrintTALBPair(daughter);

  TwoPointCrossover(mother, father, daughter);
  PrintTALBPair(daughter);
end;

procedure CalcResRemaining(const ps: ProjData; const sts: JobData; out resRem: ResourceProfile);
var
  j, t, r: Integer;
begin
  SetLength(resRem, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      resRem[r,t] := ps.capacities[r];
  for j := 0 to ps.numJobs do
    for t := sts[j] to sts[j]+ps.durations[j] - 1 do
      for r := 0 to ps.numRes-1 do
        resRem[r,t] := resRem[r,t] - ps.demands[j,r];
end;

procedure TestRightShift2;
var
  ps: ProjData;
  sts, sts2, order: JobData;
  resRem: ResourceProfile;
  maxProfit: Double;
  j: Integer;
  fnames: TStringList;
  fname: String;
begin
  ps := ProjData.Create;
  fnames := ListProjFilesInDir('j30');
  for fname in fnames do begin
    WriteLn('For ', fname);
    ps.LoadFromFile(fname);
    TopologicalOrder(ps, ps.topOrder);
    CalcMinMaxMakespanCosts(ps);
    CalcUMax(ps);
    TopologicalOrder(ps, order);
    maxProfit := SolveWithOC(ps, order, sts, True);
    //PrintSchedule(ps, sts);
    SetLength(sts2, ps.numJobs);
    for j := 0 to ps.numJobs-1 do sts2[j] := sts[j];
    CalcResRemaining(ps, sts, resRem);
    maxProfit := RightShift(ps, sts, resRem, maxProfit);
    //PrintSchedule(ps, sts);
    for j := 0 to ps.numJobs-1 do
      if sts2[j] <> sts[j] then
        WriteLn((j+1), '->', sts2[j], ' neq ', (j+1), '->', sts[j]);
  end;
  ps.Free;
end;

procedure TestRightShift;
var
  ps: ProjData;
  sts: JobData;
  resRem: ResourceProfile;
  maxProfit: Double;
begin
  ps := ProjData.Create;

  ps.numJobs := 6;
  ps.numRes := 1;
  ps.numPeriods := 8;
  ps.T := 8;

  SetLength(ps.adjMx, ps.numJobs, ps.numJobs);
  ps.adjMx[0,1] := 1;
  ps.adjMx[0,2] := 1;
  ps.adjMx[1,3] := 1;
  ps.adjMx[3,4] := 1;
  ps.adjMx[2,4] := 1;
  ps.adjMx[4,5] := 1;

  SetLength(ps.durations, ps.numJobs);
  ps.durations[0] := 0;
  ps.durations[1] := 2;
  ps.durations[2] := 2;
  ps.durations[3] := 2;
  ps.durations[4] := 2;
  ps.durations[5] := 0;

  SetLength(ps.demands, ps.numJobs, ps.numRes);
  ps.demands[0,0] := 0;
  ps.demands[1,0] := 2;
  ps.demands[2,0] := 2;
  ps.demands[3,0] := 1;
  ps.demands[4,0] := 2;
  ps.demands[5,0] := 0;

  SetLength(ps.capacities, 1);
  ps.capacities[0] := 2;

  SetLength(ps.zmax, 1);
  ps.zmax[0] := 2;

  SetLength(ps.kappa, 1);
  ps.kappa[0] := 0.5;

  TopologicalOrder(ps, ps.topOrder);
  CalcMinMaxMakespanCosts(ps);
  CalcUMax(ps);

  SetLength(sts, ps.numJobs);
  sts[0] := 0;
  sts[1] := 1;
  sts[2] := 1;
  sts[3] := 3;
  sts[4] := 5;
  sts[5] := 7;

  CalcResRemaining(ps, sts, resRem);
  maxProfit := CalcProfit(ps, sts, resRem);

  //PrintSchedule(ps, sts);
  VisualizeSchedule(ps, sts, 'beforeRS');
  maxProfit := RightShift(ps, sts, resRem, maxProfit);
  //PrintSchedule(ps, sts);
  VisualizeSchedule(ps, sts, 'afterRS');

  ps.Free;
end;

procedure TestScheduleVis;
var
  ps: ProjData;
  sts: JobData;
  z, resRem: ResourceProfile;
begin
  ps := ProjData.Create;
  ps.LoadFromFile('j30\j301_1.sm');
  TopologicalOrder(ps, ps.topOrder);
  CalcMinMaxMakespanCosts(ps);
  CalcUMax(ps);
  SetLength(z, ps.numRes, ps.numPeriods);
  ZeroOvercapacity(ps, z);
  Solve(ps, ps.topOrder, z, sts, resRem);
  VisualizeSchedule(ps, sts, 'schedule');
  ps.Free;
end;

procedure TestRightShiftAnomaly;
var
  ps: ProjData;
  sts: JobData;
  profit: Double;
  bestOrder: JobData;
begin
  ps := ProjData.Create;
  ps.LoadFromFile('j30\j305_5.sm');
  TopologicalOrder(ps, ps.topOrder);
  CalcMinMaxMakespanCosts(ps);
  CalcUMax(ps);

  RunGASSGSOC(ps, bestOrder, False);

  profit := SolveWithOC(ps, bestOrder, sts, False);
  WriteLn(Format('Profit = %f', [profit]));
  VisualizeSchedule(ps, sts, 'scheduleNORS');

  RunGASSGSOC(ps, bestOrder, True);

  (*profit := SolveWithOC(ps, bestOrder, sts);
  WriteLn(Format('Profit = %f', [profit]));
  VisualizeSchedule(ps, sts, 'scheduleNORS');*)

  profit := SolveWithOC(ps, bestOrder, sts, True);
  WriteLn(Format('Profit = %f', [profit]));
  VisualizeSchedule(ps, sts, 'scheduleRS');

  ps.Free;
end;

procedure RunTests;
begin
  (*WriteLn('Test job list');
  TestCrossoverJobList;
  WriteLn('Test talb pair');
  TestCrossoverTALBPair;
  WriteLn('Test right shift');
  TestRightShift2;*)
  //TestScheduleVis;
  TestRightShiftAnomaly;
  //ReadLn;
  //TestRightShift;
end;

end.
