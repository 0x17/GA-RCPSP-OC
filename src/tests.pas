unit tests;

interface

procedure RunTests;

implementation

uses projectdata, ssgs, globals, sysutils, topsort, profit, resprofiles, peakcrossover;

procedure InitExampleProject;
const FNAME = '../Projekte/j30filtered/j3011_7.sm';
begin
  if ps <> nil then FreeAndNil(ps);
  ps := ProjData.Create;
  ps.LoadFromFile(fname);
  TTopSort.Sort(ps.topOrder);
  TProfit.CalcMinMaxMakespanCosts;
  ps.ComputeESFTS;
end;

procedure TestScheduleToActivityList;
var sts, order: JobData;
begin
  SetLength(sts, 4);
  sts[0] := 0;
  sts[1] := 3;
  sts[2] := 0; // dup for stab check
  sts[3] := 5;

  ps.numJobs := 4;
  ps.numPeriods := 100;
  TSSGS.ScheduleToActivityList(sts, order);
  Assert(order[0] = 0);
  Assert(order[1] = 2);
  Assert(order[2] = 1);
  Assert(order[3] = 3);
end;

procedure TestReverseActivityList;
var
  order, revOrder: JobData;
  j: Integer;
begin
  SetLength(order, 6);
  SetLength(revOrder, 6);
  for j := 0 to 5 do begin
    order[j] := j;
    revOrder[j] := 5-j;
  end;

  ps.numJobs := 6;
  TSSGS.ReverseActivityList(order);

  for j := 0 to 5 do
    Assert(order[j] = revOrder[j]);
end;

function MyPr(const dset: JobData): Integer;
var j: Integer;
begin
  result := 0;
  for j := 0 to ps.numJobs-1 do
    if dset[j] = 1 then
      result := j;
end;

procedure TestPriorityRuleSGS;
var
  z, resRem: ResourceProfile;
  sts: JobData;
begin
  SetLength(resRem, ps.numRes, ps.numPeriods);
  TResProfiles.MaxOC(z);
  TSSGS.SolveWithDecider(MyPr, z, sts, resRem);
  // TODO: Add asserts for sts
end;

procedure TestCollectPeaks;
var
  order, sts: JobData;
  peaks: JobDataArray;
  resRem, z: ResourceProfile;
begin
  order := Copy(ps.topOrder, 0, ps.numJobs);
  TResProfiles.ZeroOC(z);
  TSSGS.Solve(order, z, sts, resRem);
  ps.InferProfileFromSchedule(sts, z, resRem);
  TPeakCrossover.CollectPeaks(sts, resRem, peaks);
end;

procedure RunTests;
begin
  InitExampleProject;
  TestScheduleToActivityList;
  TestReverseActivityList;
  TestPriorityRuleSGS;
  TestCollectPeaks;
  FreeAndNil(ps);
end;

end.
