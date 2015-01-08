unit fbi;

interface

uses projectdata, globals, ssgs, ssgsoc, math;

type TFBI = class
  class procedure Improve(var sts: JobData;  const z: ResourceProfile; out resRemaining: ResourceProfile); overload;
  class procedure Improve(var order: JobData; const z: ResourceProfile); overload;
  class procedure Improve(var order: JobData; const tau: JobData); overload;
end;

implementation

procedure FlipSchedule(var sts: JobData);
var
  j: Integer;
  oldSts: JobData;
begin
  oldSts := Copy(sts, 0, ps.numJobs);
  for j := 0 to ps.numJobs-1 do
    sts[j] := oldSts[0] - (oldSts[j] + ps.durations[j]);
end;

class procedure TFBI.Improve(var sts: JobData; const z: ResourceProfile; out resRemaining: ResourceProfile);
var
  order, negFts: JobData;
  j: Integer;
begin
  SetLength(negFts, ps.numJobs);

  for j := 0 to ps.numJobs-1 do
    negFts[j] := -sts[j] - ps.durations[j];
  TSSGS.ScheduleToActivityList(negFts, order);
  ps.InvertPrecedence;
  TSSGS.Solve(order, z, sts, resRemaining);

  FlipSchedule(sts);
  ps.InvertPrecedence;
  TSSGS.ScheduleToActivityList(sts, order);
  TSSGS.Solve(order, z, sts, resRemaining);
end;

class procedure TFBI.Improve(var order: JobData; const z: ResourceProfile);
var
  sts: JobData;
  resRem: ResourceProfile;
begin
  TSSGS.Solve(order, z, sts, resRem);
  Improve(sts, z, resRem);
  TSSGS.ScheduleToActivityList(sts, order);
end;

procedure InferProfileFromSchedule(const sts: JobData; out z, resRem: ResourceProfile);
var
  r, t: Integer;
  j: Integer;
begin
  SetLength(resRem, ps.numRes, ps.numPeriods);
  SetLength(z, ps.numRes, ps.numPeriods);

  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      resRem[r,t] := ps.capacities[r];


  for j := 0 to ps.numJobs-1 do
    for r := 0 to ps.numRes-1 do
      if ps.demands[j,r] > 0 then
        for t := sts[j] to sts[j]+ps.durations[j]-1 do
          resRem[r,t] := resRem[r,t] - ps.demands[j,r];

  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      z[r,t] := Max(0, -resRem[r,t]);
end;

class procedure TFBI.Improve(var order: JobData; const tau: JobData);
var
  sts: JobData;
  z, resRem: ResourceProfile;
begin
  TSSGSOC.SolveWithTau(order, tau, sts);
  InferProfileFromSchedule(sts, z, resRem);
  Improve(sts, z, resRem);
  TSSGS.ScheduleToActivityList(sts, order);
end;

end.
