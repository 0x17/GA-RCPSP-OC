unit fbi;

interface

uses projectdata;

type TFBI = class
  class procedure Improve(var sts: JobData;  const z: ResourceProfile; out resRemaining: ResourceProfile); overload;
  class procedure Improve(var order: JobData; const z: ResourceProfile); overload;
  class procedure Improve(var order: JobData; const tau: JobData); overload;
end;

implementation

uses globals, ssgs, ssgsoc;

procedure FlipSchedule(var sts: JobData); forward;

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

class procedure TFBI.Improve(var order: JobData; const tau: JobData);
var
  sts: JobData;
  z, resRem: ResourceProfile;
begin
  TSSGSOC.SolveWithTau(order, tau, sts);
  ps.InferProfileFromSchedule(sts, z, resRem);
  Improve(sts, z, resRem);
  TSSGS.ScheduleToActivityList(sts, order);
end;

procedure FlipSchedule(var sts: JobData);
var
  j: Integer;
  oldSts: JobData;
begin
  oldSts := Copy(sts, 0, ps.numJobs);
  for j := 0 to ps.numJobs-1 do
    sts[j] := oldSts[0] - (oldSts[j] + ps.durations[j]);
end;



end.
