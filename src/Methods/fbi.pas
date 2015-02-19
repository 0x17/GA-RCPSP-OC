unit fbi;

interface

uses projectdata, ssgsoc;

type TFBI = class
  class procedure Improve(var sts: JobData;  const z: ResourceProfile; out resRemaining: ResourceProfile); overload;
  class procedure Improve(var order: JobData; const z: ResourceProfile); overload;
  class procedure Improve(var order: JobData; const tau: ExtArray); overload;
private
  class procedure FlipSchedule(var sts: JobData);
end;

implementation

uses globals, ssgs;

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

class procedure TFBI.Improve(var order: JobData; const tau: ExtArray);
var
  sts: JobData;
  z, resRem: ResourceProfile;
begin
  TSSGSOC.SolveWithTau(order, tau, sts, resRem);
  ps.InferOCFromSchedule(resRem, z);
  Improve(sts, z, resRem);
  TSSGS.ScheduleToActivityList(sts, order);
end;

class procedure TFBI.FlipSchedule(var sts: JobData);
var
  j: Integer;
  oldSts: JobData;
begin
  oldSts := Copy(sts, 0, ps.numJobs);
  for j := 0 to ps.numJobs-1 do
    sts[j] := oldSts[0] - (oldSts[j] + ps.durations[j]);
end;

end.
