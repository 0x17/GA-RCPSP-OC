unit sgsminoc;
// SGS computing schedule for given deadline, which minimizes overtime costs

interface

uses projectdata;

type TSGSMinOC = class
  class procedure Solve(deadline: Integer; const order: JobData; out sts: JobData);
private
  class procedure ComputeTimeWindows(deadline: Integer; const partialSts: JobData; out ests, lsts: JobData);
  class procedure ScheduleJob(j: Integer; var partialSts: JobData; const ests, lsts: JobData);
end;

implementation

uses globals;

// Compute schedule observing given deadline with heuristically minimized overtime costs
class procedure TSGSMinOC.Solve(deadline: Integer; const order: JobData; out sts: JobData);
var
  ests, lsts, partialSts: JobData;
  i, j: Integer;
begin
  SetLength(partialSts, ps.numJobs);
  partialSts[0] := 0;
  for i := 1 to ps.numJobs-1 do
    partialSts[i] := -1;

  SetLength(ests, ps.numJobs);
  SetLength(lsts, ps.numJobs);

  ComputeTimeWindows(deadline, partialSts, ests, lsts);

  for i := 0 to ps.numJobs - 1 do begin
    j := order[i];
    ScheduleJob(j, partialSts, ests, lsts);
    ComputeTimeWindows(deadline, partialSts, ests, lsts);
  end;
end;

class procedure TSGSMinOC.ComputeTimeWindows(deadline: Integer; const partialSts: JobData; out ests, lsts: JobData);
begin
end;

class procedure TSGSMinOC.ScheduleJob(j: Integer; var partialSts: JobData; const ests, lsts: JobData);
begin
end;

end.
