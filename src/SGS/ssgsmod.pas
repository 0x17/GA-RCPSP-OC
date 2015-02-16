unit ssgsmod;

interface

uses projectdata;

type TSSGSMod = class
  class procedure Solve(const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile);
  class procedure SolveCore(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
end;

implementation

uses classes, sysutils, globals, ssgs;

class procedure TSSGSMod.Solve(const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile);
var fts: JobData;
begin
  TSSGS.InitializeResidualCapacity(resRemaining);
  TSSGS.InitializeJobTimes(sts, fts);
  SolveCore(order, b, 1, sts, fts, resRemaining);
end;

class procedure TSSGSMod.SolveCore(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
var i, j, t: Integer;
begin
  for i := startFrom to ps.numJobs-1 do
  begin
    j := order[i];

    t := TSSGS.AllPredsFinished(fts, j);
    if b[j] = 1 then while not TSSGS.ResourceFeasible(resRemaining, ps.maxOc, j, t) do inc(t)
    else while not TSSGS.ResourceFeasible(resRemaining, ps.zeroOc, j, t) do inc(t);

    TSSGS.ScheduleJob(j, t, sts, fts, resRemaining);
  end;
end;

end.