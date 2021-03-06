unit esschedule;

interface

uses projectdata;

type TESSchedule = class
  class procedure Solve(const order: JobData; out sts: JobData; out resRemaining: ResourceProfile);
end;

implementation

uses classes, sysutils, globals, ssgs;

class procedure TESSchedule.Solve(const order: JobData; out sts: JobData; out resRemaining: ResourceProfile);
var
  i, j: Integer;
  fts: JobData;
begin
  TSSGS.InitializeResidualCapacity(resRemaining);
  TSSGS.InitializeJobTimes(sts, fts);

  for i := 1 to ps.numJobs-1 do begin
    j := order[i];
    TSSGS.ScheduleJob(j, TSSGS.AllPredsFinished(fts, j), sts, fts, resRemaining);
  end;
end;

end.

