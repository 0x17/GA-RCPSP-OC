unit ssgsmod;

interface

uses projectdata;

type TSSGSMod = class
  class procedure Solve(const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile; linked, upper: Boolean);
  class procedure SolveCore(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile; linked: Boolean);
  class procedure SolveCoreUpper(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile; linked: Boolean);
private
  class procedure InitializeSeparateResiduals(out resRemNormal, resRemOvertime: ResourceProfile); inline;
  class procedure ScheduleJobSepRem(bj, j, t: Integer; var sts, fts: JobData; var resRemaining, resRemNormal, resRemOvertime: ResourceProfile);
end;

implementation

uses classes, sysutils, globals, ssgs, math;

class procedure TSSGSMod.Solve(const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile; linked, upper: Boolean);
var fts: JobData;
begin
  TSSGS.InitializeResidualCapacity(resRemaining);
  TSSGS.InitializeJobTimes(sts, fts);
  if upper then
    SolveCoreUpper(order, b, 1, sts, fts, resRemaining, linked)
  else
    SolveCore(order, b, 1, sts, fts, resRemaining, linked);
end;

class procedure TSSGSMod.SolveCore(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile; linked: Boolean);
var i, j, t: Integer;
begin
  for i := startFrom to ps.numJobs-1 do
  begin
    if linked then
      j := order[i]
    else
      j := i;

    t := TSSGS.AllPredsFinished(fts, j);
    if b[j] = 1 then while not TSSGS.ResourceFeasible(resRemaining, ps.maxOc, j, t) do inc(t)
    else while not TSSGS.ResourceFeasible(resRemaining, ps.zeroOc, j, t) do inc(t);

    TSSGS.ScheduleJob(j, t, sts, fts, resRemaining);
  end;
end;

class procedure TSSGSMod.InitializeSeparateResiduals(out resRemNormal, resRemOvertime: ResourceProfile);
var r, t: Integer;
begin
  SetLength(resRemNormal, ps.numRes, ps.numPeriods);
  SetLength(resRemOvertime, ps.numRes, ps.numPeriods);

  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do begin
      resRemNormal[r,t] := ps.capacities[r];
      resRemOvertime[r,t] := ps.zmax[r];
    end;
end;

class procedure TSSGSMod.ScheduleJobSepRem(bj, j, t: Integer; var sts, fts: JobData; var resRemaining, resRemNormal, resRemOvertime: ResourceProfile);
var r, tau, dem: Integer;
begin
    sts[j] := t;
    fts[j] := t + ps.durations[j];

    for tau := t to fts[j]-1 do
      for r := 0 to ps.numRes-1 do begin
        dem := ps.demands[j,r];

        resRemaining[r,tau] := resRemaining[r,tau] - dem;
        if bj = 1 then begin
          if dem > resRemOvertime[r,tau] then begin
            resRemNormal[r,tau] := resRemNormal[r,tau] - (dem - resRemOvertime[r,tau]);
            resRemOvertime[r,tau] := 0;
          end else
            resRemOvertime[r,tau] := resRemOvertime[r,tau] - dem;
        end else
          resRemNormal[r,tau] := resRemNormal[r,tau] - ps.demands[j,r];
      end;
end;

class procedure TSSGSMod.SolveCoreUpper(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile; linked: Boolean);
var
  i, j, t: Integer;
  resRemNormal, resRemOvertime: ResourceProfile;
begin
  InitializeSeparateResiduals(resRemNormal, resRemOvertime);

  for i := startFrom to ps.numJobs-1 do
  begin
    if linked then
      j := order[i]
    else
      j := i;

    t := TSSGS.AllPredsFinished(fts, j);
    if b[j] = 1 then while not TSSGS.ResourceFeasible(resRemaining, ps.maxOc, j, t) do inc(t)
    else while not TSSGS.ResourceFeasible(resRemNormal, ps.zeroOc, j, t) do inc(t);

    ScheduleJobSepRem(b[j], j, t, sts, fts, resRemaining, resRemNormal, resRemOvertime);
  end;
end;

end.
