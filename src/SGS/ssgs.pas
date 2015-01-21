unit ssgs;

interface

uses projectdata;

type
  TDecideFunc = function(const dset: JobData): Integer;

  TSSGS = class
    class function ResourceFeasible(const resRemaining, z: ResourceProfile; j, stj: Integer): Boolean;
    class procedure SolveCore(const order: JobData; startFrom: Integer; const z: ResourceProfile; var sts, fts: JobData; var resRemaining: ResourceProfile);
    class procedure Solve(const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);
    class procedure SolveWithDecider(decider: TDecideFunc; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);

    class function AllPredsFinished(const fts: JobData; j: Integer): Integer;
    class procedure ScheduleJob(j, stj: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
    class procedure InitializeResidualCapacity(out resRemaining: ResourceProfile);
    class procedure InitializeJobTimes(out sts, fts: JobData);

    class procedure ScheduleToActivityList(const sts: JobData; out order: JobData; leq: Boolean = False);
    class procedure ReverseActivityList(var order: JobData);
  end;

implementation

uses classes, sysutils, globals;

class function TSSGS.ResourceFeasible(const resRemaining, z: ResourceProfile; j, stj: Integer): Boolean;
var r, tau: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    if ps.demands[j,r] > 0 then
      for tau := stj to stj + ps.durations[j] - 1 do
          if resRemaining[r,tau] + z[r,tau] < ps.demands[j,r] then begin
            result := false;
            exit;
          end;
  result := true
end;

class procedure TSSGS.SolveCore(const order: JobData; startFrom: Integer; const z: ResourceProfile; var sts, fts: JobData; var resRemaining: ResourceProfile);
var i, j, t: Integer;
begin
  for i := startFrom to ps.numJobs-1 do begin
    j := order[i];

    t := AllPredsFinished(fts, j);
    while not ResourceFeasible(resRemaining, z, j, t) do
      inc(t);

    ScheduleJob(j, t, sts, fts, resRemaining);
  end;
end;

class procedure TSSGS.Solve(const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);
var fts: JobData;
begin
  InitializeResidualCapacity(resRemaining);
  InitializeJobTimes(sts, fts);
  SolveCore(order, 1, z, sts, fts, resRemaining);
end;

procedure ComputeDecisionSet(const sts: JobData; var dset: JobData); forward;

class procedure TSSGS.SolveWithDecider(decider: TDecideFunc; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);
var
  i, j, t: Integer;
  dset, fts: JobData;
begin
  SetLength(resRemaining, ps.numRes, ps.numPeriods);
  SetLength(sts, ps.numJobs);
  SetLength(fts, ps.numJobs);

  for j := 0 to ps.numJobs - 1 do
    sts[j] := -1;

  SetLength(dset, ps.numJobs);

  for i := 0 to ps.numJobs - 1 do begin
    ComputeDecisionSet(sts, dset);
    j := decider(dset);
    t := AllPredsFinished(fts, j);
    while not ResourceFeasible(resRemaining, z, j, t) do
      inc(t);
    ScheduleJob(j, t, sts, fts, resRemaining);
  end;
end;

class function TSSGS.AllPredsFinished(const fts: JobData; j: Integer): Integer;
var i: Integer;
begin
    result := 0;
    for i := 0 to ps.numJobs-1 do
      if (ps.adjMx[i,j] = 1) and (fts[i] > result) then
        result := fts[i];
end;

class procedure TSSGS.ScheduleJob(j, stj: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
var tau, r: Integer;
begin
  sts[j] := stj;
  fts[j] := stj + ps.durations[j];

  for tau := stj to fts[j]-1 do
    for r := 0 to ps.numRes-1 do
      resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
end;

class procedure TSSGS.InitializeResidualCapacity(out resRemaining: ResourceProfile);
var r, t: Integer;
begin
  SetLength(resRemaining, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      resRemaining[r,t] := ps.capacities[r];
end;

class procedure TSSGS.InitializeJobTimes(out sts, fts: JobData);
begin
  SetLength(sts, ps.numJobs);
  SetLength(fts, ps.numJobs);
  sts[0] := 0;
  fts[0] := 0;
end;

class procedure TSSGS.ScheduleToActivityList(const sts: JobData; out order: JobData; leq: Boolean);
var
  rem: JobData;
  j: Integer;

  function JobWithMinSt: Integer;
  var k, minSt: Integer;
  begin
    minSt := ps.numPeriods-1;
    result := 0;
    for k := 0 to ps.numJobs-1 do
      if (rem[k] = 1) and ((leq and (sts[k] <= minSt)) or (not(leq) and (sts[k] < minSt))) then begin
        minSt := sts[k];
        result := k;
      end;
  end;
begin
  SetLength(order, ps.numJobs);
  SetLength(rem, ps.numJobs);
  for j := 0 to ps.numJobs-1 do
    rem[j] := 1;

  for j := 0 to ps.numJobs-1 do begin
    order[j] := JobWithMinSt;
    rem[order[j]] := 0;
  end;
end;

class procedure TSSGS.ReverseActivityList(var order: JobData);
var j, tmp: Integer;
begin
  Assert(ps.numJobs mod 2 = 0);
  for j := 0 to (ps.numJobs div 2) - 1 do begin
    tmp := order[j];
    order[j] := order[ps.numJobs-1-j];
    order[ps.numJobs-1-j] := tmp;
  end;
end;

// TODO: Implement, refactor, cleanup
procedure ComputeDecisionSet(const sts: JobData; var dset: JobData);
var i, j: Integer;
begin
  for j := 0 to ps.numJobs - 1 do begin
    if sts[j] <> -1 then begin
      dset[j] := 0;
      continue;
    end;
    dset[j] := 1;
    for i := 0 to ps.numJobs - 1 do
       if (ps.adjMx[i,j] = 1) and (sts[i] = -1) then begin
         dset[j] := 0;
         break;
       end;
  end;
end;

end.

