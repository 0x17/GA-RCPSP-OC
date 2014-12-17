unit ssgs;

interface

uses classes, sysutils, projectdata, globals;

type TSSGS = class
  class function ResourceFeasible(const resRemaining, z: ResourceProfile; j, stj: Integer): Boolean;
  class procedure SolveCore(const order: JobData; startFrom: Integer; const z: ResourceProfile; var sts, fts: JobData; var resRemaining: ResourceProfile);
  class procedure Solve(const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);

  class function AllPredsFinished(const fts: JobData; j: Integer): Integer;
  class procedure ScheduleJob(j, stj: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
  class procedure InitializeResidualCapacity(out resRemaining: ResourceProfile);
  class procedure InitializeJobTimes(out sts, fts: JobData);
end;

implementation

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


end.

