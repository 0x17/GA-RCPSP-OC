unit ssgs;

interface

uses classes, sysutils, projectdata, globals;

type TSSGS = class
  class function ResourceFeasible(const resRemaining, z: ResourceProfile; j, stj: Integer): Boolean;
  class procedure SolveCore(const order: JobData; startFrom: Integer; const z: ResourceProfile; var sts, fts: JobData; var resRemaining: ResourceProfile);
  class procedure Solve(const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);
end;

implementation

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
var i, j, k, t, tau, r: Integer;
begin
  for i := startFrom to ps.numJobs-1 do begin
    j := order[i];

    t := 0;
    for k := 0 to ps.numJobs-1 do
      if (ps.adjMx[k,j] = 1) and (fts[k] > t) then
        t := fts[k];

    while not ResourceFeasible(resRemaining, z, j, t) do
      inc(t);

    sts[j] := t;
    fts[j] := t + ps.durations[j];

    for tau := t to fts[j]-1 do
      for r := 0 to ps.numRes-1 do
        resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
  end;
end;

class procedure TSSGS.Solve(const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);
var
  r, t: Integer;
  fts: JobData;
begin
  SetLength(resRemaining, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      resRemaining[r,t] := ps.capacities[r];

  SetLength(sts, ps.numJobs);
  SetLength(fts, ps.numJobs);
  sts[0] := 0;
  fts[0] := 0;

  SolveCore(order, 1, z, sts, fts, resRemaining);
end;


end.

