unit ssgs;

interface

uses classes, sysutils, projectdata;

procedure ZeroOvercapacity(const ps: ProjData; var z: ResourceProfile);
procedure MaxOvercapacity(const ps: ProjData; var z: ResourceProfile);

function ResourceFeasible(const ps: ProjData; const resRemaining, z: ResourceProfile; j, stj: Integer): Boolean;

procedure SolveCore(const ps: ProjData; const order: JobData; startFrom: Integer; const z: ResourceProfile; var sts, fts: JobData; var resRemaining: ResourceProfile);
procedure Solve(const ps: ProjData; const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);

implementation

procedure ZeroOvercapacity(const ps: ProjData; var z: ResourceProfile);
var
  r, t: Integer;
begin
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
          z[r,t] := 0;
end;

procedure MaxOvercapacity(const ps: ProjData; var z: ResourceProfile);
var
  r, t: Integer;
begin
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
          z[r,t] := ps.zmax[r];
end;

function ResourceFeasible(const ps: ProjData; const resRemaining, z: ResourceProfile; j, stj: Integer): Boolean;
var
  r, tau: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    if ps.demands[j,r] > 0 then
      for tau := stj to stj + ps.durations[j] - 1 do
          if resRemaining[r,tau] + z[r,tau] < ps.demands[j,r] then
          begin
            result := false;
            exit;
          end;
  result := true
end;

procedure SolveCore(const ps: ProjData; const order: JobData; startFrom: Integer; const z: ResourceProfile; var sts, fts: JobData; var resRemaining: ResourceProfile);
var
  i, j, k, t, tau, r: Integer;
begin
  for i := startFrom to ps.numJobs-1 do
  begin
    j := order[i];

    t := 0;
    for k := 0 to ps.numJobs-1 do
      if (ps.adjMx[k,j] = 1) and (fts[k] > t) then
        t := fts[k];

    while not ResourceFeasible(ps, resRemaining, z, j, t) do
      inc(t);

    sts[j] := t;
    fts[j] := t + ps.durations[j];

    for tau := t to fts[j]-1 do
      for r := 0 to ps.numRes-1 do
        resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
  end;
end;

procedure Solve(const ps: ProjData; const order: JobData; const z: ResourceProfile; out sts: JobData; out resRemaining: ResourceProfile);
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

  SolveCore(ps, order, 1, z, sts, fts, resRemaining);
end;

end.

