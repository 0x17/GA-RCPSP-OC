unit ssgsmod;

interface

uses classes, sysutils, projectdata;

type TSSGSMod = class
  class procedure Solve(const ps: ProjData; const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile);
  class procedure SolveCore(const ps: ProjData; const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
  class function ResourceFeasible(const useOc: Integer; const ps: ProjData; const resRemaining: ResourceProfile; j, stj: Integer): Boolean;
end;

implementation

// Modifiziertes SSGS: AG j ist bei Einplanung in Periode stj zulässig gdw.
// genügend Restkapazität über die Bearbeitungsdauer zur Verfügung steht bei
// Gesamtkapazität von
// 1. Falls b_j=0: K_r
// 2. Falls b_j=1: K_r+zmax_r
class procedure TSSGSMod.Solve(const ps: ProjData; const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile);
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

  SolveCore(ps, order, b, 1, sts, fts, resRemaining);
end;

// Wahr, gdw. AG j bei Einplanung in Periode stj über gesamte Laufzeit genügend
// Restkapazität zur Verfügung steht bei Gesamtkapazität von
// 1. useOc=false: K_r
// 2. useOc=true: K_r+zmax_r
class function TSSGSMod.ResourceFeasible(const useOc: Integer; const ps: ProjData; const resRemaining: ResourceProfile; j, stj: Integer): Boolean;
var r, tau, z: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    if ps.demands[j,r] > 0 then
    begin
      if useOc = 1 then z := ps.zmax[r]
      else z := 0;

      for tau := stj to stj + ps.durations[j] - 1 do
        if resRemaining[r,tau] + z < ps.demands[j,r] then
        begin
          result := false;
          exit;
        end;
      end;
  result := true
end;

class procedure TSSGSMod.SolveCore(const ps: ProjData; const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
var i, j, k, t, tau, r: Integer;
begin
  for i := startFrom to ps.numJobs-1 do
  begin
    j := order[i];

    t := 0;
    for k := 0 to ps.numJobs-1 do
      if (ps.adjMx[k,j] = 1) and (fts[k] > t) then
        t := fts[k];

    while not ResourceFeasible(b[j], ps, resRemaining, j, t) do
      inc(t);

    sts[j] := t;
    fts[j] := t + ps.durations[j];

    for tau := t to fts[j]-1 do
      for r := 0 to ps.numRes-1 do
        resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
  end;
end;

end.
