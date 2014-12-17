unit ssgsmod;

interface

uses classes, sysutils, projectdata, globals, ssgs;

type TSSGSMod = class
  class procedure Solve(const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile);
  class procedure SolveCore(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
  class function ResourceFeasible(const useOc: Integer; const resRemaining: ResourceProfile; j, stj: Integer): Boolean;
end;

implementation

// Modifiziertes SSGS: AG j ist bei Einplanung in Periode stj zulässig gdw.
// genügend Restkapazität über die Bearbeitungsdauer zur Verfügung steht bei
// Gesamtkapazität von
// 1. Falls b_j=0: K_r
// 2. Falls b_j=1: K_r+zmax_r
class procedure TSSGSMod.Solve(const order, b: JobData; out sts: JobData; out resRemaining: ResourceProfile);
var fts: JobData;
begin
  TSSGS.InitializeResidualCapacity(resRemaining);
  TSSGS.InitializeJobTimes(sts, fts);
  SolveCore(order, b, 1, sts, fts, resRemaining);
end;

// Wahr, gdw. AG j bei Einplanung in Periode stj über gesamte Laufzeit genügend
// Restkapazität zur Verfügung steht bei Gesamtkapazität von
// 1. useOc=false: K_r
// 2. useOc=true: K_r+zmax_r
class function TSSGSMod.ResourceFeasible(const useOc: Integer; const resRemaining: ResourceProfile; j, stj: Integer): Boolean;
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

class procedure TSSGSMod.SolveCore(const order, b: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
var i, j, t: Integer;
begin
  for i := startFrom to ps.numJobs-1 do
  begin
    j := order[i];

    t := TSSGS.AllPredsFinished(fts, j);
    while not ResourceFeasible(b[j], resRemaining, j, t) do
      inc(t);

    TSSGS.ScheduleJob(j, t, sts, fts, resRemaining);
  end;
end;

end.
