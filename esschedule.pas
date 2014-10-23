unit esschedule;

// Berechne Earliest Start Schedule (beachte nur Reihenfolgerestriktion, nicht
// Ressourcenrestriktion!)

interface

uses classes, sysutils, projectdata, globals;

type TESSchedule = class
  class procedure Solve(const order: JobData; out sts: JobData; out resRemaining: ResourceProfile);
private
  class procedure SolveCore(const order: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
end;

implementation

class procedure TESSchedule.SolveCore(const order: JobData; startFrom: Integer; var sts, fts: JobData; var resRemaining: ResourceProfile);
var i, j, k, t, tau, r: Integer;
begin
  for i := startFrom to ps.numJobs-1 do begin
    j := order[i];

    t := 0;
    for k := 0 to ps.numJobs-1 do
      if (ps.adjMx[k,j] = 1) and (fts[k] > t) then
        t := fts[k];

    sts[j] := t;
    fts[j] := t + ps.durations[j];

    for tau := t to fts[j]-1 do
      for r := 0 to ps.numRes-1 do
        resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
  end;
end;

class procedure TESSchedule.Solve(const order: JobData; out sts: JobData; out resRemaining: ResourceProfile);
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

  SolveCore(order, 1, sts, fts, resRemaining);
end;

end.

