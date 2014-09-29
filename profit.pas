unit profit;

// Berechnung des Profits für einen konkreten Plan einer Testinstanz.

interface

uses sysutils, projectdata, ssgs, esschedule, math, resprofiles;

function CalcProfit(const ps: ProjData; const sts: JobData; const resRemaining: ResourceProfile): Double;
procedure CalcMinMaxMakespanCosts(var ps: ProjData);
procedure ComputeRevenueBuffer(var ps: ProjData);

implementation

function TotalOCCosts(const ps: ProjData; const resRemaining: ResourceProfile): Double; forward;

procedure CalcMinMaxMakespanCosts(var ps: ProjData);
var
  sts: JobData;
  z, resRemaining: ResourceProfile;
  tkappa, tkappari: Integer;
  tkappar: Double;
  r, j: Integer;
begin
  ps.minCosts := 0;

  // Bestimme tkappa als maximum der tkappar
  tkappa := 0;
  for r := 0 to ps.numRes-1 do
  begin
    tkappar := 0;
    for j := 0 to ps.numJobs-1 do
      tkappar := tkappar + ps.durations[j]*ps.demands[j,r];

    tkappar := tkappar / (ps.capacities[r]+ps.zmax[r]);
    tkappari := Ceil(tkappar);

    if tkappa > tkappari then
      tkappa := tkappari;
  end;

  SolveESS(ps, ps.topOrder, sts, resRemaining);

  // Bestimme maximale Kosten als Kosten des ESS
  ps.maxCosts := TotalOCCosts(ps, resRemaining);

  // Bestimme minimale Makespan als maximum aus
  // ESS-makespan und tkappa
  ps.minMs := Max(sts[ps.numJobs-1], tkappa);

  // Bestimme maximale Makespan über SSGS mit z_rt = 0
  SetLength(z, ps.numRes, ps.numPeriods);
  ZeroOvercapacity(ps, z);
  Solve(ps, ps.topOrder, z, sts, resRemaining);
  ps.maxMs := sts[ps.numJobs-1];

end;

function Revenue(const ps: ProjData; makespan: Integer): Double;
begin
  result := ps.maxCosts - ps.maxCosts / Power(ps.maxMs - ps.minMs, 2) * Power(makespan - ps.minMs, 2);
end;

function TotalOCCosts(const ps: ProjData; const resRemaining: ResourceProfile): Double;
var r, t: Integer;
begin
  result := 0.0;
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
      if resRemaining[r,t] < 0 then
        result := result - resRemaining[r,t] * ps.kappa[r];
end;

function CalcProfit(const ps: ProjData; const sts: JobData; const resRemaining: ResourceProfile): Double;
begin
  result := Revenue(ps, sts[ps.numJobs-1]) - TotalOCCosts(ps, resRemaining);
end;

procedure ComputeRevenueBuffer(var ps: ProjData);
var t: Integer;
begin
  SetLength(ps.revenueBuf, ps.numPeriods);
  for t := 1 to ps.numPeriods do
    ps.revenueBuf[t] := Revenue(ps, t);
end;

end.

