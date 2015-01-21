unit profit;

interface

uses projectdata;

type TProfit = class
  class function CalcProfit(const sts: JobData; const resRemaining: ResourceProfile): Double;
  class procedure CalcMinMaxMakespanCosts;
  class procedure ComputeRevenueBuffer;
private
  class function TotalOCCosts(const resRemaining: ResourceProfile): Double;
  class function Revenue(makespan: Integer): Double;
end;

implementation

uses sysutils, ssgs, esschedule, math, resprofiles, globals;

class procedure TProfit.CalcMinMaxMakespanCosts;
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

    if tkappari > tkappa then
      tkappa := tkappari;
  end;

  TESSchedule.Solve(ps.topOrder, sts, resRemaining);

  // Bestimme maximale Kosten als Kosten des ESS
  ps.maxCosts := TotalOCCosts(resRemaining);

  // Bestimme minimale Makespan als maximum aus
  // ESS-makespan und tkappa
  ps.minMs := Max(sts[ps.numJobs-1], tkappa);

  // Bestimme maximale Makespan über SSGS mit z_rt = 0
  TResProfiles.ZeroOC(z);
  TSSGS.Solve(ps.topOrder, z, sts, resRemaining);
  ps.maxMs := sts[ps.numJobs-1];
end;

class function TProfit.Revenue(makespan: Integer): Double;
begin
  result := ps.maxCosts - ps.maxCosts / Power(ps.maxMs - ps.minMs, 2) * Power(makespan - ps.minMs, 2);
end;

class function TProfit.TotalOCCosts(const resRemaining: ResourceProfile): Double;
var r, t: Integer;
begin
  result := 0.0;
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
      if resRemaining[r,t] < 0 then
        result := result - resRemaining[r,t] * ps.kappa[r];
end;

class function TProfit.CalcProfit(const sts: JobData; const resRemaining: ResourceProfile): Double;
begin
  result := Revenue(sts[ps.numJobs-1]) - TotalOCCosts(resRemaining);
end;

class procedure TProfit.ComputeRevenueBuffer;
var t: Integer;
begin
  SetLength(ps.revenueBuf, ps.numPeriods);
  for t := 1 to ps.numPeriods do
    ps.revenueBuf[t] := Revenue(t);
end;

end.

