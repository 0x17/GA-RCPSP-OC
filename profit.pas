﻿unit profit;

// Berechnung des Profits für einen konkreten Plan einer Testinstanz.

interface

uses classes, sysutils, projectdata, ssgs, esschedule;

function CalcProfit(const ps: ProjData; const sts: JobData; const resRemaining: ResourceProfile): Double;
function TotalOCCostsForSchedule(const ps: ProjData; const sts: JobData): Double;
procedure CalcMinMaxMakespanCosts(var ps: ProjData);
procedure CalcUMax(var ps: ProjData);

implementation

function TotalOCCosts(const ps: ProjData; const resRemaining: ResourceProfile): Double; forward;

procedure CalcMinMaxMakespanCosts(var ps: ProjData);
var
  sts: JobData;
  z, resRemaining: ResourceProfile;
begin
  ps.minCosts := 0;

  // Bestimme maximale Kosten und minimale Makespan über SSGS mit z_rt = zmax_r
  SetLength(z, ps.numRes, ps.numPeriods);
  MaxOvercapacity(ps, z);
  Solve(ps, ps.topOrder, z, sts, resRemaining);
  ps.maxCosts := TotalOCCosts(ps, resRemaining);
  ps.minMs := sts[ps.numJobs-1];

  // Bestimme maximale Makespan über SSGS mit z_rt = 0
  ZeroOvercapacity(ps, z);
  Solve(ps, ps.topOrder, z, sts, resRemaining);
  ps.maxMs := sts[ps.numJobs-1];

  if ps.minMs = ps.maxMs then
  begin
    inc(ps.maxMs);
    ps.maxCosts := 0;
  end;
end;

procedure CalcUMax(var ps: ProjData);
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  SolveESS(ps, ps.topOrder, sts, resRemaining);
  ps.uMax := TotalOCCosts(ps, resRemaining);
end;

(*function Revenue(const ps: ProjData; makespan: Integer): Double;
var c: Double;
begin
  c := (ps.maxCosts-ps.minCosts) / (ps.maxMs-ps.minMs);
  result := -c * makespan + c * ps.maxMs;
end;*)

function Revenue(const ps: ProjData; makespan: Integer): Double;
begin
  result := ps.uMax * (ps.T-makespan)/ps.T;
end;

function TotalOCCosts(const ps: ProjData; const resRemaining: ResourceProfile): Double;
var
  r, t: Integer;
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

procedure BuildResRemainingForSchedule(const ps: ProjData; const sts: JobData; out resRemaining: ResourceProfile);
var
  r, t, tau, j: Integer;
begin
  SetLength(resRemaining, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      resRemaining[r,t] := ps.capacities[r];

  for j := 0 to ps.numJobs - 1 do
    for r := 0 to ps.numRes - 1 do
    begin
      if ps.demands[j,r] > 0 then
        for tau := sts[j] to sts[j] + ps.durations[j] - 1 do      
          resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
    end;
end;

function TotalOCCostsForSchedule(const ps: ProjData; const sts: JobData): Double;
var
  resRemaining: ResourceProfile;
begin
  BuildResRemainingForSchedule(ps, sts, resRemaining);
  result := TotalOCCosts(ps, resRemaining);
end;

end.

