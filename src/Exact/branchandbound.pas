unit branchandbound;

interface

uses projectdata;

type TBranchAndBound = class
  constructor Create(const fname: String);
  destructor Destroy; override;
  function Solve(out solution: JobData): Double;
private
  lb: Double;
  lbSts: JobData;
  makespanLb: Integer;

  procedure Branch(sts, order: JobData; k: Integer);
  function ComputeUpperBound(const sts: JobData): Double;
  function IsEligible(const sts: JobData; j: Integer): Boolean;
  function ComputeMakespanLowerBound: Integer;
end;

implementation

uses sysutils, globals, topsort, profit, ssgs, math;

constructor TBranchAndBound.Create(const fname: String);
var
  sts: JobData;
  resRem: ResourceProfile;
begin
  if ps <> nil then FreeAndNil(ps);
  ps := ProjData.Create;
  ps.LoadFromFile(fname);
  TTopSort.Sort(ps.topOrder);
  TProfit.CalcMinMaxMakespanCosts;
  ps.ReorderJobsAscDepth;
  TTopSort.Sort(ps.topOrder);
  ps.ComputeESFTS;

  TSSGS.Solve(ps.topOrder, ps.maxOc, sts, resRem);
  lb := TProfit.CalcProfit(sts, resRem);
  makespanLb := ComputeMakespanLowerBound;
end;

destructor TBranchAndBound.Destroy;
begin
  inherited;
  FreeAndNil(ps);
end;

function TBranchAndBound.Solve(out solution: JobData): Double;
var
  sts, order: JobData;
  i: Integer;
begin
  SetLength(sts, ps.numJobs);
  SetLength(order, ps.numJobs);
  for i := 0 to ps.numJobs-1 do begin
    sts[i] := -1;
    order[i] := -1;
  end;
  sts[0] := 0;
  order[0] := 0;

  Branch(sts, order, 1);
  result := lb;
  solution := Copy(lbSts, 0, ps.numJobs);
end;

function TBranchAndBound.ComputeMakespanLowerBound: Integer;
var r, j, accum: Integer;
begin
  result := 0;
  for r := 0 to ps.numRes - 1 do begin
    accum := 0;
    for j := 0 to ps.numJobs - 1 do
      accum := accum + ps.demands[j, r] * ps.durations[j];
    accum := Ceil(accum / (ps.capacities[r] + ps.zmax[r]));
    result := Max(accum, result);
  end;
end;

function TBranchAndBound.IsEligible(const sts: JobData; j: Integer): Boolean;
var i: Integer;
begin
  result := False;
  // Job itself not scheduled
  if sts[j] = -1 then begin
    // All preds scheduled?
    result := True;
    for i := 0 to ps.numJobs-1 do
      if (sts[i] = -1) and (ps.adjMx[i,j] = 1) then begin
        result := False;
        exit;
      end;
  end;
end;

procedure TBranchAndBound.Branch(sts, order: JobData; k: Integer);
var
  i, j, tPrecFeas, tResFeas, t, prev: Integer;
  ub, nprofit: Double;
begin
  // Found leaf? Update lower bound (if better)!
  if k = ps.numJobs-1 then begin
    nprofit := TProfit.CalcProfit(sts);
    if nprofit > lb then begin
      lb := nprofit;
      lbSts := Copy(sts, 0, ps.numJobs);
    end;
    exit;
  end;

  // Branch over eligibles
  for j := 0 to ps.numJobs-1 do begin
    if IsEligible(sts, j) then begin
      order[k] := j;

      // First period of precedence feasibility (all preds finished)
      tPrecFeas := 0;
      for i := 0 to ps.numJobs - 1 do
        if (ps.adjMx[i, j] = 1) and (sts[i] + ps.durations[i] > tPrecFeas) then
          tPrecFeas := sts[i] + ps.durations[i];

      // First period of resource feasibility (enough capacity throughout runtime)
      for tResFeas := tPrecFeas to ps.numPeriods - 1 do
        if TSSGS.ResourceFeasible(sts, ps.zeroOc, j, tResFeas) then
          break;

      // Branch on all feasible periods between precedence and resource feasibility
      for t := tPrecFeas to tResFeas do begin
        // Prevent duplicate scheduling orders ("activity lists") yielding same schedule
        prev := order[k-1];
        if (t < sts[prev]) or ((t = sts[prev]) and (prev > j)) then continue;

        // Don't overrun max overtime!
        if not TSSGS.ResourceFeasible(sts, ps.maxOc, j, t) then continue;

        sts[j] := t;

        // Compute upper bound profit for completed schedule
        ub := ComputeUpperBound(sts);
        if ub > lb then
          Branch(Copy(sts, 0, ps.numJobs), Copy(order, 0, ps.numJobs), k+1);
      end;
    end;
  end;
end;

function TBranchAndBound.ComputeUpperBound(const sts: JobData): Double;
begin
  result := TProfit.Revenue(makespanLb) - TProfit.TotalOCCosts(sts);
end;

end.
