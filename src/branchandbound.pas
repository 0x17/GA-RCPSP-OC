unit branchandbound;

interface

uses projectdata;

type TBranchAndBound = class
  constructor Create(const fname: String);
  destructor Destroy; override;
  procedure Solve;
private
  lb: Double;
  procedure Branch(sts: JobData; resRem: ResourceProfile);
  function ComputeUpperBound(const sts: JobData; const resRem: ResourceProfile): Double;
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
  ps.ComputeESFTS;

  TSSGS.Solve(ps.topOrder, ps.maxOc, sts, resRem);
  lb := TProfit.CalcProfit(sts, resRem);
end;

destructor TBranchAndBound.Destroy;
begin
  inherited;
  FreeAndNil(ps);
end;

procedure TBranchAndBound.Solve;
var
  sts: JobData;
  i: Integer;
  resRem: ResourceProfile;
begin
  SetLength(sts, ps.numJobs);
  for i := 0 to ps.numJobs-1 do
    sts[i] := -1;
  sts[0] := 0;
  TSSGS.InitializeResidualCapacity(resRem);
  Branch(sts, resRem);
end;

procedure TBranchAndBound.Branch(sts: JobData; resRem: ResourceProfile);
var
  i, j, tPrecFeas, tResFeas, t, tau: Integer;
  eligible: Boolean;
  ub: Double;
  r: Integer;
begin
  // Found leaf? Update lower bound!
  if sts[ps.numJobs-1] <> -1 then
    lb := Max(lb, TProfit.CalcProfit(sts, resRem));

  for j := 0 to ps.numJobs-1 do
    // Job itself not scheduled
    if sts[j] = -1 then begin
      // All preds scheduled?
      eligible := True;
      for i := 0 to ps.numJobs-1 do
        if (sts[j] = -1) and (ps.adjMx[i,j] = 1) then
          eligible := False;

      if eligible then begin
        // First period of precedence feasibility (all preds finished)
        tPrecFeas := 0;
        for i := 0 to ps.numJobs do
          if (ps.adjMx[i, j] = 1) and (sts[i] + ps.durations[i] > tPrecFeas) then
            tPrecFeas := sts[i] + ps.durations[i];

        // First period of resource feasibility (enough capacity throughout runtime)
        for tResFeas := tPrecFeas to ps.numPeriods - 1 do
          if TSSGS.ResourceFeasible(resRem, ps.zeroOc, j, tResFeas) then
            break;

        // Branch on all possible periods between precedence and resource feasibility
        for t := tPrecFeas to tResFeas do begin
          // Schedule at t and update remaining capacity
          sts[j] := t;
          for tau := t to t+ps.durations[j]-1 do
            for r := 0 to ps.numRes-1 do
              resRem[r,tau] := resRem[r,tau] - ps.demands[j,r];

          // Compute upper bound profit for completed schedule
          ub := ComputeUpperBound(sts, resRem);
          if ub >= lb then
            Branch(sts, resRem);

          // Reset remaining capacity
          for tau := t to t+ps.durations[j]-1 do
            for r := 0 to ps.numRes-1 do
              resRem[r,tau] := resRem[r,tau] + ps.demands[j,r];
        end;
      end;
    end;

end;

function TBranchAndBound.ComputeUpperBound(const sts: JobData; const resRem: ResourceProfile): Double;
var alpha, sumUnits, maxSum, maxRes, ftpartial, j, r: Integer;
begin
  maxSum := 0;
  maxRes := 0;
  for r := 0 to ps.numRes-1 do begin
    sumUnits := 0;

    for j := 0 to ps.numJobs-1 do
      if sts[j] = -1 then
        sumUnits := sumUnits + ps.durations[j] * ps.demands[j,r];

    if sumUnits > maxSum then begin
      maxSum := sumUnits;
      maxRes := r;
    end;
  end;

  // Finishing time of partial schedule
  ftpartial := 0;
  for j := 0 to ps.numJobs-1 do
    if (sts[j] <> -1) and (sts[j] + ps.durations[j] > ftpartial) then
      ftpartial := sts[j] + ps.durations[j];

  alpha := Ceil(maxSum / ps.capacities[maxRes]);
  result := TProfit.Revenue(ftpartial + alpha) - TProfit.TotalOCCosts(resRem);
end;

end.
