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

function TBranchAndBound.Solve(out solution: JobData): Double;
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
  result := lb;
  solution := Copy(lbSts, 0, ps.numJobs);
end;

procedure BranchLog(const sts: JobData); inline;
var j: Integer;
begin
  write('BRANCH (');
  for j := 0 to ps.numJobs-1 do write(sts[j], '; ');
  writeln(')');
end;

type BoolArray = Array of Boolean;

function ComputeEligibles(const sts: JobData): BoolArray;
var i, j: Integer;
begin
  SetLength(result, ps.numJobs);
  for j := 0 to ps.numJobs-1 do begin
    result[j] := False;
    // Job itself not scheduled
    if sts[j] = -1 then begin
      // All preds scheduled?
      result[j] := True;
      for i := 0 to ps.numJobs-1 do
        if (sts[i] = -1) and (ps.adjMx[i,j] = 1) then begin
          result[j] := False;
          break;
        end;
    end;
  end;
end;

function MaxST(const sts: JobData; out mjob: Integer): Integer; inline;
var j: Integer;
begin
  result := sts[0];
  mjob := 0;
  for j := 1 to ps.numJobs-1 do
    if (sts[j] <> -1) and (sts[j] > result) then begin
      result := sts[j];
      mjob := j;
    end;
end;

procedure TBranchAndBound.Branch(sts: JobData; resRem: ResourceProfile);
var
  i, j, tPrecFeas, tResFeas, t, tau, r, mjob: Integer;
  eligibles: BoolArray;
  ub, nprofit: Double;
  resRemCp: ResourceProfile;

  resRemDbg: ResourceProfile;
begin
  // Found leaf? Update lower bound (if better)!
  if sts[ps.numJobs-1] <> -1 then begin
    nprofit := TProfit.CalcProfit(sts, resRem);
    if nprofit > lb then begin
      lb := nprofit;
      lbSts := Copy(sts, 0, ps.numJobs);
    end;
    exit;
  end;

  // Compute eligibles
  eligibles := ComputeEligibles(sts);

  // Branch over eligibles
  for j := 0 to ps.numJobs-1 do begin
    if eligibles[j] then begin
        // First period of precedence feasibility (all preds finished)
        tPrecFeas := 0;
        for i := 0 to ps.numJobs - 1 do
          if (ps.adjMx[i, j] = 1) and (sts[i] + ps.durations[i] > tPrecFeas) then
            tPrecFeas := sts[i] + ps.durations[i];

        // First period of resource feasibility (enough capacity throughout runtime)
        for tResFeas := tPrecFeas to ps.numPeriods - 1 do
          if TSSGS.ResourceFeasible(resRem, ps.zeroOc, j, tResFeas) then
            break;

        // Branch on all possible periods between precedence and resource feasibility
        for t := tPrecFeas to tResFeas do begin
          if not TSSGS.ResourceFeasible(resRem, ps.maxOc, j, t) then
            continue;

          // Prevent duplicate scheduling orders ("activity lists")
          if t < MaxST(sts, mjob) then // or build activity list while branching?
            continue;
          if (t = sts[mjob]) and (mjob > j) then
            continue;

          writeln(j, ' ', t);

          //sts[j] := -1;

          (*ps.InferProfileFromSchedule(sts, resRemDbg);
          if TProfit.TotalOCCosts(resRem) <> TProfit.TotalOCCosts(resRemDbg) then
            writeln(TProfit.TotalOCCosts(resRem), TProfit.TotalOCCosts(resRemDbg));
          assert(TProfit.TotalOCCosts(resRem) = TProfit.TotalOCCosts(resRemDbg));*)

          sts[j] := t;
          // Copy residual capacities and update
          resRemCp := resRem;
          SetLength(resRemCp, ps.numRes, ps.numPeriods);
          for r := 0 to ps.numRes-1 do
            if ps.demands[j,r] > 0 then
              for tau := t to t+ps.durations[j]-1 do begin
                resRemCp[r,tau] := resRemCp[r,tau] - ps.demands[j,r];
                Assert(resRem[r,tau] = resRemCp[r,tau] + ps.demands[j,r]);
              end;

          ps.InferProfileFromSchedule(sts, resRemDbg);
          assert(TProfit.TotalOCCosts(resRemCp) = TProfit.TotalOCCosts(resRemDbg));

          // Compute upper bound profit for completed schedule
          ub := ComputeUpperBound(sts, resRemCp);
          if ub > lb then begin
            Branch(Copy(sts, 0, ps.numJobs), resRemCp);
          end else writeln('BOUND'); // FIXME: resRem und sts werden beim BOUNDING unsync!!!!!!!!!!!
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

  // FIXME: Also use zmax but ut getting negative!
  alpha := Ceil(maxSum / (ps.capacities[maxRes] + ps.zmax[maxRes]));
  result := TProfit.Revenue(ftpartial + alpha) - TProfit.TotalOCCosts(resRem);
end;

end.
