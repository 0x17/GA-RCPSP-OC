unit algens;

interface

uses projectdata, globals, topsort, math;

type
  IALGenerator = class
    procedure PickSample(out order: JobData); virtual; abstract;
  end;

  NaiveGenerator = class(IALGenerator)
    constructor Create;
    procedure PickSample(out order: JobData); override;
  private
    i: Integer;
    prioRules: JobDataArray;
  end;

  DblArray = Array of Double;
  RBBRSGenerator = class(IALGenerator)
    procedure PickSample(out order: JobData); override;
  protected
    procedure ComputeCumulativeProbabilities(const dset: JobData; out cumProbs: DblArray); virtual;
    function PickJobFromDecisionSet(const dset: JobData): Integer; virtual;
  end;

  BBRSMNeighbourhood = class(RBBRSGenerator)
    constructor Create(const origin: JobData);
  private
    origin: JobData;
    beta: Double;
  protected
    procedure ComputeCumulativeProbabilities(const dset: JobData; out cumProbs: DblArray); override;
    function PickJobFromDecisionSet(const dset: JobData): Integer; override;
  end;

implementation

//==============================================================================

constructor NaiveGenerator.Create;
begin
  ProjData.InitPriorityRulesFromFile(ps, prioRules);
  i := 0;
end;

procedure NaiveGenerator.PickSample(out order: JobData);
var j: Integer;
begin
  if i <= 12 then begin
    SetLength(order, ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      order[j] := prioRules[i, j];
  end else begin
    TTopSort.RandomSort(order);
  end;
  inc(i);
end;

//==============================================================================

// regret based biased random sampling
// LFTs priority rule -> primary priority_j = -lft_j in decision set
function ArrSetCardinality(const aset: JobData): Integer;
var j: Integer;
begin
  result := 0;
  for j := 0 to ps.numJobs-1 do
    result := result + aset[j];
end;

function ArrSetNth(const aset: JobData; n: Integer): Integer;
var j, ctr: Integer;
begin
  ctr := 0;
  result := 0;
  for j := 0 to ps.numJobs-1 do
  begin
    ctr := ctr + aset[j];
    if ctr = n+1 then begin
      result := j;
      Exit;
    end;
  end;
end;

procedure RBBRSGenerator.ComputeCumulativeProbabilities(const dset: JobData; out cumProbs: DblArray);
var
  j, sumRegrets, dsetSize, k: Integer;
  regretValues: JobData;
  c: Double;
begin
  dsetSize := ArrSetCardinality(dset);
  SetLength(regretValues, dsetSize);
  sumRegrets := 0;
  k := 0;
  for j := 0 to ps.numJobs-1 do begin
    if dset[j] = 1 then begin
      regretValues[k] := -ps.lfts[j] - (-ps.lfts[ps.numJobs-1]);
      sumRegrets := sumRegrets + regretValues[k];
      inc(k);
    end;
  end;

  c := 1 / (dsetSize + sumRegrets);

  SetLength(cumProbs, dsetSize);
  cumProbs[0] := c * (1+regretValues[0]);
  for j := 1 to dsetSize-1 do begin
    cumProbs[j] := cumProbs[j-1] + c * (1+regretValues[j]);
  end;
end;

function RBBRSGenerator.PickJobFromDecisionSet(const dset: JobData): Integer;
var
  r: Double;
  j: Integer;
  cumProbs: DblArray;
  dsetSize: Integer;
begin
  ComputeCumulativeProbabilities(dset, cumProbs);
  dsetSize := ArrSetCardinality(dset);
  r := Random;

  if r < cumProbs[0] then begin
    result := ArrSetNth(dset, 0);
    Exit;
  end;

  for j := 1 to dsetSize-1 do
    if (r >= cumProbs[j-1]) and (r < cumProbs[j]) then begin
      result := ArrSetNth(dset, j);
      Exit;
    end;

  result := ArrSetNth(dset, dsetSize-1);
end;

procedure RBBRSGenerator.PickSample(out order: JobData);
var
  j: Integer;
  dset: JobData;
begin
  SetLength(dset, ps.numJobs);
  SetLength(order, ps.numJobs);
  for j := 0 to ps.numJobs-1 do begin
    TTopSort.UpdateDecisionSet(order, dset, j);
    order[j] := PickJobFromDecisionSet(dset);
  end;
end;

//==============================================================================

// beta biased random sampling
constructor BBRSMNeighbourhood.Create(const origin: JobData);
begin
  self.origin := Copy(origin, 0, ps.numJobs);
  beta := 1 - 20 / ps.numJobs;
end;

procedure BBRSMNeighbourhood.ComputeCumulativeProbabilities(const dset: JobData; out cumProbs: DblArray);
var
  j, dsetSize, k: Integer;
  sumPrios, firstPos: Integer;
begin
  firstPos := 0;
  sumPrios := 0;
  for j := 0 to ps.numJobs - 1 do begin
    k := origin[j];
    if dset[k] = 1 then begin
      if sumPrios = 0 then
        firstPos := k+1;
      sumPrios := sumPrios + (k+1);
    end;
  end;

  dsetSize := ArrSetCardinality(dset);
  SetLength(cumProbs, dsetSize);

  cumProbs[0] := firstPos / sumPrios;
  k := 0;
  for j := 0 to ps.numJobs - 1 do
    if dset[j] = 1 then begin
      if k > 0 then
         cumProbs[k] := cumProbs[k-1] + (j+1) / sumPrios;
      inc(k);
    end;
end;

function BBRSMNeighbourhood.PickJobFromDecisionSet(const dset: JobData): Integer;
var
  r: Double;
  j, k: Integer;
begin
  result := 0;
  r := Random;
  if r < beta then begin
    for j := 0 to ps.numJobs-1 do begin
      k := origin[j];
      if dset[k] = 1 then
        result := k;
    end;
  end
  else result := inherited PickJobFromDecisionSet(dset);
end;

end.
