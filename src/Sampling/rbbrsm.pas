unit rbbrsm;

interface

uses projectdata, algenerator;

// regret based biased random sampling
// LFTs priority rule -> primary priority_j = -lft_j in decision set
type RBBRSGenerator = class(IALGenerator)
  procedure PickSample(out order: JobData); override;
protected
  procedure ComputeCumulativeProbabilities(const dset: JobSet; out cumProbs: DblArray); virtual;
  function PickJobFromDecisionSet(const dset: JobSet): Integer; virtual;
end;

implementation

uses globals, topsort {$ifndef FPC},math{$endif};

procedure RBBRSGenerator.ComputeCumulativeProbabilities(const dset: JobSet; out cumProbs: DblArray);
var
  j, sumRegrets, dsetSize, k: Integer;
  regretValues: JobData;
  c: Double;
begin
  dsetSize := ps.JobSetCardinality(dset);
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

function RBBRSGenerator.PickJobFromDecisionSet(const dset: JobSet): Integer;
var
  r: Double;
  j: Integer;
  cumProbs: DblArray;
  dsetSize: Integer;
begin
  ComputeCumulativeProbabilities(dset, cumProbs);
  dsetSize := ps.JobSetCardinality(dset);
  r := Random;

  if r < cumProbs[0] then begin
    result := ps.JobSetNth(dset, 0);
    Exit;
  end;

  for j := 1 to dsetSize-1 do
    if (r >= cumProbs[j-1]) and (r < cumProbs[j]) then begin
      result := ps.JobSetNth(dset, j);
      Exit;
    end;

  result := ps.JobSetNth(dset, dsetSize-1);
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

end.
