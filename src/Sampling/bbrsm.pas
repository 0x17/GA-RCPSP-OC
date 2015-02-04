unit bbrsm;

interface

uses algenerator, rbbrsm, projectdata;

type BBRSMNeighbourhood = class(RBBRSGenerator)
  constructor Create(const origin: JobData);
private
  origin: JobData;
  beta: Double;
protected
  procedure ComputeCumulativeProbabilities(const dset: JobSet; out cumProbs: DblArray); override;
  function PickJobFromDecisionSet(const dset: JobSet): Integer; override;
end;

implementation

uses globals;

// beta biased random sampling
constructor BBRSMNeighbourhood.Create(const origin: JobData);
begin
  self.origin := Copy(origin, 0, ps.numJobs);
  beta := 1 - 20 / ps.numJobs;
end;

procedure BBRSMNeighbourhood.ComputeCumulativeProbabilities(const dset: JobSet; out cumProbs: DblArray);
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

  dsetSize := ps.JobSetCardinality(dset);
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

function BBRSMNeighbourhood.PickJobFromDecisionSet(const dset: JobSet): Integer;
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
