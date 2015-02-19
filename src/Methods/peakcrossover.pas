unit peakcrossover;

interface

uses projectdata, gassgsoc;

type TPeakCrossover = class
  class procedure Crossover(const mother, father: TActivityListIndividual; var daughter: TActivityListIndividual);
private
  class procedure CollectPeaks(sts: JobData; const resRemaining: ResourceProfile; out peaks: JobSetArray; out valley: JobSet);
  class procedure AddJobsActiveInPeriod(const sts: JobData; var activeSet, valley: JobData; t: Integer);

  class function DeterminePeakStartIndex(const peak: JobSet): Integer;
  class function DeterminePeakPredecessors(const peak: JobSet): JobSet;
end;

implementation

uses globals, classes;

// peaks = Array of sets of jobs
// job in peak if peak[j] = 1
// valley is set of jobs
class procedure TPeakCrossover.CollectPeaks(sts: JobData; const resRemaining: ResourceProfile; out peaks: JobSetArray; out valley: JobSet);
const
  LTHRESHOLD = 0.75;
  UTHRESHOLD = 0.9;
var
  t: Integer;
  delta: Double;
  peakBefore: Boolean;
begin
  SetLength(peaks, 0);

  SetLength(valley, ps.numJobs);
  ps.Fill(valley, 1);

  delta := LTHRESHOLD + Random * (UTHRESHOLD - LTHRESHOLD);
  peakBefore := False;

  for t := 0 to ps.numPeriods - 1 do begin
    if ps.ResourceUtilisationRatio(resRemaining, t) >= delta then begin
      if not(peakBefore) then begin
        // Start new peak, add all jobs running in this period
        SetLength(peaks, Length(peaks)+1);
        SetLength(peaks[Length(peaks)-1], ps.numJobs);
        ps.Fill(peaks[Length(peaks)-1], 0);
      end;
      AddJobsActiveInPeriod(sts, peaks[Length(peaks)-1], valley, t);
      peakBefore := True;
    end else begin
      peakBefore := False;
    end;
  end;
end;

class procedure TPeakCrossover.AddJobsActiveInPeriod(const sts: JobData; var activeSet, valley: JobData; t: Integer);
var j: Integer;
begin
  for j := 0 to ps.numJobs-1 do
    if (sts[j] < t) and (t <= sts[j] + ps.durations[j]) then begin
      activeSet[j] := 1;
      valley[j] := 0;
    end;
end;

class function TPeakCrossover.DeterminePeakStartIndex(const peak: JobSet): Integer;
var j: Integer;
begin
  result:= 0;
  for j := 0 to ps.numJobs - 1 do
    if peak[j] = 1 then begin
      result := j;
      break;
    end;
end;

class procedure TPeakCrossover.Crossover(const mother, father: TActivityListIndividual; var daughter: TActivityListIndividual);
var
  peaks: JobSetArray;
  valley: JobSet;
  p, h, j, peakStartIx: Integer;
  peakPreds: JobSet;

  function IsCandidate(const order: JobData; job, peakIx: Integer): Boolean;
  var
    k, l: Integer;
    predAssigned: Boolean;
  begin
    // not yet assigned to child
    result := True;
    for k := 0 to h do
      if daughter.order[k] = order[job] then result := False;
    // and not in peak
    result := result and (peaks[peakIx][job] = 0);
    // and all preds already assigned
    for k := 0 to ps.numJobs-1 do
      if ps.adjMx[k,order[job]] = 1 then begin
        predAssigned := False;
        for l := 0 to h do
          if daughter.order[l] = k then
            predAssigned := True;
        if predAssigned = False then begin
          result := False;
          exit;
        end;
      end;
  end;

begin
  TPeakCrossover.CollectPeaks(mother.sts, mother.resRem, peaks, valley);

  h := 0;
  for p := 0 to Length(peaks) - 1 do begin
    // put fathers activities before peak
    peakStartIx := DeterminePeakStartIndex(peaks[p]);
    j := 0;
    while (j < peakStartIx) and (j < ps.numJobs) do begin
      if IsCandidate(father.order, j, p) then begin
        daughter.order[h] := father.order[j];
        inc(h);
      end;
    end;

    // complete peak preds
    peakPreds := DeterminePeakPredecessors(peaks[p]);
    for j := 0 to ps.numJobs-1 do
      if (peakPreds[j] = 1) and IsCandidate(father.order, j, p) then begin
        daughter.order[h] := father.order[j];
        inc(h);
      end;

    // put peak
    for j := 0 to ps.numJobs-1 do
      if peaks[p][j] = 1 then begin
        daughter.order[h] := mother.order[j];
        inc(h);
      end;
  end;

  // put remaining behind last peak
  for j := 0 to ps.numJobs-1 do
    if IsCandidate(father.order, j, Length(peaks)-1) then begin
      daughter.order[h] := father.order[j];
      inc(h);
    end;
end;

class function TPeakCrossover.DeterminePeakPredecessors(const peak: JobSet): JobSet;
var i, j: Integer;
begin
  SetLength(result, ps.numJobs);

  ps.Fill(result, 0);

  for j := 0 to ps.numJobs-1 do
    if peak[j] = 1 then
      for i := 0 to ps.numJobs-1 do
        if ps.adjMx[i,j] = 1 then
          result[i] := 1;
end;

end.

