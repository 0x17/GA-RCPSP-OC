unit peakcrossover;

interface

uses projectdata;

type TPeakCrossover = class
  class procedure CollectPeaks(sts: JobData; const resRemaining: ResourceProfile; out peaks: JobDataArray);
private
  class procedure AddJobsActiveInPeriod(const sts: JobData; var activeSet: JobData; t: Integer);
end;

implementation

uses globals, classes;

class procedure TPeakCrossover.CollectPeaks(sts: JobData; const resRemaining: ResourceProfile; out peaks: JobDataArray);
const
  LTHRESHOLD = 0.75;
  UTHRESHOLD = 0.9;
var
  t: Integer;
  delta: Double;
  peakBefore: Boolean;
begin
  SetLength(peaks, 0);

  delta := LTHRESHOLD + Random * (UTHRESHOLD - LTHRESHOLD);
  peakBefore := False;

  for t := 0 to ps.numPeriods - 1 do begin
    if ps.ResourceUtilisationRatio(resRemaining, t) >= delta then begin
      if not(peakBefore) then begin
        // Start new peak, add all jobs running in this period
        SetLength(peaks, Length(peaks)+1);
        SetLength(peaks[Length(peaks)-1], ps.numJobs);
      end;
      AddJobsActiveInPeriod(sts, peaks[Length(peaks)-1], t);
      peakBefore := True;
    end else begin
      peakBefore := False;
    end;
  end;
end;

class procedure TPeakCrossover.AddJobsActiveInPeriod(const sts: JobData; var activeSet: JobData; t: Integer);
var j: Integer;
begin
  for j := 0 to ps.numJobs-1 do
    if (sts[j] <= t) and (t < sts[j] + ps.durations[j]) then
      activeSet[j] := 1;
end;

end.
