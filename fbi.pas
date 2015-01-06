unit fbi;

interface

uses projectdata, globals, ssgs;

type TFBI = class
  class procedure Improve(var sts: JobData;  const z: ResourceProfile; out resRemaining: ResourceProfile);
end;

implementation

class procedure TFBI.Improve(var sts: JobData; const z: ResourceProfile; out resRemaining: ResourceProfile);
var
  order, rem: JobData;

  function JobWithMaxFt: Integer;
  var k, maxFt: Integer;
  begin
    maxFt := 0;
    result := 0;
    for k := 0 to ps.numJobs-1 do begin
      if rem[k] = 0 then
        continue;
      if sts[k] + ps.durations[k] >= maxFt then begin
        maxFt := sts[k] + ps.durations[k];
        result := k;
      end;
    end;
  end;

  function JobWithMinSt: Integer;
  var k, minSt: Integer;
  begin
    minSt := ps.numPeriods-1;
    result := 0;
    for k := 0 to ps.numJobs-1 do begin
      if rem[k] = 0 then
        continue;

      if sts[k] <= minSt then begin
        minSt := sts[k];
        result := k;
      end;
    end;
  end;

  procedure FillSet;
  var j: Integer;
  begin
    for j := 0 to ps.numJobs-1 do
      rem[j] := 1;
  end;

  procedure SetOrderToDescFts;
  var j: Integer;
  begin
    FillSet;
    for j := 0 to ps.numJobs-1 do begin
      order[j] := JobWithMaxFt;
      rem[order[j]] := 0;
    end;
  end;

  procedure SetOrderToAscSts;
  var j: Integer;
  begin
    FillSet;
    for j := 0 to ps.numJobs-1 do begin
      order[j] := JobWithMinSt;
      rem[order[j]] := 0;
    end;
  end;

begin
  SetLength(order, ps.numJobs);
  SetLength(rem, ps.numJobs);

  SetOrderToDescFts;
  ps.InvertPrecedence;
  TSSGS.Solve(order, z, sts, resRemaining);

  SetOrderToAscSts;
  ps.InvertPrecedence;
  TSSGS.Solve(order, z, sts, resRemaining);
end;

end.
