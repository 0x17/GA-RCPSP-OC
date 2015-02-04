unit naive;

interface

uses projectdata, algenerator;

type NaiveGenerator = class(IALGenerator)
    constructor Create;
    procedure PickSample(out order: JobData); override;
  private
    i: Integer;
    prioRules: JobDataArray;
  end;

implementation

uses globals, topsort;

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

end.
