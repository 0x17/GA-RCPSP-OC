unit tests;

interface

uses classes, sysutils, projectdata, stopwatch, topsort, ssgs, printing, ssgsoc, profit, operators;

implementation

procedure TestSSGS(const ps: ProjData);
var
  sts, lambda: JobData;
  overcapacity, resRemaining: ResourceProfile;
  sw: TStopwatch;
//  i: Integer;
begin
  TopologicalOrder(ps, lambda);

  SetLength(overcapacity, ps.numRes, ps.numPeriods);
  ZeroOvercapacity(ps, overcapacity);

  sw := TStopwatch.Create;
  sw.Start();

//  for i := 0 to 5096-1 do
  Solve(ps, lambda, overcapacity, sts, resRemaining);

  WriteLn(sw.Stop(), ' msec');
  sw.Free;

  printSchedule(ps, sts);
end;

procedure TestSSGSOC(const ps: ProjData);
var
  sts, lambda: JobData;
  profit: Double;
begin
  TopologicalOrder(ps, lambda);
  profit := SolveWithOC(ps, lambda, sts);
  WriteLn(Format('Profit = %f', [profit]));
  WriteLn(Format('Total oc costs = %f', [TotalOCCostsForSchedule(ps, sts)]));
  printSchedule(ps, sts);
end;

procedure TestSwapNeighborhood(const ps: ProjData);
var
  i: Integer;
  lambda: JobData;
begin
  TopologicalOrder(ps, lambda);
  SwapNeighborhood(ps, lambda);
  for i := 0 to ps.numJobs - 1 do
      Write(lambda[i], ' ');
  WriteLn;
end;

end.

