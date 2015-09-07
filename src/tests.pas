unit tests;

interface

procedure RunTests;

implementation

uses projectdata, ssgs, globals, sysutils, topsort, profit, visualizer, classes, ssgsoc, ssgsmod, stopwatch, main;

type TTestFunc = procedure;

procedure TestVisualizeGraph; begin TVisualizer.VisualizeGraph('testgraph'); end;

procedure TestDispositionMethod; begin
  TVisualizer.VisualizeGraph('beforedisposition');
  ps.ReorderJobsAscDepth;
  TVisualizer.VisualizeGraph('afterdisposition');
end;

procedure Reorder;
begin
  ps.ReorderJobsAscDepth;
  TTopSort.Sort(ps.topOrder);
  ps.ComputeELSFTS;
end;

procedure TestSSGS;
var
  sts: JobData;
  resRem: ResourceProfile;
begin
  Reorder;
  TSSGS.Solve(ps.topOrder, ps.zeroOc, sts, resRem);
  TVisualizer.VisualizeSchedule(sts, 'ssgsschedule');
end;

procedure TestSSGSTau;
var
  sts: JobData;
  tau: ExtArray;
  resRem: ResourceProfile;
  j: Integer;
begin
  Reorder;

  SetLength(tau, 8);
  for j := 0 to 7 do
      tau[j] := 0.0;
  tau[2] := 2.0 / 3.0;

  TSSGSOC.SolveWithTau(ps.topOrder, tau, sts, resRem);
  TVisualizer.VisualizeSchedule(sts, 'tauschedule');
end;

procedure TestSSGSLowerUnlinked;
var
  order, b, sts: JobData;
  resRem: ResourceProfile;
  linked, upper: Boolean;
  j: Integer;
begin
  linked := False;
  upper := False;

  Reorder;

  SetLength(order, 8);
  order[0] := 0;
  order[1] := 2;
  order[2] := 4;
  order[3] := 1;
  order[4] := 3;
  order[5] := 5;
  order[6] := 6;
  order[7] := 7;

  SetLength(b, 8);
  for j := 0 to 7 do
    b[j] := 0;
  b[2] := 1;

  TSSGSMod.Solve(order, b, sts, resRem, linked, upper);
  TVisualizer.VisualizeSchedule(sts, 'lowerschedule');
end;

procedure TestSSGSUpperUnlinked;
var
  order, b, sts: JobData;
  resRem: ResourceProfile;
  linked, upper: Boolean;
  j: Integer;
begin
  linked := False;
  upper := True;

  Reorder;

  SetLength(order, 8);
  order[0] := 0;
  order[1] := 2;
  order[2] := 4;
  order[3] := 1;
  order[4] := 3;
  order[5] := 5;
  order[6] := 6;
  order[7] := 7;

  SetLength(b, 8);
  for j := 0 to 7 do
    b[j] := 0;
  b[2] := 1;

  TSSGSMod.Solve(order, b, sts, resRem, linked, upper);
  TVisualizer.VisualizeSchedule(sts, 'upperschedule');
end;

procedure TestSSGSOC;
var
  sts: JobData;
  resRem: ResourceProfile;
begin
  Reorder;
  TSSGSOC.Solve(ps.topOrder, sts, resRem);
  TVisualizer.VisualizeSchedule(sts, 'ocschedule');
end;

procedure TestStopwatch;
var sw: TStopwatch;
begin
  sw := TStopwatch.Create;
  FreeAndNil(sw);
end;

procedure CostMinHeurRelaxed(const prioVals: JobData; out sts: JobData);
var
  cests, clsts: JobData;
  jobsRemaining, j, k: Integer;

  function JobWithHighestPriority: Integer;
  var i: Integer;
  begin
    result := 0;
    for i := 1 to ps.numJobs-1 do
      if (prioVals[i] > prioVals[result]) and (sts[i] = -1) then
        result := i;
  end;

  function ExtensionCosts(job: Integer; stj: Integer): Double;
  begin
    result := 0.0;
  end;

  function ComputeBestStartingTime(job: Integer): Integer;
  var
    candidateSt: Integer;
    candidateCosts: Double;
  begin
    candidateSt := cests[job];
    candidateCosts := ExtensionCosts(job, cests[job]);
  end;

  procedure UpdateTimeWindows(job: Integer);
  begin

  end;

begin
  SetLength(cests, ps.numJobs);
  SetLength(clsts, ps.numJobs);
  SetLength(sts, ps.numJobs);

  cests := Copy(ps.ests, 0, ps.numJobs);
  clsts := Copy(ps.lsts, 0, ps.numJobs);
  ps.Fill(sts, -1);
  sts[0] := 0;

  jobsRemaining := ps.numJobs-1;

  while jobsRemaining > 0 do begin
    j := JobWithHighestPriority;
    sts[j] := ComputeBestStartingTime(j);
    UpdateTimeWindows(j);
  end;

end;

procedure TestCostMinHeurRelaxed;
var
  sts: JobData;
  prioVals: JobData;
begin
  prioVals := Copy(ps.topOrder, 0, ps.numJobs);
  CostMinHeurRelaxed(prioVals, sts);
end;

function SetupTests: TList;
begin
  result := TList.Create;

  (*
  result.Add(@TestVisualizeGraph);
  result.Add(@TestDispositionMethod);
  *)

  (*
  result.Add(@TestSSGS);
  result.Add(@TestSSGSTau);
  result.Add(@TestSSGSUpperUnlinked);
  result.Add(@TestSSGSLowerUnlinked);
  result.Add(@TestSSGSOC);
  *)

  //result.Add(@TestStopwatch);

  result.Add(@TestCostMinHeurRelaxed);
end;

procedure RunTests;
var
  testfns: TList;
  funcp: Pointer;
begin
  testfns := SetupTests;

  for funcp in testfns do begin
    TMain.InitProject('testproj.sm');
    TTestFunc(funcp)();
    FreeAndNil(ps);
  end;

  FreeAndNil(testfns);
end;

end.
