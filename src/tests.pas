unit tests;

interface

procedure RunTests;

implementation

uses projectdata, ssgs, globals, sysutils, topsort, profit, visualizer, classes, ssgsoc, ssgsmod;

type TTestFunc = procedure;

procedure InitExampleProject;
const FNAME = 'testproj.sm';
begin
  if ps <> nil then FreeAndNil(ps);

  ps := ProjData.Create;
  ps.LoadFromFile(fname);

  TTopSort.Sort(ps.topOrder);
  TProfit.CalcMinMaxMakespanCosts;

  ps.ComputeESFTS;
end;

procedure TestVisualizeGraph; begin TVisualizer.VisualizeGraph('testgraph'); end;

procedure TestDispositionMethod; begin
  TVisualizer.VisualizeGraph('beforedisposition');
  ps.ReorderJobsAscDepth;
  TVisualizer.VisualizeGraph('afterdisposition');
end;

procedure TestSSGSTau;
var
  sts: JobData;
  tau: ExtArray;
  resRem: ResourceProfile;
  j: Integer;
begin
  ps.ReorderJobsAscDepth;
  TTopSort.Sort(ps.topOrder);
  ps.ComputeESFTS;

  SetLength(tau, 8);
  for j := 0 to 7 do
      tau[j] := 0.0;
  tau[2] := 2.0 / 3.0;

  TSSGSOC.SolveWithTau(ps.topOrder, tau, sts, resRem);
  TVisualizer.VisualizeSchedule(sts, 'tauschedule');
end;

procedure TestSSGSLower;
var
  order, b, sts: JobData;
  resRem: ResourceProfile;
  linked, upper: Boolean;
  j: Integer;
begin
  linked := False;
  upper := False;

  ps.ReorderJobsAscDepth;
  TTopSort.Sort(ps.topOrder);
  ps.ComputeESFTS;

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

procedure TestSSGSUpper;
var
  order, b, sts: JobData;
  resRem: ResourceProfile;
  linked, upper: Boolean;
  j: Integer;
begin
  linked := False;
  upper := True;

  ps.ReorderJobsAscDepth;
  TTopSort.Sort(ps.topOrder);
  ps.ComputeESFTS;

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

function SetupTests: TList;
begin
  result := TList.Create;
  result.Add(@TestVisualizeGraph);
  result.Add(@TestDispositionMethod);
  result.Add(@TestSSGSTau);
  result.Add(@TestSSGSUpper);
  result.Add(@TestSSGSLower);
end;

procedure RunTests;
var
  testfs: TList;
  funcp: Pointer;
begin
  testfs := SetupTests;

  for funcp in testfs do begin
    InitExampleProject;
    TTestFunc(funcp)();
    FreeAndNil(ps);
  end;

  FreeAndNil(testfs);
end;

end.
