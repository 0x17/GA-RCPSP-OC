unit tests;

interface

procedure RunTests;

implementation

uses projectdata, ssgs, globals, sysutils, topsort, profit, visualizer, classes;

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

function SetupTests: TList;
begin
  result := TList.Create;
  //result.Add(@TestVisualizeGraph);
  //result.Add(@TestDispositionMethod);
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
