unit main;

//{$mode objfpc}{$H+}

interface

procedure Entrypoint();

implementation

uses Classes, SysUtils, projectdata, ssgs, topsort, printing, ssgsoc, profit, helpers, operators, gassgsoc, stopwatch;

procedure TestSSGS(const ps: ProjData); forward;
procedure TestSSGSOC(const ps: ProjData); forward;
procedure PrintProjects(); forward;
procedure TestSwapNeighborhood(const ps: ProjData); forward;
procedure TestGeneticAlgorithms(const ps: ProjData); forward;

procedure WriteOptsAndTime;
var
   fnames: TStringList;
   fname: String;
   ps: ProjData;
   sts: JobData;
   algo: TGA_SSGS_OC;
   sw: TStopwatch;
   profit: Double;
   fp: TextFile;
   time: Cardinal;
   timeSec: Double;
   ctr: Integer;
   bestOrder: JobData;
begin
  AssignFile(fp, 'ssgsocOptsAndTime.txt');
  Rewrite(fp);
  WriteLn(fp, 'filename;profit;solvetime');
  fnames := ListProjFilesInDir('32Jobs');
  ps := ProjData.Create;
  ctr := 0;
  for fname in fnames do
  begin
    ps.LoadFromFile(fname);
    TopologicalOrder(ps, ps.topOrder);
    CalcMinMaxMakespanCosts(ps);

    sw := TStopwatch.Create;
    sw.Start;

    algo := TGA_SSGS_OC.Create(ps);
    profit := algo.Run(sts, bestOrder);
    algo.Free;

    time := sw.Stop();
    timeSec := time / 1000.0;
    sw.Free;

    WriteLn(fp, fname, ';', Format('%f', [profit]), ';', Format('%f', [timeSec]));
    Flush(fp);
    WriteLn(fname, ';', Format('%f', [profit]), ';', Format('%f', [timeSec]));
    inc(ctr);
    if ctr = 50 then break;
  end;
  CloseFile(fp);
  fnames.Free;
  ps.Free;
end;

procedure TestOneProject;
const
  PROJ_FILENAME = '32Jobs\Modellendogen0001.DAT';
var
  ps: ProjData;
begin
  ps := ProjData.Create;
  ps.LoadFromFile(PROJ_FILENAME);
  TopologicalOrder(ps, ps.topOrder);
  CalcMinMaxMakespanCosts(ps);

//  PrintProject(ps);
//  TestSSGS(ps);
  WriteLn('---------------------');

//  TestSSGSOC(ps);
//  PrintProjects();

  TestGeneticAlgorithms(ps);
  //TestSSGSOC(ps);

  ps.Free;

  ReadLn;
end;

procedure Entrypoint();
begin
  //ReportMemoryLeaksOnShutdown := True;
  //TestOneProject;
  WriteOptsAndTime;
end;

procedure TestGeneticAlgorithms(const ps: ProjData);
var
  sts: JobData;
  algo: TGA_SSGS_OC;
  sw: TStopwatch;
  profit: Double;
  bestOrder: JobData;
begin
  sw := TStopwatch.Create;
  sw.Start;

  algo := TGA_SSGS_OC.Create(ps);
  profit := algo.Run(sts, bestOrder);

  PrintActivityList(bestOrder);
  PrintSchedule(ps, sts);

  WriteLn(Format('Profit = %f', [profit]));
  WriteLn(Format('Total oc costs = %f', [TotalOCCostsForSchedule(ps, sts)]));
  WriteLn('Time = ', sw.Stop());

  sw.Free;
  algo.Free;
end;

procedure PrintProjects();
var
   fnames: TStringList;
   fname: String;
begin
  fnames := ListProjFilesInDir('j30');
  for fname in fnames do
      WriteLn(fname);
  fnames.Free;
end;

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

