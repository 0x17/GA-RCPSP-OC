unit projectdata;

// Projekt/Testinstanz-Klasse und verschiedene Datentypen

interface

uses classes, sysutils, helpers;

type
  ByteMx2D = Array of Array of Byte;
  JobData = Array of Integer;
  ResData = Array of Integer;
  ResDataDbl = Array of Double;
  JobResData = Array of Array of Integer;
  ResourceProfile = Array of Array of Integer;
  JobDataArray = Array of Array of Integer;

  ProjData = class(TObject)
    numJobs, numRes, numPeriods: Integer;
    adjMx: ByteMx2D;
    durations: JobData;
    demands: JobResData;
    capacities, zmax: ResData;
    kappa: ResDataDbl;
    topOrder: JobData;

    minMs, maxMs: Integer;
    minCosts, maxCosts: Double;

    name: String;

    procedure LoadFromFile(filename: String);
    class procedure InitPriorityRulesFromFile(const ps: ProjData; out rules: JobDataArray);

    procedure WriteToFile(const sts: JobData);
    procedure CheckScheduleFeasibility(const sts: JobData);
    procedure CheckOrderFeasibility(const order: JobData);

  private
    procedure ParsePrecedenceLine(var fp: TextFile);
    procedure ParseReqDur(var fp: TextFile);
  end;

implementation

const
  lineFeed = #10;

class procedure ProjData.InitPriorityRulesFromFile(const ps: ProjData; out rules: JobDataArray);
var
  i, j: Integer;
  fp: TextFile;
begin
  SetLength(rules, 13, ps.numJobs);
  AssignFile(fp, ps.name+'.PRULES');
  Reset(fp);
  for i := 0 to 12 do
  begin
    for j := 0 to ps.numJobs-1 do
    begin
      Read(fp, rules[i,j]);
      dec(rules[i,j]);
    end;
  end;
  CloseFile(fp);
end;

procedure ProjData.ParsePrecedenceLine(var fp: TextFile);
var
  i, jobNr, ignore, numSuccs, succNr: Integer;
begin
  Read(fp, jobNr);
  Read(fp, ignore);
  Read(fp, numSuccs);
  for i := 0 to numSuccs - 1 do
  begin
    Read(fp, succNr);
    adjMx[jobNr-1, succNr-1] := 1;
  end;
end;

procedure ProjData.ParseReqDur(var fp: TextFile);
var
  r, ignore, jobNr: Integer;
begin
  Read(fp, jobNr);
  Read(fp, ignore);
  Read(fp, durations[jobNr-1]);
  for r := 0 to numRes - 1 do
      Read(fp, demands[jobNr-1, r]);
  SkipChar(fp, lineFeed, 1);
end;

procedure ProjData.LoadFromFile(filename: String);
var
  fp: TextFile;
  i, j, r: Integer;
begin
  name := filename;
  AssignFile(fp, filename);
  Reset(fp);

  // Cardinalities
  SkipChar(fp, ':', 4);
  Read(fp, numJobs);

  SkipChar(fp, ':', 1);
  Read(fp, numPeriods);

  SkipChar(fp, ':', 1);
  Read(fp, numRes);

  SkipChar(fp, ':', 4);
  SkipChar(fp, lineFeed, 2);

  // Precedence
  SetLength(adjMx, numJobs, numJobs);
  for i := 0 to numJobs - 1 do
    for j := 0 to numJobs - 1 do
        adjMx[i,j] := 0;

  for j := 1 to numJobs do
    ParsePrecedenceLine(fp);

  // Durations and requests
  SkipChar(fp, lineFeed, 5);
  SetLength(durations, numJobs);
  SetLength(demands, numJobs, numRes);
  for j := 1 to numJobs do
    ParseReqDur(fp);

  // Capacities
  SkipChar(fp, lineFeed, 3);
  SetLength(capacities, numRes);
  for r := 0 to numRes - 1 do
      Read(fp, capacities[r]);

  // Setze kappa und zmax erstmal auf feste (bzw. kapazitätsabhängige) Werte für alle Projekte.
  SetLength(zmax, numRes);
  SetLength(kappa, numRes);
  for r := 0 to numRes - 1 do
  begin
      zmax[r] := Trunc(0.5 * capacities[r]);
      kappa[r] := 0.5;
  end;

  CloseFile(fp);
end;

procedure ProjData.WriteToFile(const sts: JobData);
const
  lineFeed = #10;
var
  fp: TextFile;
  i: Integer;
begin
  AssignFile(fp, 'testsched.txt');
  ReWrite(fp);
  for i := 1 to numJobs do
    Write(fp, i, '->', sts[i-1], lineFeed);
  CloseFile(fp);
end;

procedure ProjData.CheckScheduleFeasibility(const sts: JobData);
var
  i, j: Integer;
begin
  for i := 0 to numJobs-1 do
    for j := 0 to numJobs-1 do
      if (adjMx[i,j] = 1) and (sts[i]+durations[i]>sts[j]) then
        WriteLn(i, '->', j, ' infeasible!');
end;

procedure ProjData.CheckOrderFeasibility(const order: JobData);
var
  i, j, k, ix1, ix2: Integer;
begin
  ix1 := 0;
  ix2 := 0;
  for i := 0 to numJobs-1 do
    for j := 0 to numJobs-1 do
    begin
      for k := 0 to numJobs-1 do
      begin
        if order[k] = i then ix1 := k;
        if order[k] = j then ix2 := k;
      end;
      if (adjMx[i,j] = 1) and (ix1 > ix2) then
        WriteLn(i, '->', j, ' infeasible!');
    end;
end;

end.

