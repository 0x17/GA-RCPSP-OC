unit projectdata;

interface

uses helpers;

type
  JobData = Array of Integer;
  JobSet = JobData;
  ResData = Array of Integer;
  ResDataDbl = Array of Double;
  JobResData = Array of Array of Integer;
  ResourceProfile = Array of Array of Integer;
  JobDataArray = Array of JobData;
  JobSetArray = JobDataArray;

  ProjData = class
    numJobs, numRes, numPeriods, T: Integer;
    adjMx: ByteMx2D;
    durations: JobData;
    demands: JobResData;
    capacities, zmax: ResData;
    kappa: ResDataDbl;
    topOrder: JobData;

    ests, efts, lsts, lfts: JobData;
    revenueBuf, revenueBuf2: Array of Double;

    minMs, maxMs: Integer;
    minCosts, maxCosts: Double;

    zeroOc, maxOc: ResourceProfile;

    name: String;

    class procedure InitPriorityRulesFromFile(const ps: ProjData; out rules: JobDataArray);

    procedure LoadFromFile(const filename: String);
    procedure ComputeESFTS;

    procedure InvertPrecedence;
    procedure ReorderJobsAscDepth;

    procedure CheckSchedulePrecedenceFeasibility(const sts: JobData);
    function OrderFeasible(const order: JobData): Boolean;

    procedure InferProfileFromSchedule(const sts: JobData; out resRem: ResourceProfile); overload;
    procedure InferProfileFromSchedule(const sts: JobData; out z, resRem: ResourceProfile); overload;
    procedure InferOCFromSchedule(const resRem: ResourceProfile; out z: ResourceProfile);
    function ResourceUtilisationRatio(const resRemaining: ResourceProfile; t: Integer): Double;

    procedure Fill(var jobs: JobData; val: Integer); inline;

    function JobSetCardinality(const aset: JobSet): Integer;
    function JobSetNth(const aset: JobSet; n: Integer): Integer;

    function MaxValue(jobs: JobData): Integer; inline;

  private
    procedure ParsePrecedenceLine(var fp: TextFile);
    procedure ParseReqDur(var fp: TextFile);

    procedure InitMaxOC;
    procedure InitZeroOC;
  end;

implementation

uses classes, sysutils, math;

const lineFeed = #10;

class procedure ProjData.InitPriorityRulesFromFile(const ps: ProjData; out rules: JobDataArray);
var
  i, j: Integer;
  fp: TextFile;
begin
  SetLength(rules, 13, ps.numJobs);
  AssignFile(fp, ps.name+'.PRULES');
  Reset(fp);
  for i := 0 to 12 do begin
    for j := 0 to ps.numJobs-1 do begin
      Read(fp, rules[i,j]);
      dec(rules[i,j]);
    end;
  end;
  CloseFile(fp);
end;

procedure ProjData.ParsePrecedenceLine(var fp: TextFile);
var i, jobNr, ignore, numSuccs, succNr: Integer;
begin
  Read(fp, jobNr);
  Read(fp, ignore);
  Read(fp, numSuccs);
  for i := 0 to numSuccs - 1 do begin
    Read(fp, succNr);
    adjMx[jobNr-1, succNr-1] := 1;
  end;
end;

procedure ProjData.ParseReqDur(var fp: TextFile);
var r, ignore, jobNr: Integer;
begin
  Read(fp, jobNr);
  Read(fp, ignore);
  Read(fp, durations[jobNr-1]);
  for r := 0 to numRes - 1 do
      Read(fp, demands[jobNr-1, r]);
  THelper.SkipChar(fp, lineFeed, 1);
end;

procedure ProjData.ComputeESFTS;
var i, j, k, l, lastPredFt, firstSuccSt: Integer;
begin
  SetLength(ests, numJobs);
  SetLength(efts, numJobs);
  SetLength(lsts, numJobs);
  SetLength(lfts, numJobs);

  ests[0] := 0;
  efts[0] := 0;

  for i := 1 to numJobs-1 do begin
    lastPredFt := 0;
    j := topOrder[i];
    for k := 0 to i-1 do begin
      l := topOrder[k];
      if adjMx[l, j] = 1 then
        if efts[l] > lastPredFt then
          lastPredFt := efts[l];
    end;
    ests[j] := lastPredFt;
    efts[j] := ests[j] + durations[j];
  end;

  lfts[numJobs-1] := T;
  lsts[numJobs-1] := lfts[numJobs-1];

  for i := numJobs-2 downto 0 do begin
    firstSuccSt := T;
    j := topOrder[i];
    for k := i+1 to numJobs-1 do begin
      l := topOrder[k];
      if adjMx[j, l] = 1 then
        if lsts[l] < firstSuccSt then
          firstSuccSt := lsts[l];
    end;
    lfts[j] := firstSuccSt;
    lsts[j] := lfts[j] - durations[j];
  end;
end;

procedure ProjData.LoadFromFile(const filename: String);
var
  fp: TextFile;
  i, j, r: Integer;
begin
  name := filename;
  AssignFile(fp, filename);
  Reset(fp);

  // Cardinalities
  THelper.SkipChar(fp, ':', 4);
  Read(fp, numJobs);

  THelper.SkipChar(fp, ':', 1);
  Read(fp, numPeriods);

  THelper.SkipChar(fp, ':', 1);
  Read(fp, numRes);

  THelper.SkipChar(fp, ':', 4);
  THelper.SkipChar(fp, lineFeed, 2);

  // Precedence
  SetLength(adjMx, numJobs, numJobs);
  for i := 0 to numJobs-1 do
    for j := 0 to numJobs-1 do
      adjMx[i,j] := 0;

  for j := 1 to numJobs do
    ParsePrecedenceLine(fp);

  // Durations and requests
  THelper.SkipChar(fp, lineFeed, 5);
  SetLength(durations, numJobs);
  SetLength(demands, numJobs, numRes);
  for j := 1 to numJobs do
    ParseReqDur(fp);

  // Capacities
  THelper.SkipChar(fp, lineFeed, 3);
  SetLength(capacities, numRes);
  for r := 0 to numRes - 1 do
    Read(fp, capacities[r]);

  // Setze kappa und zmax erstmal auf feste (bzw. kapazitätsabhängige) Werte für alle Projekte.
  SetLength(zmax, numRes);
  SetLength(kappa, numRes);
  for r := 0 to numRes - 1 do begin
    zmax[r] := Trunc(0.5 * capacities[r]);
    kappa[r] := 0.5;
  end;

  T := 0;
  for j := 0 to numJobs-1 do
    T := T + durations[j];

  CloseFile(fp);

  InitMaxOC;
  InitZeroOC;
end;

// Disposition method
procedure ProjData.ReorderJobsAscDepth;
var
  depths, mapping, invMapping: JobData;

  procedure Traverse(src, depth: Integer);
  var k: Integer;
  begin
    if depth > depths[src] then
      depths[src] := depth;

    for k := 0 to numJobs-1 do
      if adjMx[src,k] = 1 then
        Traverse(k, depth+1);
  end;

  procedure DepthsToMapping;
  var ctr, d, j, maxDepth: Integer;
  begin
    maxDepth := MaxValue(depths);

    ctr := 0;
    for d := 0 to maxDepth do
      for j := 0 to numJobs-1 do
        if depths[j] = d then begin
          mapping[ctr] := j;
          invMapping[j] := ctr;
          inc(ctr);
        end;
  end;

  procedure AdaptPrecedenceRelation;
  var
    i, j: Integer;
    adjMxCp: ByteMx2D;
  begin
    adjMxCp := adjMx;
    SetLength(adjMxCp, numJobs, numJobs);
    for i := 0 to numJobs-1 do
      for j := 0 to numJobs-1 do
        adjMx[i,j] := adjMxCp[mapping[i], mapping[j]];
  end;

  procedure AdaptDurationsAndDemands;
  var
    j, r: Integer;
    durationsCp: JobData;
    demandsCp: JobResData;
  begin
    durationsCp := durations;
    SetLength(durationsCp, numJobs);
    for j := 0 to numJobs - 1  do
      durations[invMapping[j]] := durationsCp[j];

    demandsCp := demands;
    SetLength(demandsCp, numJobs, numRes);
    for j := 0 to numJobs - 1 do
      for r := 0 to numRes - 1 do
        demands[invMapping[j], r] := demandsCp[j, r];
  end;

begin
  SetLength(depths, numJobs);
  Traverse(0, 0);

  SetLength(mapping, numJobs);
  SetLength(invMapping, numJobs);
  DepthsToMapping;

  AdaptPrecedenceRelation;
  AdaptDurationsAndDemands;
end;

procedure ProjData.InvertPrecedence;
begin
  THelper.Transpose(adjMx);
end;

procedure ProjData.CheckSchedulePrecedenceFeasibility(const sts: JobData);
var i, j: Integer;
begin
  for j := 0 to numJobs-1 do
    for i := 0 to numJobs-1 do
      if adjMx[i,j] = 1 then
        Assert(sts[i] + durations[i] <= sts[j]);
end;

function ProjData.OrderFeasible(const order: JobData): Boolean;
var i, j: Integer;
begin
  result := True;
  for j := 0 to numJobs-1 do
    for i := 0 to j do
      if adjMx[order[j], order[i]] = 1 then begin
        result := False;
        Exit;
      end;
end;

procedure ProjData.InferProfileFromSchedule(const sts: JobData; out resRem: ResourceProfile);
var j, r, t: Integer;
begin
  SetLength(resRem, numRes, numPeriods);

  for r := 0 to numRes-1 do
    for t := 0 to numPeriods-1 do
      resRem[r,t] := capacities[r];

  for j := 0 to numJobs-1 do
    for r := 0 to numRes-1 do
      if demands[j,r] > 0 then
        for t := sts[j] to sts[j]+durations[j]-1 do
          resRem[r,t] := resRem[r,t] - demands[j,r];
end;

procedure ProjData.InferProfileFromSchedule(const sts: JobData; out z, resRem: ResourceProfile);
begin
  InferProfileFromSchedule(sts, resRem);
  InferOCFromSchedule(resRem, z);
end;

procedure ProjData.InferOCFromSchedule(const resRem: ResourceProfile; out z: ResourceProfile);
var r, t: Integer;
begin
  SetLength(z, numRes, numPeriods);
  for r := 0 to numRes-1 do
    for t := 0 to numPeriods-1 do
      z[r,t] := Max(0, -resRem[r,t]);
end;

function ProjData.ResourceUtilisationRatio(const resRemaining: ResourceProfile; t: Integer): Double;
var
  r: Integer;
  sumRatios: Double;
begin
  sumRatios := 0;
  for r := 0 to numRes - 1 do
    sumRatios := sumRatios + (capacities[r] - resRemaining[r,t])/(capacities[r] + zmax[r]);
  result := 1 / numRes * sumRatios;
end;

procedure ProjData.Fill(var jobs: JobData; val: Integer);
var j: Integer;
begin
  for j := 0 to numJobs - 1 do
    jobs[j] := val;
end;

function ProjData.JobSetCardinality(const aset: JobSet): Integer;
var j: Integer;
begin
  result := 0;
  for j := 0 to numJobs-1 do
    result := result + aset[j];
end;

function ProjData.JobSetNth(const aset: JobSet; n: Integer): Integer;
var j, ctr: Integer;
begin
  ctr := 0;
  result := 0;
  for j := 0 to numJobs-1 do
  begin
    ctr := ctr + aset[j];
    if ctr = n+1 then begin
      result := j;
      Exit;
    end;
  end;
end;

procedure ProjData.InitZeroOC;
var r, t: Integer;
begin
  SetLength(zeroOc, numRes, numPeriods);
  for r := 0 to numRes - 1 do
      for t := 0 to numPeriods - 1 do
          zeroOc[r,t] := 0;
end;

procedure ProjData.InitMaxOC;
var r, t: Integer;
begin
  SetLength(maxOc, numRes, numPeriods);
  for r := 0 to numRes - 1 do
      for t := 0 to numPeriods - 1 do
          maxOc[r,t] := zmax[r];
end;

function ProjData.MaxValue(jobs: JobData): Integer;
var j: Integer;
begin
  result := 0;
  for j := 0 to numJobs - 1 do
    if jobs[j] > result then
      result := jobs[j];
end;

end.

