unit priorityrules;

interface

uses projectdata, globals, sysutils, topsort, profit;

procedure TestPRules;

procedure PrecomputePriorityRules(filename: String);
procedure ComputePriorityRules(out prules: JobDataArray);

implementation

procedure TestPRules;
begin
  ps := ProjData.Create;
  ps.LoadFromFile('j301_1.sm');
  TTopSort.Sort(ps.topOrder);
  CalcMinMaxMakespanCosts;
  ps.ComputeESFTS;

  PrecomputePriorityRules('myprules.txt');

  ps.Free;
end;

const NUM_PRULES = 13;

procedure PrecomputePriorityRules(filename: String);
var
  fp: TextFile;
  prules: JobDataArray;
  i, j: Integer;
begin
  ComputePriorityRules(prules);
  AssignFile(fp, filename);
  Rewrite(fp);
  
  for i := 0 to NUM_PRULES - 1 do
  begin
    for j := 0 to ps.numJobs - 1 do
      Write(fp, IntToStr(j) + ' ');
    WriteLn;
  end;
  
  CloseFile(fp);
end;

procedure SortActivityListByPriorities(var order: JobData; var pvals: array of Integer); forward;

procedure Reverse(const inOrder: JobData; out outOrder: JobData);
var i: Integer;
begin
  SetLength(outOrder, ps.numJobs);
  for i := 0 to ps.numJobs-1 do
    outOrder[i] := inOrder[ps.numJobs-1-i];
end;

procedure ComputePriorityRules(out prules: JobDataArray);
var
  i, j, r: Integer;
  numSuccs, isSucc, gprw, cumResReq, slts: JobData;

  procedure CollectSuccs(rjob: Integer);
  var
    k: Integer;
  begin
    for k := 0 to ps.numJobs - 1 do
        if ps.adjMx[rjob, k] = 1 then
        begin
          isSucc[i] := 1;
          gprw[i] := gprw[i] + ps.durations[k];
          CollectSuccs(k);
        end;
  end;
  
  procedure ComputeSlackTimes;
  var
    job: Integer;
  begin
    SetLength(slts, ps.numJobs);
    for job := 0 to ps.numJobs - 1 do
      slts[job] := ps.lfts[job] - (ps.ests[job] + ps.durations[job]);
  end;

begin
  SetLength(prules, NUM_PRULES);
  for i := 0 to NUM_PRULES-1 do
  begin
    SetLength(prules[i], ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      prules[i][j] := ps.topOrder[j];
  end;

  // shortest processing time
  SortActivityListByPriorities(prules[0], ps.durations);
  // largest processing time
  Reverse(prules[0], prules[1]);

  // most immediate successors
  SetLength(numSuccs, ps.numJobs);
  for i := 0 to ps.numJobs-1 do
  begin
    numSuccs[i] := 0;
    for j := 0 to ps.numJobs-1 do
      if ps.adjMx[i, j] = 1 then
        inc(numSuccs[i]);
  end;
  SortActivityListByPriorities(numSuccs, prules[2]);
  // least immediate successors
  Reverse(prules[2], prules[3]);

  // most total successors & gprw
  SetLength(gprw, ps.numJobs);
  SetLength(isSucc, ps.numJobs);
  for i := 0 to ps.numJobs-1 do
  begin
    gprw[i] := 0;

    for j := 0 to ps.numJobs-1 do
      isSucc[j] := 0;
    CollectSuccs(i);

    numSuccs[i] := 0;
    for j := 0 to ps.numJobs-1 do
        if isSucc[j] = 1 then
          inc(numSuccs[i]);
  end;
  SortActivityListByPriorities(numSuccs, prules[4]);
  SortActivityListByPriorities(gprw, prules[5]);
  // least total successors
  Reverse(prules[4], prules[6]);

  // est
  SortActivityListByPriorities(ps.ests, prules[7]);
  // ect
  SortActivityListByPriorities(ps.efts, prules[8]);

  // lst
  SortActivityListByPriorities(ps.lsts, prules[9]);
  // lct
  SortActivityListByPriorities(ps.lfts, prules[10]);

  // mslk
  ComputeSlackTimes;
  SortActivityListByPriorities(slts, prules[11]);

  // grr
  SetLength(cumResReq, ps.numJobs);
  for i := 0 to ps.numJobs - 1 do
  begin
      cumResReq[i] := 0;
      for r := 0 to ps.numRes - 1 do
          cumResReq[i] := cumResReq[i] + ps.demands[i, r];
  end;
  SortActivityListByPriorities(cumResReq, prules[12]);

end;

procedure SortActivityListByPriorities(var order: JobData; var pvals: array of Integer);
  procedure Helper(iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid, T: Integer;
    T2: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := pvals[(Lo + Hi) div 2];
    repeat
      while pvals[Lo] < Mid do Inc(Lo);
      while pvals[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := pvals[Lo];
        pvals[Lo] := pvals[Hi];
        pvals[Hi] := T;

        T2 := order[Lo];
        order[Lo] := order[Hi];
        order[Hi] := T2;

        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Helper(iLo, Hi);
    if Lo < iHi then Helper(Lo, iHi);
  end;
begin
  Helper(0, ps.numJobs-1);
end;

end.
