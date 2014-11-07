unit priorityrules;

interface

uses projectdata, globals;

procedure ComputePriorityRules(out prules: JobDataArray);

implementation

procedure SortActivityListByPriorities(var order: JobData; var pvals: array of Integer;  iLo, iHi: Integer); forward;

procedure Reverse(const inOrder: JobData; out outOrder: JobData);
var i: Integer;
begin
  for i := 0 to ps.numJobs-1 do
    outOrder[i] := inOrder[ps.numJobs-1-i];
end;

procedure ComputePriorityRules(out prules: JobDataArray);
var
  i, j: Integer;
  numSuccs: array of Integer;
begin
  SetLength(prules, 13);
  for i := 0 to 12 do
  begin
    SetLength(prules[i], ps.numJobs);
    for j := 0 to ps.numJobs-1 do
      prules[i][j] := ps.topOrder[j];
  end;

  // shortest processing time
  SortActivityListByPriorities(prules[0], ps.durations, 0, ps.numJobs-1);
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
  // least immediate successors
  Reverse(prules[2], prules[3]);

  // most total successors

end;

procedure SortActivityListByPriorities(var order: JobData; var pvals: array of Integer;  iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid, T: Integer;
  T2: JobData;
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
  if Hi > iLo then SortActivityListByPriorities(order, pvals, iLo, Hi);
  if Lo < iHi then SortActivityListByPriorities(order, pvals, Lo, iHi);
end;

end.
