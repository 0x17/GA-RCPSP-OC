unit printing;

interface

uses classes, sysutils, projectdata;

procedure PrintSchedule(const ps: ProjData; const sts: JobData);
procedure PrintProject(var ps: ProjData);
procedure PrintActivityList(const order: JobData);
procedure PrintTALBPair(const pair: TALBPair);
procedure PrintTALOCPair(const pair: TALOCPair);

implementation

procedure PrintActivityList(const order: JobData);
var i: Integer;
begin
  for i := Low(order) to High(order) do
      Write(order[i], ',');
  WriteLn;
end;

procedure PrintResourceProfile(const profile: ResourceProfile);
var r, t: Integer;
begin
  for r := Low(profile) to High(profile) do
  begin
    for t := Low(profile[0]) to High(profile[0]) do
      Write(profile[r,t], ',');
    WriteLn;
  end;
  WriteLn;
end;

procedure PrintTALOCPair(const pair: TALOCPair);
begin
  PrintActivityList(pair.order);
  PrintResourceProfile(pair.oc);
end;

procedure PrintTALBPair(const pair: TALBPair);
begin
  PrintActivityList(pair.order);
  PrintActivityList(pair.b);
end;

procedure PrintSchedule(const ps: ProjData; const sts: JobData);
var i: Integer;
begin
  for i := 0 to ps.numJobs-1 do
    WriteLn(IntToStr(i), '->', IntToStr(sts[i]));
  WriteLn('Makespan = ', sts[ps.numJobs-1]);
end;

procedure PrintProject(var ps: ProjData);
var i, j: Integer;
begin
  WriteLn('Num jobs = ', ps.numJobs);
  WriteLn('Links');
  for i := 0 to ps.numJobs - 1 do
      for j := 0 to ps.numJobs - 1 do
          if ps.adjMx[i,j] = 1 then
            WriteLn(i+1, '->', j+1);
  WriteLn('Durations');
  for i := 0 to ps.numJobs - 1 do
      WriteLn('d[', i, ']=', ps.durations[i]);
  WriteLn('Demands');
  for i := 0 to ps.numJobs - 1 do
      for j := 0 to ps.numRes - 1 do
          WriteLn('k[', i, ',', j, ']=', ps.demands[i,j]);
end;

end.

