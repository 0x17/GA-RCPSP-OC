unit printing;

//{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, projectdata;

procedure PrintSchedule(const ps: ProjData; const sts: JobData);
procedure PrintProject(var ps: ProjData);
procedure PrintActivityList(const order: JobData);

implementation

procedure PrintActivityList(const order: JobData);
var i: Integer;
begin
  for i := Low(order) to High(order) do
      Write(order[i], ',');
  WriteLn;
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

