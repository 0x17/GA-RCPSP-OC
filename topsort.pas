unit topsort;

//{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, projectdata;

procedure TopologicalOrder(const ps: ProjData; out order: JobData);

implementation

function AllPredsBefore(const ps: ProjData; var order: JobData; i, j: Integer): Boolean;
var k, l: Integer;
    predInOrder: Boolean;
begin
  for k := 0 to ps.numJobs - 1 do
    // k ist Vorgänger von j
    if ps.adjMx[k, j] = 1 then
    begin
      predInOrder := false;
      for l := 0 to i - 1 do
        if order[l] = k then
        begin
           predInOrder := true;
           break;
        end;
      // und k ist noch nicht in Aktivitätenliste -> j darf nicht dazu
      if not predInOrder then
      begin
        result := false;
        exit;
      end;
    end;
  result := true;
end;

function FindJobWithAllPredsBefore(const ps: ProjData; var order: JobData; i: Integer): Integer;
var
  j, k: Integer;
  alreadyInOrder: Boolean;
begin
  for j := 0 to ps.numJobs - 1 do
  begin
    alreadyInOrder := false;
    // Job j bereits in Aktivitätenliste?
    for k := 0 to i - 1 do
        if order[k] = j then
          alreadyInOrder := true;
    // Nur falls noch nicht in Aktivitätenliste -> reinnehmbar
    if not alreadyInOrder then
      // Alle Vorgänger schon davor in Aktivitätenliste
      if AllPredsBefore(ps, order, i, j) then
      begin
        result := j;
        exit;
      end;
  end;
  result := -1;
end;

procedure TopologicalOrder(const ps: ProjData; out order: JobData);
var i: Integer;
begin
  SetLength(order, ps.numJobs);
  order[0] := 0;
  for i := 1 to ps.numJobs - 1 do
    order[i] := FindJobWithAllPredsBefore(ps, order, i);
end;

end.

