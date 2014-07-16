unit topsort;

interface

uses classes, sysutils, projectdata, helpers, math;

procedure TopologicalOrder(const ps: ProjData; out order: JobData);
procedure RandomTopologicalOrder(const ps: ProjData; out order: JobData);

implementation

// Wahr, gdw. alle Vorgänger von j in order von 0..index-1 liegen.
function AllPredsBefore(const ps: ProjData; var order: JobData; index, j: Integer): Boolean;
var k, l: Integer;
    predInOrder: Boolean;
begin
  for k := 0 to ps.numJobs - 1 do
    // k ist Vorgänger von j
    if ps.adjMx[k, j] = 1 then
    begin
      predInOrder := false;
      for l := 0 to index - 1 do
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

// Suche nächstmöglichen Job, für den alle Vorgänger bereits in Aktivitätenliste
// order vor index liegen.
function FindJobWithAllPredsBefore(const ps: ProjData; var order: JobData; index: Integer): Integer;
var
  j, k: Integer;
  alreadyInOrder: Boolean;
begin
  for j := 0 to ps.numJobs - 1 do
  begin
    alreadyInOrder := false;
    // Job j bereits in Aktivitätenliste?
    for k := 0 to index - 1 do
        if order[k] = j then
          alreadyInOrder := true;
    // Nur falls noch nicht in Aktivitätenliste -> reinnehmbar
    if not alreadyInOrder then
      // Alle Vorgänger schon davor in Aktivitätenliste
      if AllPredsBefore(ps, order, index, j) then
      begin
        result := j;
        exit;
      end;
  end;
  result := -1;
end;

// Schreibe topologische Sortierung der Arbeitsgänge in Ausgabeparameter order.
procedure TopologicalOrder(const ps: ProjData; out order: JobData);
var index: Integer;
begin
  SetLength(order, ps.numJobs);
  order[0] := 0;
  for index := 1 to ps.numJobs - 1 do
    order[index] := FindJobWithAllPredsBefore(ps, order, index);
end;

// Wähle zufällig einen Job aus der Menge der Jobs, welche bereits alle
// Vorgänger links vom index in der Aktivitätenliste haben.
// Für Bestimmung von Initialpopulation von Aktivitätenlisten.
function FindRandomJobWithAllPredsBefore(const ps: ProjData; var order: JobData; index: Integer): Integer;
var
  j, k, numJobsFeasible, rval: Integer;
  alreadyInOrder: Boolean;
  jobFeasible: JobData;
begin
  SetLength(jobFeasible, ps.numJobs);
  numJobsFeasible := 0;

  for j := 0 to ps.numJobs - 1 do
  begin
    alreadyInOrder := false;
    // Job j bereits in Aktivitätenliste?
    for k := 0 to index - 1 do
      if order[k] = j then
      begin
        alreadyInOrder := true;
        jobFeasible[j] := 0;
      end;

    // Nur falls noch nicht in Aktivitätenliste -> reinnehmbar
    if not alreadyInOrder then
      // Alle Vorgänger schon davor in Aktivitätenliste
      if AllPredsBefore(ps, order, index, j) then
      begin
        jobFeasible[j] := 1;
        inc(numJobsFeasible);
      end
      else
        jobFeasible[j] := 0;
  end;

  rval := RandomRangeIncl(1, numJobsFeasible);
  k := 0;
  for j := 1 to ps.numJobs - 1 do
  begin
    if jobFeasible[j] = 1 then
      inc(k);

    if k = rval then
    begin
      result := j;
      exit;
    end;
  end;

  result := -1;
end;

// Schreibe randomisierte topologische Sortierung der Arbeitsgänge in
// Ausgabeparameter order.
procedure RandomTopologicalOrder(const ps: ProjData; out order: JobData);
var i: Integer;
begin
  SetLength(order, ps.numJobs);
  order[0] := 0;
  for i := 1 to ps.numJobs - 1 do
    order[i] := FindRandomJobWithAllPredsBefore(ps, order, i);
end;

end.

