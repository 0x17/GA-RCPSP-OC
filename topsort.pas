unit topsort;

interface

uses helpers, projectdata, globals;

type TTopSort = class
  class procedure Sort(out order: JobData);
  class procedure RandomSort(out order: JobData);
private
  class function AllPredsBefore(var order: JobData; index, j: Integer): Boolean;
  class function FindJobWithAllPredsBefore(var order: JobData; index: Integer): Integer;
  class function FindRandomJobWithAllPredsBefore(var order: JobData; index: Integer): Integer;
end;

implementation

// Wahr, gdw. alle Vorgänger von j in order von 0..index-1 liegen.
class function TTopSort.AllPredsBefore(var order: JobData; index, j: Integer): Boolean;
var
  k, l: Integer;
  predInOrder: Boolean;
begin
  for k := 0 to ps.numJobs - 1 do
    // k ist Vorgänger von j
    if ps.adjMx[k, j] = 1 then begin
      predInOrder := false;
      for l := 0 to index - 1 do
        if order[l] = k then begin
           predInOrder := true;
           break;
        end;
      // und k ist noch nicht in Aktivitätenliste -> j darf nicht dazu
      if not predInOrder then begin
        result := false;
        exit;
      end;
    end;
  result := true;
end;

// Suche nächstmöglichen Job, für den alle Vorgänger bereits in Aktivitätenliste
// order vor index liegen.
class function TTopSort.FindJobWithAllPredsBefore(var order: JobData; index: Integer): Integer;
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
      if AllPredsBefore(order, index, j) then begin
        result := j;
        exit;
      end;
  end;
  result := -1;
end;

// Schreibe topologische Sortierung der Arbeitsgänge in Ausgabeparameter order.
class procedure TTopSort.Sort(out order: JobData);
var index: Integer;
begin
  SetLength(order, ps.numJobs);
  order[0] := 0;
  for index := 1 to ps.numJobs - 1 do
    order[index] := FindJobWithAllPredsBefore(order, index);
end;

// Wähle zufällig einen Job aus der Menge der Jobs, welche bereits alle
// Vorgänger links vom index in der Aktivitätenliste haben.
// Für Bestimmung von Initialpopulation von Aktivitätenlisten.
class function TTopSort.FindRandomJobWithAllPredsBefore(var order: JobData; index: Integer): Integer;
var
  j, k, numJobsFeasible, rval: Integer;
  alreadyInOrder: Boolean;
  jobFeasible: JobData;
begin
  SetLength(jobFeasible, ps.numJobs);
  numJobsFeasible := 0;

  for j := 0 to ps.numJobs - 1 do begin
    alreadyInOrder := false;
    // Job j bereits in Aktivitätenliste?
    for k := 0 to index - 1 do
      if order[k] = j then begin
        alreadyInOrder := true;
        jobFeasible[j] := 0;
      end;

    // Nur falls noch nicht in Aktivitätenliste -> reinnehmbar
    if not alreadyInOrder then
      // Alle Vorgänger schon davor in Aktivitätenliste
      if AllPredsBefore(order, index, j) then begin
        jobFeasible[j] := 1;
        inc(numJobsFeasible);
      end
      else
        jobFeasible[j] := 0;
  end;

  rval := THelper.RandomRangeIncl(1, numJobsFeasible);
  k := 0;
  for j := 1 to ps.numJobs - 1 do begin
    if jobFeasible[j] = 1 then
      inc(k);

    if k = rval then begin
      result := j;
      exit;
    end;
  end;

  result := -1;
end;

// Schreibe randomisierte topologische Sortierung der Arbeitsgänge in
// Ausgabeparameter order.
class procedure TTopSort.RandomSort(out order: JobData);
var i: Integer;
begin
  SetLength(order, ps.numJobs);
  order[0] := 0;
  for i := 1 to ps.numJobs - 1 do
    order[i] := FindRandomJobWithAllPredsBefore(order, i);
end;

end.

