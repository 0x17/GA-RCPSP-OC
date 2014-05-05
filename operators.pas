unit operators;

interface

uses classes, sysutils, projectdata, math, constants;

procedure SwapNeighborhood(const ps: ProjData; var lambda: JobData);
procedure OnePointCrossover(const mother, father: JobData; var daughter: JobData); overload;
procedure OnePointCrossover(const mother, father: ResourceProfile; var daughter: ResourceProfile; numRes, numPeriods: Integer); overload;

implementation

procedure Swap(var lambda: JobData; i1, i2: Integer);
var
  tmp: Integer;
begin
  tmp := lambda[i1];
  lambda[i1] := lambda[i2];
  lambda[i2] := tmp;
end;

procedure SwapNeighborhood(const ps: ProjData; var lambda: JobData);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 1 do // TODO: Variable Mutationswkeit.
    if (RandomRange(1, 100) <= 3) and (ps.adjMx[lambda[i-1], lambda[i]] = 0) then
      Swap(lambda, i, i-1);
end;

procedure OnePointCrossover(const mother, father: JobData; var daughter: JobData);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
begin
  len := Length(mother);
  q := RandomRange(0, len-1);

  // 0,..,q von Mutter
  for i := 0 to q do
    daughter[i] := mother[i];

  // q+1,..,len-1 von Vater, falls nicht von Mutter
  k := q+1;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q
    fromMother := false;
    for j := 0 to q do
      if daughter[j] = father[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughter[k] := father[i];
      inc(k);
    end;
  end;
end;

procedure OnePointCrossover(const mother, father: ResourceProfile; var daughter: ResourceProfile; numRes, numPeriods: Integer);
var
  r, q, t: Integer;
begin
  for r := 0 to numRes-1 do
  begin
    q := RandomRange(0, numPeriods-1);
    for t := 0 to numPeriods-1 do
      if t <= q then
        daughter[r,t] := mother[r,t]
      else
        daughter[r,t] := father[r,t];
  end;
end;

end.

