unit operators;

interface

uses classes, sysutils, projectdata, globals, helpers;

type TALBPair = record
  order, b: JobData;
end;

procedure SwapNeighborhood(var lambda: JobData); overload;
procedure SwapNeighborhood(var lambda, tau: JobData); overload;

procedure OnePointCrossover(const mother, father: JobData; var daughter: JobData); overload;
procedure OnePointCrossover(maxQ: Integer; const mother, father: ResourceProfile; var daughter: ResourceProfile; numRes, numPeriods: Integer); overload;
procedure OnePointCrossover(const mother, father: TALBPair; var daughter: TALBPair); overload;
procedure OnePointCrossover(const motherOrder, motherTau, fatherOrder, fatherTau: JobData; var daughterOrder, daughterTau: JobData); overload;

procedure TwoPointCrossover(const mother, father: JobData; var daughter: JobData) overload;
procedure TwoPointCrossover(maxQ: Integer; const mother, father: ResourceProfile; var daughter: ResourceProfile; numRes, numPeriods: Integer); overload;
procedure TwoPointCrossover(const mother, father: TALBPair; var daughter: TALBPair); overload;

implementation

procedure Swap(var lambda, tau: JobData; i1, i2: Integer); forward; overload;
procedure Swap(var lambda: JobData; i1, i2: Integer); forward; overload;

procedure Swap(var lambda, tau: JobData; i1, i2: Integer);
var
  tmp, tmp2: Integer;
begin
  tmp := lambda[i1];
  tmp2 := tau[i1];

  lambda[i1] := lambda[i2];
  tau[i1] := tau[i2];

  lambda[i2] := tmp;
  tau[i2] := tmp2;
end;

procedure SwapNeighborhood(var lambda, tau: JobData);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 2 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[lambda[i-1], lambda[i]] = 0) then
      Swap(lambda, tau, i, i-1);
end;

procedure Swap(var lambda: JobData; i1, i2: Integer);
var
  tmp: Integer;
begin
  tmp := lambda[i1];
  lambda[i1] := lambda[i2];
  lambda[i2] := tmp;
end;

procedure SwapNeighborhood(var lambda: JobData);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 2 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[lambda[i-1], lambda[i]] = 0) then
      Swap(lambda, i, i-1);
end;

procedure OnePointCrossover(const motherOrder, motherTau, fatherOrder, fatherTau: JobData; var daughterOrder, daughterTau: JobData);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
begin
  len := Length(motherOrder);
  q := THelper.RandomRangeIncl(1, len);

  // Ersten q1: 0,..,q-1 von Mutter
  for i := 0 to q-1 do
  begin
    daughterOrder[i] := motherOrder[i];
    daughterTau[i] := motherTau[i];
  end;

  // q,..,len-1 von Vater, falls nicht von Mutter
  k := q;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q-1
    fromMother := false;
    for j := 0 to q-1 do
      if daughterOrder[j] = fatherOrder[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q-1 übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughterOrder[k] := fatherOrder[i];
      daughterTau[k] := fatherTau[i];
      inc(k);
    end;
  end;
end;

procedure OnePointCrossover(const mother, father: JobData; var daughter: JobData);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
begin
  len := Length(mother);
  q := THelper.RandomRangeIncl(1, len);

  // Ersten q1: 0,..,q-1 von Mutter
  for i := 0 to q-1 do
    daughter[i] := mother[i];

  // q,..,len-1 von Vater, falls nicht von Mutter
  k := q;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q-1
    fromMother := false;
    for j := 0 to q-1 do
      if daughter[j] = father[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q-1 übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughter[k] := father[i];
      inc(k);
    end;
  end;
end;

procedure OnePointCrossover(maxQ: Integer; const mother, father: ResourceProfile; var daughter: ResourceProfile; numRes, numPeriods: Integer);
var
  r, q, t: Integer;
begin
  for r := 0 to numRes-1 do
  begin
    q := THelper.RandomRangeIncl(0, maxQ-1);
    for t := 0 to numPeriods-1 do
      if t <= q then
        daughter[r,t] := mother[r,t]
      else
        daughter[r,t] := father[r,t];
  end;
end;

procedure OnePointCrossover(const mother, father: TALBPair; var daughter: TALBPair);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
begin
  len := Length(mother.order);
  q := THelper.RandomRangeIncl(1, len);

  // Ersten q: 0,..,q-1 von Mutter
  for i := 0 to q-1 do
  begin
    daughter.order[i] := mother.order[i];
    daughter.b[i] := mother.b[i];
  end;

  // q,..,len-1 von Vater, falls nicht von Mutter
  k := q;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q-1
    fromMother := false;
    for j := 0 to q-1 do
      if daughter.order[j] = father.order[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q-1 übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughter.order[k] := father.order[i];
      daughter.b[k] := father.b[i];
      inc(k);
    end;
  end;
end;

procedure TwoPointCrossover(const mother, father: JobData; var daughter: JobData);
var
  i, j, k, q1, q2, len, tmp: Integer;
  already: Boolean;
begin
  len := Length(mother);

  q1 := THelper.RandomRangeIncl(1, len);
  q2 := THelper.RandomRangeIncl(1, len);
  if q1 > q2 then begin
    tmp := q1;
    q1 := q2;
    q2 := tmp;
  end;

  // Ersten q1 von Mutter
  for i := 0 to q1-1 do
    daughter[i] := mother[i];

  // q1,..,q2-1 von Vater, falls nicht bereits
  k := q1;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q1-1
    already := false;
    for j := 0 to q1-1 do
      if daughter[j] = father[i] then
        already := true;

    // Falls nicht bereits in 0,..,q übernehme an nächste Stelle in Tochter
    if not(already) then
    begin
      daughter[k] := father[i];
      inc(k);
    end;

    if k = q2 then break;
  end;

  // q2,..,len-1 von Mutter, falls nicht bereits
  k := q2;
  // Probiere alle von Mutter
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q2-1
    already := false;
    for j := 0 to q2-1 do
      if daughter[j] = mother[i] then
        already := true;

    // Falls nicht bereits in 0,..,q übernehme an nächste Stelle in Tochter
    if not(already) then
    begin
      daughter[k] := mother[i];
      inc(k);
    end;
  end;
end;

procedure TwoPointCrossover(maxQ: Integer; const mother, father: ResourceProfile; var daughter: ResourceProfile; numRes, numPeriods: Integer);
var
  r, q1, q2, t: Integer;
begin
  for r := 0 to numRes-1 do
  begin
    q1 := THelper.RandomRangeIncl(0, maxQ-1);
    q2 := THelper.RandomRangeIncl(q1, numPeriods-1);
    for t := 0 to numPeriods-1 do
      if (t <= q1) or (t > q2) then
        daughter[r,t] := mother[r,t]
      else
        daughter[r,t] := father[r,t];
  end;
end;

procedure TwoPointCrossover(const mother, father: TALBPair; var daughter: TALBPair);
var
  i, j, k, q1, q2, len, tmp: Integer;
  already: Boolean;
begin
  len := Length(mother.order);
  q1 := THelper.RandomRangeIncl(1, len);
  q2 := THelper.RandomRangeIncl(1, len);
  if q1 > q2 then begin
    tmp := q1;
    q1 := q2;
    q2 := tmp;
  end;

  // Ersten q1: 0,..,q1-1 von Mutter
  for i := 0 to q1-1 do
  begin
    daughter.order[i] := mother.order[i];
    daughter.b[i] := mother.b[i];
  end;

  // q1,..,q2-1 von Vater, falls nicht bereits
  k := q1;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q1
    already := false;
    for j := 0 to q1-1 do
      if daughter.order[j] = father.order[i] then
        already := true;

    // Falls nicht bereits in 0,..,q übernehme an nächste Stelle in Tochter
    if not(already) then
    begin
      daughter.order[k] := father.order[i];
      daughter.b[k] := father.b[i];
      inc(k);
    end;

    if k = q2 then break;
  end;

  // q2,..,len-1 von Mutter, falls nicht bereits
  k := q2;
  // Probiere alle von Mutter
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q2-1
    already := false;
    for j := 0 to q2-1 do
      if daughter.order[j] = mother.order[i] then
        already := true;

    // Falls nicht bereits in 0,..,q übernehme an nächste Stelle in Tochter
    if not(already) then
    begin
      daughter.order[k] := mother.order[i];
      daughter.b[k] := mother.b[i];
      inc(k);
    end;
  end;
end;

end.

