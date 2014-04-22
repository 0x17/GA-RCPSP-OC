unit gassgsoc;

interface

uses projectdata, individual, ssgsoc, classes, sysutils, math, parallelfitness, constants;

type TGA_SSGS_OC = class(TObject)
private
  population: TIndivArray;
  m_ps: ProjData;

  procedure InitializePopulation;
  procedure FreePopulation;

  procedure Mutation;
  procedure Crossover;
  procedure Selection;

  procedure SelectBestOrder(out best: JobData);

  procedure SortDescendingFitness;

  procedure InitPriorityRulesFromFile(out rules: JobDataArray);
public
  constructor Create(const ps: ProjData);
  function Run(out sts, bestOrder: JobData): Double;
end;

implementation

constructor TGA_SSGS_OC.Create(const ps: ProjData);
begin
  m_ps := ps;
end;

function TGA_SSGS_OC.Run(out sts, bestOrder: JobData): Double;
var
  i: Integer;
begin
  RandSeed := 23;

  SetLength(sts, m_ps.numJobs);
  InitializePopulation;

  for i := 1 to NUM_GENS do
  begin
    Crossover;
    Mutation;
    Selection;
    Write('Generation ', i, #13);
  end;

  WriteLn;

  SelectBestOrder(bestOrder);
  result := SolveWithOC(m_ps, bestOrder, sts);

  FreePopulation;
end;

procedure TGA_SSGS_OC.InitializePopulation;
var
  i: Integer;
  prioRules: JobDataArray;
  j: Integer;
begin
  TIndividual.SetProjectStructure(m_ps);
  InitPriorityRulesFromFile(prioRules);

  for i := 0 to 12 do
  begin
    population[i] := TIndividual.Create;
    for j := 0 to m_ps.numJobs-1 do
      population[i].order[j] := prioRules[i, j];
  end;

  for i := 13 to POP_SIZE * 2 - 1 do
  begin
      population[i] := TIndividual.Create;
      for j := 1 to RandomRange(0, 1024) do
        population[i].Mutate;
  end;
end;

procedure TGA_SSGS_OC.FreePopulation;
var i: Integer;
begin
  for i := 0 to POP_SIZE*2-1 do
    population[i].Free;
end;

procedure TGA_SSGS_OC.Mutation;
var i: Integer;
begin
  for i := 0 to POP_SIZE*2 - 1 do
      population[i].Mutate;
end;

procedure TGA_SSGS_OC.Crossover;
var
   i, i1, i2, ix1, ix2, ctr, k: Integer;
   taken: Array[0..POP_SIZE-1] of Boolean;
begin
  for i := 0 to POP_SIZE - 1 do
      taken[i] := false;

  // Für jedes zu findene Paar
  for i := 0 to (POP_SIZE div 2) - 1 do
  begin
    // Wähle i1/i2-ten Index aus noch nicht vergebenen
    i1 := RandomRange(0, POP_SIZE-1-2*i);
    i2 := RandomRange(0, POP_SIZE-1-2*i);

    if i1 = i2 then continue;

    // Suche i1-ten nicht vergebenen Index raus
    k := 0;
    ctr := 0;
    while ctr < i1 do begin
      if not(taken[k]) then inc(ctr);
      inc(k);
    end;
    ix1 := k;

    // Such i2-ten nicht vergebenen Index raus
    k := 0;
    ctr := 0;
    while ctr < i2 do begin
      if not(taken[k]) then inc(ctr);
      inc(k);
    end;
    ix2 := k;

    taken[ix1] := true;
    taken[ix2] := true;

    population[ix1].Crossover(population[ix2], population[POP_SIZE+i*2], population[POP_SIZE+i*2+1]);
  end;
end;

procedure TGA_SSGS_OC.Selection;
begin
  SortDescendingFitness;
end;

procedure TGA_SSGS_OC.SelectBestOrder(out best: JobData);
var
   i: Integer;
begin
  SetLength(best, m_ps.numJobs);
  for i := 0 to m_ps.numJobs - 1 do
    best[i] := population[0].order[i];
end;

procedure TGA_SSGS_OC.SortDescendingFitness;
  procedure qsort(var fvals: array of Double;  iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Mid, T: Double;
    T2: TIndividual;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := fvals[(Lo + Hi) div 2];
    repeat
      while fvals[Lo] < Mid do Inc(Lo);
      while fvals[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := fvals[Lo];
        fvals[Lo] := fvals[Hi];
        fvals[Hi] := T;

        T2 := population[Lo];
        population[Lo] := population[Hi];
        population[Hi] := T2;

        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then qsort(fvals, iLo, Hi);
    if Lo < iHi then qsort(fvals, Lo, iHi);
  end;
var
  fvals: TFValArray;
begin
  //CalcFitnessValuesSerial(population, fvals);
  CalcFitnessValuesParallel(population, fvals);
  qsort(fvals, 0, POP_SIZE*2-1);
end;

procedure TGA_SSGS_OC.InitPriorityRulesFromFile(out rules: JobDataArray);
var
  i, j: Integer;
  fp: TextFile;
begin
  SetLength(rules, 13, m_ps.numJobs);
  AssignFile(fp, m_ps.name+'.PRULES');
  Reset(fp);
  for i := 0 to 12 do
  begin
    for j := 0 to m_ps.numJobs-1 do
    begin
      Read(fp, rules[i,j]);
      dec(rules[i,j]);
    end;
  end;
  CloseFile(fp);
end;

end.
