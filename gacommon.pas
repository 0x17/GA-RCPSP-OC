unit gacommon;

interface

uses sysutils, constants, math, projectdata, parallelfitness, helpers, gaactivitylist;

type TGAProcs<T> = record
  initProc: TInitProc<T>;
  crossProc: TCrossoverProc<T>;
  fitnessFunc: TFitnessFunc<T>;
  mutateProc: TMutateProc<T>;
end;

type TGACore<T> = class
  initProc: TInitProc<T>;
  crossProc: TCrossoverProc<T>;
  fitnessFunc: TFitnessFunc<T>;
  mutateProc: TMutateProc<T>;

  constructor Create(procs: TGAProcs<T>);
  function Run(out sts: JobData; best: T): Double;

private
  procedure Cross(var population: TPop<T>; crossFunc: TCrossoverProc<T>);
end;

implementation

constructor TGACore<T>.Create(procs: TGAProcs<T>);
begin
  initProc := procs.initProc;
  crossProc := procs.crossProc;
  fitnessFunc := procs.fitnessFunc;
  mutateProc := procs.mutateProc;
end;

function TGACore<T>.Run(out sts: JobData; best: T): Double;
var
  population: TPop<T>;
  i, j: Integer;
  fvals: TFValArray;
  fcomp: TFitnessComputation<T>;
begin
  RandSeed := 23;

  initProc(population);

  for i := 1 to NUM_GENS do
  begin
    Cross(population, crossProc);

    for j := POP_SIZE to POP_SIZE*2 - 1 do
      mutateProc(population[j]);

    fcomp := TFitnessComputation<T>.Create(fitnessFunc);
    fcomp.CalcParallel(population, fvals);
    FreeAndNil(fcomp);

    TSortHelper<T>.QuickSortKeys(population, fvals, 0, POP_SIZE*2-1);

    Write('Generation ', i, #13);
  end;
  WriteLn;

  result := fitnessFunc(population[0]);
end;

procedure TGACore<T>.Cross(var population: TPop<T>; crossFunc: TCrossoverProc<T>);
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

    crossFunc(population[ix1], population[ix2], population[POP_SIZE+i*2], population[POP_SIZE+i*2+1]);
  end;
end;

end.

