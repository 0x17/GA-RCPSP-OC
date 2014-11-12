unit individual;

interface

uses globals, helpers, classes, sysutils;

const NUM_THREADS = 8;
      NUM_INDIV_PER_THREAD = POP_SIZE*2 div NUM_THREADS;

type
  IIndividual = class; // forward decl.
  IndivArray = Array of IIndividual;

  IIndividual = class
    procedure InitializePopulation(var pop: IndivArray); virtual; abstract;
    procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); virtual; abstract;
    procedure Mutate; virtual; abstract;
    function Fitness: Double; virtual; abstract;
end;

type TFValArray = Array[0..POP_SIZE*2-1] of Double;
type TFValPartArray = Array[0..NUM_INDIV_PER_THREAD-1] of Double;

type TSortHelper = class
  class procedure QuickSortKeys(var keys: IndivArray; var fvals: array of Double;  iLo, iHi: Integer);
end;

type TFitnessComputation = class
  class procedure CalcSerial(var population: IndivArray; out fvals: TFValArray);
  class procedure CalcParallel(var population: IndivArray; out fvals: TFValArray);
end;

type TCalcFitnessThread = class(TThread)
  six, eix: Integer;
  pop: IndivArray;
  fvp: TFValPartArray;
  constructor Create(var population: IndivArray; startIx, endIx: Integer);
protected
  procedure Execute; override;
end;

function RunGA(var population: IndivArray; out best: IIndividual; parallelComp: Boolean): Double;
procedure FreePopulation(var population: IndivArray);

implementation

class procedure TSortHelper.QuickSortKeys(var keys: IndivArray; var fvals: array of Double;  iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid, T: Double;
  T2: IIndividual;
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

      T2 := keys[Lo];
      keys[Lo] := keys[Hi];
      keys[Hi] := T2;

      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then QuickSortKeys(keys, fvals, iLo, Hi);
  if Lo < iHi then QuickSortKeys(keys, fvals, Lo, iHi);
end;

//==============================================================================
procedure Cross(var population: IndivArray);
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
    i1 := THelper.RandomRangeIncl(0, POP_SIZE-1-2*i);
    i2 := THelper.RandomRangeIncl(0, POP_SIZE-1-2*i);

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

procedure FreePopulation(var population: IndivArray);
var i: Integer;
begin
  for i := 0 to POP_SIZE*2-1 do
    FreeAndNil(population[i]);
  SetLength(population, 0);
end;

function RunGA(var population: IndivArray; out best: IIndividual; parallelComp: Boolean): Double;
var
  i, j: Integer;
  fvals: TFValArray;
begin
  RandSeed := 23;

  for i := 1 to NUM_GENS do
  begin
    Cross(population);

    for j := POP_SIZE to POP_SIZE * 2 - 1 do
      population[j].Mutate;

    if parallelComp then
      TFitnessComputation.CalcParallel(population, fvals)
    else
      TFitnessComputation.CalcSerial(population, fvals);

    TSortHelper.QuickSortKeys(population, fvals, 0, POP_SIZE*2-1);

    Write('Generation ', i, #13);
  end;
  WriteLn;

  best := population[0];
  result := population[0].Fitness;
end;

//==============================================================================
class procedure TFitnessComputation.CalcSerial(var population: IndivArray; out fvals: TFValArray);
var i: Integer;
begin
  for i := 0 to POP_SIZE*2-1 do
    fvals[i] := -population[i].Fitness;
end;

class procedure TFitnessComputation.CalcParallel(var population: IndivArray; out fvals: TFValArray);
var
  i, j: Integer;
  threads: Array[0..NUM_THREADS-1] of TCalcFitnessThread;
begin
  for i := 0 to NUM_THREADS - 1 do
    threads[i] := TCalcFitnessThread.Create(population, NUM_INDIV_PER_THREAD * i, NUM_INDIV_PER_THREAD * (i+1) - 1);

  for i := 0 to NUM_THREADS - 1 do
  begin
    threads[i].WaitFor;
    for j := 0 to NUM_INDIV_PER_THREAD - 1 do
      fvals[i*NUM_INDIV_PER_THREAD+j] := threads[i].fvp[j];
    FreeAndNil(threads[i]);
  end;
end;

constructor TCalcFitnessThread.Create(var population: IndivArray; startIx, endIx: Integer);
begin
  pop := population;
  six := startIx;
  eix := endIx;
  inherited Create(False);
end;

procedure TCalcFitnessThread.Execute;
var
  i: Integer;
begin
  for i := six to eix do
    fvp[i-six] := -pop[i].Fitness;
end;

end.
