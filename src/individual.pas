unit individual;

interface

uses classes;

type
  IIndividual = class; // forward decl.
  IndivArray = Array of IIndividual;

  IIndividual = class
    procedure InitializePopulation(var pop: IndivArray); virtual;
    procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); virtual; abstract;
    procedure Mutate; virtual; abstract;
    function Fitness: Double; virtual; abstract;
  end;

  TSortHelper = class
    class procedure QuickSortKeys(var keys: IndivArray; var fvals: array of Double;  iLo, iHi: Integer);
  end;

  TFValArray = Array of Double;
  TFValPartArray = Array of Double;

  TFitnessComputation = class
    class procedure CalcSerial(var population: IndivArray; out fvals: TFValArray);
    class procedure CalcParallel(var population: IndivArray; out fvals: TFValArray);
  end;

  TCalcFitnessThread = class(TThread)
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

uses globals, helpers, sysutils;

const NUM_THREADS = 8;

procedure IIndividual.InitializePopulation(var pop: IndivArray);
begin
  RandSeed := 23;
end;

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
type BArray = Array of Boolean;
var
   i, i2, ix1, ix2, ctr, k, popSize: Integer;
   taken: BArray;
begin
  SetLength(taken, Length(population) div 2);
  for i := 0 to Length(taken) - 1 do
      taken[i] := false;
  popSize := Length(population) div 2;

  // Für jedes zu findene Paar
  for i := 0 to popSize div 2 - 1 do
  begin
    // Wähle Index 1 aus besten noch nicht vergebenen
    ix1 := 0;
    for ctr := 0 to popSize div 2 - 1 do
      if not(taken[ctr]) then begin
        ix1 := ctr;
        break;
      end;

    // Wähle Index 2 zufällig aus schlechteren noch nicht vergebenen
    // Bestimme Index von i2-tem nicht vergebenem in schlechteren Bereich
    i2 := THelper.RandomRangeIncl(0, popSize div 2 - 1 - i);
    k := popSize div 2;
    ctr := 0;
    while ctr < i2 do begin
      if not(taken[k]) then inc(ctr);
      inc(k);
    end;
    ix2 := k;

    taken[ix1] := true;
    taken[ix2] := true;

    population[ix1].Crossover(population[ix2], population[popSize+i*2], population[popSize+i*2+1]);
  end;
end;

procedure FreePopulation(var population: IndivArray);
var i: Integer;
begin
  for i := 0 to Length(population)-1 do
    FreeAndNil(population[i]);
  SetLength(population, 0);
end;

function RunGA(var population: IndivArray; out best: IIndividual; parallelComp: Boolean): Double;
var
  i, j, numIter: Integer;
  maxParentIx, minChildIx, maxChildIx: Integer;
  fvals: TFValArray;
begin
  maxParentIx := Length(population) div 2 - 1;
  minChildIx := maxParentIx + 1;
  maxChildIx := Length(population) - 1;

  // TODO: In Valls wohl nur Elterngeneration erst erzeugt, nicht POP_SIZE*2=Length(population)
  SetLength(fvals, Length(population));
  for i := 0 to maxChildIx do
    fvals[i] := -population[i].Fitness;
  TSortHelper.QuickSortKeys(population, fvals, 0, maxChildIx);

  numIter := (numSchedules div 2) div (Length(population) div 2);

  for i := 1 to numIter do
  begin
    Cross(population);

    for j := minChildIx to maxChildIx do
      population[j].Mutate;

    // FBI (DJ) sollte hier bereits auf alle Kinder angewendet werden! Siehe Fig 3

    if parallelComp then
      TFitnessComputation.CalcParallel(population, fvals)
    else
      TFitnessComputation.CalcSerial(population, fvals);

    TSortHelper.QuickSortKeys(population, fvals, 0, maxChildIx);
  end;

  best := population[0];
  result := population[0].Fitness;
end;

//==============================================================================
class procedure TFitnessComputation.CalcSerial(var population: IndivArray; out fvals: TFValArray);
var i: Integer;
begin
  SetLength(fvals, Length(population));
  for i := 0 to Length(population)-1 do
    fvals[i] := -population[i].Fitness;
end;

class procedure TFitnessComputation.CalcParallel(var population: IndivArray; out fvals: TFValArray);
var
  i, j, numIndivPerThread: Integer;
  threads: Array[0..NUM_THREADS-1] of TCalcFitnessThread;
begin
  SetLength(fvals, Length(population));
  numIndivPerThread := Length(population) div NUM_THREADS;

  for i := 0 to NUM_THREADS - 1 do
    threads[i] := TCalcFitnessThread.Create(population, numIndivPerThread * i, numIndivPerThread * (i+1) - 1);

  for i := 0 to NUM_THREADS - 1 do
  begin
    threads[i].WaitFor;
    for j := 0 to numIndivPerThread - 1 do
      fvals[i*numIndivPerThread+j] := threads[i].fvp[j];
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
var i: Integer;
begin
  SetLength(fvp, eix-six+1);
  for i := six to eix do
    fvp[i-six] := -pop[i].Fitness;
end;

end.
