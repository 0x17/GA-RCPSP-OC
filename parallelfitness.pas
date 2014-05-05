unit parallelfitness;

// Berechnung der Fitness ist Flaschenhals in GA. (insbesondere bei Verwendung von SSGS-OC)
// Parallelisierte Fitness-Berechnung für Individuen durch grobgranulare Aufteilung und Threads je Slice von Individuen
// TODO: Fitnessfunktion und Individueentyp parametrisierbar (Delphi generics)

interface

uses classes, sysutils, constants, operators;

const
  NUM_THREADS = 8;
  NUM_INDIV_PER_THREAD = POP_SIZE*2 div NUM_THREADS;

type TFValArray = Array[0..POP_SIZE*2-1] of Double;
type TFValPartArray = Array[0..NUM_INDIV_PER_THREAD-1] of Double;

type TFitnessComputation<T> = class
  ffunc: TFitnessFunc<T>;

  constructor Create(f: TFitnessFunc<T>);

  procedure CalcSerial(var population: TPop<T>; out fvals: TFValArray);
  procedure CalcParallel(var population: TPop<T>; out fvals: TFValArray);
end;


type TCalcFitnessThread<T> = class(TThread)
  six, eix: Integer;
  pop: TPop<T>;
  fvp: TFValPartArray;
  ffunc: TFitnessFunc<T>;

  constructor Create(f: TFitnessFunc<T>; var population: TPop<T>; startIx, endIx: Integer);

  protected
    procedure Execute; override;
end;

implementation

constructor TFitnessComputation<T>.Create(f: TFitnessFunc<T>);
begin
  ffunc := f;
end;

procedure TFitnessComputation<T>.CalcSerial(var population: TPop<T>; out fvals: TFValArray);
var i: Integer;
begin
  for i := 0 to POP_SIZE*2-1 do
    fvals[i] := -ffunc(population[i]);
end;

procedure TFitnessComputation<T>.CalcParallel(var population: TPop<T>; out fvals: TFValArray);
var
  i, j: Integer;
  threads: Array[0..NUM_THREADS-1] of TCalcFitnessThread<T>;
begin
  for i := 0 to NUM_THREADS - 1 do
    threads[i] := TCalcFitnessThread<T>.Create(ffunc, population, NUM_INDIV_PER_THREAD * i, NUM_INDIV_PER_THREAD * (i+1) - 1);

  for i := 0 to NUM_THREADS - 1 do
  begin
    threads[i].WaitFor;
    for j := 0 to NUM_INDIV_PER_THREAD - 1 do
      fvals[i*NUM_INDIV_PER_THREAD+j] := threads[i].fvp[j];
    FreeAndNil(threads[i]);
  end;
end;

constructor TCalcFitnessThread<T>.Create(f: TFitnessFunc<T>; var population: TPop<T>; startIx, endIx: Integer);
begin
  pop := population;
  six := startIx;
  eix := endIx;
  ffunc := f;
  inherited Create(False);
end;

procedure TCalcFitnessThread<T>.Execute;
var
  i: Integer;
begin
  for i := six to eix do
    fvp[i-six] := -ffunc(pop[i]);
end;

end.

