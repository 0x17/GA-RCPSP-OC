unit compfitness;

interface

uses classes, individual;

type
  TFitnessComputation = class
    class procedure CalcSerial(var population: IndivArray; var fvals: TFValArray; fromIx, toIx: Integer);
    class procedure CalcParallel(var population: IndivArray; var fvals: TFValArray; fromIx, toIx: Integer);
  end;

  TCalcFitnessThread = class(TThread)
    six, eix: Integer;
    pop: IndivArray;
    fvp: TFValPartArray;
    constructor Create(var population: IndivArray; startIx, endIx: Integer);
  protected
    procedure Execute; override;
  end;

implementation

uses sysutils;

const NUM_THREADS = 8;

class procedure TFitnessComputation.CalcSerial(var population: IndivArray; var fvals: TFValArray; fromIx, toIx: Integer);
var i: Integer;
begin
  for i := fromIx to toIx do
    fvals[i] := -population[i].Fitness;
end;

class procedure TFitnessComputation.CalcParallel(var population: IndivArray; var fvals: TFValArray; fromIx, toIx: Integer);
var
  i, j, numIndivPerThread: Integer;
  threads: Array[0..NUM_THREADS-1] of TCalcFitnessThread;
begin
  numIndivPerThread := (toIx-fromIx+1) div NUM_THREADS;

  for i := 0 to NUM_THREADS - 1 do
    threads[i] := TCalcFitnessThread.Create(population, fromIx + numIndivPerThread * i, fromIx + numIndivPerThread * (i+1) - 1);

  for i := 0 to NUM_THREADS - 1 do
  begin
    threads[i].WaitFor;
    for j := 0 to numIndivPerThread - 1 do
      fvals[fromIx + i*numIndivPerThread+j] := threads[i].fvp[j];
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
