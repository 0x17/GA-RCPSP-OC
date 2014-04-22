unit parallelfitness;

//{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, individual, constants;

const
  NUM_THREADS = 8;
  NUM_INDIV_PER_THREAD = POP_SIZE*2 div NUM_THREADS;

type TFValArray = Array[0..POP_SIZE*2-1] of Double;
type TFValPartArray = Array[0..NUM_INDIV_PER_THREAD-1] of Double;

procedure CalcFitnessValuesSerial(var population: TIndivArray; out fvals: TFValArray);
procedure CalcFitnessValuesParallel(var population: TIndivArray; out fvals: TFValArray);

type TCalcFitnessThread = class(TThread)
  six, eix: Integer;
  pop: TIndivArray;
  fvp: TFValPartArray;

  constructor Create(var population: TIndivArray; startIx, endIx: Integer);

  protected
    procedure Execute; override;
end;

implementation

procedure CalcFitnessValuesSerial(var population: TIndivArray; out fvals: TFValArray);
var i: Integer;
begin
  for i := 0 to POP_SIZE*2-1 do
    fvals[i] := -population[i].Fitness;
end;

procedure CalcFitnessValuesParallel(var population: TIndivArray; out fvals: TFValArray);
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

constructor TCalcFitnessThread.Create(var population: TIndivArray; startIx, endIx: Integer);
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

