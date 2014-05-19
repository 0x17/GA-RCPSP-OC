unit testing;

interface

uses operators, projectdata, printing, helpers;

procedure RunTests;

implementation

procedure TestCrossoverJobList;
const NJOBS = 32;
var
  i: Integer;
  mother, father, daughter: JobData;
begin
  SetLength(daughter, NJOBS);
  SetLength(mother, NJOBS);
  SetLength(father, NJOBS);

  for i := 0 to NJOBS-1 do
  begin
    mother[i] := i+1;
    father[i] := NJOBS-i;
  end;

  PrintActivityList(mother);
  PrintActivityList(father);

  OnePointCrossover(mother, father, daughter);
  PrintActivityList(daughter);

  TwoPointCrossover(mother, father, daughter);
  PrintActivityList(daughter);
end;

procedure TestCrossoverTALBPair;
const NJOBS = 32;
  procedure Init(var pair: TALBPair);
  begin
    SetLength(pair.order, NJOBS);
    SetLength(pair.b, NJOBS);
  end;
var
  mother, father, daughter: TALBPair;
  i: Integer;
begin
  Init(mother); Init(father); Init(daughter);
  for i := 0 to NJOBS-1 do
  begin
    mother.order[i] := i+1;
    mother.b[i] := RandomRangeIncl(0, 1);
    father.order[i] := NJOBS-i;
    father.b[i] := RandomRangeIncl(0, 1);
  end;
  PrintTALBPair(father);
  PrintTALBPair(mother);

  OnePointCrossover(mother, father, daughter);
  PrintTALBPair(daughter);

  TwoPointCrossover(mother, father, daughter);
  PrintTALBPair(daughter);
end;

procedure RunTests;
begin
  WriteLn('Test job list');
  TestCrossoverJobList;
  WriteLn('Test talb pair');
  TestCrossoverTALBPair;
  ReadLn;
end;

end.
