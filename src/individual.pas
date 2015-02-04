unit individual;

interface

type
  IIndividual = class;
  IndivArray = Array of IIndividual;

  IIndividual = class
    procedure InitializePopulation(var population: IndivArray); virtual;
    procedure FillNeighborhood(const origin: IIndividual; var population: IndivArray); virtual; abstract;
    procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); virtual; abstract;
    procedure Mutate; virtual; abstract;
    function Fitness: Double; virtual; abstract;
  end;

  TSortHelper = class
    class procedure QuickSortKeys(var keys: IndivArray; var fvals: array of Double;  iLo, iHi: Integer);
  end;

  TFValArray = Array of Double;
  TFValPartArray = Array of Double;

  IndivAllocator = function: IIndividual;

  TGACore = class
    class function Run(allocator: IndivAllocator; parallelComp: Boolean): Double;
    class function MainLoop(var population: IndivArray; out best: IIndividual; parallelComp: Boolean): Double;
    class procedure FreePopulation(var population: IndivArray);
  private
    class procedure Cross(var population: IndivArray);
  end;

implementation

uses globals, helpers, sysutils, compfitness;

procedure AllocIndividuals(out population: IndivArray; size: Integer; allocator: IndivAllocator);
var i: Integer;
begin
  SetLength(population, size);
  for i := 0 to size - 1 do
    population[i] := allocator;
end;

procedure IIndividual.InitializePopulation(var population: IndivArray);
begin
  RandSeed := 23;
end;

//==============================================================================
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
class function TGACore.Run(allocator: IndivAllocator; parallelComp: Boolean): Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
begin
  AllocIndividuals(population, POP_SIZE*2, allocator);
  population[0].InitializePopulation(population);
  result := TGACore.MainLoop(population, bestIndiv, parallelComp);

  // Populationsgröße halbieren
  {for i := POP_SIZE to POP_SIZE * 2 - 1 do
    population[i].Free;
  SetLength(population, POP_SIZE);

  population[0].FillNeighborhood(bestIndiv, population);
  result := TGACore.MainLoop(population, bestIndiv, True);}

  TGACore.FreePopulation(population);
end;

class function TGACore.MainLoop(var population: IndivArray; out best: IIndividual; parallelComp: Boolean): Double;
var
  i, j, numIter: Integer;
  maxParentIx, minChildIx, maxChildIx: Integer;
  fvals: TFValArray;
begin
  maxParentIx := Length(population) div 2 - 1;
  minChildIx := maxParentIx + 1;
  maxChildIx := Length(population) - 1;

  // Compute fitness (objective value, schedule, residual capacity) for all parents
  SetLength(fvals, Length(population));
  for i := 0 to maxParentIx do
    fvals[i] := -population[i].Fitness;
  TSortHelper.QuickSortKeys(population, fvals, 0, maxParentIx);

  //numIter := (numSchedules div 2) div (Length(population) div 2);
  numIter := 100;

  for i := 1 to numIter do
  begin
    Cross(population);

    // Only mutate children, leave parents unchanged
    for j := minChildIx to maxChildIx do
      population[j].Mutate;

    // Only compute fitness for children here (parents already computed!)
    if parallelComp then TFitnessComputation.CalcParallel(population, fvals, minChildIx, maxChildIx)
    else TFitnessComputation.CalcSerial(population, fvals, minChildIx, maxChildIx);

    TSortHelper.QuickSortKeys(population, fvals, 0, maxChildIx);
  end;

  best := population[0];
  result := population[0].Fitness;
end;

class procedure TGACore.FreePopulation(var population: IndivArray);
var i: Integer;
begin
  for i := 0 to Length(population)-1 do
    FreeAndNil(population[i]);
  SetLength(population, 0);
end;

class procedure TGACore.Cross(var population: IndivArray);
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

end.
