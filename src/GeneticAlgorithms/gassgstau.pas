unit gassgstau;

interface

uses individual, gassgsoc, ssgsoc;

function RunGASSGSTau: Double;

type TActivityListTauIndividual = class(TActivityListIndividual)
  tau: ExtArray;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure SwapNeighborhood;
  procedure Swap(i1, i2: Integer); inline;
protected
  procedure InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer); override;
end;

implementation

uses classes, sysutils, globals, helpers;

function AllocateIndividual: IIndividual;
begin
  result := TActivityListTauIndividual.Create;
end;

function RunGASSGSTau: Double;
begin
  result := TGACore.Run(AllocateIndividual, False);
end;

procedure TActivityListTauIndividual.InitializePopulation(var population: IndivArray);
var i, j: Integer;
begin
  inherited InitializePopulation(population);

  // Initialize parents
  for i := 0 to Length(population) div 2 - 1 do
  begin
    SetLength(TActivityListTauIndividual(population[i]).tau, ps.numJobs);
    for j := 0 to ps.numJobs - 1 do begin
      TActivityListTauIndividual(population[i]).tau[j] := Random;
    end;
  end;

  // Initialize children without information
  for i := Length(population) div 2 to Length(population) - 1 do
    SetLength(TActivityListTauIndividual(population[i]).tau, ps.numJobs);
end;

procedure TActivityListTauIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var otherT, daughterT, sonT: TActivityListIndividual;
begin
  otherT := TActivityListIndividual(other);
  daughterT := TActivityListIndividual(daughter);
  sonT := TActivityListIndividual(son);

  daughterT.OnePointCrossover(self, otherT);
  sonT.OnePointCrossover(otherT, self);
end;

procedure TActivityListTauIndividual.Mutate;
var j: Integer;	
begin
  SwapNeighborhood;
  for j := 0 to ps.numJobs - 1 do
    if THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE then
      tau[j] := Random;
end;

function TActivityListTauIndividual.Fitness: Double;
begin
  result := TSSGSOC.SolveWithTau(order, tau, sts, resRem);
end;

procedure TActivityListTauIndividual.SwapNeighborhood;
var i: Integer;
begin
  for i := 2 to ps.numJobs - 2 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[order[i-1], order[i]] = 0) then
      Swap(i, i-1);
end;

procedure TActivityListTauIndividual.Swap(i1, i2: Integer);
var
  tmp: Integer;
  tmp2: Extended;
begin
  tmp := order[i1];
  tmp2 := tau[i1];

  order[i1] := order[i2];
  tau[i1] := tau[i2];

  order[i2] := tmp;
  tau[i2] := tmp2;
end;

procedure TActivityListTauIndividual.InheritGene(const parent: TActivityListIndividual; parentIndex, childIndex: Integer);
begin
  order[childIndex] := parent.order[parentIndex];
  tau[childIndex] := TActivityListTauIndividual(parent).tau[parentIndex];
end;

end.

