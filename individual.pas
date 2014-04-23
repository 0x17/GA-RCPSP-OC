unit individual;

interface

uses classes, projectdata, operators, ssgsoc, topsort, constants;

type TIndividual = class(TObject)
  order: JobData;
  class var ps: ProjData;
  class procedure SetProjectStructure(const nps: ProjData);
  class var topOrder: JobData;

  constructor Create;
  procedure Mutate;
  procedure Crossover(const other: TIndividual; var daughter, son: TIndividual);
  function Fitness: Double;
end;

type TIndivArray = Array[0..POP_SIZE*2-1] of TIndividual;

implementation

class procedure TIndividual.SetProjectStructure(const nps: ProjData);
begin
  ps := nps;
  TopologicalOrder(ps, topOrder);
end;

constructor TIndividual.Create;
var
  i: Integer;
begin
  SetLength(order, ps.numJobs);
  for i := 0 to ps.numJobs - 1 do
      order[i] := topOrder[i];
end;

procedure TIndividual.Mutate;
begin
  SwapNeighborhood(ps, order);
end;

procedure TIndividual.Crossover(const other: TIndividual; var daughter, son: TIndividual);
begin
  OnePointCrossover(other.order, order, daughter.order);
  OnePointCrossover(order, other.order, son.order);
end;

function TIndividual.Fitness: Double;
var
  sts: JobData;
begin
  result := SolveWithOC(ps, order, sts);
end;

end.
