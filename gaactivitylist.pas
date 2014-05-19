unit gaactivitylist;

interface

uses classes, projectdata, operators, ssgsoc, topsort, constants, printing;

procedure SetProjectStructureAL(const nps: ProjData);

procedure InitAL(out order: JobData);
procedure MutateAL(var order: JobData);
procedure CrossoverAL(const order, other: JobData; var daughter, son: JobData);

function FitnessSSGSOC(const order: JobData): Double;

implementation

var
  ps: ProjData;

procedure SetProjectStructureAL(const nps: ProjData);
begin
  ps := nps;
end;

procedure InitAL(out order: JobData);
begin
  RandomTopologicalOrder(ps, order);
end;

procedure MutateAL(var order: JobData);
begin
  SwapNeighborhood(ps, order);
end;

procedure CrossoverAL(const order, other: JobData; var daughter, son: JobData);
begin
  OnePointCrossover(other, order, daughter);
  OnePointCrossover(order, other, son);
  //TwoPointCrossover(other, order, daughter);
  //TwoPointCrossover(order, other, son);
end;

function FitnessSSGSOC(const order: JobData): Double;
var
  sts: JobData;
begin
  result := SolveWithOC(ps, order, sts);
end;

end.
