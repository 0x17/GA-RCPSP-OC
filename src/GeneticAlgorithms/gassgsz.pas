unit gassgsz;

interface

uses classes, sysutils, individual, projectdata, globals, ssgs, helpers, profit, gassgsoc, fbi;

function RunGASSGSZ: Double;

type TActivityListZIndividual = class(TActivityListIndividual)
  z: ResData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
end;

implementation

function RunGASSGSZ: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListZIndividual.Create;

  population[0].InitializePopulation(population);
  result := RunGA(population, bestIndiv, False);
  FreePopulation(population);
end;

procedure TActivityListZIndividual.InitializePopulation(var population: IndivArray);
var
  i, r, t: Integer;
  z: ResourceProfile;
begin
  inherited InitializePopulation(population);

  SetLength(z, ps.numRes, ps.numPeriods);

  for i := 0 to Length(population) - 1 do
  begin
    SetLength(TActivityListZIndividual(population[i]).z, ps.numRes);
    for r := 0 to ps.numRes - 1 do begin
      TActivityListZIndividual(population[i]).z[r] := THelper.RandomRangeIncl(0, ps.zmax[r]);
      for t := 0 to ps.numPeriods-1 do
        z[r,t] := TActivityListZIndividual(population[i]).z[r];
    end;

    TFBI.Improve(TActivityListZIndividual(population[i]).order, z);
  end;
end;

procedure RandomCrossoverOC(const mother, father: ResData; var daughter: ResData); forward;

procedure TActivityListZIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var otherC, daughterC, sonC: TActivityListZIndividual;
begin
  // Crossover activity list
  TActivityListIndividual(daughter).OnePointCrossover(self, TActivityListIndividual(other));
  TActivityListIndividual(son).OnePointCrossover(TActivityListIndividual(other), self);

  // Crossover overtime
  otherC := TActivityListZIndividual(other);
  daughterC := TActivityListZIndividual(daughter);
  sonC := TActivityListZIndividual(son);
  RandomCrossoverOC(z, otherC.z, daughterC.z);
  RandomCrossoverOC(otherC.z, z, sonC.z);
end;

procedure MutateOC(var z: ResData); forward;

procedure TActivityListZIndividual.Mutate;
begin
  SwapNeighborhood(order);
  MutateOC(z);
end;

function TActivityListZIndividual.Fitness: Double;
var
  sts: JobData;
  zfilled, resRemaining: ResourceProfile;
  r, t: Integer;
begin
  SetLength(zfilled, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      zfilled[r,t] := z[r];

  TSSGS.Solve(order, zfilled, sts, resRemaining);
  TFBI.Improve(sts, zfilled, resRemaining);
  result := CalcProfit(sts, resRemaining);
end;

procedure RandomCrossoverOC(const mother, father: ResData; var daughter: ResData);
var r, q: Integer;
begin
  for r := 0 to ps.numRes - 1 do
  begin
    q := Random(2);
    if q = 0 then
      daughter[r] := mother[r]
    else
      daughter[r] := father[r];
  end;
end;

procedure MutateOC(var z: ResData);
var
  r, q: Integer;
begin
  for r := 0 to ps.numRes-1 do
  begin
    q := THelper.RandomRangeIncl(1, 100);
    if q <= PROB_MUTATE then
    begin
      if Random(2) = 0 then
        inc(z[r])
      else
        dec(z[r]);

      if z[r] < 0 then z[r] := 0;
      if z[r] > ps.zmax[r] then z[r] := ps.zmax[r];
    end;
  end;
end;

end.
