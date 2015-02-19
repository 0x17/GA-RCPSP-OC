unit gassgsz;

interface

uses individual, gassgsoc, projectdata;

function RunGASSGSZ: Double;

type TActivityListZIndividual = class(TActivityListIndividual)
  z: ResData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure RandomCrossoverOC(const mother, father: ResData; var daughter: ResData);
  procedure MutateOC;
end;

implementation

uses classes, sysutils, globals, ssgs, helpers, profit;

function AllocateIndividual: IIndividual;
begin
  result := TActivityListZIndividual.Create;
end;

function RunGASSGSZ: Double;
begin
  result := TGACore.Run(AllocateIndividual, False);
end;

procedure TActivityListZIndividual.InitializePopulation(var population: IndivArray);
var
  i, r, t: Integer;
  z: ResourceProfile;
begin
  inherited InitializePopulation(population);

  SetLength(z, ps.numRes, ps.numPeriods);

  // Initialize parents
  for i := 0 to Length(population) div 2 - 1 do
  begin
    SetLength(TActivityListZIndividual(population[i]).z, ps.numRes);
    for r := 0 to ps.numRes - 1 do begin
      TActivityListZIndividual(population[i]).z[r] := THelper.RandomRangeIncl(0, ps.zmax[r]);
      for t := 0 to ps.numPeriods-1 do
        z[r,t] := TActivityListZIndividual(population[i]).z[r];
    end;
  end;

  // Initialize children without information
  for i := Length(population) div 2 to Length(population) - 1 do
    SetLength(TActivityListZIndividual(population[i]).z, ps.numRes);
end;

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

procedure TActivityListZIndividual.Mutate;
begin
  SwapNeighborhood(order);
  MutateOC;
end;

function TActivityListZIndividual.Fitness: Double;
var
  zfilled: ResourceProfile;
  r, t: Integer;
begin
  SetLength(zfilled, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      zfilled[r,t] := z[r];

  TSSGS.Solve(order, zfilled, sts, resRem);
  result := TProfit.CalcProfit(sts, resRem);
end;

procedure TActivityListZIndividual.RandomCrossoverOC(const mother, father: ResData; var daughter: ResData);
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

procedure TActivityListZIndividual.MutateOC;
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
