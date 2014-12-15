unit gassgstau;

interface

uses classes, sysutils, individual, projectdata, globals, ssgsoc, gassgsoc, helpers;

function RunGASSGSTau: Double;

type TActivityListTauIndividual = class(TActivityListIndividual)
  tau: JobData;
  procedure InitializePopulation(var population: IndivArray); override;
  procedure Crossover(const other: IIndividual; var daughter, son: IIndividual); override;
  procedure Mutate; override;
  function Fitness: Double; override;
private
  procedure SwapNeighborhood(var lambda, tau: JobData);
  procedure Swap(var lambda, tau: JobData; i1, i2: Integer); inline;
  procedure OnePointCrossover(const motherOrder, motherTau, fatherOrder, fatherTau: JobData; var daughterOrder, daughterTau: JobData);
end;

implementation

procedure TActivityListTauIndividual.OnePointCrossover(const motherOrder, motherTau, fatherOrder, fatherTau: JobData; var daughterOrder, daughterTau: JobData);
var
  i, j, k, q, len: Integer;
  fromMother: Boolean;
begin
  len := Length(motherOrder);
  q := THelper.RandomRangeIncl(1, len);

  // Ersten q1: 0,..,q-1 von Mutter
  for i := 0 to q-1 do
  begin
    daughterOrder[i] := motherOrder[i];
    daughterTau[i] := motherTau[i];
  end;

  // q,..,len-1 von Vater, falls nicht von Mutter
  k := q;
  // Probiere alle von Vater
  for i := 0 to len-1 do
  begin
    // Schaue ob bereits in 0,..,q-1
    fromMother := false;
    for j := 0 to q-1 do
      if daughterOrder[j] = fatherOrder[i] then
        fromMother := true;

    // Falls nicht bereits in 0,..,q-1 übernehme an nächste Stelle in Tochter
    if not(fromMother) then
    begin
      daughterOrder[k] := fatherOrder[i];
      daughterTau[k] := fatherTau[i];
      inc(k);
    end;
  end;
end;

procedure TActivityListTauIndividual.Swap(var lambda, tau: JobData; i1, i2: Integer);
var
  tmp, tmp2: Integer;
begin
  tmp := lambda[i1];
  tmp2 := tau[i1];

  lambda[i1] := lambda[i2];
  tau[i1] := tau[i2];

  lambda[i2] := tmp;
  tau[i2] := tmp2;
end;

procedure TActivityListTauIndividual.SwapNeighborhood(var lambda, tau: JobData);
var
  i: Integer;
begin
  for i := 2 to ps.numJobs - 2 do
    if (THelper.RandomRangeIncl(1, 100) <= PROB_MUTATE) and (ps.adjMx[lambda[i-1], lambda[i]] = 0) then
      Swap(lambda, tau, i, i-1);
end;

procedure TActivityListTauIndividual.Crossover(const other: IIndividual; var daughter, son: IIndividual);
var otherT, daughterT, sonT: TActivityListTauIndividual;
begin
  otherT := TActivityListTauIndividual(other);
  daughterT := TActivityListTauIndividual(daughter);
  sonT := TActivityListTauIndividual(son);

  OnePointCrossover(order, tau, otherT.order, otherT.tau, daughterT.order, daughterT.tau);
  OnePointCrossover(otherT.order, otherT.tau, order, tau, sonT.order, sonT.tau);
end;

procedure TActivityListTauIndividual.Mutate;
var j: Integer;	
begin
  SwapNeighborhood(order, tau);
  for j := 0 to ps.numJobs - 1 do
    if THelper.RandomRangeIncl(0, 99) <= PROB_MUTATE-1 then
      tau[j] := THelper.RandomRangeIncl(0, 99);
end;

function TActivityListTauIndividual.Fitness: Double;
var sts: JobData;
begin
  result := TSSGSOC.SolveWithTau(order, tau, sts);
end;

procedure TActivityListTauIndividual.InitializePopulation(var population: IndivArray);
var i, j: Integer;
begin
  inherited InitializePopulation(population);

  for i := 0 to POP_SIZE * 2 - 1 do
  begin
    SetLength(TActivityListTauIndividual(population[i]).tau, ps.numJobs);
    for j := 0 to ps.numJobs - 1 do
      TActivityListTauIndividual(population[i]).tau[j] := THelper.RandomRangeIncl(0, 99);
  end;
end;

function RunGASSGSTau: Double;
var
  population: IndivArray;
  bestIndiv: IIndividual;
  i: Integer;
begin
  SetLength(population, POP_SIZE*2);
  for i := 0 to POP_SIZE * 2 - 1 do
    population[i] := TActivityListTauIndividual.Create;

  population[0].InitializePopulation(population);
  result := RunGA(population, bestIndiv, False);
  FreePopulation(population);
end;

end.

