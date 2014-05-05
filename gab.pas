unit gab;

// Genetische Operationen für Datenstruktur B
// b ist Vektor aus Binärzahlen (11010010101...)
// Einplanung von j in SSGS zu STj ist ress.zuläss. gdw. von STj..STj+dj für jede Ress. Restkapazität + ZUSATZ >= k_jr
// ZUSATZ = zmax_r, falls bj = 1
// ZUSATZ = 0, falls bj = 0

interface

uses classes, sysutils, projectdata, ssgsmod, profit, math;

type TALBPair = record
  order, b: JobData;
end;

procedure SetProjectStructureB(const nps: ProjData);

procedure InitB(out b: JobData);
procedure MutateB(var b: JobData);
procedure CrossoverB(const b, other: JobData; var daughter, son: JobData);
function FitnessSSGSMod(const individual: TALBPair): Double;

implementation

var ps: ProjData;

procedure SetProjectStructureB(const nps: ProjData);
begin
  ps := nps;
end;

procedure InitB(out b: JobData);
var j: Integer;
begin
  SetLength(b, ps.numJobs);
  for j := 0 to ps.numJobs - 1 do
    b[j] := RandomRange(0, 1);
end;

procedure MutateB(var b: JobData);
var j: Integer;
begin
  for j := 0 to ps.numJobs - 1 do
      if RandomRange(1, 100) <= 3 then
         b[j] := 1 - b[j];
end;

procedure OPC(const mother, father: JobData; var daughter: JobData);
var
  j, q: Integer;
begin
    q := RandomRange(0, ps.numJobs-1);
    for j := 0 to ps.numJobs-1 do
      if j <= q then
        daughter[j] := mother[j]
      else
        daughter[j] := father[j];
end;

procedure CrossoverB(const b, other: JobData; var daughter, son: JobData);
begin
  OPC(b, other, daughter);
  OPC(other, b, son);
end;

function FitnessSSGSMod(const individual: TALBPair): Double;
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  SolveMod(ps, individual.order, individual.b, sts, resRemaining);
  result := CalcProfit(ps, sts, resRemaining);
end;

end.
