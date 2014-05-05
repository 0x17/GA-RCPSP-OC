unit gaovercapacity;

interface

uses classes, sysutils, projectdata, ssgs, profit, math, operators;

type TALOCPair = record
  order: JobData;
  oc: ResourceProfile;
end;

procedure SetProjectStructureOC(const nps: ProjData);

procedure InitOC(out oc: ResourceProfile);
procedure MutateOC(var oc: ResourceProfile);
procedure CrossoverOC(const order, other: ResourceProfile; var daughter, son: ResourceProfile);
function FitnessSSGS(const pair: TALOCPair): Double;

implementation

var ps: ProjData;

procedure SetProjectStructureOC(const nps: ProjData);
begin
  ps := nps;
end;

procedure InitOC(out oc: ResourceProfile);
var r, t: Integer;
begin
  SetLength(oc, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
      oc[r,t] := RandomRange(0, ps.zmax[r]);
end;

procedure MutateOC(var oc: ResourceProfile);
var r, t: Integer;
begin
  for r := 0 to ps.numRes - 1 do
    for t := 0 to ps.numPeriods - 1 do
      if RandomRange(1, 100) <= 3 then
      begin
        if RandomRange(1,2) = 1 then
          inc(oc[r,t])
        else
          dec(oc[r,t]);

        if oc[r,t] < 0 then oc[r,t] := 0;
        if oc[r,t] > ps.zmax[r] then oc[r,t] := ps.zmax[r];
      end;
end;

procedure CrossoverOC(const order, other: ResourceProfile; var daughter, son: ResourceProfile);
begin
  OnePointCrossover(order, other, daughter, ps.numRes, ps.numPeriods);
  OnePointCrossover(other, order, son, ps.numRes, ps.numPeriods);
end;

function FitnessSSGS(const pair: TALOCPair): Double;
var
  sts: JobData;
  resRemaining: ResourceProfile;
begin
  Solve(ps, pair.order, pair.oc, sts, resRemaining);
  result := CalcProfit(ps, sts, resRemaining);
end;

end.

