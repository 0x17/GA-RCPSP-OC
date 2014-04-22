unit ssgsoc;

//{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, projectdata, ssgs, profit;

function SolveWithOC(const ps: ProjData; const order: JobData; out sts: JobData): Double;

implementation

procedure SubtractResRemaining(const ps: ProjData; var resRemaining: ResourceProfile; j, stj: Integer);
var
  r, tau: Integer;
begin
  for tau := stj to stj+ps.durations[j]-1 do
    for r := 0 to ps.numRes-1 do
      resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
end;

function SolveWithOC(const ps: ProjData; const order: JobData; out sts: JobData): Double;
var
  r, k, ix, j, tauPrecFeas, tauResFeas, t, bestT, k1, k2: Integer;
  p, bestProfit: Double;
  zeroOc, maxOc, resRemaining, resRemainingTmp: ResourceProfile;
  fts: JobData;
begin
  SetLength(resRemainingTmp, ps.numRes, ps.numPeriods);
  // Setze verbleibende Ressourcen r,t auf Kapazität
  SetLength(resRemaining, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes-1 do
    for t := 0 to ps.numPeriods-1 do
      resRemaining[r,t] := ps.capacities[r];

  // Setze Zusatzkapazitätsprofile
  SetLength(zeroOc, ps.numRes, ps.numPeriods);
  ZeroOvercapacity(ps, zeroOc);
  SetLength(maxOc, ps.numRes, ps.numPeriods);
  MaxOvercapacity(ps, maxOc);

  // Initialisiere sts, fts
  SetLength(sts, ps.numJobs);
  SetLength(fts, ps.numJobs);
  sts[0] := 0;
  fts[0] := 0;

  for ix := 1 to ps.numJobs - 1 do
  begin
    j := order[ix];

    // Bestimme Zeitpunkt, wo alle Vorgänger beendet sind
    tauPrecFeas := 0;
    for k := 0 to ps.numJobs-1 do
      if (ps.adjMx[k,j] = 1) and (fts[k] > tauPrecFeas) then
        tauPrecFeas := fts[k];

    // Bestimme Zeitpunkt, wo Einplanung ressourcenzulässig ohne Zusatzkapazität ist
    for tauResFeas := tauPrecFeas to ps.numPeriods - 1 do
      if ResourceFeasible(ps, resRemaining, zeroOc, j, tauResFeas) then
        break;

    // Probiere alle t in tauPrecFeas..tauResFeas und wähle bestes
    bestProfit := -4.56E100;
    bestT := tauPrecFeas;

    for t := tauPrecFeas to tauResFeas do
      if ResourceFeasible(ps, resRemaining, maxOc, j, t) then
      begin
        //resRemainingTmp := Copy(resRemaining, 0, SizeOf(Integer)*ps.numRes*ps.numPeriods);
        for k1 := 0 to ps.numRes - 1 do
            for k2 := 0 to ps.numPeriods - 1 do
                resRemainingTmp[k1,k2] := resRemaining[k1,k2];

        sts[j] := t;
        fts[j] := t + ps.durations[j];
        SubtractResRemaining(ps, resRemainingTmp, j, t);

        SolveCore(ps, order, ix+1, zeroOc, sts, fts, resRemainingTmp);

        p := CalcProfit(ps, sts, resRemainingTmp);
        if p > bestProfit then
        begin
          bestProfit := p;
          bestT := t;
        end;
      end;

    // Plane j zu bestem t ein
    sts[j] := bestT;
    fts[j] := sts[j] + ps.durations[j];
    // Ziehe entsprechend Bedarf j in laufenden Perioden Restkapazität ab
    SubtractResRemaining(ps, resRemaining, j, bestT);
  end;

  result := bestProfit;
end;

end.

