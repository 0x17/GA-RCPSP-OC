unit ssgsoc;

// Modifiziertes SSGS. Plant auch Zusatzkapazitäten ein.
// Probiert mögliche Einplanungsperioden von Reihenfolgezulässigkeit bis
// Ressourcenzulässigkeit unter Beachtung der maximal erlaubten ZK. Oder
// wähle per tau.

interface

uses projectdata;

type TSSGSOC = class
  class function SolveWithTau(const order, tau: JobData; out sts: JobData): Double;
  class function Solve(const order: JobData; out sts: JobData): Double;
private
  class function SolveCommon(const order, tau: JobData; out sts: JobData): Double;
end;

implementation

uses classes, sysutils, profit, ssgs, globals, resprofiles, math;

class function TSSGSOC.SolveWithTau(const order, tau: JobData; out sts: JobData): Double;
begin
  result := SolveCommon(order, tau, sts);
end;

class function TSSGSOC.Solve(const order: JobData; out sts: JobData): Double;
var tau: JobData;
begin
  SetLength(tau, ps.numJobs);
  tau[0] := -1;
  result := SolveCommon(order, tau, sts);
end;

class function TSSGSOC.SolveCommon(const order, tau: JobData; out sts: JobData): Double;
var
  ix, j, tauPrecFeas, tauResFeas, t, bestT, k1, k2: Integer;
  zeroOc, maxOc, resRemaining, resRemainingTmp: ResourceProfile;
  p, bestProfit: Double;
  fts: JobData;
begin
  bestProfit := 0;

  SetLength(resRemainingTmp, ps.numRes, ps.numPeriods);
  // Setze verbleibende Ressourcen r,t auf Kapazität
  TSSGS.InitializeResidualCapacity(resRemaining);

  // Setze Zusatzkapazitätsprofile
  TResProfiles.ZeroOC(zeroOc);
  TResProfiles.MaxOC(maxOc);

  // Initialisiere sts, fts
  TSSGS.InitializeJobTimes(sts, fts);

  for ix := 1 to ps.numJobs - 1 do begin
    j := order[ix];

    // Bestimme Zeitpunkt, wo alle Vorgänger beendet sind
    tauPrecFeas := TSSGS.AllPredsFinished(fts, j);

    // Bestimme Zeitpunkt, wo Einplanung ressourcenzulässig ohne Zusatzkapazität ist
    for tauResFeas := tauPrecFeas to ps.numPeriods - 1 do
      if TSSGS.ResourceFeasible(resRemaining, zeroOc, j, tauResFeas) then
        break;

    if tau[0] = -1 then
      begin
        // Probiere alle t in tauPrecFeas..tauResFeas und wähle bestes
        bestProfit := -4.56E100;
        bestT := tauPrecFeas;

        for t := tauPrecFeas to tauResFeas do
          if TSSGS.ResourceFeasible(resRemaining, maxOc, j, t) then begin
            //resRemainingTmp := Copy(resRemaining, 0, ps.numRes*ps.numPeriods);
            for k1 := 0 to ps.numRes - 1 do
                for k2 := 0 to ps.numPeriods - 1 do
                    resRemainingTmp[k1,k2] := resRemaining[k1,k2];

            TSSGS.ScheduleJob(j, t, sts, fts, resRemainingTmp);
            TSSGS.SolveCore(order, ix+1, zeroOc, sts, fts, resRemainingTmp);

            p := TProfit.CalcProfit(sts, resRemainingTmp);
            if p > bestProfit then
            begin
              bestProfit := p;
              bestT := t;
            end;
          end;
      end
    else
      begin
        // Wähle t zwischen tauPrecFeas..tauResFeas mithilfe von tau-Vektor
        bestT := tauPrecFeas + Floor((tauResFeas - tauPrecFeas) / 100 * tau[j]);
        while not(TSSGS.ResourceFeasible(resRemaining, maxOc, j, bestT)) do inc(bestT);
      end;

    TSSGS.ScheduleJob(j, bestT, sts, fts, resRemaining);
  end;

  if tau[0] = -1 then result := bestProfit
  else result := TProfit.CalcProfit(sts, resRemaining);
end;

end.

