unit ssgsoc;

// Modifiziertes SSGS. Plant auch Zusatzkapazitäten ein.
// Probiert mögliche Einplanungsperioden von Reihenfolgezulässigkeit bis
// Ressourcenzulässigkeit unter Beachtung der maximal erlaubten ZK. Oder
// wähle per tau.

interface

uses classes, sysutils, projectdata, profit, ssgs, globals, resprofiles, math;

type TSSGSOC = class
  class function SolveWithTau(const order, tau: JobData; out sts: JobData): Double;
  class function Solve(const order: JobData; out sts: JobData; doRightShift: Boolean): Double;
private
  class function SolveCommon(const order, tau: JobData; out sts: JobData; doRightShift: Boolean): Double;
  class function RightShift(var sts: JobData; var resRemaining: ResourceProfile; maxProfit: Double): Double;
end;

implementation

class function TSSGSOC.SolveCommon(const order, tau: JobData; out sts: JobData; doRightShift: Boolean): Double;
var
  ix, j, tauPrecFeas, tauResFeas, t, bestT, k1, k2: Integer;
  zeroOc, maxOc, resRemaining, resRemainingTmp: ResourceProfile;
  p, bestProfit: Double;
  fts: JobData;
begin
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
            //resRemainingTmp := Copy(resRemaining, 0, SizeOf(Integer)*ps.numRes*ps.numPeriods);
            for k1 := 0 to ps.numRes - 1 do
                for k2 := 0 to ps.numPeriods - 1 do
                    resRemainingTmp[k1,k2] := resRemaining[k1,k2];

            TSSGS.ScheduleJob(j, t, sts, fts, resRemainingTmp);
            TSSGS.SolveCore(order, ix+1, zeroOc, sts, fts, resRemainingTmp);

            p := CalcProfit(sts, resRemainingTmp);
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

  if tau[0] = -1 then
    begin
      if doRightShift then
         result := RightShift(sts, resRemaining, bestProfit)
      else
        result := bestProfit;
    end
  else
    begin
      result := CalcProfit(sts, resRemaining);
      if doRightShift then
         result := RightShift(sts, resRemaining, result);
    end;
end;

class function TSSGSOC.SolveWithTau(const order, tau: JobData; out sts: JobData): Double;
begin
  result := SolveCommon(order, tau, sts, False);
end;

class function TSSGSOC.Solve(const order: JobData; out sts: JobData; doRightShift: Boolean): Double;
var tau: JobData;
begin
  SetLength(tau, ps.numJobs);
  tau[0] := -1;
  result := SolveCommon(order, tau, sts, doRightShift);
end;

// Versuche die ZK-Kosten durch Rechtsverschiebung von Jobs zu minimieren.
// Rechtsverschiebung genau dann, wenn zulässig (vor frühestem Nachfolger,
// genug Kapazität) und ZK-minimierend.
class function TSSGSOC.RightShift(var sts: JobData; var resRemaining: ResourceProfile; maxProfit: Double): Double;
var
  j, esucc, t, i, st, oldSt, r, bestT: Integer;
  p: Double;
  feasible: Boolean;
  resRemTmp: ResourceProfile;
begin
  SetLength(resRemTmp, ps.numRes, ps.numPeriods);
  // Für jeden Job...
  for j := 0 to ps.numJobs-1 do begin
    // Job aus Restkapazität raus nehmen
    for r := 0 to ps.numRes-1 do
      for t := 0 to ps.numPeriods-1 do
        if (t >= sts[j]) and (t < sts[j] + ps.durations[j]) then
          resRemTmp[r,t] := resRemaining[r,t] + ps.demands[j,r]
        else
          resRemTmp[r,t] := resRemaining[r,t];

    // ... verschiebe nach rechts bis frühester Nachfolger
    // und prüfe wo zulässig und ZK minimal

    // Frühesten Nachfolger bestimmen
    esucc := -1;
    for i := 0 to ps.numJobs-1 do
      if ps.adjMx[j,i] = 1 then
        if (sts[i] < esucc) or (esucc = -1) then
          esucc := sts[i];

    // Prüfen ob einplanung zulässig und welchen DB
    oldSt := sts[j];
    bestT := oldSt;
    for st := oldSt to esucc - ps.durations[j] do begin
      feasible := True;
      for t := st to st+ps.durations[j]-1 do
        for r := 0 to ps.numRes-1 do
          if ps.demands[j,r] > resRemTmp[r,t] + ps.zmax[r] then
            feasible := False;
      if feasible then begin
        sts[j] := st;

        // resremtmp verbrauch für st=t einsetzen
        for r := 0 to ps.numRes-1 do
          for t := 0 to ps.numPeriods-1 do
            if (t >= st) and (t < st + ps.durations[j]) then
              resRemTmp[r,t] := resRemTmp[r,t] - ps.demands[j,r]
            else
              resRemTmp[r,t] := resRemTmp[r,t];

        p := CalcProfit(sts, resRemTmp);

        // zurücksetzen
        for r := 0 to ps.numRes-1 do
          for t := 0 to ps.numPeriods-1 do
            if (t >= st) and (t < st + ps.durations[j]) then
              resRemTmp[r,t] := resRemTmp[r,t] + ps.demands[j,r]
            else
              resRemTmp[r,t] := resRemTmp[r,t];

        if p > maxProfit then begin
          maxProfit := p;
          bestT := st;
        end;
      end;
    end;
    sts[j] := bestT;
  end;
  result := maxProfit;
end;

end.

