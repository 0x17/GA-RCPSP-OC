unit ssgsoc;

// Modifiziertes SSGS. Plant auch Zusatzkapazitäten ein.
// Probiert mögliche Einplanungsperioden von Reihenfolgezulässigkeit bis
// Ressourcenzulässigkeit unter Beachtung der maximal erlaubten ZK.

interface

uses classes, sysutils, projectdata, ssgs, profit, constants;

function SolveWithOC(const ps: ProjData; const order: JobData; out sts: JobData; doRightShift: Boolean): Double;
function RightShift(const ps: ProjData; var sts: JobData; var resRemaining: ResourceProfile; maxProfit: Double): Double;

implementation

// Verringere die Restkapazität um den Verbrauch von j bei Einplanung in
// Periode stj.
procedure SubtractResRemaining(const ps: ProjData; var resRemaining: ResourceProfile; j, stj: Integer);
var
  r, tau: Integer;
begin
  for tau := stj to stj+ps.durations[j]-1 do
    for r := 0 to ps.numRes-1 do
      resRemaining[r,tau] := resRemaining[r,tau] - ps.demands[j,r];
end;

// Modifiziertes SSGS zur heuristischen Bestimmung eines Ablaufplans für ein
// gegebenes Projekt und Aktivitätenliste.
// Führe danach optional eine Rechtsverschiebung durch.
function SolveWithOC(const ps: ProjData; const order: JobData; out sts: JobData; doRightShift: Boolean): Double;
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

  if doRightShift then
    result := RightShift(ps, sts, resRemaining, bestProfit)
  else
    result := bestProfit

end;

// Versuche die ZK-Kosten durch Rechtsverschiebung von Jobs zu minimieren.
// Rechtsverschiebung genau dann, wenn zulässig (vor frühestem Nachfolger,
// genug Kapazität) und ZK-minimierend.
function RightShift(const ps: ProjData; var sts: JobData; var resRemaining: ResourceProfile; maxProfit: Double): Double;
var
  j, esucc, t, i, st, oldSt, r, bestT: Integer;
  p: Double;
  feasible: Boolean;
  resRemTmp: ResourceProfile;
begin
  SetLength(resRemTmp, ps.numRes, ps.numPeriods);
  // Für jeden Job...
  for j := 0 to ps.numJobs-1 do
  begin
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
    for st := oldSt to esucc - ps.durations[j] do
    begin
      feasible := True;
      for t := st to st+ps.durations[j]-1 do
        for r := 0 to ps.numRes-1 do
          if ps.demands[j,r] > resRemTmp[r,t] + ps.zmax[r] then
            feasible := False;
      if feasible then
      begin
        sts[j] := st;

        // resremtmp verbrauch für st=t einsetzen
        for r := 0 to ps.numRes-1 do
          for t := 0 to ps.numPeriods-1 do
            if (t >= st) and (t < st + ps.durations[j]) then
              resRemTmp[r,t] := resRemTmp[r,t] - ps.demands[j,r]
            else
              resRemTmp[r,t] := resRemTmp[r,t];

        p := CalcProfit(ps, sts, resRemTmp);

        // zurücksetzen
        for r := 0 to ps.numRes-1 do
          for t := 0 to ps.numPeriods-1 do
            if (t >= st) and (t < st + ps.durations[j]) then
              resRemTmp[r,t] := resRemTmp[r,t] + ps.demands[j,r]
            else
              resRemTmp[r,t] := resRemTmp[r,t];

        if p > maxProfit then
        begin
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

