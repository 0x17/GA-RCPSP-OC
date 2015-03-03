unit main;

interface

uses individual;

type TMain = class
  constructor Create;
  procedure Entrypoint();
private
  const NHEURS = 8 + 4;
  type
    TComputeOpt = function: TDblArr;
    THeur = record
      name, texName: String;
      fn: TComputeOpt;
    end;
    THeurs = Array of THeur;

  var heurs: THeurs;

  procedure InitProject(const fname: String);
  procedure WriteOptsAndTime(const path, outFname: String; upperLimitIx: Integer);
  procedure RunBranchAndBound;
end;

implementation

uses classes, strutils, sysutils, projectdata, math, topsort, branchandbound, profit, helpers, globals, gassgsoc, gassgsbeta, gassgsz, gassgszt, gassgstau, tests, variants;

constructor TMain.Create;
var k: Integer;
begin
  k := 0;
  SetLength(heurs, NHEURS);

  heurs[k].name := 'GA-SSGS-Zrt';
  heurs[k].texName := '(\lambda|z_{rt})';
  heurs[k].fn := @RunGASSGSZT;
  inc(k);
  heurs[k].name := 'GA-SSGS-Zr';
  heurs[k].texName := '(\lambda|z_r)';
  heurs[k].fn := @RunGASSGSZ;
  inc(k);

  // BETA VARIANTS
  heurs[k].name := 'GA-SSGS-Beta1-'+GetBetaName(0);
  heurs[k].texName := GetBetaTexName(0);
  heurs[k].fn := @RunGASSGSBeta1;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta2-'+GetBetaName(1);
  heurs[k].texName := GetBetaTexName(1);
  heurs[k].fn := @RunGASSGSBeta2;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta3-'+GetBetaName(2);
  heurs[k].texName := GetBetaTexName(2);
  heurs[k].fn := @RunGASSGSBeta3;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta4-'+GetBetaName(3);
  heurs[k].texName := GetBetaTexName(3);
  heurs[k].fn := @RunGASSGSBeta4;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta5-'+GetBetaName(4);
  heurs[k].texName := GetBetaTexName(4);
  heurs[k].fn := @RunGASSGSBeta5;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta6-'+GetBetaName(5);
  heurs[k].texName := GetBetaTexName(5);
  heurs[k].fn := @RunGASSGSBeta6;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta7-'+GetBetaName(6);
  heurs[k].texName := GetBetaTexName(6);
  heurs[k].fn := @RunGASSGSBeta7;
  inc(k);

  heurs[k].name := 'GA-SSGS-Beta8-'+GetBetaName(7);
  heurs[k].texName := GetBetaTexName(7);
  heurs[k].fn := @RunGASSGSBeta8;
  inc(k);

  // OTHERS
  heurs[k].name := 'GA-SSGS-Tau';
  heurs[k].texName := '(\lambda|\tau)';
  heurs[k].fn := @RunGASSGSTau;
  inc(k);

  heurs[k].name := 'GA-SSGS-OC';
  heurs[k].texName := '(\lambda)';
  heurs[k].fn := @RunGASSGSOC;
  inc(k);
end;

procedure TMain.Entrypoint;
begin
  {$ifndef FPC}
  ReportMemoryLeaksOnShutdown := False;
  {$endif}

  //RunTests;

  WriteOptsAndTime('../Projekte/j30filtered', 'HeursRawj30.txt', 3);
  (*WriteOptsAndTime('../Projekte/j60', 'HeursRawj60.txt', 4);
  WriteOptsAndTime('../Projekte/j90', 'HeursRawj90.txt', 6);
  WriteOptsAndTime('../Projekte/j120', 'HeursRawj120.txt', 7);*)

  //RunBranchAndBound;
end;

procedure TMain.RunBranchAndBound;
var
  bb: TBranchAndBound;
  sts: JobData;
  db, solvetime: Double;
  time: Cardinal;
  j: Integer;
  sw: TStopwatch;
  resRem: ResourceProfile;
begin
  bb := TBranchAndBound.Create('testproj.sm');
  SetLength(sts, ps.numJobs);

  sw := TStopwatch.Create;
  sw.Start;
  db := bb.Solve(sts);
  time := sw.Stop;
  solvetime := time / 1000.0;

  writeln('makespan = ', sts[ps.numJobs-1]);
  ps.InferProfileFromSchedule(sts, resRem);
  writeln('oc costs = ', FloatToStr(TProfit.TotalOCCosts(resRem)));
  writeln('revenue = ', FloatToStr(TProfit.Revenue(sts[ps.numJobs-1])));
  writeln('profit = ', FloatToStr(db));
  writeln('solvetime = ', FloatToStr(solvetime));
  write('(');
  for j := 0 to ps.numJobs-1 do write(sts[j], '; ');
  writeln(')');
  readln;

  FreeAndNil(bb);
  FreeAndNil(sw);
end;

procedure TMain.InitProject(const fname: String);
begin
  if ps <> nil then FreeAndNil(ps);
  ps := ProjData.Create;
  ps.LoadFromFile(fname);
  TTopSort.Sort(ps.topOrder);
  TProfit.CalcMinMaxMakespanCosts;
  ps.ReorderJobsAscDepth;
  TTopSort.Sort(ps.topOrder);
  ps.ComputeESFTS;
end;

procedure TMain.WriteOptsAndTime(const path, outFname: String; upperLimitIx: Integer);
var
  fnames: TStringList;
  headerStr, line, fname: String;
  fp: TextFile;
  ctr, i, takeCount: Integer;
  profits: TDblArr;

  procedure BuildHeaderStr;
  var i: Integer;
  begin
    headerStr := 'filename';
    for i := 0 to NHEURS-1 do
      headerStr := headerStr + ';' + heurs[i].texName;
  end;

  procedure WriteStr(var fp: TextFile; const s: String);
  begin
    WriteLn(fp, s);
    Flush(fp);
  end;

  procedure SolveHeur(const h: THeur);
  var
    x: Integer;
    profitsLine: String;
  begin
    profits := h.fn;
    profitsLine := '';
    for x := Low(profits) to High(profits) do begin
      profitsLine := profitsLine + FloatToStr(RoundTo(profits[x], -2));
      if x < High(profits) then
        profitsLine := profitsLine + ':';
    end;
    line := line + ';' + profitsLine
  end;

begin
  g_upperLimitIx := upperLimitIx;

  AssignFile(fp, outFname);
  Rewrite(fp);

  BuildHeaderStr;
  WriteStr(fp, headerStr);

  fnames := THelper.ListProjFilesInDir(path);

  takeCount := fnames.Count;

  ctr := 0;
  for fname in fnames do begin
    InitProject(fname);

    if ps.minMs <> ps.maxMs then begin
      line := ChangeFileExt(ExtractFileName(fname), '');
      WriteLn('Solving ', line, '...');
      for i := 0 to NHEURS - 1 do
        SolveHeur(heurs[i]);

      WriteStr(fp, line);
      inc(ctr);

      if ctr >= takeCount then break;
    end
  end;

  CloseFile(fp);

  FreeAndNil(fnames);
  FreeAndNil(ps);
end;

end.

