unit main;

interface

uses helpers;

type TMain = class
  constructor Create;
  procedure Entrypoint();

  class procedure InitProject(const fname: String);
private
  const NHEURS = 12;
  type
    TComputeOpt = function: TDblArr;
    THeur = record
      name, texName: String;
      fn: TComputeOpt;
    end;
    THeurs = Array of THeur;

  var heurs: THeurs;

  procedure InitHeuristic(const name, texName: String; const fn: TComputeOpt; var ix: Integer);
  procedure WriteOptsAndTime(const path, outFname: String; takeCount: Integer = -1);
end;

implementation

uses classes, strutils, sysutils, projectdata, math, topsort, profit, globals, gassgsoc, gassgsbeta, gassgsz, gassgszt, gassgstau, tests, variants;

constructor TMain.Create;
var
  k: Integer;
  fns: Array of TComputeOpt;
  i: Integer;
begin
  k := 0;
  SetLength(heurs, NHEURS);

  InitHeuristic('GA-SSGS-Zrt', '(\lambda|z_{rt})', @RunGASSGSZT, k);
  InitHeuristic('GA-SSGS-Zr', '(\lambda|z_r)', @RunGASSGSZ, k);

  // BETA VARIANTS
  SetLength(fns, 8);
  fns[0] := @RunGASSGSBeta1;
  fns[1] := @RunGASSGSBeta2;
  fns[2] := @RunGASSGSBeta3;
  fns[3] := @RunGASSGSBeta4;
  fns[4] := @RunGASSGSBeta5;
  fns[5] := @RunGASSGSBeta6;
  fns[6] := @RunGASSGSBeta7;
  fns[7] := @RunGASSGSBeta8;

  for i := 0 to 7 do
    InitHeuristic('GA-SSGS-Beta' + IntToStr(i+1) + '-' + GetBetaName(i), GetBetaTexName(i), fns[i], k);

  // OTHERS
  InitHeuristic('GA-SSGS-Tau', '(\lambda|\tau)', @RunGASSGSTau, k);
  InitHeuristic('GA-SSGS-OC', '(\lambda)', @RunGASSGSOC, k);
end;

procedure TMain.Entrypoint;
begin
  {$ifndef FPC}
  ReportMemoryLeaksOnShutdown := False;
  {$endif}

  if SINGLE_RESULT then g_upperTimeLimitIndex := 0
  else g_upperTimeLimitIndex := 3;

  RunTests;

  //WriteOptsAndTime('../../Projekte/j30filtered', 'HeursRawj30out.csv');

  (*WriteOptsAndTime('../../Projekte/j60', 'HeursRawj60.csv', 4);
  WriteOptsAndTime('../../Projekte/j90', 'HeursRawj90.csv', 6);
  WriteOptsAndTime('../../Projekte/j120', 'HeursRawj120.csv', 7);*)
end;

class procedure TMain.InitProject(const fname: String);
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

procedure TMain.InitHeuristic(const name, texName: String; const fn: TComputeOpt; var ix: Integer);
begin
  heurs[ix].name := name;
  heurs[ix].texName := texName;
  heurs[ix].fn := fn;
  inc(ix);
end;

procedure TMain.WriteOptsAndTime(const path, outFname: String; takeCount: Integer);
var
  fnames: TStringList;
  headerStr, line, fname: String;
  fp: TextFile;
  ctr, i: Integer;
  profits: TDblArr;

  procedure BuildHeaderStr;
  var i: Integer;
  begin
    headerStr := 'filename';
    for i := 0 to NHEURS-1 do
      headerStr := headerStr + ';' + heurs[i].texName;
  end;

  procedure WriteStr(var fp: TextFile; const s: String); begin
    WriteLn(fp, s);
    Flush(fp);
    WriteLn(s);
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
  AssignFile(fp, outFname);
  Rewrite(fp);

  BuildHeaderStr;
  WriteStr(fp, headerStr);

  fnames := THelper.ListProjFilesInDir(path);

  if takeCount < 0 then
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

