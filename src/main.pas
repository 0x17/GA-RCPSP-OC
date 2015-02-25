unit main;

interface

type TMain = class
  constructor Create;
  procedure Entrypoint();
private
  const NHEURS = 8 + 4;
  type
    TComputeOpt = function: Double;
    THeur = record
      name, texName: String;
      fn: TComputeOpt;
    end;
    THeurs = Array of THeur;

  var heurs: THeurs;

  procedure InitProject(const fname: String);
  procedure WriteOptsAndTime(const path, outFname: String);
  procedure WriteConvergence(const projFname, outFname: String; maxGens: Integer);
  procedure RunBranchAndBound;
end;

implementation

uses classes, strutils, sysutils, projectdata, math, topsort, branchandbound, profit, helpers, globals, gassgsoc, gassgsbeta, gassgsz, gassgszt, gassgstau, tests, evaluation, variants
{$ifdef MSWINDOWS}
  , comobj
  {$ifdef FPC}{$else}, excel2000, types{$endif}
{$endif};

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

  //WriteConvergence('j30filtered/j3011_7.sm' ,'convergence.txt', 100);
  RunTests;
  //WriteOptsAndTime('../Projekte/j30filtered', 'heursOptsAndTime.txt');
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
  bb := TBranchAndBound.Create('../Projekte/j30filtered/j3011_7.sm');
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

function ParseOptProfit(const projName: String): Double;
var
  fp: TextFile;
  line: String;
begin
  result := -1.0;
  AssignFile(fp, 'OptProfits.csv');
  Reset(fp);

  while not eof(fp) do begin
    ReadLn(fp, line);
    if AnsiStartsStr(projName, line) then begin
      line := AnsiReplaceStr(line, projName+';', '');
      result := StrToFloat(line);
      break;
    end;
  end;

  CloseFile(fp);
end;

procedure TMain.WriteOptsAndTime(const path, outFname: String);
var
  fnames: TStringList;
  headerStr, line, fname: String;
  sw: TStopwatch;
  fp: TextFile;
  time: Cardinal;
  ctr, fileCount, i: Integer;
  excObj, excWb, excSheet: Variant;
  solvetime, profit: Double;

  profits, solvetimes: TResultTable;
  heurNames: TStrArr;

  procedure BuildHeaderStr;
  var i: Integer;
  begin
    headerStr := 'filename';
    for i := 0 to NHEURS-1 do
      headerStr := headerStr + ';profit(' + heurs[i].name + ');solvetime(' + heurs[i].name + ')';
  end;

  procedure ExcelPreamble;
  begin
    {$ifdef MSWINDOWS}
    if USE_EXCEL then
    begin
      if FileExists('test.xlsx') then DeleteFile('test.xlsx');
      excObj := CreateOleObject('Excel.Application');
      excObj.SheetsInNewWorkbook := 1;
      excWb := excObj.Workbooks.Add;
      excSheet := excWb.Worksheets[1];
      excSheet.Name := 'Sheet1';
      FormatSettings.LongTimeFormat := 'yyyy-mm-dd-hh-mm-ss';
      excWb.SaveAs('test.xlsx');
      excObj.Visible := True;
      //excSheet.Cells[1, 1] := 'Test';
    end;
    {$endif}
  end;

  procedure WriteStr(var fp: TextFile; const s: String; row: Integer);
  begin
    WriteLn(s);
    WriteLn(fp, s);
    Flush(fp);
    if USE_EXCEL then
      THelper.WriteCSVToExcel(excSheet, row, s);
  end;

  procedure SolveHeur(const h: THeur);
  begin
    WriteLn(h.name);

    sw.Start;
    profit := h.fn;
    time := sw.Stop;
    solvetime := time / 1000.0;

    line := line + ';' + FloatToStr(profit)  + ';' + FloatToStr(solvetime);
    WriteLn('Profit=' + FloatToStr(profit) + #10 + 'Solvetime=' + FloatToStr(solvetime) + #10);

    profits[ctr, i+1] := RoundTo(profit, -2);
    solvetimes[ctr, i+1] := solvetime;
  end;

begin
  numSchedules := 50000;

  BuildHeaderStr;
  ExcelPreamble;

  AssignFile(fp, outFname);
  Rewrite(fp);

  WriteStr(fp, headerStr, 1);

  fnames := THelper.ListProjFilesInDir(path);
  ctr := 0;
  sw := TStopwatch.Create;

  fileCount := fnames.Count;

  SetLength(profits, fileCount, 1+NHEURS);
  SetLength(solvetimes, fileCount, 1+NHEURS);

  for fname in fnames do begin
    InitProject(fname);

    if ps.minMs <> ps.maxMs then begin
      line := ChangeFileExt(ExtractFileName(fname), '');
      profits[ctr, 0] := ParseOptProfit(line);
      solvetimes[ctr, 0] := -1.0;
      for i := 0 to NHEURS - 1 do
        SolveHeur(heurs[i]);
    end else
      line := 'NA;NA;NA;NA;NA;NA;NA;NA;NA;NA;NA';

    WriteStr(fp, line, ctr+1);
    WriteLn(Format('Progress: %.0f%%', [ctr/fileCount*100]));
    inc(ctr);
  end;

  if USE_EXCEL then
    excWb.Close(SaveChanges := True);

  CloseFile(fp);

  FreeAndNil(sw);
  FreeAndNil(fnames);
  FreeAndNil(ps);

  SetLength(heurNames, NHEURS);
  for i := 0 to NHEURS - 1 do
    heurNames[i] := heurs[i].texName;
  TEvaluator.EvalResultsToTeX(heurNames, profits, solvetimes, 'results.tex');
  THelper.RunCommand('pdflatex', 'results.tex');
  THelper.RunCommand('C:\Program Files (x86)\Adobe\Acrobat 11.0\Acrobat\AcroRd32.exe', 'results.pdf');
end;

procedure TMain.WriteConvergence(const projFname, outFname: String; maxGens: Integer);
var
   fp: TextFile;
   headerStr: String;
   heurs: THeurs;
   i, k: Integer;
   profit: Double;
begin
  InitProject(projFname);

  AssignFile(fp, outFname);
  Rewrite(fp);

  headerStr := 'ngens';
  for i := 0 to NHEURS-1 do
    headerStr := headerStr + ';profit(' + heurs[i].name + ')';

  Writeln(fp, headerStr);

  for k := 1 to maxGens do begin
    numSchedules := k;
    WriteLn('Num schedules = ' + IntToStr(numSchedules));
    Write(fp, IntToStr(numSchedules));
    for i := 0 to NHEURS - 1 do begin
      WriteLn(heurs[i].name);
      profit := heurs[i].fn;
      Write(fp, ';' + FloatToStr(profit));
      WriteLn('Profit=' + FloatToStr(profit));
      WriteLn;
    end;
    WriteLn(fp);
  end;

  CloseFile(fp);
end;

end.
