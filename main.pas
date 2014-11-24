unit main;

interface

procedure Entrypoint();

implementation

uses classes, sysutils, projectdata, topsort, profit, helpers, globals, gassgsoc, gassgsbeta, gassgsz, gassgszt, gassgstau, variants
{$ifdef Win32}
  , comobj
  {$ifdef FPC}{$else}, excel2000, types, strutils{$endif}
{$endif};

const NHEURS = 5;

type TComputeOpt = function: Double;
     THeur = record
       name: String;
       fn: TComputeOpt;
     end;
     THeurs = Array of THeur;

procedure FillHeuristics(out heurs: THeurs);
begin
  SetLength(heurs, NHEURS);
  heurs[0].name := 'GA-SSGS-Zrt';
  heurs[0].fn := @RunGASSGSZT;
  heurs[1].name := 'GA-SSGS-Zr';
  heurs[1].fn := @RunGASSGSZ;
  heurs[2].name := 'GA-SSGS-Beta';
  heurs[2].fn := @RunGASSGSBeta;
  heurs[3].name := 'GA-SSGS-Tau';
  heurs[3].fn := @RunGASSGSTau;
  heurs[4].name := 'GA-SSGS-OC';
  heurs[4].fn := @RunGASSGSOC;
end;

procedure InitProject(fname: String);
begin
  if ps <> nil then FreeAndNil(ps);
  ps := ProjData.Create;
  ps.LoadFromFile(fname);
  TTopSort.Sort(ps.topOrder);
  CalcMinMaxMakespanCosts;
  ps.ComputeESFTS;
end;

procedure WriteOptsAndTime;
var
  fnames: TStringList;
  headerStr, line, fname: String;
  sw: TStopwatch;
  fp: TextFile;
  time: Cardinal;
  ctr, fileCount, i: Integer;
  excObj, excWb, excSheet: Variant;
  solvetime, profit: Double;
  heurs: THeurs;
begin
  NUM_GENS := 100;
  FillHeuristics(heurs);

  headerStr := 'filename';
  for i := 0 to NHEURS-1 do
    headerStr := headerStr + ';profit(' + heurs[i].name + ');solvetime(' + heurs[i].name + ')';

  {$ifdef Win32}
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

  AssignFile(fp, 'heurOptsAndTime.txt');
  Rewrite(fp);

  WriteLn(headerStr);
  WriteLn(fp, headerStr);
  if USE_EXCEL then
    THelper.WriteCSVToExcel(excSheet, 1, headerStr);

  fnames := THelper.ListProjFilesInDir('j30filtered');
  ctr := 0;
  sw := TStopwatch.Create;

  fileCount := fnames.Count;

  for fname in fnames do
  begin
    if not(FileExists(fname+'.PRULES')) then
      raise Exception.Create('Priority rules not found for ' + fname);

    InitProject(fname);

    if ps.minMs <> ps.maxMs then begin
      line := fname;
      for i := 0 to NHEURS - 1 do
      begin
        WriteLn(heurs[i].name);
        sw.Start;
        profit := heurs[i].fn;
        time := sw.Stop;
        solvetime := time / 1000.0;
        line := line + ';' + FloatToStr(profit)  + ';' + FloatToStr(solvetime);
        WriteLn('Profit=' + FloatToStr(profit));
        WriteLn('Solvetime=' + FloatToStr(solvetime));
        WriteLn;
      end;
    end
    else
      line := Concat(fname, ';NA;NA;NA;NA;NA;NA');

    WriteLn(fp, line);
    WriteLn(Format('Progress: %.0f%%', [ctr/fileCount*100]));
    Flush(fp);

    inc(ctr);
    if USE_EXCEL then
      THelper.WriteCSVToExcel(excSheet, ctr+1, line, False);
  end;

  if USE_EXCEL then
    excWb.Close(SaveChanges := True);

  CloseFile(fp);
  FreeAndNil(sw);
  FreeAndNil(fnames);
  FreeAndNil(ps);
end;

procedure WriteConvergence(maxGens: Integer);
var
   fp: TextFile;
   headerStr: String;
   heurs: THeurs;
   i, k: Integer;
   profit: Double;
begin
  FillHeuristics(heurs);
  InitProject('j30filtered/j3011_7.sm');

  AssignFile(fp, 'convergence.txt');
  Rewrite(fp);

  headerStr := 'ngens';
  for i := 0 to NHEURS-1 do
    headerStr := headerStr + ';profit(' + heurs[i].name + ')';

  Writeln(fp, headerStr);

  for k := 1 to maxGens do
  begin
    NUM_GENS := k;
    WriteLn('Num generations = ' + IntToStr(NUM_GENS));
    Write(fp, IntToStr(NUM_GENS));
    for i := 0 to NHEURS - 1 do
    begin
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

procedure SolveCmdLineProj;
var
  fp: TextFile;
  heurs: THeurs;
  i: Integer;
  sw: TStopwatch;
  time: Cardinal;
  solvetime, profit: Double;
  line: String;
begin
  NUM_GENS := 100;
  FillHeuristics(heurs);
  InitProject(ParamStr(1));
  AssignFile(fp, ParamStr(2));
  Rewrite(fp);
  sw := TStopwatch.Create;
  line := ParamStr(1);

  for i := 0 to NHEURS - 1 do
  begin
    sw.Start;
    profit := heurs[i].fn;
    time := sw.Stop;
    solvetime := time / 1000.0;
    line := line + ';' + FloatToStr(profit)  + ';' + FloatToStr(solvetime);
  end;

  WriteLn(fp, line);
  CloseFile(fp);

  FreeAndNil(ps);
  FreeAndNil(sw);
end;

procedure Entrypoint;
begin
  {$ifndef FPC}
  ReportMemoryLeaksOnShutdown := False;
  {$endif}

  //WriteConvergence(100);
  WriteOptsAndTime;
  //SolveCmdLineProj;
end;

end.

