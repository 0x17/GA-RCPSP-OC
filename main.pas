unit main;

interface

procedure Entrypoint();

implementation

uses classes, sysutils, projectdata, topsort, printing, profit, helpers, gassgsoc, stopwatch, gassgs, gassgsmod, excel2000, comobj, strutils, types, constants, gassgsmod2, testing, ssgsoc;

procedure TestGeneticAlgorithms(const ps: ProjData); forward;

procedure WriteCSVToExcel(sheet: Variant; rowNum: Integer; csvStr: String; allStrings: Boolean = True);
var
  parts: TStringList;
  part: String;
  colCtr: Integer;
begin
  if not USE_EXCEL then exit;

  parts := TStringList.Create;
  parts.Clear;
  parts.Delimiter := #59;
  parts.DelimitedText := csvStr;

  colCtr := 1;
  for part in parts do
  begin
    if (colCtr > 1) and not(allStrings) then
      sheet.Cells[rowNum, colCtr] := StrToFloat(part)
    else
      sheet.Cells[rowNum, colCtr] := part;

    inc(colCtr);
  end;

  FreeAndNil(parts);
end;

type TProfitTimePair = record
  profit, time: Double;
end;

function ParseProfitAndTimeForMIP(resultFilename, projFilename: String): TProfitTimePair;
var
  line, coreFn: String;
  fp: TextFile;
  parts: TStringDynArray;
begin
  AssignFile(fp, resultFilename);
  Reset(fp);

  coreFn := FilenameFromPath(projFilename);

  while not(Eof(fp)) do
  begin
    ReadLn(fp, line);
    if AnsiContainsText(line, coreFn) then
    begin
       parts := SplitString(line, ';');
       FormatSettings.DecimalSeparator := '.';
       result.profit := StrToFloat(parts[1]);

       if parts[2] = 'Timeout' then
         result.time := 3600.0
       else
         result.time := StrToFloat(parts[2]);

       FormatSettings.DecimalSeparator := ',';
       Exit;
    end;
  end;

  CloseFile(fp);
end;

procedure WriteOptsAndTime;
const
  HEADER_STR = 'filename;'
             + 'profitGASSGSOC;solvetimeGASSGSOC;'
             + 'profitGASSGS;solvetimeGASSGS;'
             + 'profitGASSGSMod;solvetimeGASSGSMod;'
             + 'profitGASSGSMod2;solvetimeGASSGSMod2;'
             + 'profitMIP;solvetimeMIP';
var
  fnames: TStringList;
  line, fname: String;
  ps: ProjData;
  bestOrder: JobData;
  sw: TStopwatch;
  ptp: Array[0..4] of TProfitTimePair;
  fp: TextFile;
  time: Cardinal;
  ctr: Integer;
  best: TALOCPair;
  best2: TALBPair;
  excObj, excWb, excSheet: Variant;
begin
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

  AssignFile(fp, 'ssgsocOptsAndTime.txt');
  Rewrite(fp);
  WriteLn(HEADER_STR);
  WriteLn(fp, HEADER_STR);
  WriteCSVToExcel(excSheet, 1, HEADER_STR);

  fnames := ListProjFilesInDir('j30');
  ps := ProjData.Create;
  ctr := 0;
  sw := TStopwatch.Create;

  for fname in fnames do
  begin
    if not(FileExists(fname+'.PRULES')) then
      continue;

    ps.LoadFromFile(fname);
    TopologicalOrder(ps, ps.topOrder);
    CalcMinMaxMakespanCosts(ps);
    CalcUMax(ps);

    ptp[4] := ParseProfitAndTimeForMIP('optimalStats.txt', fname);
    if (ptp[4].time = 3600.0) or (ptp[4].profit = 0.0) then
      continue;

    sw.Start;
    ptp[0].profit := RunGASSGSOC(ps, bestOrder, False);
    time := sw.Stop();
    ptp[0].time := time / 1000.0;

    //PrintActivityList(bestOrder);
    //WriteLn;

    sw.Start;
    ptp[1].profit := RunGASSGS(ps, best);
    time := sw.Stop();
    ptp[1].time := time / 1000.0;

    //PrintTALOCPair(best);
    //WriteLn;

    sw.Start;
    ptp[2].profit := RunGASSGSMod(ps, best2);
    time := sw.Stop();
    ptp[2].time := time / 1000.0;

    ///PrintTALBPair(best2);
    //WriteLn;

    sw.Start;
    ptp[3].profit := RunGASSGSMod2(ps, best2);
    time := sw.Stop();
    ptp[3].time := time / 1000.0;

    //PrintTALBPair(best2);
    //Readln;

    line := Concat(fname, ';',
                Format('%f', [ptp[0].profit]), ';',
                Format('%f', [ptp[0].time]), ';',
                Format('%f', [ptp[1].profit]), ';',
                Format('%f', [ptp[1].time]), ';',
                Format('%f', [ptp[2].profit]), ';',
                Format('%f', [ptp[2].time]), ';',
                Format('%f', [ptp[3].profit]), ';',
                Format('%f', [ptp[3].time]), ';',
                Format('%f', [ptp[4].profit]), ';',
                Format('%f', [ptp[4].time]));

    WriteLn(line);
    WriteLn(fp, line);
    Flush(fp);

    inc(ctr);
    WriteCSVToExcel(excSheet, ctr+1, line, False);
  end;

  if USE_EXCEL then
    excWb.Close(SaveChanges := True);

  CloseFile(fp);
  sw.Free;
  fnames.Free;
  ps.Free;
end;

procedure TestOneProject;
const
  PROJ_FILENAME = 'j30\j3010_10.sm';
var
  ps: ProjData;
begin
  ps := ProjData.Create;
  ps.LoadFromFile(PROJ_FILENAME);
  TopologicalOrder(ps, ps.topOrder);
  CalcMinMaxMakespanCosts(ps);
  CalcUMax(ps);

//  PrintProject(ps);
//  TestSSGS(ps);
  WriteLn('---------------------');

//  TestSSGSOC(ps);
//  PrintProjects();

  TestGeneticAlgorithms(ps);
  //TestSSGSOC(ps);

  ps.Free;

//  ReadLn;
end;

procedure Entrypoint();
begin
  //ReportMemoryLeaksOnShutdown := True;
  //TestOneProject;
  WriteOptsAndTime;
  //RunTests;
end;

procedure TestGeneticAlgorithms(const ps: ProjData);
var
  sw: TStopwatch;
  profit: Double;
  bestOrder: JobData;
  best: TALOCPair;
  best2: TALBPair;
begin
  sw := TStopwatch.Create;

  sw.Start;
  profit := RunGASSGSOC(ps, bestOrder, False);
  WriteLn(Format('Profit = %f', [profit]));
  WriteLn('Time = ', sw.Stop());

  sw.Start;
  profit := RunGASSGS(ps, best);
  WriteLn(Format('Profit = %f', [profit]));
  WriteLn('Time = ', sw.Stop());

  sw.Start;
  profit := RunGASSGSMod(ps, best2);
  WriteLn(Format('Profit = %f', [profit]));
  WriteLn('Time = ', sw.Stop());

  sw.Free;

  ReadLn;
end;

end.

