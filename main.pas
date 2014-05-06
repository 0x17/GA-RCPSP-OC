unit main;

interface

procedure Entrypoint();

implementation

uses classes, sysutils, projectdata, topsort, printing, profit, helpers, gassgsoc, stopwatch, gassgs, gassgsmod, excel2000, comobj;

const useExcel = True;

procedure TestGeneticAlgorithms(const ps: ProjData); forward;

procedure WriteCSVToExcel(sheet: Variant; rowNum: Integer; csvStr: String; allStrings: Boolean = True);
var
  parts: TStringList;
  part: String;
  colCtr: Integer;
begin
  if not useExcel then exit;

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

procedure WriteOptsAndTime;
var
  fnames: TStringList;
  fname: String;
  ps: ProjData;
  bestOrder: JobData;
  sw: TStopwatch;
  profit, timeSec: Array[0..2] of Double;
  fp: TextFile;
  time: Cardinal;
  ctr: Integer;
  best: TALOCPair;
  best2: TALBPair;
  excObj, excWb, excSheet: Variant;
begin
  if useExcel then
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
  WriteLn(fp, 'filename;profitGASSGSOC;solvetimeGASSGSOC;profitGASSGS;solvetimeGASSGS;profitGASSGSMod;solvetimeGASSGSMod');
  WriteCSVToExcel(excSheet, 1, 'filename;profitGASSGSOC;solvetimeGASSGSOC;profitGASSGS;solvetimeGASSGS;profitGASSGSMod;solvetimeGASSGSMod');

  fnames := ListProjFilesInDir('32Jobs');
  ps := ProjData.Create;
  ctr := 0;
  sw := TStopwatch.Create;

  for fname in fnames do
  begin
    ps.LoadFromFile(fname);
    TopologicalOrder(ps, ps.topOrder);
    CalcMinMaxMakespanCosts(ps);

    sw.Start;
    profit[0] := RunGASSGSOC(ps, bestOrder);
    time := sw.Stop();
    timeSec[0] := time / 1000.0;

    sw.Start;
    profit[1] := RunGASSGS(ps, best);
    time := sw.Stop();
    timeSec[1] := time / 1000.0;

    sw.Start;
    profit[2] := RunGASSGSMod(ps, best2);
    time := sw.Stop();
    timeSec[2] := time / 1000.0;

    WriteLn(fp, fname, ';',
                Format('%f', [profit[0]]), ';',
                Format('%f', [timeSec[0]]), ';',
                Format('%f', [profit[1]]), ';',
                Format('%f', [timeSec[1]]), ';',
                Format('%f', [profit[2]]), ';',
                Format('%f', [timeSec[2]]));
    Flush(fp);

    WriteLn(fname, ';',
            Format('%f', [profit[0]]), ';',
            Format('%f', [timeSec[0]]), ';',
            Format('%f', [profit[1]]), ';',
            Format('%f', [timeSec[1]]), ';',
            Format('%f', [profit[2]]), ';',
            Format('%f', [timeSec[2]]));

    inc(ctr);

    WriteCSVToExcel(excSheet, ctr+1, Concat(fname, ';',
            Format('%f', [profit[0]]), ';',
            Format('%f', [timeSec[0]]), ';',
            Format('%f', [profit[1]]), ';',
            Format('%f', [timeSec[1]]), ';',
            Format('%f', [profit[2]]), ';',
            Format('%f', [timeSec[2]])), False);

    if ctr = 50 then break;
  end;

  //excWb.Close(SaveChanges := True);

  CloseFile(fp);
  sw.Free;
  fnames.Free;
  ps.Free;
end;

procedure TestOneProject;
const
  PROJ_FILENAME = '32Jobs\Modellendogen0001.DAT';
var
  ps: ProjData;
begin
  ps := ProjData.Create;
  ps.LoadFromFile(PROJ_FILENAME);
  TopologicalOrder(ps, ps.topOrder);
  CalcMinMaxMakespanCosts(ps);

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
  profit := RunGASSGSOC(ps, bestOrder);
  PrintActivityList(bestOrder);
//  PrintSchedule(ps, sts);
  WriteLn(Format('Profit = %f', [profit]));
//  WriteLn(Format('Total oc costs = %f', [TotalOCCostsForSchedule(ps, sts)]));
  WriteLn('Time = ', sw.Stop());

  sw.Start;
  profit := RunGASSGS(ps, best);
  WriteLn(Format('Profit = %f', [profit]));
//  WriteLn(Format('Total oc costs = %f', [TotalOCCostsForSchedule(ps, sts)]));
  WriteLn('Time = ', sw.Stop());

  sw.Start;
  profit := RunGASSGSMod(ps, best2);
  WriteLn(Format('Profit = %f', [profit]));
//  WriteLn(Format('Total oc costs = %f', [TotalOCCostsForSchedule(ps, sts)]));
  WriteLn('Time = ', sw.Stop());

  sw.Free;
end;

end.

