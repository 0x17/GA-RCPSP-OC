unit main;

interface

procedure Entrypoint();

implementation

uses classes, sysutils, projectdata, topsort, profit, helpers, globals, gassgsoc, gassgsbeta, operators
{$ifdef Win32}
  {$ifdef FPC},comobj{$else},comobj, excel2000, types{$endif}
{$else}
{$endif};

procedure WriteOptsAndTime;
type TProfitTimePair = record
  profit, time: Double;
end;
const
  HEADER_STR = 'filename;'
             + 'profitGASSGSOC;solvetimeGASSGSOC;'
             + 'profitGASSGSMod;solvetimeGASSGSMod';
var
  fnames: TStringList;
  line, fname: String;
  bestOrder: JobData;
  sw: TStopwatch;
  ptp: Array[0..1] of TProfitTimePair;
  fp: TextFile;
  time: Cardinal;
  ctr: Integer;
  best2: TALBPair;
  excObj, excWb, excSheet: Variant;
begin
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
  WriteLn(HEADER_STR);
  WriteLn(fp, HEADER_STR);
  if USE_EXCEL then
    THelper.WriteCSVToExcel(excSheet, 1, HEADER_STR);

  fnames := THelper.ListProjFilesInDir('j30');
  ps := ProjData.Create;
  ctr := 0;
  sw := TStopwatch.Create;

  for fname in fnames do
  begin
    if not(FileExists(fname+'.PRULES')) then
      continue;

    ps.LoadFromFile(fname);
    TTopSort.Sort(ps.topOrder);
    CalcMinMaxMakespanCosts;
    ps.ComputeESFTS;

    if ps.minMs <> ps.maxMs then begin
      sw.Start;
      ptp[0].profit := RunGASSGSOC(bestOrder);
      time := sw.Stop();
      ptp[0].time := time / 1000.0;

      sw.Start;
      ptp[1].profit := RunGASSGSBeta(best2);
      time := sw.Stop();
      ptp[1].time := time / 1000.0;

      line := Format('%s;%f;%f;%f;%f', [fname, ptp[0].profit, ptp[0].time, ptp[1].profit, ptp[1].time]);
    end
    else
      line := Concat(fname, ';NA;NA;NA;NA');

    WriteLn(line);
    WriteLn(fp, line);
    Flush(fp);

    inc(ctr);
    if USE_EXCEL then
      THelper.WriteCSVToExcel(excSheet, ctr+1, line, False);

    break;
  end;

  if USE_EXCEL then
    excWb.Close(SaveChanges := True);

  CloseFile(fp);
  sw.Free;
  fnames.Free;
  ps.Free;
end;

procedure Entrypoint();
begin
  {$ifndef FPC}
  ReportMemoryLeaksOnShutdown := True;
  {$endif}

  WriteOptsAndTime;
  //TestGAMS;
end;

end.

