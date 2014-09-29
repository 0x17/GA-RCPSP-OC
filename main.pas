unit main;

interface

procedure Entrypoint();

implementation

uses classes, sysutils, projectdata, topsort, printing, profit, helpers, gassgsoc, stopwatch, gassgs, gassgsmod, excel2000, comobj, strutils, types, constants, gassgsmod2, ssgsoc, gams;

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

procedure WriteOptsAndTime(fromFn, toFn: String);
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
  fromIx, toIx, fctr: Integer;
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

  AssignFile(fp, 'heurOptsAndTime.txt');
  Rewrite(fp);
  WriteLn(HEADER_STR);
  WriteLn(fp, HEADER_STR);
  WriteCSVToExcel(excSheet, 1, HEADER_STR);

  fnames := THelper.ListProjFilesInDir('j30');
  ps := ProjData.Create;
  ctr := 0;
  sw := TStopwatch.Create;

  if fromFn.IsEmpty and toFn.IsEmpty then begin
    fromIx := 0;
    toIx := fnames.Count-1;
  end else begin
    fromIx := fnames.IndexOf(fromFn);
    toIx := fnames.IndexOf(toFn);
  end;

  for fctr := fromIx to toIx do
  begin
    fname := fnames.Strings[fctr];

    if not(FileExists(fname+'.PRULES')) then
      continue;

    ps.LoadFromFile(fname);
    TTopSort.Sort(ps, ps.topOrder);
    CalcMinMaxMakespanCosts(ps);
    ps.ComputeESFTS;

    if ps.minMs <> ps.maxMs then begin
      sw.Start;
      ptp[0].profit := RunGASSGSOC(ps, bestOrder, False);
      time := sw.Stop();
      ptp[0].time := time / 1000.0;

      sw.Start;
      ptp[1].profit := RunGASSGSMod2(ps, best2);
      time := sw.Stop();
      ptp[1].time := time / 1000.0;

      line := Concat(fname, ';',
                Format('%f', [ptp[0].profit]), ';',
                Format('%f', [ptp[0].time]), ';',
                Format('%f', [ptp[1].profit]), ';',
                Format('%f', [ptp[1].time]));
    end
    else
      line := Concat(fname, ';NA;NA;NA;NA');

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

procedure Entrypoint();
begin
  //ReportMemoryLeaksOnShutdown := True;
  WriteOptsAndTime('', '');
  //TestGAMS;
end;

end.

