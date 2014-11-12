unit main;

interface

procedure Entrypoint();

implementation

uses classes, sysutils, projectdata, topsort, profit, helpers, globals, gassgsoc, gassgsbeta, gassgsz, gassgszt, gassgstau, variants
{$ifdef Win32}
  , comobj
  {$ifdef FPC}{$else}, excel2000, types, strutils{$endif}
{$endif};



procedure WriteOptsAndTime;
type TComputeOpt = function: Double;
     THeur = record
       name: String;
       fn: TComputeOpt;
     end;
const
  HEADER_STR = 'filename;'
             + 'profitGASSGSOC;solvetimeGASSGSOC;'
             + 'profitGASSGSBeta;solvetimeGASSGSBeta;'
             + 'profitGASSGSZ;solvetimeGASSGSZ;'
             + 'profitGASSGStau;solvetimeGASSGStau';
  NHEURS = 5;
var
  fnames: TStringList;
  line, fname: String;
  sw: TStopwatch;
  fp: TextFile;
  time: Cardinal;
  ctr, fileCount, i: Integer;
  excObj, excWb, excSheet: Variant;
  solvetime, profit: Double;
  heurs: Array[0..NHEURS-1] of THeur;
begin
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

  fileCount := fnames.Count;

  for fname in fnames do
  begin
    if not(FileExists(fname+'.PRULES')) then
      raise Exception.Create('Priority rules not found for ' + fname);

    ps.LoadFromFile(fname);
    TTopSort.Sort(ps.topOrder);
    CalcMinMaxMakespanCosts;
    ps.ComputeESFTS;

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
      end;
    end
    else
      line := Concat(fname, ';NA;NA;NA;NA;NA;NA');

    WriteLn(line);
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
  sw.Free;
  fnames.Free;
  ps.Free;
end;



procedure Entrypoint;
begin
  {$ifndef FPC}
  ReportMemoryLeaksOnShutdown := False;
  {$endif}

  WriteOptsAndTime;
  //TestPRules;
end;

end.

