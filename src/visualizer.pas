unit visualizer;

// Visualisiere Ablaufplan je Ressource in einer Excel-Tabelle

interface

uses projectdata, {$ifndef FPC}excel2000,{$endif} {$ifdef MSWINDOWS}comobj,{$endif} sysutils, globals, helpers;

type TVisualizer = class
  class procedure Mute;
  class procedure Unmute;

  class procedure VisualizeSchedule(const sts: JobData; filename: String);
  class procedure VisualizeGraph(filename: String);
private
  class var quiet: Boolean;
  class procedure SerializePrecedenceToDot(filename: String);
end;

implementation

class procedure TVisualizer.Mute;
begin
  quiet := True;
end;

class procedure TVisualizer.Unmute;
begin
  quiet := False;
end;

class procedure TVisualizer.VisualizeSchedule(const sts: JobData; filename: String);
{$ifndef FPC}
var
  excObj, excWb, excSheet: Variant;
  r, bottomRow: Integer;

  procedure FillCells;
  var t, c, j, k: Integer;
  begin
    for c := 1 to ps.capacities[r] + ps.zmax[r] do begin
      excSheet.Cells[bottomRow-c, 1] := c;
      if c > ps.capacities[r] then
        excSheet.Cells[bottomRow-c, 1].Font.ColorIndex := '3';
    end;

    for t := 0 to ps.numPeriods - 1 do begin
      c := bottomRow;
      excSheet.Cells[c, t+2] := IntToStr(t+1);
      dec(c);
      for j := 0 to ps.numJobs-1 do begin
        if (ps.demands[j,r] > 0) and (sts[j] <= t) and (sts[j] + ps.durations[j] > t) then begin
          for k := 1 to ps.demands[j,r] do begin
            excSheet.Cells[c, t+2] := IntToStr(j);
            excSheet.Cells[c, t+2].Interior.ColorIndex := IntToStr(j+1);
            dec(c);
          end;
        end;
      end;
    end;
  end;
begin
  if quiet then Exit;

  //DeleteFile('C:\Users\a.schnabel\Documents\'+filename+'.xlsx');

  excObj := CreateOleObject('Excel.Application');
  excObj.SheetsInNewWorkbook := 1;

  excWb := excObj.Workbooks.Add;

  excSheet := excWb.Worksheets[1];
  excSheet.Name := 'Schedule';
  excSheet.StandardWidth := 2;

  FormatSettings.LongTimeFormat := 'yyyy-mm-dd-hh-mm-ss';

  excWb.SaveAs(filename+'.xlsx');
  excObj.Visible := True;

  bottomRow := 0;
  for r := 0 to ps.numRes-1 do begin
    excSheet.Cells[bottomRow+1, 2] := 'Resource ' + IntToStr(r+1) + ' K=' + IntToStr(ps.capacities[r]) + ' zmax=' + IntToStr(ps.zmax[r]);
    bottomRow := bottomRow + (ps.capacities[r] + ps.zmax[r]) + 1;
    FillCells;
  end;

  //excWb.Close(SaveChanges := True);
  excWb.Save;
end;
{$else}
begin
end;
{$endif}

class procedure TVisualizer.SerializePrecedenceToDot(filename: String);
var
  dotFile: TextFile;
  i, j: Integer;
  resStr, sep: String;
  r: Integer;
begin
  AssignFile(dotFile, filename+'.dot');
  Rewrite(dotFile);

  WriteLn(dotFile, 'digraph precedence {');
  for i := 0 to ps.numJobs-1 do
    for j := 0 to ps.numJobs-1 do
      if ps.adjMx[i,j] = 1 then
        WriteLn(dotFile, IntToStr(i)+'->'+IntToStr(j));
  for j := 0 to ps.numJobs-1 do begin
    resStr := '';
    for r := 0 to ps.numRes-1 do begin
      if r <> 0 then sep := ',' else sep := '';
      resStr := Concat(resStr, sep, IntToStr(ps.demands[j,r]));
    end;
    WriteLn(dotFile, IntToStr(j)+'[label="('+IntToStr(ps.durations[j])+') '+IntToStr(j)+' ('+ resStr +')"];');
  end;
  WriteLn(dotFile, '}');

  CloseFile(dotFile);
end;

class procedure TVisualizer.VisualizeGraph(filename: String);
begin
  if quiet then Exit;
  SerializePrecedenceToDot(filename);
  THelper.RunCommand('C:\Program Files (x86)\Graphviz2.34\bin\dot.exe', '-Tpdf ' + filename + '.dot -o ' + filename + '.pdf');
  THelper.RunCommand('C:\Program Files (x86)\Adobe\Acrobat 11.0\Acrobat\Acrobat.exe', filename+'.pdf');
end;

end.
