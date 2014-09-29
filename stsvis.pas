unit stsvis;

// Visualisiere Ablaufplan je Ressource in einer Excel-Tabelle

interface

uses projectdata, excel2000, comobj, strutils, sysutils;

procedure VisualizeSchedule(const ps: ProjData; const sts: JobData; filename: String);

implementation

procedure VisualizeSchedule(const ps: ProjData; const sts: JobData; filename: String);
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
  DeleteFile('C:\Users\a.schnabel\Documents\'+filename+'.xlsx');

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

end.
