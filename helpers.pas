unit helpers;

interface

uses classes, sysutils, strutils

{$ifdef Win32}
  {$ifdef FPC},windows{$else},math,windows{$endif}
{$else}
  ,dateutils
{$endif};

type THelper = class
  class function FilenameFromPath(path: String): String;
  class function RandomRangeIncl(lb, ub: Integer): Integer;
  class procedure SkipChar(var fp: TextFile; c: Char; n: Integer);
  class function ListProjFilesInDir(path: String): TStringList;
  class procedure WriteCSVToExcel(sheet: Variant; rowNum: Integer; csvStr: String; allStrings: Boolean = True);
end;

type TStopwatch = class(TObject)
  procedure Start();
  function Stop(): Cardinal;
private
  before: {$ifndef Win32}TDateTime{$else}Cardinal{$endif};
end;

implementation

const PATH_SEP = {$ifdef Win32}'\'{$else}'/'{$endif};

class function THelper.FilenameFromPath(path: String): String;
begin
  result := RightStr(path, Length(path) - LastDelimiter(PATH_SEP, path));
end;

class function THelper.RandomRangeIncl(lb, ub: Integer): Integer;
begin
  result := Random(ub-lb+1)+lb;
end;

class procedure THelper.SkipChar(var fp: TextFile; c: Char; n: Integer);
var
  curChar: Char;
  skipCounter: Integer;
begin
  skipCounter := 0;
  repeat
    Read(fp, curChar);
    if curChar = c then
      inc(skipCounter);
  until skipCounter = n;
end;

class function THelper.ListProjFilesInDir(path: String): TStringList;
var
  sr: TSearchRec;
  oldwd: String;
begin
  result := TStringList.Create;
  oldwd := GetCurrentDir;
  SetCurrentDir(path);
  if FindFirst('*.sm', faAnyFile, sr) = 0 then
  begin
    repeat
      result.Add(path+PATH_SEP+sr.Name);
    until FindNext(sr) <> 0;
  end;
  SetCurrentDir(oldwd);
end;

class procedure THelper.WriteCSVToExcel(sheet: Variant; rowNum: Integer; csvStr: String; allStrings: Boolean = True);
var
  parts: TStringList;
  part: String;
  colCtr: Integer;
begin
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

//==============================================================================
procedure TStopwatch.Start();
begin
  {$ifndef Win32}
  before := Now;
  {$else}
  before := GetTickCount;
  {$endif}
end;

function TStopwatch.Stop(): Cardinal;
begin
  {$ifndef Win32}
  result := MilliSecondsBetween(Now, before);
  {$else}
  result := GetTickCount - before;
  {$endif}
end;

end.

