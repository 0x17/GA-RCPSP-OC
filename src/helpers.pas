unit helpers;

interface

uses classes;

type
  ByteMx2D = Array of Array of Byte;

  THelper = class
    class function FilenameFromPath(const path: String): String;
    class function RandomRangeIncl(lb, ub: Integer): Integer;
    class procedure SkipChar(var fp: TextFile; c: Char; n: Integer);
    class function ListProjFilesInDir(const path: String): TStringList;
    class procedure WriteCSVToExcel(sheet: Variant; rowNum: Integer; csvStr: String; allStrings: Boolean = True);
    class procedure Transpose(var A: ByteMx2D);
    class procedure RunCommand(const cmd, args: String);
end;

type TStopwatch = class(TObject)
  procedure Start();
  function Stop(): Cardinal;
private
  before: {$ifndef MSWINDOWS}TDateTime{$else}Cardinal{$endif};
end;

implementation

uses sysutils, strutils
{$ifdef MSWINDOWS}
  {$ifdef FPC},windows{$else},math,windows,shellapi{$endif}
{$else}
  ,dateutils
{$endif};

const PATH_SEP = {$ifdef MSWINDOWS}'\'{$else}'/'{$endif};

class function THelper.FilenameFromPath(const path: String): String; begin
  result := RightStr(path, Length(path) - LastDelimiter(PATH_SEP, path));
end;

class function THelper.RandomRangeIncl(lb, ub: Integer): Integer; begin
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

class function THelper.ListProjFilesInDir(const path: String): TStringList;
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

class procedure THelper.Transpose(var A: ByteMx2D);
var i, j, tmp: Integer;
begin
  Assert(High(A)=High(A[0]), 'Matrix must be quadratic!');
  for j := 0 to High(A[0]) do
    for i := j+1 to High(A) do begin
      tmp := A[i,j];
      A[i,j] := A[j,i];
      A[j,i] := tmp;
    end;
end;

class procedure THelper.RunCommand(const cmd, args: String);
const BUF_SIZE = 512;
var argsWchar, cmdWchar: array[0..BUF_SIZE-1] of WideChar;
begin
  StringToWideChar(cmd, cmdWchar, BUF_SIZE);
  StringToWideChar(args, argsWchar, BUF_SIZE);
  {$ifdef MSWINDOWS}
  ShellExecuteW(0, 'open', cmdWchar, argsWchar, nil, 1);
  {$endif}
end;

//==============================================================================
procedure TStopwatch.Start();
begin
  {$ifndef MSWINDOWS}
  before := Now;
  {$else}
  before := GetTickCount;
  {$endif}
end;

function TStopwatch.Stop(): Cardinal;
begin
  {$ifndef MSWINDOWS}
  result := MilliSecondsBetween(Now, before);
  {$else}
  result := GetTickCount - before;
  {$endif}
end;

end.

