unit helpers;

interface

uses classes, sysutils, math, strutils, types;

type THelper = class
  class function FilenameFromPath(path: String): String;
  class function RandomRangeIncl(lb, ub: Integer): Integer;
  class procedure SkipChar(var fp: TextFile; c: Char; n: Integer);
  class function ListProjFilesInDir(path: String): TStringList;
end;

type TSortHelper<KeyType> = class
  class procedure QuickSortKeys(var keys: array of KeyType; var fvals: array of Double;  iLo, iHi: Integer);
end;

implementation

class function THelper.FilenameFromPath(path: String): String;
var
  parts: TStringDynArray;
begin
  parts := SplitString(path, '/\');
  result := parts[Length(parts)-1];
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
      result.Add(path+'\'+sr.Name);
    until FindNext(sr) <> 0;
  end;
  SetCurrentDir(oldwd);
end;

//==============================================================================
class procedure TSortHelper<KeyType>.QuickSortKeys(var keys: array of KeyType; var fvals: array of Double;  iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid, T: Double;
  T2: KeyType;
begin
  Lo := iLo;
  Hi := iHi;
  Mid := fvals[(Lo + Hi) div 2];
  repeat
    while fvals[Lo] < Mid do Inc(Lo);
    while fvals[Hi] > Mid do Dec(Hi);
    if Lo <= Hi then
    begin
      T := fvals[Lo];
      fvals[Lo] := fvals[Hi];
      fvals[Hi] := T;

      T2 := keys[Lo];
      keys[Lo] := keys[Hi];
      keys[Hi] := T2;

      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then QuickSortKeys(keys, fvals, iLo, Hi);
  if Lo < iHi then QuickSortKeys(keys, fvals, Lo, iHi);
end;

end.

