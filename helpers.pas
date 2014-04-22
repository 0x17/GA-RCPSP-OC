unit helpers;

//{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

procedure SkipChar(var fp: TextFile; c: Char; n: Integer);
function ListProjFilesInDir(path: String): TStringList;

implementation

procedure SkipChar(var fp: TextFile; c: Char; n: Integer);
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

function ListProjFilesInDir(path: String): TStringList;
var
  sr: TSearchRec;
  oldwd: String;
begin
  result := TStringList.Create;
  oldwd := GetCurrentDir;
  SetCurrentDir(path);
  if FindFirst('*.DAT', faAnyFile, sr) = 0 then
  begin
    repeat
      result.Add(path+'\'+sr.Name);
    until FindNext(sr) <> 0;
  end;
  SetCurrentDir(oldwd);
end;



end.

