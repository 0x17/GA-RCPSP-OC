unit evaluation;

interface

type
  TResultTable = Array of Array of Double;
  TStrArr = Array of String;
  TEvaluator = class
    class procedure EvalResultsToTeX(const names: TStrArr; const profits, solvetimes: TResultTable; const outFname: String);
  end;

implementation

uses math, strutils, sysutils;

class procedure TEvaluator.EvalResultsToTeX(const names: TStrArr; const profits, solvetimes: TResultTable; const outFname: String);
const
  NCHARACTERISTICS = 6;
  characteristics: Array[0..NCHARACTERISTICS-1] of String = ('$\varnothing$ deviation', 'max. deviation', 'vark(deviation)', 'optimal', '\# best', '$\varnothing$ solvetime');
var
  i, j, numHeurs: Integer;
  val, columnFormat, contents: String;

  function Gap(heurProfit, optProfit: Double): Double;
  begin
    result := (optProfit - heurProfit) / optProfit;
  end;

  function GetAvgDeviation(heurIndex: Integer): Double;
  var k: Integer;
  begin
    result := 0.0;
    for k := 0 to High(profits) do
      result := result + Gap(profits[k, heurIndex+1], profits[k, 0]);
    result := result / Length(profits);
  end;

  function GetMaxDeviation(heurIndex: Integer): Double;
  var k: Integer;
  begin
    result := 0.0;
    for k := 0 to High(profits) do
      if Gap(profits[k, heurIndex+1], profits[k,0]) > result then
        result := Gap(profits[k, heurIndex+1], profits[k,0]);
  end;

  function GetStdDev(heurIndex: Integer): Double;
  var
     k: Integer;
     avg: Double;
  begin
    result := 0.0;
    avg := GetAvgDeviation(heurIndex);
    for k := 0 to High(profits) do
      result := result + Power(Gap(profits[k, heurIndex+1], profits[k, 0]) - avg, 2);
    result := Sqrt(result / (Length(profits) - 1));
  end;

  function GetVarCoeffDev(heurIndex: Integer): Double;
  begin
    result := GetStdDev(heurIndex) / GetAvgDeviation(heurIndex);
  end;

  function GetPercOptimal(heurIndex: Integer): Double;
  var k: Integer;
  begin
    result := 0.0;
    for k := 0 to High(profits) do
      if Gap(profits[k, heurIndex+1], profits[k,0]) = 0.0 then
        result := result + 1.0;
    result := result / Length(profits);
  end;

  function GetAvgSolvetime(heurIndex: Integer): Double;
  var k: Integer;
  begin
    result := 0.0;
    for k := 0 to High(solvetimes) do
      result := result + solvetimes[k, heurIndex+1];
    result := result / Length(profits);
  end;

  function GetTimesBest(heurIndex: Integer): Integer;
  var
    k, h: Integer;
    isMax: Boolean;
  begin
    result := 0;
    for k := 0 to High(profits) do begin
      isMax := true;
      for h := 1 to High(profits[0]) do
        if profits[k,h] > profits[k, heurIndex+1] then begin
          isMax := false;
          break;
        end;
      if isMax then inc(result);
    end;
  end;

  procedure InsertContentsInSkeleton(const skeletonFn: String);
  var
    fp: TextFile;
    composed, line: String;
  begin
    AssignFile(fp, skeletonFn);
    Reset(fp);
    composed := '';
    while not eof(fp) do begin
      ReadLn(fp, line);
      if AnsiContainsStr(line, '%%CONTENTS%%') then
        line := AnsiReplaceStr(line, '%%CONTENTS%%', contents);
      composed := composed + line;
    end;
    CloseFile(fp);

    AssignFile(fp, outFname);
    Rewrite(fp);
    Write(fp, composed);
    CloseFile(fp);
  end;

  function FormatPercent(n: Double): String; inline;
  begin result := FloatToStr(RoundTo(n*100, -2)) + '\%'; end;

  function FormatDecimal(n: Double): String; inline;
  begin result := FloatToStr(RoundTo(n, -2)); end;

begin
  numHeurs := Length(names);

  columnFormat := '';
  for i := 0 to numHeurs do
    columnFormat := columnFormat + 'c';

  contents := '\begin{tabular}{'+columnFormat+'}'+#10+'\hline'+#10;

  // Write head row
  for i := 0 to numHeurs do begin
    if i = 0 then contents := contents + 'representation'
    else contents := contents + '$' + names[i-1] + '$';

    if i < numHeurs then contents := contents + '&'
    else contents := contents + '\\[3pt]'+#10+'\hline\hline'+#10;
  end;

  // Write body
  for i := 0 to NCHARACTERISTICS-1 do
    for j := 0 to numHeurs do begin
      if j = 0 then contents := contents + characteristics[i]
      else begin
        case i of
          0: val := FormatPercent(GetAvgDeviation(j-1));
          1: val := FormatPercent(GetMaxDeviation(j-1));
          2: val := FormatDecimal(GetVarCoeffDev(j-1));
          3: val := FormatPercent(GetPercOptimal(j-1));
          4: val := IntToStr(GetTimesBest(j-1));
          5: val := FormatDecimal(GetAvgSolvetime(j-1))+'s';
        end;
        contents := contents + val;
      end;

      if j < numHeurs then
        contents := contents + '&'
      else
        contents := contents + '\\'+#10'\hline'+#10;

    end;

  contents := contents + '\hline'+#10+'\end{tabular}';

  InsertContentsInSkeleton('skeleton.tex');
end;

end.