unit stopwatch;

interface

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
