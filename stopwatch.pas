unit stopwatch;

interface

uses Windows;

type TStopwatch = class(TObject)
  procedure Start();
  function Stop(): Cardinal;
private
  before: Cardinal;
end;

implementation

procedure TStopwatch.Start();
begin
  before := GetTickCount;
end;

function TStopwatch.Stop(): Cardinal;
begin
  result := GetTickCount - before;
end;

end.
