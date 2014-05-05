unit stopwatch;

// Hilfsklasse zur Zeitmessung in Millisekunden
// Genauigkeit von Systemuhr begrenzt, meist so im 10-16ms Bereich.

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
