unit algenerator;

interface

uses projectdata;

type
  IALGenerator = class
    procedure PickSample(out order: JobData); virtual; abstract;
  end;
  DblArray = Array of Double;

implementation

end.

