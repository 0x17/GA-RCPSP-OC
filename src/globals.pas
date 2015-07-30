unit globals;

interface

uses projectdata;

const POP_SIZE = 80;
      PROB_MUTATE = 5;
      NLIMITS = 8;
      TIME_LIMITS: Array[0..NLIMITS-1] of Double = (0.01, 0.1, 0.5, 1, 2, 5, 10, 15);
      SINGLE_RESULT = True;

var ps: ProjData;
    g_upperTimeLimitIndex: Integer;

implementation

end.

