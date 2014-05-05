program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  sysutils,
  main in 'main.pas',
  constants in 'constants.pas',
  gaactivitylist in 'gaactivitylist.pas',
  gab in 'gab.pas',
  gaovercapacity in 'gaovercapacity.pas',
  gassgs in 'gassgs.pas',
  gassgsmod in 'gassgsmod.pas',
  gassgsoc in 'gassgsoc.pas',
  helpers in 'helpers.pas',
  operators in 'operators.pas',
  parallelfitness in 'parallelfitness.pas',
  printing in 'printing.pas',
  profit in 'profit.pas',
  projectdata in 'projectdata.pas',
  ssgs in 'ssgs.pas',
  ssgsmod in 'ssgsmod.pas',
  ssgsoc in 'ssgsoc.pas',
  stopwatch in 'stopwatch.pas',
  tests in 'tests.pas',
  topsort in 'topsort.pas',
  gacommon in 'gacommon.pas';

begin
  try
    Entrypoint();
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
