program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

// TODO:
// MIP-Solver (GAMS) Integration über GAMS-API für Pascal oder Rausschrebien von Textdatei (.inc)?
// Priority rules Aktivitätenliste-Generierung von F# hier rüber portieren. Über generisches SortBy f vals.

uses
  sysutils,
  main in 'main.pas',
  constants in 'constants.pas',
  gaactivitylist in 'gaactivitylist.pas',
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
  topsort in 'topsort.pas',
  gacommon in 'gacommon.pas',
  gassgsmod2 in 'gassgsmod2.pas',
  esschedule in 'esschedule.pas',
  testing in 'testing.pas';

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
