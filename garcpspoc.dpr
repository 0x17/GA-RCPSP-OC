program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

// TODO:
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
  stsvis in 'stsvis.pas',
  gams in 'gams.pas',
  resprofiles in 'resprofiles.pas',
  gamsxdcpdef in 'gamsapi\gamsxdcpdef.pas',
  gamsxdocpdef in 'gamsapi\gamsxdocpdef.pas',
  gamsxdopdef in 'gamsapi\gamsxdopdef.pas',
  gamsxdpdef in 'gamsapi\gamsxdpdef.pas',
  gdxdcpdef in 'gamsapi\gdxdcpdef.pas',
  gdxdocpdef in 'gamsapi\gdxdocpdef.pas',
  gdxdopdef in 'gamsapi\gdxdopdef.pas',
  gdxdpdef in 'gamsapi\gdxdpdef.pas',
  gmsgen in 'gamsapi\gmsgen.pas',
  gmsspecs in 'gamsapi\gmsspecs.pas',
  gopdopdef in 'gamsapi\gopdopdef.pas',
  gxdefs in 'gamsapi\gxdefs.pas',
  optdcon in 'gamsapi\optdcon.pas',
  optdcpdef in 'gamsapi\optdcpdef.pas',
  optdocpdef in 'gamsapi\optdocpdef.pas',
  optdopdef in 'gamsapi\optdopdef.pas',
  optdpdef in 'gamsapi\optdpdef.pas';

begin
  try
    Entrypoint();
  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
