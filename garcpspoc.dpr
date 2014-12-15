program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

// TODO:
// Priority rules Aktivitätenliste-Generierung von F# hier rüber portieren. Über generisches SortBy f vals.

uses
  sysutils,
  main in 'main.pas',
  globals in 'globals.pas',
  helpers in 'helpers.pas',
  profit in 'profit.pas',
  projectdata in 'projectdata.pas',
  ssgs in 'ssgs.pas',
  ssgsmod in 'ssgsmod.pas',
  ssgsoc in 'ssgsoc.pas',
  gassgsoc in 'gassgsoc.pas',
  gassgsbeta in 'gassgsbeta.pas',
  topsort in 'topsort.pas',
  esschedule in 'esschedule.pas',
  schedulevisualizer in 'schedulevisualizer.pas',
  resprofiles in 'resprofiles.pas',
  individual in 'individual.pas',
  gassgsz in 'gassgsz.pas',
  gassgstau in 'gassgstau.pas',
  priorityrules in 'priorityrules.pas',
  gassgszt in 'gassgszt.pas';

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
