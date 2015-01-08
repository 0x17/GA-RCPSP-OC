program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

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
  visualizer in 'visualizer.pas',
  resprofiles in 'resprofiles.pas',
  individual in 'individual.pas',
  gassgsz in 'gassgsz.pas',
  gassgstau in 'gassgstau.pas',
  gassgszt in 'gassgszt.pas',
  fbi in 'fbi.pas',
  algens in 'algens.pas',
  tests in 'tests.pas';

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
