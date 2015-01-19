program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  sysutils,
  main in 'src/main.pas',
  globals in 'src/globals.pas',
  helpers in 'src/helpers.pas',
  profit in 'src/profit.pas',
  projectdata in 'src/projectdata.pas',
  ssgs in 'src/SGS/ssgs.pas',
  ssgsmod in 'src/SGS/ssgsmod.pas',
  ssgsoc in 'src/SGS/ssgsoc.pas',
  gassgsoc in 'src/GeneticAlgorithms/gassgsoc.pas',
  gassgsbeta in 'src/GeneticAlgorithms/gassgsbeta.pas',
  topsort in 'src/topsort.pas',
  esschedule in 'src/SGS/esschedule.pas',
  visualizer in 'src/visualizer.pas',
  resprofiles in 'src/resprofiles.pas',
  individual in 'src/individual.pas',
  gassgsz in 'src/GeneticAlgorithms/gassgsz.pas',
  gassgstau in 'src/GeneticAlgorithms/gassgstau.pas',
  gassgszt in 'src/GeneticAlgorithms/gassgszt.pas',
  fbi in 'src/fbi.pas',
  algens in 'src/algens.pas',
  tests in 'src/tests.pas';

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
