program garcpspoc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  sysutils,
  main in 'src\main.pas',
  globals in 'src\globals.pas',
  helpers in 'src\helpers.pas',
  peakcrossover in 'src\Methods\peakcrossover.pas',
  profit in 'src\profit.pas',
  projectdata in 'src\projectdata.pas',
  bbrsm in 'src\Sampling\bbrsm.pas',
  algenerator in 'src\Sampling\algenerator.pas',
  naive in 'src\Sampling\naive.pas',
  rbbrsm in 'src\Sampling\rbbrsm.pas',
  ssgs in 'src\SGS\ssgs.pas',
  ssgsmod in 'src\SGS\ssgsmod.pas',
  ssgsoc in 'src\SGS\ssgsoc.pas',
  gassgsoc in 'src\GeneticAlgorithms\gassgsoc.pas',
  gassgsbeta in 'src\GeneticAlgorithms\gassgsbeta.pas',
  topsort in 'src\topsort.pas',
  esschedule in 'src\SGS\esschedule.pas',
  visualizer in 'src\visualizer.pas',
  individual in 'src\GeneticAlgorithms\individual.pas',
  gassgsz in 'src\GeneticAlgorithms\gassgsz.pas',
  gassgstau in 'src\GeneticAlgorithms\gassgstau.pas',
  gassgszt in 'src\GeneticAlgorithms\gassgszt.pas',
  fbi in 'src\Methods\fbi.pas',
  tests in 'src\tests.pas',
  compfitness in 'src\GeneticAlgorithms\compfitness.pas',
  branchandbound in 'src\Exact\branchandbound.pas',
  evaluation in 'src\evaluation.pas';

var main: TMain;
begin
  try
    main := TMain.Create;
    main.Entrypoint;
    main.Free;
  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
