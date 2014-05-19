unit constants;

interface

const USE_EXCEL = True;
      POP_SIZE = 80;
      NUM_GENS = 100;
      PROB_MUTATE = 5;

type TPop<T> = Array[0..POP_SIZE*2-1] of T;
type TInitProc<T> = procedure(var population: TPop<T>);
type TCrossoverProc<T> = procedure(const mother, father: T; var daughter, son: T);
type TFitnessFunc<T> = function(const individual: T): Double;
type TMutateProc<T> = procedure(var individual: T);

implementation

end.

