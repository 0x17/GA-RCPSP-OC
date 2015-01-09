program rcpspoc;

{$ifdef FPC}{$mode delphi}{$H+}{$endif}

uses {$ifndef Win32}cthreads,{$endif} main, tests;

begin
  Entrypoint();
end.

