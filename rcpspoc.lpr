program rcpspoc;

{$ifdef FPC}{$mode delphi}{$H+}{$endif}

uses {$ifndef Win32}cthreads,{$endif} main;

var main: TMain;
begin
  main := TMain.Create;
  main.Entrypoint();
  main.Free;
end.

