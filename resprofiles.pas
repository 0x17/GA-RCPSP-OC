unit resprofiles;

interface

uses projectdata, globals;

type TResProfiles = class
  class procedure ZeroOC(var z: ResourceProfile);
  class procedure MaxOC(var z: ResourceProfile);
end;

implementation

class procedure TResProfiles.ZeroOC(var z: ResourceProfile);
var r, t: Integer;
begin
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
          z[r,t] := 0;
end;

class procedure TResProfiles.MaxOC(var z: ResourceProfile);
var r, t: Integer;
begin
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
          z[r,t] := ps.zmax[r];
end;

end.
