unit resprofiles;

interface

uses projectdata;

type TResProfiles = class
  class procedure ZeroOC(out z: ResourceProfile);
  class procedure MaxOC(out z: ResourceProfile);
end;

implementation

uses globals;

class procedure TResProfiles.ZeroOC(out z: ResourceProfile);
var r, t: Integer;
begin
  SetLength(z, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
          z[r,t] := 0;
end;

class procedure TResProfiles.MaxOC(out z: ResourceProfile);
var r, t: Integer;
begin
  SetLength(z, ps.numRes, ps.numPeriods);
  for r := 0 to ps.numRes - 1 do
      for t := 0 to ps.numPeriods - 1 do
          z[r,t] := ps.zmax[r];
end;

end.
