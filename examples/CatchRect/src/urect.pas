// Author:  helltar
// Created: 21.02.2015 03:32:38

unit uRect;

interface

{ public declarations }

procedure DrawBox(Speed: integer);
procedure DrawBomb(Speed: integer);
procedure DrawBonus(Speed: integer);

var
  Life, Score: integer;

implementation

{ add unit functions & procedures here }

uses
  uCore,
  uPlatfrm,
  Sensor;

var
  X, Y: array[0..2] of integer;
  CB_SIZE: integer;

function IsRectHite(Index: integer): boolean;
begin
  if (Y[Index] + CB_SIZE >= Platfrm.Y) and
    (X[Index] + CB_SIZE >= Platfrm.X) and
    (X[Index] <= Platfrm.X + 128) then
    Result := true;
end;

procedure SetX(Index: integer);
begin
  Randomize;

  X[Index] := Random(SW);

  if X[Index] > (SW - CB_SIZE) then
    X[Index] := X[Index] - CB_SIZE;
end;

procedure MoveCube(X, Y, R, G, B: integer);
begin
  SetColor(R, G, B);
  FillRect(X, Y, CB_SIZE, CB_SIZE);
end;

procedure DrawBox(Speed: integer);
begin
  Y[0] := Y[0] + Speed;

  MoveCube(X[0], Y[0], 210, 210, 210);

  if IsRectHite(0) then
  begin
    Score := Score + 10;
    Y[0] := -50 - Random(100);
  end;

  if Y[0] >= SH then
  begin
    Life := Life - 1;

    SetX(0);
    Y[0] := -50 - Random(100);
  end;
end;

procedure DrawBomb(Speed: integer);
begin
  Y[1] := Y[1] + Speed;

  MoveCube(X[1], Y[1], 210, 0, 0);

  if IsRectHite(1) then
  begin
    Life := Life - 1;
    Score := Score - 10;
    Y[1] := -50 - Random(100);
  end;

  if Y[1] >= SH then
  begin
    SetX(1);
    Y[1] := -50 - Random(100);
  end;
end;

procedure DrawBonus(Speed: integer);
begin
  Y[2] := Y[2] + Speed;

  MoveCube(X[2], Y[2], 0, 210, 0);

  if IsRectHite(2) then
  begin
    Score := Score + 50;
    Y[2] := -4000 - Random(1000);
  end;

  if Y[2] >= SH then
  begin
    SetX(2);
    Y[2] := -4000 - Random(1000);
  end;
end;

procedure Init;
var
  i: integer;

begin
  for i := 0 to 2 do
    X[i] := Random(SW) - CB_SIZE;

  Y[0] := -50 - Random(100);
  Y[1] := -50 - Random(100);
  Y[2] := -4000 - Random(1000);

  Life := 3;
end;

initialization

  CB_SIZE := 16;

  if isAndroid then CB_SIZE := 32;

  Init;

end.

