program Space;

type
  TStar = record
    X, Y, Z: integer;
  end;

const
  MAX_STARS = 280;
  SPEED = 1000;

var
  Stars: array [1..MAX_STARS] of TStar;
  Time, Dt: integer;
  Scr_W: integer;
  Scr_H: integer;
  i: integer;

  procedure SetPix(c: integer);
  var
    sx, sy: integer;

  begin
    sx := Trunc(Scr_W / 2 + Stars[i].X * 200 / (Stars[i].Z + 200));
    sy := Trunc(Scr_H / 2 - Stars[i].Y * 200 / (Stars[i].Z + 200));
    SetColor(c, c, c);
    Plot(sx, sy);
  end;

begin
  Scr_W := GetWidth;
  Scr_H := GetHeight;

  Randomize;

  for i := 1 to MAX_STARS do
  begin
    Stars[i].X := Random(Scr_W * 4) - Scr_W * 2;
    Stars[i].Y := Random(Scr_H * 4) - Scr_H * 2;
    Stars[i].Z := Random(1900);
  end;

  SetColor(0, 0, 0);
  FillRect(0, 0, Scr_W, Scr_H);

  Time := GetRelativeTimeMs;

  repeat
    Dt := GetRelativeTimeMs - Time;
    Time := GetRelativeTimeMs;

    for i := 1 to MAX_STARS do
    begin
      SetPix(0);
      Stars[i].Z := Stars[i].Z - SPEED * Dt / 1000;

      if Stars[i].Z <= -200 then
      begin
        Stars[i].X := Random(Scr_W * 4) - Scr_W * 2;
        Stars[i].Y := Random(Scr_H * 4) - Scr_H * 2;
        Stars[i].Z := 1900;
      end;

      SetPix(trunc(255 - 255 * (Stars[i].Z + 200) / 2100));
    end;

    Repaint;
  until GetKeyClicked = KE_KEY0;
end.

