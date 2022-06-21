program Space;

type
  TStar = record
    x, y, z: integer;
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
    sx := Trunc(Scr_W / 2 + Stars[i].x * 200 / (Stars[i].z + 200));
    sy := Trunc(Scr_H / 2 - Stars[i].y * 200 / (Stars[i].z + 200));
    SetColor(c, c, c);
    Plot(sx, sy);
  end;

begin
  Scr_W := GetWidth;
  Scr_H := GetHeight;

  Randomize;

  for i := 1 to MAX_STARS do
  begin
    Stars[i].x := Random(Scr_W * 4) - Scr_W * 2;
    Stars[i].y := Random(Scr_H * 4) - Scr_H * 2;
    Stars[i].z := Random(1900);
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
      Stars[i].z := Stars[i].z - SPEED * Dt / 1000;

      if Stars[i].z <= -200 then
      begin
        Stars[i].x := Random(Scr_W * 4) - Scr_W * 2;
        Stars[i].y := Random(Scr_H * 4) - Scr_H * 2;
        Stars[i].z := 1900;
      end;

      SetPix(trunc(255 - 255 * (Stars[i].z + 200) / 2100));
    end;

    Repaint;
  until GetKeyClicked = KE_KEY0;
end.

