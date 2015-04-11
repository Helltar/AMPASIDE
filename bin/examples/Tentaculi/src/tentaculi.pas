program Tentaculi;

const
  S = 12;
  N = 8;

var
  i, j: integer;
  x, y: real;
  tx, ty: real;
  k, d: real;
  a: array [1..N] of real;
  len: real;

begin
  if GetWidth > GetHeight then
    len := GetHeight / 2 / N
  else
    len := GetWidth / 2 / N;

  Randomize;

  k := Random(360) * pi / 180;
  d := pi * 2 / S;

  repeat
    if Random(50) = 0 then
      k := Random(360) * pi / 180;

    a[1] := a[1] + Sin(k) / 10;

    for i := 2 to N do
      a[i] := a[i] + (a[i - 1] - a[i]) * 0.1;

    SetColor(0, 0, 0);
    FillRect(0, 0, GetWidth, GetHeight);

    for j := 0 to S - 1 do
    begin
      x := 0.5 * GetWidth;
      y := 0.5 * GetHeight;

      for i := 2 to N do
      begin
        SetColor(255, Trunc(255 - 255 * i / N), 255);

        tx := x + Cos(j * d + a[i]) * len;
        ty := y + Sin(j * d + a[i]) * len;

        DrawLine(Trunc(x), Trunc(y), Trunc(tx), Trunc(ty));

        x := tx;
        y := ty;
      end;
    end;

    Repaint;
    Delay(24);
  until GetKeyClicked = KE_KEY0;
end.

