program Tank;

const
  Speed = 1;

var
  Tank: array [0..3] of Image;
  Dir: integer;
  x, y: integer;
  i: integer;
  Key: integer;

begin
  for i := 0 to 3 do
    Tank[i] := LoadImage('/' + IntegerToString(i) + '.png');

  Dir := 0;
  x := 32;
  y := 32;

  SetColor(0, 0, 0);
  FillRect(0, 0, GetWidth, GetHeight);

  repeat
    FillRect(x, y, 16, 16);
    Key := GetKeyPressed;

    if Key = KE_KEY6 then
    begin
      Dir := 0;
      x := x + Speed;
    end
    else
    if Key = KE_KEY8 then
    begin
      Dir := 1;
      y := y + Speed;
    end
    else
    if Key = KE_KEY4 then
    begin
      Dir := 2;
      x := x - Speed;
    end
    else
    if Key = KE_KEY2 then
    begin
      Dir := 3;
      y := y - Speed;
    end;

    if x < 0 then
      x := 0;

    if y < 0 then
      y := 0;

    if x > GetWidth - 16 then
      x := GetWidth - 16;

    if y > GetHeight - 16 then
      y := GetHeight - 16;

    DrawImage(Tank[Dir], x, y);

    Repaint;
    Delay(20);
  until GetKeyClicked = KE_KEY0;
end.

