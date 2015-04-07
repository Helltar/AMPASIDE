program Tank;

const
  Speed = 1;

var
  i: integer;
  Tank: array [0..3] of Image;
  Dir: integer;
  X, Y: integer;
  Key: integer;

begin
  for i := 0 to 3 do
    Tank[i] := LoadImage('/' + IntegerToString(i) + '.png');

  Dir := 0;
  X := 32;
  Y := 32;

  SetColor(0, 0, 0);
  FillRect(0, 0, GetWidth, GetHeight);

  repeat
    FillRect(X, Y, 16, 16);
    Key := GetKeyPressed;

    if Key = KE_KEY6 then
    begin
      Dir := 0;
      X := X + Speed;
    end
    else
    if Key = KE_KEY8 then
    begin
      Dir := 1;
      Y := Y + Speed;
    end
    else
    if Key = KE_KEY4 then
    begin
      Dir := 2;
      X := X - Speed;
    end
    else
    if Key = KE_KEY2 then
    begin
      Dir := 3;
      Y := Y - Speed;
    end;

    if X < 0 then
      X := 0;

    if Y < 0 then
      Y := 0;

    if X > GetWidth - 16 then
      X := GetWidth - 16;

    if Y > GetHeight - 16 then
      Y := GetHeight - 16;

    DrawImage(Tank[Dir], X, Y);

    Repaint;
    Delay(20);
  until GetKeyClicked = KE_KEY0;
end.

