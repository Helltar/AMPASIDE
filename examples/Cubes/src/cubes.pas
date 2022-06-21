program Cubes;

  procedure DrawCubes;
  const
    BLOCK_SIZE = 24;

  var
    r, g, b: integer;
    i, j: integer;

  begin
    for i := 0 to GetHeight do
    begin
      for j := 0 to GetWidth do
      begin
        r := Random(256);
        g := Random(256);
        b := Random(256);

        SetColor(r, g, b);
        FillRect(j, i, BLOCK_SIZE, BLOCK_SIZE);

        SetColor(255, 255, 255);
        DrawRect(j, i, BLOCK_SIZE, BLOCK_SIZE);

        Repaint;
        Delay(30);

        j := j + BLOCK_SIZE;
      end;

      i := i + BLOCK_SIZE;
    end;
  end;

begin
  DrawCubes;
end.

