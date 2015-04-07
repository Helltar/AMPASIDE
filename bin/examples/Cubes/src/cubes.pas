program Cubes;

  procedure DrawCubes;
  const
    BLOCK_SIZE = 24;

  var
    R, G, B: integer;
    i, j: integer;

  begin
    for i := 0 to GetHeight do
    begin
      for j := 0 to GetWidth do
      begin
        R := Random(256);
        G := Random(256);
        B := Random(256);

        SetColor(R, G, B);
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

