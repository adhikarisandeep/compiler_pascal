VAR X;
FUNCTION HANOI (VAL A,B,C,D);

BEGIN
  IF (A=1) THEN
    WRITELN(B,C)
  ELSE 
  BEGIN
    HANOI := CALL HANOI((A-1),B,D,C);
    HANOI := CALL HANOI(1,B,C,D);
    HANOI := CALL HANOI((A-1),D,C,B);
  END;

END;

BEGIN
  X := CALL HANOI(4,1,3,2);
END.