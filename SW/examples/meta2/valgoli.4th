DEFER VALGOLI 

DEFER EXP 

DEFER PRIMARY 
:NONAME 
  id DUP IF 
    ci S" @" cl out 
  THEN 
  DUP NOT IF 
    num DUP IF 
      ci out 
    THEN 
  THEN 
  DUP NOT IF 
    S" (" tst DUP IF 
      EXP be 
      S" )" tst be 
    THEN 
  THEN ; IS PRIMARY 

DEFER TERM 
:NONAME 
  PRIMARY DUP IF 
    BEGIN 
      S" *" tst 
      DUP IF 
        PRIMARY be 
        S" *" cl out 
      THEN 
      DUP IF 
      THEN 
      DUP NOT IF 
        S" /" tst 
        DUP IF 
          PRIMARY be 
          S" /" cl out 
        THEN 
        DUP IF 
        THEN 
      THEN 
      DUP NOT
    UNTIL 
    set be 
  THEN ; IS TERM 

DEFER EXP1 
:NONAME 
  TERM DUP IF 
    BEGIN 
      S" +" tst DUP IF 
        TERM be 
        S" +" cl out 
      THEN 
      DUP NOT IF 
      S" -" tst DUP IF 
        TERM be 
        S" -" cl out 
      THEN 
     THEN 
     DUP NOT
   UNTIL 
   set be 
  THEN ; IS EXP1 

:NONAME 
  EXP1 DUP IF 
    S" .=" tst DUP IF 
      EXP1 be 
      S" =" cl out 
    THEN 
    DUP NOT IF 
      set DUP IF 
      THEN 
    THEN 
    be 
  THEN ; IS EXP

DEFER ASSIGNST 
:NONAME 
  EXP DUP IF 
    S" =" tst be 
    id be 
    ci S" !" cl out 
  THEN ; IS ASSIGNST 

DEFER ST 

DEFER UNTILST 

:NONAME 
  S" .UNTIL" tst DUP IF 
    S" BEGIN" cl out 
    EXP be 
    S" .DO" tst be 
    S" NOT WHILE" cl out 
    ST be 
    S" REPEAT" cl out 
  THEN ; IS UNTILST 

DEFER CONDITIONALST 
:NONAME 
  S" .IF" tst DUP IF
    EXP be 
    S" .THEN" tst be 
    S" IF" cl out 
    ST be 
    S" .ELSE" tst be 
    S" ELSE" cl out 
    ST be 
    S" THEN" cl out 
  THEN ; IS CONDITIONALST 

DEFER IOST 
:NONAME 
  S" EDIT" tst DUP IF 
    S" (" tst be 
    EXP be 
    S" ," tst be 
    S" SPACES" cl out 
    sr be 
    ci S" TYPE" cl out 
    S" )" tst be 
  THEN 
  DUP NOT IF 
    S" PRINT" tst DUP IF 
      S" CR" cl out 
    THEN 
  THEN ; IS IOST 

DEFER IDSEQ1 
:NONAME 
  id DUP IF 
    S" VARIABLE" cl ci out 
  THEN ; IS IDSEQ1 

DEFER IDSEQ 
:NONAME 
  IDSEQ1 DUP IF 
    BEGIN 
      S" ," tst DUP IF 
        IDSEQ1 be 
      THEN 
      DUP NOT
    UNTIL 
    set be 
  THEN ; IS IDSEQ 

DEFER DEC 
:NONAME 
  S" .INTEGER" tst DUP IF
    IDSEQ be
  THEN ; IS DEC 

DEFER BLOCK 
:NONAME 
  S" .BEGIN" tst DUP IF 
    DEC DUP IF 
      S" .," tst be 
    THEN 
    DUP NOT IF 
      set DUP IF 
      THEN 
    THEN 
    be 
    ST be 
    BEGIN 
      S" .," tst DUP IF 
        ST be 
      THEN 
      DUP NOT
    UNTIL 
    set be 
    S" .END" tst be 
  THEN ; IS BLOCK 

:NONAME 
  IOST DUP IF 
    THEN 
    DUP NOT IF 
    ASSIGNST DUP IF 
    THEN 
  THEN 
  DUP NOT IF 
    UNTILST DUP IF 
    THEN 
  THEN 
  DUP NOT IF 
    CONDITIONALST DUP IF 
    THEN 
  THEN 
  DUP NOT IF 
    BLOCK DUP IF 
    THEN 
  THEN ; IS ST 

:NONAME 
  S" .BEGIN" tst DUP IF 
    DEC DUP IF 
      S" .," tst be 
    THEN 
    DUP NOT IF 
      set DUP IF 
      THEN 
    THEN 
    be 
    S" : RUN" cl out 
    ST be 
    BEGIN 
      S" .," tst DUP IF 
        ST be 
      THEN 
      DUP NOT
    UNTIL 
    set be 
    S" .END" tst be 
    S" ;" cl out 
  THEN ; IS VALGOLI 

