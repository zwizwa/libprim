#define A |(1<<0)
#define B |(1<<1)
#define C |(1<<2)
#define D |(1<<3)
#define E |(1<<4)
#define F |(1<<5)
#define G |(1<<6)

static char segment_digits[10] = {

[0] = 0
   A
F     B

E     C
   D
,

[1] = 0

      B

      C

,

[2] = 0
   A
      B
   G
E
   D
,

[3] = 0
   A
      B
   G
      C
   D
,

[4] = 0

F     B
   G
      C

,

[5] = 0
  A
F
  G
    C
  D
,

[6] = 0
  A
F
  G
E   C
  D
,

[7] = 0
  A
    B

    C

,

[8] = 0
   A
F     B
   G
E     C
   D
,

[9] = 0
   A
F     B
   G
      C
   D
,

};

#undef A
#undef B
#undef C
#undef D
#undef E
#undef F
#undef G


