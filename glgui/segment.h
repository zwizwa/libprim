#define S_A (1<<0)
#define S_B (1<<1)
#define S_C (1<<2)
#define S_D (1<<3)
#define S_E (1<<4)
#define S_F (1<<5)
#define S_G (1<<6)

#define S_DIGITS_INIT {                         \
                                                \
[0] = 0                                         \
   |S_A                                         \
|S_F   |S_B                                     \
                                                \
|S_E   |S_C                                     \
   |S_D                                         \
,                                               \
                                                \
[1] = 0                                         \
                                                \
     |S_B                                       \
                                                \
     |S_C                                       \
                                                \
,                                               \
                                                \
[2] = 0                                         \
   |S_A                                         \
      |S_B                                      \
   |S_G                                         \
|S_E                                            \
   |S_D                                         \
,                                               \
                                                \
[3] = 0                                         \
   |S_A                                         \
      |S_B                                      \
   |S_G                                         \
      |S_C                                      \
   |S_D                                         \
,                                               \
                                                \
[4] = 0                                         \
                                                \
|S_F   |S_B                                     \
   |S_G                                         \
      |S_C                                      \
                                                \
,                                               \
                                                \
[5] = 0                                         \
   |S_A                                         \
|S_F                                            \
   |S_G                                         \
      |S_C                                      \
  |S_D                                          \
,                                               \
                                                \
[6] = 0                                         \
   |S_A                                         \
|S_F                                            \
   |S_G                                         \
|S_E   |S_C                                     \
   |S_D                                         \
,                                               \
                                                \
[7] = 0                                         \
  |S_A                                          \
     |S_B                                       \
                                                \
     |S_C                                       \
                                                \
,                                               \
                                                \
[8] = 0                                         \
   |S_A                                         \
|S_F   |S_B                                     \
   |S_G                                         \
|S_E   |S_C                                     \
   |S_D                                         \
,                                               \
                                                \
[9] = 0                                         \
   |S_A                                         \
|S_F   |S_B                                     \
   |S_G                                         \
      |S_C                                      \
   |S_D                                         \
,                                               \
}




