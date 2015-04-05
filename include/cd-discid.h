#ifndef CD_DISCID
#define CD_DISCID

#ifdef __cplusplus

extern "C" int read_tocentry(int fd, const int last,
                             unsigned int* discid,
                             unsigned int* result);


#else

int read_tocentry(int fd, const int last,
                  unsigned int* discid,
                  unsigned int* result);

#endif

#endif
