#ifndef CD_DISCID
#define CD_DISCID

int read_tocentry(int fd, const int last,
                  unsigned int* discid,
                  unsigned int** result);

#endif
