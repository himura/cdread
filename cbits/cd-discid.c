include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include <linux/cdrom.h>
#define cdte_track_address      cdte_addr.lba

int cddb_sum(int n)
{
  /* a number like 2344 becomes 2+3+4+4 (13) */
  int ret = 0;

  while (n > 0) {
    ret = ret + (n % 10);
    n = n / 10;
  }

  return ret;
}

int read_tocentry(int fd, const int last,
                  unsigned int* discid,
                  unsigned int* result)
{
  const size_t len = (last + 1) * sizeof(struct cdrom_tocentry);
  struct cdrom_tocentry* toc_entries = (struct cdrom_tocentry*) malloc(len);
  int i = 0;

  for (i = 0; i < last; i++) {
    toc_entries[i].cdte_track = i + 1;
    toc_entries[i].cdte_format = CDROM_LBA;
    if (ioctl(fd, CDROMREADTOCENTRY, &toc_entries[i]) < 0) {
      free(toc_entries);
      return -1;
    }
  }

  toc_entries[last].cdte_track = CDROM_LEADOUT;
  toc_entries[last].cdte_format = CDROM_LBA;
  if (ioctl(fd, CDROMREADTOCENTRY, &toc_entries[last]) < 0) {
    free(toc_entries);
    return -1;
  }

  long int cksum = 0;
  for (i = 0; i < last; i++) {
    cksum += cddb_sum((toc_entries[i].cdte_track_address + CD_MSF_OFFSET) / CD_FRAMES);
  }

  int totaltime = ((toc_entries[last].cdte_track_address + CD_MSF_OFFSET) / CD_FRAMES) -
    ((toc_entries[0].cdte_track_address + CD_MSF_OFFSET) / CD_FRAMES);

  *discid = (cksum % 0xff) << 24 | totaltime << 8 | last;

  for (i = 0; i < last; i++) {
    result[i] = toc_entries[i].cdte_track_address + CD_MSF_OFFSET;
  }
  result[last] = (toc_entries[last].cdte_track_address + CD_MSF_OFFSET) / CD_FRAMES;

  free(toc_entries);

  return last;
}
