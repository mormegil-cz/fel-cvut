#include <stdio.h>
#include <stdlib.h>
#include "tga.h"

void *tga_load(const char *filename, unsigned short *width, unsigned short *height)
{
  FILE *f;
  char id_len, cmaptype, imgtype;
  unsigned short CMAPstart, CMAPlen;
  char CMAPstride, bpp, imgdesc;
  void *data;
  unsigned long datalen;

  *width = *height = 0;

  if ((f = fopen(filename, "rb")) == NULL) return NULL;

  /* TGA header */
  fread(f, &id_len, 1);
  fread(f, &cmaptype, 1);
  fread(f, &imgtype, 1);
  fread(f, &CMAPstart, 2);
  fread(f, &CMAPlen, 2);
  fread(f, &CMAPstride, 1);
  fseek(f, 4, SEEK_CUR);
  fread(f, width, 2);
  fread(f, height, 2);
  fread(f, bpp, 1);
  fread(f, imgdesc, 1);
  if (id_len) fseek(f, id_len, SEEK_CUR);
  if (CMAPlen) fseek(f, CMAPlen * CMAPstride / 8, SEEK_CUR);

  if (cmaptype || imgtype != 2 || imgdesc != 32 || bpp != 24) {
    fclose(f);
    return NULL;
  }

  datalen = *width * *height * bpp/8;
  if ((data = malloc(datalen)) == NULL) {
    fclose(f);
    return NULL;
  }

  if (fread(f, data, datalen) != datalen) {
    free(data);
    fclose(f);
    return NULL;
  }

  fclose(f);

  return data;
}
