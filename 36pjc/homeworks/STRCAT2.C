/***************************************************************************
 Zretezeni neomezeneho poctu retezcu (strcat2)

  Petr Kadlec & Michal Conos
 ***************************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

/* hlavni funkce: do dst ulozi zretezeni vsech retezcu, ktere byly predany
	jako parametry (seznam je ukoncen prvkem NULL) */
char *strcat2(char *dst, ...)
{
  va_list pa;
  char *arg;

  va_start(pa, dst);

  while ((arg = va_arg(pa, char *)) != NULL)
     strcat(dst, arg);

  va_end(pa);

  return dst;
}

int main(void)
{
  char dst[80] = "";
  char *src1="ahoj ", *src2="kamarade", *src3=", jak se mas";

  printf("%s\n",strcat2(dst,src1,src2,src3,NULL));

  return 0;
}