#include <string.h>
#include <ctype.h>

#include <assert.h>

#include <stdio.h>

/**************************************************************************
 *
 *    strtrim
 *    -------
 *
 *  Odreze bile znaky (tak, jak jsou definovany v isspace, tj. mezera,
 * tabulator, navrat voziku, nova radka, vertikalni tabulator, konec stranky)
 * z obou koncu retezce s. Vraci opet ukazatel na cilovy retezec (tj. vraci
 * s), takze lze funkci pouzit ve vyrazech.
 *
 **************************************************************************/
char *strtrim(char *s)
{
  char *newstart = s;
  char *newend;

  /* Nalezni prvni ne-whitespace znak ve str */
  while (*newstart && isspace(*newstart)) newstart++;
  newend = newstart;

  /* Nalezni puvodni konec retezce */
  while (*newend) newend++;

  /* Nalezni posledni ne-whitespace znak ve str */
  if (newend>s) {			/* newend==s ==> prazdny retezec */
    newend--;				/* newend -> posledni znak ve str */
    while (newend>s && isspace(*newend)) newend--;
  }

  /* Ukonci retezec hned po poslednim ne-whitespace znaku (trim zprava) */
  assert(newend>s || (newend==s && *newend=='\0'));
  if (newend>s)
	*(newend+1) = '\0';

  /* Zkopiruj retezec dolu (trim zleva) - pokud je to zapotrebi */
  if (newstart != s)
	strcpy(s, newstart);

  return s;
}

/* ------ testovaci programek ------ */

#ifdef __BORLANDC__
/* Pouze jediny z testx je pouzit, tak vypneme Borland C++ varovani
	"'testx' is assigned a value that is never used": */
#pragma warn -aus
#endif
void main(void) {
  char *test1 = strdup("  \t  \nTest programu\tOK... ...\n\t\n    \n ");
  char *test2 = strdup("");
  char *test3 = strdup("   ");
  char *test4 = strdup("abcd");

  /* zde mozno nastavit ruzne testovaci retezce */
  char *test = test1;

  strtrim(test);

  printf("'%s'\n",test);
}