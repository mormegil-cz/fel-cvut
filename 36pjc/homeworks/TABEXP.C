/************************************************************************
 3. domaci ukol z PJC (varianta d)

 Konverze tabulatoru na mezery

 Zadani lehce upraveno: Parametrem neni jedine jmeno souboru. (Kam by se
	psal vystup?) Misto toho tento program pomerne komplexne zpracuje
	vstupni radku, pokud mu nejsou zadana jmena souboru, pracuje jako
	filtr (stdin->stdout).

 ************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* implicitni delka tabulatoru */
#define DEFAULT_TABLEN 8

/* navratove hodnoty */
#define EXITCODE_FAILURE 1
#define EXITCODE_SUCCESS 0

/* pomocne makro pro chybovy vystup (hlaseni chyb) */
#define eprintf(msg) fprintf(stderr, msg)

unsigned tablen=DEFAULT_TABLEN;		/* delka tabulatoru */

void error_usage(char *msg)
{
  eprintf(msg);
  eprintf("Program slouzi pro nahrazeni tabulatoru mezerami.\n"
	  "Pouziti:TABEXP [-t delka] [vstup] [vystup]\n"
	  "\tvstup\tJmeno vstupniho souboru\n"
	  "\tvstup\tJmeno vystupniho souboru\n"
	  "\tdelka\tDelka tabulatoru ve znacich (implicitne 8)\n");

  exit(EXITCODE_FAILURE);
}

/* zpracovani prikazove radky */
void process_params(int argc, char *argv[])
{
  register int i;
  char *error_ptr;
  char fnames = 0;

  for (i=1; i<argc; i++) {
    if (!stricmp(argv[i],"-t")) {
	    i++;
	    if (i>=argc) error_usage("Chybi delka tabulatoru\n\n");
	    /* prevod parametru na cislo */
	    tablen = strtoul(argv[i], &error_ptr, 10);
	    /* v poradku? */
	    if (*error_ptr || !tablen) error_usage("Chybny parametr (ocekavano kladne cislo)\n\n");
    } else
     if (!stricmp(argv[i],"-h") || !stricmp(argv[i],"-?")) error_usage("");
     else
	/* jedna se o jmeno souboru, nikoliv option */
	switch (fnames) {
	  case 0: if (freopen(argv[i], "r", stdin) == NULL) {
		    perror("Chyba otevirani vstupniho souboru");
		    exit(EXITCODE_FAILURE);
		  }
		  fnames++;
		  break;
	  case 1: if (freopen(argv[i], "w", stdout) == NULL) {
		    perror("Chyba otevirani vystupniho souboru");
		    exit(EXITCODE_FAILURE);
		  }
		  fnames++;
		  break;
	  default: error_usage("Prilis mnoho parametru\n\n");
	}
  }
}

int main(int argc, char *argv[])
{
  int c;
  unsigned linepos = 0;		/* pozice na radce modulo velikost tabulatoru */

  process_params(argc, argv);

  while (( c = getchar() ) != EOF) {

    if (c == '\t') {

      /* expanze tabulatoru */
      register unsigned i;
      unsigned thistab = tablen - linepos; /* delka tohoto tabulatoru */

      for (i = thistab; i > 0; i--)
	  if (putchar(' ') == EOF) {
	    perror("Chyba zapisu");
	    return EXITCODE_FAILURE;
	  }

      linepos += thistab;

    } else {

      if (putchar(c) == EOF) {
	perror("Chyba zapisu");
	return EXITCODE_FAILURE;
      }

      if (c == '\n') linepos = 0;
		else linepos++;

    }

    linepos %= tablen;
  }

  if (!feof(stdin)) {
    perror("Chyba cteni");
    return EXITCODE_FAILURE;
  }

  return EXITCODE_SUCCESS;
}