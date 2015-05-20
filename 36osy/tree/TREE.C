/**************************************************************************
  TREE
  ~~~~

    Program, ktery v ASCII grafice zobrazi strom adresaru pod zadanym
    adresarem.
    (3.semestralni uloha z OSY, uloha c.3)

    Petr Kadlec (cviceni z OSY ve ctvrtek od 11:00)
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <dirent.h>

#define NDEBUG
#include <assert.h>

#include "platform.h"

/* -- Definice ASCII znaku pro vykresleni -- */

/* pri definovanem FULL_ASCII se vyuziva znaku horni poloviny ASCII tabulky,
   jinak pouze tech dolnich */
/*#define FULL_ASCII*/

#ifdef FULL_ASCII
#ifdef DOUBLE_FRAMES
#define C_SPACE  ' '
#define C_VERT   '\272'
#define C_BRANCH '\314'
#define C_LAST   '\310'
#define C_ENTRY  '\315'
#else
#define C_SPACE  ' '
#define C_VERT   '\263'
#define C_BRANCH '\303'
#define C_LAST   '\300'
#define C_ENTRY  '\304'
#endif
#else
#define C_SPACE  ' '
#define C_VERT   '|'
#define C_BRANCH '+'
#define C_LAST   '`'
#define C_ENTRY  '-'
#endif

/* pomocna funkce - ukonceni s chybou */
void fail(char *msg)
{
  fflush(stdout);
  perror(msg);
  exit(EXIT_FAILURE);
}

/* prave prochazeny adresar */
char current_dir[PATH_MAX] = "." PATH_STR;
char terminate_ = '\0';		/* "hack", ochranujici proti preteceni cesty */

int depth = 0;
struct dirent *ent[MAXRECURSION];

char show_hidden = 0; 	/* konfiguracni promenna: prochazet skryte adresare? */

/* funkce pro "detekci" "." a ".." */
#define no_dots(d_name) ((d_name)[0] != '.' || 				\
			 ((d_name)[1] != '\0' && 			\
			  ((d_name)[1] != '.' || (d_name)[2] != '\0') 	\
			 ) 						\
			)

char not_hidden(char *fname)	/* adresar/soubor fname neni hidden */
{
#if (defined(PLATFORM_UNIX))
  assert(fname != NULL);

  return (fname[0] != '.');
#elif (defined(PLATFORM_DOS))
  unsigned attr;

  assert(fname != NULL);

  if (_dos_getfileattr(fname, &attr)) fail("Error getting file attributes");

  return !(attr & FA_HIDDEN);
#else
  return (fname != NULL);
#endif
}

/* Pomocna funkce pro nalezeni dalsiho podadresare adresare dir */
struct dirent *find_next_subdir(DIR *dir)
{
  struct dirent *ent;
  struct stat fst;
  char this_file[PATH_MAX];

  strcpy(this_file, current_dir);

  while ((ent = readdir(dir)) != NULL) {
    /* preskoc "." a ".." */
    if (no_dots(ent->d_name)) {

      /* vytvor plne jmeno souboru */
      strcat(this_file, ent->d_name);

      /* preskoc pripadne i skryte adresare */
      if (show_hidden || not_hidden(this_file)) {
	if (get_stat(this_file, &fst)) fail("Error getting file information");

	/* nalezen adresar? */
	if (fst.st_mode & S_IFDIR) return ent;
      }

      /* Odstran posledni soubor z current_dir */
      assert(strrchr(this_file, PATH_CHAR) != NULL);
      strrchr(this_file, PATH_CHAR)[1] = '\0';

    } /* if no_dots */
  } /* while ent != NULL */

  return NULL;
}

/* Pomocna funkce pro ziskani dalsiho podadresare */
void get_next_ent(DIR *dir, struct dirent **ent)
{
  char old_name[PATH_MAX] = "";

  assert(ent != NULL);
  assert(dir != NULL);

  /* Uschovej puvodni hodnotu ent->d_name */
  if (*ent != NULL)
     strcpy(old_name, (*ent)->d_name);

  /* Priprav dalsi podadresar (nebo NULL) */
  *ent = find_next_subdir(dir);

  if (old_name[0] != '\0') {
    /* Vytvor plne jmeno adresare */
    strcat(current_dir, old_name);

    /* Pridej '/' na konec current_dir */
    strcat(current_dir, PATH_STR);
  }
}

/* Hlavni rekurzivni funkce, ktera prochazi adresarovy strom */
void recurse_directory(void)
{
  DIR *dir;

  /* zobraz "vetve" stromu pod soucasnym adresarem */
  register int i;
  for (i=0; i<depth-1; i++) {
	putchar(ent[i] != NULL ? C_VERT : C_SPACE);
	putchar(C_SPACE);
  }

  /* pokud toto neni korenovy adresar, zobraz jeho jmeno, spolu s odbockou */
  if (depth) {
    register char *slash;

    /* zobraz odbocku */
    putchar(ent[depth-1] != NULL ? C_BRANCH : C_LAST);
    putchar(C_ENTRY);

    /* odstran koncove '/' */
    slash = current_dir + strlen(current_dir) - 1;
    assert(*slash == PATH_CHAR);
    *slash = '\0';

    /* zobraz jenom posledni jmeno */
    fputs(strrchr(current_dir, PATH_CHAR)+1, stdout);

    /* obnov koncove '/' */
    *slash = PATH_CHAR;

    /* zkontroluj opravneni (poznamka - toto se pro korenovy adresar nedela,
       jelikoz se to kontroluje jiz ve funkci main()) */
    if (check_access(current_dir)) {
      /* nemame opravneni prochazet tento adresar, ukazeme to znakem '/' a skoncime s timto adresarem */
      puts(PATH_STR);
      /* printf(PATH_STR"\t(%s)\n",strerror(errno)); <-- alternativni indikace neuspechu */
      return;
    } else putchar('\n');
  }

  /* otevri adresar */
  if ((dir = opendir(current_dir)) == NULL) fail("Cannot open directory");

  /* Hlavni smycka:
	v ent[depth] je vzdy DALSI nalezeny adresar,
	v current_dir je prave zpracovavany adresar
   */
  ent[depth] = find_next_subdir(dir);
  while (ent[depth] != NULL) {
    /* Nastav do current_dir adresar z ent[depth] a do ent[depth] dej
       dalsi nalezeny (nebo NULL) */
    get_next_ent(dir, &ent[depth]);

    depth++;
    assert(depth <= MAXRECURSION);

    /* Rekurzivne vykresli podstrom */
    recurse_directory();

    /* Odstran '/' pridany na konec current_dir */
    assert(current_dir[strlen(current_dir)-1] == PATH_CHAR);
    current_dir[strlen(current_dir)-1] = '\0';

    /* Odstran posledni podadresar z current_dir */
    assert(strrchr(current_dir, PATH_CHAR) != NULL);
    strrchr(current_dir, PATH_CHAR)[1] = '\0';

    depth--;
    assert(depth <= MAXRECURSION);
  }

  /* konec prace s adresarem */
  if (closedir(dir)) fail("Error closing directory");
}

void process_args(int argc, char *argv[])
{
  int i;

  for (i=1; i<argc; i++) {
    assert(argv[i] != NULL);

    if (*(argv[i]) == OPTION_CHAR) {
      /* Parametr "option" */

      if (!stricmp(argv[i]+1, "h") || !strcmp(argv[i]+1, "?")) {
	printf("Tento program zobrazi strom adresarove struktury od daneho adresare\n"
	       "Pouziti: tree [" OPTION_STR "a] [adresar]\n"
	       "\tadresar popisuje koren zobrazovaneho stromu (pokud neni zadan,\n"
	       "\t\tzobrazuje se od aktualniho adresare)\n"
	       "\t" OPTION_STR "a Zpusobi zobrazovani i skrytych adresaru\n");
	exit(EXIT_FAILURE);
      } else if (!stricmp(argv[i]+1, "a")) {
	       show_hidden = 1;
	     } else {
	       fprintf(stderr, "Unknown option\n");
	       exit(EXIT_FAILURE);
	     }
    } else {
      /* Parametr "pocatecni adresar" */
      if (strlen(argv[i]) > PATH_MAX-1) {
	fprintf(stderr, "Too long pathname\n");
	exit(EXIT_FAILURE);
      }

      /* Toto musi byt posledni parametr */
      if (i<argc-1) {
	fprintf(stderr, "Too many arguments\n");
	exit(EXIT_FAILURE);
      }

      /* zkopiruj argv[i] do current_dir a pripadne pridej '/' */
      if (current_dir[strxfrm(current_dir, argv[i], PATH_MAX)-1] != PATH_CHAR)
	  strcat(current_dir, PATH_STR);

      /* zjisti, zda se od zadaneho adresare skutecne da prochazet */
      if (check_access(current_dir)) fail("Cannot display directory tree");
    }
  }
}

int main(int argc, char *argv[])
{
 /* Zpracuj parametry prikazove radky */
 process_args(argc, argv);

 /* a jedem! */
 recurse_directory();

 /* To je vse, pratele! */
 return EXIT_SUCCESS;
}
