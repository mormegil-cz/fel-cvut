#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "stack.h"

#undef TRACE_ENABLE
#include "utils.h"

#ifndef __WIN32__
#include <sys/resource.h>
#endif

/* ========================================================================== */

typedef long uzel_id;
#define INVALID ((uzel_id) -1)

typedef struct {                /* uzel a cesta do nej */
    uzel_id *cesta;
    long    delka_cesty;
} cesta_t, *cesta_p;

long N;                         /* pocet uzlu v grafu */
char *matice;                   /* matice sousednosti grafu, _MUSI_ mit na diagonale nuly! */
char *navstivene;               /* indikuje, ktere uzly byly navstiveny */

long nejdelsi_delka;            /* delka nejdelsi kruznice */
cesta_p nejdelsi = NULL;        /* nejdelsi kruznice */

/* ========================================================================== */

/* Alokuje novou cestu o delce delka (tzn. s delka + 1 uzly v cesta) */
cesta_p nova_cesta(long delka)
{
  cesta_p res = (cesta_p) mallocate(sizeof(cesta_t));
  assert(res != NULL);
  res->cesta = (uzel_id *) mallocate(sizeof(uzel_id) * (delka + 1));
  assert(res->cesta != NULL);
  res->delka_cesty = delka;

  return res;
}

/* Dealokuje cestu */
void uvolni_cestu(cesta_p cesta)
{
  mfree(cesta->cesta);
  mfree(cesta);
}

/* Najde prvni dosud nenavstiveny uzel, vraci INVALID pokud byly vsechny uzly navstiveny */
uzel_id prvni_nenavstiveny(void)
{
  long i;
  char *p;

  for (i = 0, p = navstivene; i < N; i++, p++)
    if (*p == 0) return i;

  return INVALID;
}

/* ========================================================================== */

/* Hlavni funkce - najde nejdelsi kruznici, vraci jeji delku (kterou ulozi i do
   nejdelsi_delka), kruznice je ulozena v nejdelsi. */
long najdi_nejdelsi(void) {
  uzel_id zacatek;

  init_stack();

  nejdelsi_delka = 0;
  if (nejdelsi != NULL) {
    free(nejdelsi);
    nejdelsi = NULL;
  }

  while ((zacatek = prvni_nenavstiveny()) != INVALID) {
    cesta_p zacatek_uzel = nova_cesta(0);
    zacatek_uzel->cesta[0] = zacatek;
    push(zacatek_uzel);
    navstivene[zacatek] = 1;

    TRACE1("--- Komponenta od %ld ---\n", zacatek);

    while (!stack_empty()) {
      long i;
      cesta_p aktualni;
      char *radek;
      char aktualni_nepouzit = 1;
      /*long limit;*/

      aktualni = pop();
      radek = matice + aktualni->cesta[aktualni->delka_cesty] * N * sizeof(char);

      /*limit = aktualni->delka_cesty - nejdelsi_delka;*/

      for (i = 0; i < N; i++, radek++) {
        if (*radek && (aktualni->delka_cesty == 0 || i != aktualni->cesta[aktualni->delka_cesty-1])) {
           long j;
           uzel_id *n;
           cesta_p dalsi;

           #ifdef TRACE_ENABLE
           for (j = 0, n = aktualni->cesta; j <= aktualni->delka_cesty; j++, n++)
             TRACE1("%ld ", *n);
           TRACE1("%ld\n", i);
           #endif

           navstivene[i] = 1;

           for (j = 0, n = aktualni->cesta; j < aktualni->delka_cesty - 1; j++, n++) {
             if (*n == i) {
               /* nasel jsem kruznici */
               long delka = aktualni->delka_cesty - j + 1;
               TRACE1("Nalezena kruznice delky %ld\n", delka);
               if (delka > nejdelsi_delka) {
                 if (nejdelsi != NULL && nejdelsi != aktualni) uvolni_cestu(nejdelsi);
                 nejdelsi = aktualni;
                 nejdelsi_delka = delka;
                 aktualni_nepouzit = 0;
                 TRACE0("...a je nejdelsi!\n");
                 if (delka == N) {
                   /* nasel jsme maximalni kruznici -- hotovo */

                   /* uvolni cely zbytek zasobniku */
                   while (!stack_empty())
                     uvolni_cestu(pop());

                   TRACE0("...presneji receno, dokonce Hamiltonovska!\n");
                   return delka;
                 }
                 /*limit = aktualni->delka_cesty - nejdelsi_delka;*/
               }

               goto dalsi_naslednik;
             }
           }

           dalsi = nova_cesta(aktualni->delka_cesty + 1);
           memcpy(dalsi->cesta, aktualni->cesta, dalsi->delka_cesty * sizeof(uzel_id));
           dalsi->cesta[dalsi->delka_cesty] = i;
           push(dalsi);

           dalsi_naslednik:;
        }
      }
      if (aktualni_nepouzit)
        uvolni_cestu(aktualni);
    }
  }
  return nejdelsi_delka;
}

/* ========================================================================== */

int main(int argc, char *argv[])
{
  FILE *f;
#ifndef __WIN32__
  struct rlimit rlp = { 0, 0 };
#endif
  time_t start;

  if (argc != 2) {
    fprintf(stderr, "Pouziti: %s graf\n", argv[0]);
    return 2;
  }

  if ((f = fopen(argv[1], "rb")) == NULL) {
     perror("Graf nenalezen");
     return 1;
  }

  if (fread(&N, sizeof(N), 1, f) != 1) {
    perror("Spatny soubor");
    fclose(f);
    return 1;
  }

#ifndef __WIN32__
  if (N & 0xFF000000)
    N = ((N & 0xFF) << 24) | ((N & 0xFF00) << 8) | ((N & 0xFF0000) >> 8) | ((N & 0xFF000000) >> 24);
#endif


  matice = (char *) mallocate(N * N * sizeof(char));
  navstivene = callocate(N, sizeof(char));
  assert(matice != NULL && navstivene != NULL);

  if (fread(matice, sizeof(char), N * N, f) != (size_t)(N * N)) {
    perror("Spatny soubor");
    fclose(f);
    return 1;
  }

  fclose(f);

#ifndef __WIN32__
  /* zakaz tvorbu core */
  if (setrlimit(RLIMIT_CORE, &rlp)) {
    perror("Nelze nastavit core limit\n");
    return 1;
  }
#endif

  time(&start);

  if (najdi_nejdelsi() == 0) {
    start = time(NULL) - start;
    printf("Graf je acyklicky.\n");
  } else {
    long i;
    uzel_id *p;
    start = time(NULL) - start;

    printf("Nejdelsi nalezena kruznice ma delku %ld a vypada nasledovne: ", nejdelsi_delka);
    for (i = 0, p = nejdelsi->cesta; i < nejdelsi_delka; i++, p++)
      printf("%ld ", *p);
    putchar('\n');
  }
  printf("Cas: \%lu s\n", start);

  free_stack();

  return 0;
}

