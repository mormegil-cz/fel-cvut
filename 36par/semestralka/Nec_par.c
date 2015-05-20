#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "stack.h"

#include "mpi.h"

//#undef TRACE_ENABLE
//#define TRACE_ENABLE
//#define STDOUT_REDIR

#include "utils.h"

#ifndef __WIN32__
#include <sys/resource.h>
#endif

/* ========================================================================== */

/* Minimalni objem prace, ktery se muze pridelovat */
#define MIN_PROCESS_TASK_SIZE 1

/* Pouzivane tagy */
#define TAG_CESTA_DELKA_CESTY 1
#define TAG_CESTA_CESTA       2
#define TAG_END_OF_WORK       3
#define TAG_REQUEST_WORK      4
#define TAG_HERE_YOU_ARE      5
#define TAG_NO_WORK_TO_GIVE   6
#define TAG_ROBIN             7
#define TAG_NODE_VISITED      8

/* Barvy peska/procesu */
#define ROBIN_COLOR_W         0
#define ROBIN_COLOR_B         1

typedef long uzel_id;
#define INVALID ((uzel_id) -1)

typedef struct {                /* uzel a cesta do nej */
    uzel_id *cesta;
    long    delka_cesty;
} cesta_t, *cesta_p;

long N;                         /* pocet uzlu v grafu */
char *matice;                   /* matice sousednosti grafu, _MUSI_ mit na diagonale nuly! */
char *navstivene;               /* indikuje, ktere uzly byly navstiveny -- pro zpracovani nesouvislych grafu */

long nejdelsi_delka;            /* delka nejdelsi kruznice */
cesta_p nejdelsi = NULL;        /* nejdelsi kruznice */

int my_rank;                    /* identifikator tohoto procesu */
int processes_count;            /* pocet procesu */

char moje_barva = 0;            /* barva tohoto procesu */
char nalezena_nejdelsi = 0;     /* mam se ukoncit? */
char barva_peska = 0;           /* barva mnou vlastneneho peska */
char mam_peska = 0;             /* jsem majitelem peska? */

/* ========================================================================== */

#define CHECK(result) { if (!(result)) { printf("%d: FAILED\n", my_rank); exit(99); }

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

/* Posle zaznam ze zasobniku procesu cil */
void posli_cestu(cesta_p cesta, int cil)
{
  if (cesta == NULL) {
    long nulova = 0;
    TRACE1("posli_cestu(null,%d)\n", cil);
    MPI_Send(&nulova, 1, MPI_LONG, cil, TAG_CESTA_DELKA_CESTY, MPI_COMM_WORLD);
  } else {
    TRACE2("posli_cestu(%d,%d)\n", cesta->delka_cesty,cil);
    MPI_Send(&cesta->delka_cesty, 1, MPI_LONG, cil, TAG_CESTA_DELKA_CESTY, MPI_COMM_WORLD);
    MPI_Send(cesta->cesta, 1 + cesta->delka_cesty, MPI_LONG, cil, TAG_CESTA_CESTA, MPI_COMM_WORLD);
  }
}

/* Prijme zaznam cesty od procesu zdroj */
cesta_p prijmi_cestu(int zdroj)
{
  long delka;
  MPI_Status status;
  cesta_p result;

  TRACE0("prijimam cestu...\n");
  MPI_Recv(&delka, 1, MPI_LONG, zdroj, TAG_CESTA_DELKA_CESTY, MPI_COMM_WORLD, &status);
  TRACE2("prijmi_cestu(%d) => %d\n", zdroj, delka);
  if (delka == 0) {
    return NULL;
  } else {
    result = nova_cesta(delka);
    MPI_Recv(result->cesta, 1 + delka, MPI_LONG, zdroj, TAG_CESTA_CESTA, MPI_COMM_WORLD, &status);
    return result;
  }
}

/* Odladovaci nastroj -- vypise strukturu cesta_t */
void vypis_cestu(cesta_p cesta)
{
  if (cesta == NULL) {
    TRACE0("vypis_cestu: NULL\n");
  } else {
    int i;
    TRACE1("vypis_cestu: delka = %d: ", cesta->delka_cesty);
    for (i = 0; i <= cesta->delka_cesty; i++)
      TRACE1("%d ", cesta->cesta[i]);
    TRACE0("\n");
  }
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
long najdi_nejdelsi(char inicializace) {
  if (inicializace) {
    /* ulozi na zasobnik prvni nenavstiveny uzel */
    uzel_id zacatek = prvni_nenavstiveny();
    cesta_p zacatek_uzel;
    char pokracujeme = zacatek != INVALID;

    MPI_Bcast(&pokracujeme, 1, MPI_CHAR, 0, MPI_COMM_WORLD);
    if (!pokracujeme) return -1;

    zacatek_uzel = nova_cesta(0);
    zacatek_uzel->cesta[0] = zacatek;
    init_stack();
    push(zacatek_uzel);
    navstivene[zacatek] = 1;
  }

  moje_barva = ROBIN_COLOR_W;
  mam_peska = inicializace;
  barva_peska = ROBIN_COLOR_W;

  while(1) {           /* hlavni cyklus, opusten pri prichodu ukoncovaci zpravy */
    int ot;

    while(1) {         /* cyklus pro zpracovani cekajicich zprav, opusten pri prazdnem bufferu */
      int ceka_zprava;
      MPI_Status status;
      //TRACE1("%d: Iprobe...", my_rank);
      MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &ceka_zprava, &status);
      //TRACE1("done: %d\n", ceka_zprava);
      if (!ceka_zprava) break;
      TRACE2("%d: Prichod zpravy (%d)\n", my_rank, status.MPI_TAG);
      switch (status.MPI_TAG) {
      case TAG_END_OF_WORK: {
        /* Nekdo nalezl Hamiltonovskou kruznici: koncime! */
	char buf;
        int i;
        char *p;
	MPI_Recv(&buf, 1, MPI_CHAR, status.MPI_SOURCE, TAG_END_OF_WORK, MPI_COMM_WORLD, &status);
        /* poznac vsechny jako navstivene -- stejne tam nebude delsi kruznice */
        for (i = 0, p = navstivene; i < N; i++, p++)
          *p = 1;
	nalezena_nejdelsi = 1;
	return nejdelsi_delka;
      }

      case TAG_REQUEST_WORK: {
	long buf;
	MPI_Recv(&buf, 1, MPI_CHAR, status.MPI_SOURCE, TAG_REQUEST_WORK, MPI_COMM_WORLD, &status);
	if (stack_length() > MIN_PROCESS_TASK_SIZE) {
	  cesta_p *iterator = (cesta_p*) get_stack_bottom();
	  long delka_na_dne = (*iterator)->delka_cesty;
	  long velikost_dna = 0;
	  cesta_p *i;
	  int j;

	  for (i = iterator; *i != NULL && (*i)->delka_cesty == delka_na_dne; i++) velikost_dna++;

	  velikost_dna = (velikost_dna + 1) / 2;

	  TRACE2("OK, posilam nejakou praci %d (%d uzlu)\n", status.MPI_SOURCE, velikost_dna);

	  MPI_Send(&velikost_dna, 1, MPI_LONG, status.MPI_SOURCE, TAG_HERE_YOU_ARE, MPI_COMM_WORLD);

	  for (i = iterator, j = 0; j < velikost_dna; i++, j++) {
	    cesta_p p = (cesta_p) stack_remove_bottom();
	    posli_cestu(p, status.MPI_SOURCE);
	  }

	  if (my_rank > status.MPI_SOURCE) {
            TRACE0("==> prebarvuji se na cerno\n");
	    moje_barva = ROBIN_COLOR_B;
          }
	} else {
	  TRACE1("Sorry, %d, zadnou praci nemam\n", status.MPI_SOURCE);
	  buf = -1;
	  MPI_Send(&buf, 1, MPI_CHAR, status.MPI_SOURCE, TAG_NO_WORK_TO_GIVE, MPI_COMM_WORLD);
	}
	break;
      }

      case TAG_ROBIN: {
	char prijaty_pesek;
	MPI_Status stat;
	assert(!mam_peska);
	MPI_Recv(&prijaty_pesek, 1, MPI_CHAR, status.MPI_SOURCE, TAG_ROBIN, MPI_COMM_WORLD, &stat);

	TRACE2("Prijal jsem peska %d, moje barva je %d\n", prijaty_pesek, moje_barva);

	if (my_rank == 0) {
          if (prijaty_pesek == ROBIN_COLOR_W && moje_barva == ROBIN_COLOR_W) {
            int i;
            nalezena_nejdelsi = 1;
            TRACE0("==> Budu koncit!\n");
            for (i = 1; i < processes_count; i++) {
  	      char buf;
              MPI_Send(&buf, 1, MPI_CHAR, i, TAG_END_OF_WORK, MPI_COMM_WORLD);
            }
          } else barva_peska = ROBIN_COLOR_W; /* P0 posila vzdy bileho peska */
        }

        mam_peska = 1;
        barva_peska = prijaty_pesek;

	break;
      }

      case TAG_NODE_VISITED: {
	uzel_id uzel;
	MPI_Status stat;
	MPI_Recv(&uzel, 1, MPI_LONG, status.MPI_SOURCE, TAG_NODE_VISITED, MPI_COMM_WORLD, &stat);
	navstivene[uzel] = 1;
	break;
      }

      default:
	printf("%d: Podivna zprava (%d)\n", my_rank, status.MPI_TAG);
	exit(66);
      }
    }

    while (stack_empty()) {
      /* Nemam praci. Zacnu o ni zadat, ovsem muze to take znamenat uplne ukonceni. */
      int partner;
      MPI_Status status;
      MPI_Request request;
      char odpovezeno;         /* byla mi dorucena odpoved na zadost o praci? */

      TRACE0("Nemam praci...\n");

      if (nalezena_nejdelsi) return nejdelsi_delka;

      if (mam_peska) {
	MPI_Request request;
        if (moje_barva == ROBIN_COLOR_B) barva_peska = ROBIN_COLOR_B;  /* cerny proces posila vzdy cerneho peska */
	MPI_Isend(&barva_peska, 1, MPI_CHAR, (my_rank + 1) % processes_count, TAG_ROBIN, MPI_COMM_WORLD, &request);
	TRACE1("Odesilam peska barvy %d\n", barva_peska);
        
	moje_barva = ROBIN_COLOR_W;
	mam_peska = 0;
      }

      do
	partner = rand() % processes_count;
      while (partner == my_rank);

      TRACE2("%d: Zadam o praci %d\n", my_rank, partner);

      MPI_Isend(&odpovezeno, 1, MPI_CHAR, partner, TAG_REQUEST_WORK, MPI_COMM_WORLD, &request);
      odpovezeno = 0;
      while (!odpovezeno) {
	/*MPI_Recv(&buf, 1, MPI_LONG, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);*/
	MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

	TRACE2("%d: Odpoved na zadost o praci: %d\n", my_rank, status.MPI_TAG);

	switch (status.MPI_TAG) {
	case TAG_END_OF_WORK: {
	  char buf;
	  MPI_Recv(&buf, 1, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
	  nalezena_nejdelsi = 1;
	  return nejdelsi_delka;
	}

	case TAG_REQUEST_WORK: {
	  char buf;
	  long reply;
	  MPI_Recv(&buf, 1, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
	  reply = -1;
	  MPI_Send(&reply, 1, MPI_LONG, status.MPI_SOURCE, TAG_NO_WORK_TO_GIVE, MPI_COMM_WORLD);
	  break;
	}

	case TAG_ROBIN: {
	  char prijaty_pesek;
	  MPI_Status stat;
	  assert(!mam_peska);
	  MPI_Recv(&prijaty_pesek, 1, MPI_CHAR, status.MPI_SOURCE, TAG_ROBIN, MPI_COMM_WORLD, &stat);
	  TRACE2("Prijal jsem peska barvy %d, moje barva je %d\n", prijaty_pesek, moje_barva);

	  if (my_rank == 0) {
	    if (prijaty_pesek == ROBIN_COLOR_W) {
	      int i;
	      TRACE0("==> konec prace!\n");
	      for (i = 1; i < processes_count; i++) {
		char buf;
		MPI_Send(&buf, 1, MPI_CHAR, i, TAG_END_OF_WORK, MPI_COMM_WORLD);
	      }
	      return nejdelsi_delka;
	    } else {
	      barva_peska = ROBIN_COLOR_W;
	      MPI_Isend(&barva_peska, 1, MPI_CHAR, my_rank + 1, TAG_ROBIN, MPI_COMM_WORLD, &request);
	      moje_barva = ROBIN_COLOR_W;
	      TRACE1("Odesilam peska barvy %d\n", barva_peska);
	    }
	  } else {
	    if (moje_barva == ROBIN_COLOR_B) {
	      /* cerny proces posila jenom cerne pesky */
	      barva_peska = ROBIN_COLOR_B;
	    } else {
	      barva_peska = prijaty_pesek;
	    }
	    MPI_Isend(&barva_peska, 1, MPI_CHAR, (my_rank + 1) % processes_count, TAG_ROBIN, MPI_COMM_WORLD, &request);
	    TRACE1("Odesilam peska barvy %d\n", barva_peska);
	    moje_barva = ROBIN_COLOR_W;
	  }
	  break;
	}

	case TAG_NO_WORK_TO_GIVE: {
	  long buf;
	  MPI_Recv(&buf, 1, MPI_LONG, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
	  odpovezeno = 1;
	  break;
	}

	case TAG_HERE_YOU_ARE: {
	  long i;
	  long buf;
	  MPI_Recv(&buf, 1, MPI_LONG, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
	  for (i = 0; i < buf; i++)
	    push(prijmi_cestu(partner));
	  odpovezeno = 1;
	  break;
	}

	default:
	  printf("%d: Podivna zprava pri cekani (%d)\n", my_rank, status.MPI_TAG);
	  exit(66);
	}
      }
    }

    /* HLAVNI PRACE */
    for (ot = 0; ot < 500 && !stack_empty(); ot++)
    {
      /* provede jednu praci */
      long i;
      cesta_p aktualni;
      char *radek;
      char aktualni_nepouzit = 1;

      /*TRACE1("%d: Popping\n", my_rank);*/
      aktualni = pop();
      /*TRACE2("%d: Pop: %d\n", my_rank, aktualni->delka_cesty);*/
      /*vypis_cestu(aktualni);*/
      radek = matice + aktualni->cesta[aktualni->delka_cesty] * N * sizeof(char);

      for (i = 0; i < N; i++, radek++) {                    /* prodluz cestu ke vsem sousedum */
        if (*radek && (aktualni->delka_cesty == 0 || i != aktualni->cesta[aktualni->delka_cesty-1])) {
           long j;
           uzel_id *n;
           cesta_p dalsi;

           /*#ifdef TRACE_ENABLE
           for (j = 0, n = aktualni->cesta; j <= aktualni->delka_cesty; j++, n++)
             TRACE1("%ld ", *n);
           TRACE1("%ld\n", i);
           #endif*/

	   if (!navstivene[i]) {
	     uzel_id uzel = i;
	     MPI_Request req;
	     navstivene[i] = 1;
             if (my_rank > 0)
	       MPI_Isend(&uzel, 1, MPI_LONG, 0, TAG_NODE_VISITED, MPI_COMM_WORLD, &req);
	   }

           /* zkontroluj, jestli jsme nenasli kruznici */
           for (j = 0, n = aktualni->cesta; j < aktualni->delka_cesty - 1; j++, n++) {
             if (*n == i) {
               /* nasel jsem kruznici */
               long delka = aktualni->delka_cesty - j + 1;
               TRACE2("%d: Nalezena kruznice delky %ld\n", my_rank, delka);
               if (delka > nejdelsi_delka) {
                 if (nejdelsi != NULL && nejdelsi != aktualni) uvolni_cestu(nejdelsi);
                 nejdelsi = aktualni;
                 nejdelsi_delka = delka;
                 aktualni_nepouzit = 0;
                 TRACE1("%d: ...a je nejdelsi!\n", my_rank);
                 if (delka == N) {
		   int p;
                   /* nasel jsme maximalni kruznici -- hotovo */
		   nalezena_nejdelsi = 1;

                   /* uvolni cely zbytek zasobniku */
                   while (!stack_empty())
                     uvolni_cestu(pop());

		   /* posli vsem ostatnim, aby skoncili */
                   /*TRACE0("...presneji receno, dokonce Hamiltonovska!\n");*/
		   for (p = 0; p < processes_count; p++)
		     if (p != my_rank) {
		       char buf;
		       MPI_Request request;
		       MPI_Isend(&buf, 1, MPI_CHAR, p, TAG_END_OF_WORK, MPI_COMM_WORLD, &request);
		     }
                   return delka;
                 }
               }

               goto dalsi_naslednik;
             }
           }

	   /* cesta neni kruznice -- uloz na zasobnik */
           dalsi = nova_cesta(aktualni->delka_cesty + 1);
           memcpy(dalsi->cesta, aktualni->cesta, dalsi->delka_cesty * sizeof(uzel_id));
           dalsi->cesta[dalsi->delka_cesty] = i;
           push(dalsi);

	   if (inicializace && stack_length() >= MIN_PROCESS_TASK_SIZE * processes_count) {
	     int cil;
	     TRACE1("%d: Delim pocatecni zasobnik a rozesilam praci...\n", my_rank);
	     inicializace = 0;
	     /* rozdel zasobnik a rozesli ho procesum */
	     for (cil = 1; cil < processes_count; cil++) {
	       cesta_p odesilany = pop();
	       posli_cestu(odesilany, cil);
	       uvolni_cestu(odesilany);
	     }
	   }

           dalsi_naslednik:;
        }
      }
      if (aktualni_nepouzit)
        uvolni_cestu(aktualni);
    }
  }
}

/* ========================================================================== */

int main(int argc, char *argv[])
{
  FILE *f;
#ifndef __WIN32__
  struct rlimit rlp = { 0, 0 };
#endif
  /*  time_t start;*/

#ifndef __WIN32__
  /* zakaz tvorbu core */
  if (setrlimit(RLIMIT_CORE, &rlp)) {
    perror("Nelze nastavit core limit\n");
    return 1;
  }
#endif

  TRACE0("AHOJ!\n");

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
  MPI_Comm_size(MPI_COMM_WORLD, &processes_count);

  nejdelsi_delka = 0;
  nejdelsi = NULL;

  #ifdef STDOUT_REDIR
  {
    char logname[15];
    sprintf(logname, "stdout%d.log", my_rank);
    TRACE2("%d: stdout redirected to %s\n", my_rank, logname);
    freopen(logname, "w", stdout);
  }
  #endif

  TRACE1("%d: Waiting for all processes ...\n", my_rank);
  MPI_Barrier(MPI_COMM_WORLD);
  TRACE1("%d: OK, starting ...\n", my_rank);

  if (my_rank == 0) {
      /* jsem sef */
      int i;
      double starttime, stoptime;

      if (argc != 2) {
        fprintf(stderr, "Pouziti: %s graf\n", argv[0]);
        /*MPI_Finalize();*/
	return 2;
      }
    
      /* Nacteni grafu -- format grafu: na zacatku long (4B) pocet uzlu, dale matice sousednosti. (char[]) */

      if ((f = fopen(argv[1], "rb")) == NULL) {
         perror("Graf nenalezen");
         MPI_Finalize();
         return 1;
      }

      if (fread(&N, sizeof(N), 1, f) != 1) {
        perror("Spatny soubor");
        fclose(f);
        MPI_Finalize();
        return 1;
      }

#ifndef __WIN32__
      /* pro podporu prenositelnosti -- pokud nejsme na W32 a vypada to jako spatny endian, preved na opacny endian. */
      if (N & 0xFF000000)
        N = ((N & 0xFF) << 24) | ((N & 0xFF00) << 8) | ((N & 0xFF0000) >> 8) | ((N & 0xFF000000) >> 24);
#endif

      matice = (char *) mallocate(N * N * sizeof(char));
      navstivene = callocate(N, sizeof(char));
      assert(matice != NULL && navstivene != NULL);

      if (fread(matice, sizeof(char), N * N, f) != (size_t)(N * N)) {
        perror("Spatny soubor");
        fclose(f);
        MPI_Finalize();
        return 1;
      }

      fclose(f);
      
      /* rozesli graf vsem */
      TRACE0("Rozesilam graf...\n");
      MPI_Bcast(&N, 1, MPI_LONG, 0, MPI_COMM_WORLD);
      MPI_Bcast(matice, N * N, MPI_CHAR, 0, MPI_COMM_WORLD);
      TRACE0("Cekam na synchronizaci...\n");

      MPI_Barrier(MPI_COMM_WORLD);
      printf("Nyni prosim vyckejte, pracuji...");
      fflush(stdout);
      starttime = MPI_Wtime();

      /* prace */
      while (najdi_nejdelsi(1) >= 0)
	TRACE1("%d: Konec jednoho najdi_nejdelsi\n", my_rank);

      TRACE0("---konec hlavniho najdi_nejdelsi cyklu---\n");

      MPI_Barrier(MPI_COMM_WORLD);
      stoptime = MPI_Wtime();

      for (i = 1; i < processes_count; i++) {
	cesta_p vzdalena = prijmi_cestu(i);
	if (vzdalena != NULL) {
	  if (vzdalena->delka_cesty > nejdelsi_delka) {
	    uvolni_cestu(nejdelsi);
	    nejdelsi = vzdalena;
	    nejdelsi_delka = nejdelsi->delka_cesty;
	  } else {
	    uvolni_cestu(vzdalena);
	  }
	}
      }

      printf("OK, hotovo, doba vypoctu: %lf.\n", stoptime-starttime);
      if (nejdelsi_delka == 0) {
	printf("Graf je acyklicky.\n");
      } else {
	long i;
	uzel_id *p;
	
	printf("Nejdelsi nalezena kruznice ma delku %ld a vypada nasledovne: ", nejdelsi_delka);
	for (i = 0, p = nejdelsi->cesta; i < nejdelsi_delka; i++, p++)
	  printf("%ld ", *p);
	putchar('\n');
      }
  } else {
      /* podrizeny */

      /* prijmi graf */
      MPI_Bcast(&N, 1, MPI_LONG, 0, MPI_COMM_WORLD);

      matice = (char *) mallocate(N * N * sizeof(char));
      navstivene = callocate(N, sizeof(char));
      assert(matice != NULL && navstivene != NULL);

      MPI_Bcast(matice, N * N, MPI_CHAR, 0, MPI_COMM_WORLD);

      TRACE2("%d: Prijal jsem graf o %ld uzlech\n", my_rank, N);
      TRACE0("Cekam na synchronizaci...\n");

      MPI_Barrier(MPI_COMM_WORLD);

      /* prace */
      while(1) {
	MPI_Status status;
	cesta_p prijata_cesta;
        char pokracujeme;
        TRACE1("%d: Cekam na pokracovani\n", my_rank);
	MPI_Bcast(&pokracujeme, 1, MPI_CHAR, 0, MPI_COMM_WORLD);
	TRACE2("%d: pokracujeme = %d\n", my_rank, pokracujeme);
	if (!pokracujeme) break;
	TRACE1("%d: Cekam na prideleni prace\n", my_rank);
	MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        TRACE2("%d: Prichod odpovedi (%d)\n", my_rank, status.MPI_TAG);
	if (status.MPI_TAG != TAG_CESTA_DELKA_CESTY) {
          long *buff;
	  TRACE1("%d: Zadna prace uz neni!\n", my_rank);
          MPI_Recv(buff, 1, MPI_LONG, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
          continue;
	}
	TRACE2("%d: Prijimam praci od %d\n", my_rank, status.MPI_SOURCE);
	init_stack();
	prijata_cesta = prijmi_cestu(status.MPI_SOURCE);
	vypis_cestu(prijata_cesta);
	push(prijata_cesta);
	TRACE1("%d: Pracuji...\n", my_rank);
        najdi_nejdelsi(0);
      }
      TRACE1("%d: --- Skoncil jsem ---\n", my_rank);

      MPI_Barrier(MPI_COMM_WORLD);

      posli_cestu(nejdelsi, 0);
  }

  TRACE1("%d: Terminating\n", my_rank);

  MPI_Finalize();

  TRACE0("SBOHEM\n");

  return 0;
}
