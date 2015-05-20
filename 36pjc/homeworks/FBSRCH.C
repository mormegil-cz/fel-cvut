/***************************************************************************
 Hledani v usporadanem souboru pomoci binarniho puleni

  Petr Kadlec & Michal Conos
 ***************************************************************************/

#include <stdio.h>
#include <assert.h>

/* hlavni funkce: V souboru fname nalezne key, vrati jeho pozici, nebo
	vrati -1 pokud nenalezen, -2 pokud doslo k chybe pri praci se
	souborem (neexistuje, apod.) */
long fbsearch(char *fname, int key)
{
  FILE *f;
  long l, r;

  if ((f = fopen(fname, "rb")) == NULL) return -2;

  /* zjisteni a kontrola delky */
  if (fseek(f, 0, SEEK_END)) { fclose(f); return -2; }
  l = 0;
  if ((r = ftell(f)) < 0L) { fclose(f); return -2; }
  if (r % sizeof(int)) { fclose(f); return -2; }
  r /= sizeof(int);

  /* vyhledavani */
  while (l != r) {
    long m = (l+r)/2;
    int data;

    /* cteni prvku uprostred rozsahu */
    if (fseek(f, m*sizeof(int), SEEK_SET)) { fclose(f); return -2; }
    if (fread(&data, sizeof(int), 1, f) != 1) { fclose(f); return -2; }

    /* nalezen? */
    if (data == key) {
      fclose(f);
      return m;
    }

    /* nenalezen - uprav rozsah */
    if (data < key) {
      l = m+1;
    } else {
      r = m;
    }

    assert(l<=r);
  }

  /* prvek nenalezen */
  fclose(f);
  return -1;
}

/* pomocna funkce pro vytvoreni nejakeho pokusneho datoveho souboru
   Soubor pak obsahuje cisla 0..63, 65..1024 */
void generate(char *fname)
{
  FILE *f;
  int i, data = 0;

  f = fopen(fname, "wb");
  for (i=0; i<1024; i++,data++) {
    if (data == 64) data = 65;
    fwrite(&data, sizeof(int), 1, f);
  }

  fclose(f);
}

int main(void)
{
  int key;

  generate("data.bin");

  printf("CO hledame? ");
  scanf("%d", &key);

  printf("Nalezeno: %ld\n", fbsearch("data.bin", key));

  return 0;
}