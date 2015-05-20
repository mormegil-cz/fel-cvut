/***************************************************************************
  Ukladani&nahravani binarnich stromu.

  Poznamka: Jelikoz jsem si to potreboval nejak otestovat, musel jsem si
  vytvorit i dalsi jednotky pro praci se stromy, takze ted je z toho takova
  miniknihovna. Ale funkce, o ktere tu jde, se jmenuji
  btree_load a btree_save.

  Petr Kadlec
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "btrees.h"

/* Vnitrni funkce pro ukonceni s chybovou hlaskou */
void fail(char *msg)
{
  perror(msg);
  abort();
}

/* Ulozeni stromu s korenem root do souboru stream (binarni, otevreny pro zapis) */
void btree_save(FILE *stream, node_p root)
{
  static char tag;

  if (root == NULL) return; /* prazdny strom */

  /* zapis datove casti korene */
  if (fwrite(&root->data, sizeof(data_t), 1, stream) != 1) fail("Error writing data");

  /* znacka, popisujici potomky uzlu */
  tag = (root->left == NULL ? 0 : 1) | (root->right == NULL ? 0 : 2);
  if (fwrite(&tag, sizeof(tag), 1, stream) != 1) fail("Error writing tag");

  /* zapis potomku */
  btree_save(stream, root->left);
  btree_save(stream, root->right);
}

/* Nahrani stromu z binarniho souboru otevreneho pro cteni.
   Vrati koren stromu nahraneho ze souboru stream. */
node_p btree_load(FILE *stream)
{
  node_p result;
  char tag;

  if (feof(stream)) return NULL;  /* prazdny strom */

  /* alokace mista pro koren */
  if ((result = (node_p)malloc(sizeof(node_t))) == NULL) fail("Error allocating node");

  /* cteni datove casti korene */
  if (fread(&result->data, sizeof(data_t), 1, stream) != 1) fail("Error reading data");

  /* cteni znacky */
  if (fread(&tag, sizeof(tag), 1, stream) != 1) fail("Error reading tag");
  assert(tag <= 3);

  /* cteni potomku */
  if (tag & 1) result->left = btree_load(stream);
	else   result->left = NULL;
  assert(!(tag & 1) || result->left != NULL);

  if (tag & 2) result->right = btree_load(stream);
	else   result->right = NULL;
  assert(!(tag & 2) || result->right != NULL);

  return result;
}

/* Vytvori novy uzel s hodnotou data a vlozi jej do stromu zadanem korenem root */
node_p btree_insert(node_p root, data_t data)
{
  node_p result, prev;

  if ((result = (node_p)malloc(sizeof(node_t))) == NULL) fail("Error allocating node");

  result->data = data;
  result->left = NULL;
  result->right = NULL;

  if (root == NULL) {

    return result;

  } else {

     node_p p = root;

     while (p != NULL) {
	prev = p;
	p = (p->data > data) ? p->left : p->right;
     }
     if (prev->data > data) prev->left = result;
		       else prev->right = result;

     return root;

  }
}

/* Vytiskne hodnoty vsech uzlu stromu zadaneho korenem root,
   uzly se vypisuji v poradi dle velikosti (od nejmensiho po nejvetsi). */
void btree_print(node_p root, char *format)
{
  if (root == NULL) return;

  btree_print(root->left, format);
  printf(format, root->data);
  btree_print(root->right, format);
}

/* Pro vsechny uzly ve strome zadanem korenem root provede funkci efunc,
   uzly se vyjmenovavaji v poradi dle velikosti (od nejmensiho po nejvetsi). */
void btree_enumerate(node_p root, void (*efunc)(node_p))
{
  if (root == NULL) return;

  btree_enumerate(root->left, efunc);
  efunc(root);
  btree_enumerate(root->right, efunc);
}

/* Vytvori strom, ktery obsahuje items polozek, ktere jsou procedure dodany
   (seznam items polozek typu data_t) */
node_p btree_create(int items, ...)
{
  va_list ap;
  int i;
  node_p tree = NULL;

  va_start(ap, items);

  for (i=0; i<items; i++)
    tree = btree_insert(tree, va_arg(ap, data_t));

  va_end(ap);

  return tree;
}

/* --- Testovaci programek --- */

int main(void)
{
 node_p tree1, tree2;
 FILE *f;

 /* vytvorime strom tree1 */
 tree1 = btree_create(5, 3, 10, 6, 2, 12);

 /* vypiseme jeho obsah */
 printf("tree1: ");
 btree_print(tree1, "%d ");
 printf("\n");

 /* a ulozime ho */
 f = fopen("strom.dat", "wb");
 btree_save(f, tree1);
 fclose(f);

 /* nahrajeme strom do tree2 */
 f=fopen("strom.dat", "rb");
 tree2 = btree_load(f);
 fclose(f);

 /* a vypiseme jeho obsah */
 printf("tree2: ");
 btree_print(tree2, "%d ");
 printf("\n");

 return 0;
}