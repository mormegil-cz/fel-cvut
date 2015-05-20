#ifndef __BTREES_H
#define __BTREES_H

#include <stdio.h>

/***************************************************************************
 Miniknihovna pro praci s binarnimi vyhledavacimi stromy (nevyvazovanymi)
 ***************************************************************************/

/* Typ datove casti polozky stromu */
typedef int data_t;

/* Jeden uzel stromu */
typedef struct tagNODE node_t, *node_p;
	struct tagNODE {
		data_t data;
		node_p left;
		node_p right;
	};

/* Ulozeni stromu s korenem root do souboru stream (binarni, otevreny pro zapis) */
void btree_save(FILE *stream, node_p root);

/* Nahrani stromu z binarniho souboru otevreneho pro cteni.
   Vrati koren stromu nahraneho ze souboru stream. */
node_p btree_load(FILE *stream);

/* Vytvori novy uzel s hodnotou data a vlozi jej do stromu zadanem korenem root */
node_p btree_insert(node_p root, data_t data);

/* Vytiskne hodnoty vsech uzlu stromu zadaneho korenem root,
   uzly se vypisuji v poradi dle velikosti (od nejmensiho po nejvetsi). */
void btree_print(node_p root, char *format);

/* Pro vsechny uzly ve strome zadanem korenem root provede funkci efunc,
   uzly se vyjmenovavaji v poradi dle velikosti (od nejmensiho po nejvetsi). */
void btree_enumerate(node_p root, void (*efunc)(node_p));

/* Vytvori strom, ktery obsahuje polozky, ktere jsou procedure dodany
   (seznam items polozek typu data_t) */
node_p btree_create(int items, ...);

#endif /* __BTREES_H */
