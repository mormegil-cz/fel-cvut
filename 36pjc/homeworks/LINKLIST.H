#ifndef __LINKLIST_H
#define __LINKLIST_H

/* Typ datove hodnoty prvku seznamu */
typedef double item_data;

/* Jedna polozka seznamu */
typedef struct tagNODE {
		item_data data;
		struct tagNODE	*next;
	} node_t, *node_p;

/* Seznam */
typedef struct {
		node_p   head; 	/* Prvni (nejmensi) prvek seznamu */
		node_p   tail;	/* Posledni (nejvetsi) prvek seznamu */
		unsigned count;	/* Pocet prvku v seznamu */
	} linklist_t, *linklist_p;

/* Inicializace seznamu (nastavi dodany seznam jako prazdny),
	vraci opet list */
linklist_p init(linklist_p list);

/* Uvolni pamet zabranou polozkami seznamu (nikoliv samotnym listem!) */
void	   done(linklist_p list);

/* Prida do seznamu list (na spravne misto) polozku, obsahujici item.
   Potrebnou pamet alokuje pomoci calloc. Vraci opet list. */
linklist_p add(linklist_p list, item_data item);

/* Vytiskne obsah seznamu, formatuje pomoci itemformat. */
void	   print(linklist_p list, char *itemformat);

/* Pro vsechny prvky seznamu provede prislusnou callback funkci efunc.
   Pokud tato funkce vrati nulu, enumerace je ukoncena. */
void       enumerate(linklist_p list, int (*efunc)(node_p));

#endif	/* #ifndef __LINKLIST_H */