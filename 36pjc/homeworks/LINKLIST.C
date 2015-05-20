#include <stdlib.h>
#include <stdio.h>

#include <assert.h>

#include "linklist.h"

linklist_p init(linklist_p list)
{
  assert(list != NULL);

  list->head = NULL;
  list->tail = NULL;
  list->count = 0;

  return list;
}

void done(linklist_p list)
{
  node_p item;
  node_p next;

  assert(list != NULL);

  item = list->head;

  while (item != NULL) {
     next = item->next;
     free(item);
     item = next;
  }

  list->head = NULL;
  list->tail = NULL;
  list->count = 0;
}

linklist_p add(linklist_p list, item_data item)
{
  node_p p;
  node_p prev = NULL;

  assert(list != NULL);
  p = list->head;
  /* najdi spravne misto pro prvek */
  while (p != NULL && p->data<item) {
    prev = p;
    p = p->next;
  }

  /* vytvor prislusnou polozku */
  p = (node_p)calloc(1, sizeof(node_t));
  p->data = item;
  p->next = NULL;

  /* zarad ji do seznamu */
  if (prev == NULL) {
    if (list->head == NULL) {
      /* prazdny seznam */
      list->tail = list->head = p;
    } else {
      /* na prvni misto */
      p->next = list->head;
      list->head = p;
    }
  } else {
    /* doprostred seznamu */
    p->next = prev->next;
    prev->next = p;
    /* pokud je na konci, uprav list->tail */
    if (p->next == NULL)
	list->tail = p;
  }

  /* zvedni pocitadlo velikosti seznamu */
  list->count++;

  return list;
}

void print(linklist_p list, char *itemformat)
{
  node_p p;

  assert(list != NULL);

  for (p = list->head; p != NULL; p = p->next)
	printf(itemformat, p->data);
}

void enumerate(linklist_p list, int (*efunc)(node_p))
{
  node_p p;

  assert(list != NULL);

  for (p = list->head; p != NULL; p = p->next)
	if (!efunc(p)) return;
}



/* testovaci programek */

/* enumeracni callback funkce - vypis pouze prvku s lichou celou casti */
int enumfunc(node_p item)
{
  static int count = 0;
  count++;
  if (((int)item->data) & 1) printf("%d. prvek = %.3f\n", count, item->data);
  return 1;
}

void main(void)
{
  linklist_t list;
  int d;

  init(&list);

  /* pridame nekolik polozek */
  add(add(add(add(add(&list, 2.5), 3.5), 1.5), -0.5), 0.0);

  /* a nechame uzivatele zadat zbytek */
  do {
    printf("Cislo (0=konec): ");
    /* nejak mi nechodi scanf floatu (scanf: floating point formats not linked) ... co to je? */
    scanf("%d",&d);
    if (d!=0) add(&list, (double)d);
  } while (d!=0);

  printf("Vypis seznamu: (%d prvku)\n", list.count);
  print(&list, "%.3lf ");
  printf("\n");

  printf("Vsechny prvky s lichou celou casti:\n");
  enumerate(&list, enumfunc);
}