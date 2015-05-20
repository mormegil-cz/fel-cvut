#ifndef __FAILS_H
#define __FAILS_H

void simplefail(char *msg); /* Ukonci program s chybou */
void fail(char *msg); 		/* Ukonci program s chybou a popisem standardni chyby */
void sockfail(char *msg); 	/* Ukonci program s chybou a popisem posledni socket chyby */

#endif // ifndef __FAILS_H
 
