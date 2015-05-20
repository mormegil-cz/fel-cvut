
// POP3 stream

#include <stdio.h>
#include <string.h>


const char *OK    = "+OK";
const char *ERR   = "-ERR";
const char *USER  = "USER";
const char *PASS  = "PASS";
const char *STAT  = "STAT";
const char *LIST  = "LIST";
const char *UIDL  = "UIDL";
const char *RETR  = "RETR";
const char *QUIT  = "QUIT";
const char *DELE  = "DELE";
const char *NOOP  = "NOOP";
const char *RSET  = "RSET";
const char *TOP  = "TOP";
const char *APOP  = "APOP";

int parseCom (const char * radek,FILE * out){
   if (!strncmp ( USER, radek, 4 )){
      fprintf (out,"\nClient: Posilam uzivatelske jmeno ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
   else if (!strncmp ( PASS, radek, 4 )){
      fprintf (out,"\nClient: Posilam heslo ");
      fprintf (out,"%s\n", radek+4);
      return 1;
  if (!strncmp ( APOP, radek, 4 )){
      fprintf (out,"\nClient: Posilam sifrovany autentizacni klic ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
   }
   else if (!strncmp ( STAT, radek, 4 )){
      fprintf (out,"\nClient: Zadam o udaje o poste, format pocet e-mailu/velikost ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
   else if (!strncmp ( LIST, radek, 4 )){
      fprintf (out,"\nClient: Zadam seznam posty, format cislo e-mailu/jeho velikost ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
   else if (!strncmp ( UIDL, radek, 4 )){
      fprintf (out,"\nClient: Pozadevek na unikatni ID  ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
   else if (!strncmp ( RETR, radek, 4 )){
      fprintf (out,"\nClient: Zadam zaslat zpravu ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
      else if (!strncmp ( DELE, radek, 4 )){
      fprintf (out,"\nClient: Zadam o smazani zpravy ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
	  else if (!strncmp ( NOOP, radek, 4 )){
      fprintf (out,"\nClient: Zadam o provedeni prazdneho prikazu ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
	  else if (!strncmp ( RSET, radek, 4 )){
      fprintf (out,"\nClient: Zadam o obnoveni smazanych zprav ");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }
	  else if (!strncmp ( TOP, radek, 3 )){
      fprintf (out,"\nClient: Zadam o zaslani hlavicky a casti zpravy, cislo e-mailu/pocet radku tela:   ");
      fprintf (out,"%s\n", radek+3);
	  return 1;
   }
      else if (!strncmp ( QUIT, radek, 4 )){
      fprintf (out,"\nClient: Zadam ukonceni teto faze komunikace");
      fprintf (out,"%s\n", radek+4);
      return 1;
   }


   return 0;
}


int parseRe (char * buf,FILE* tmp,FILE* out){
   	
   fgets (buf, 1024, tmp);
   if (strncmp ( OK, buf, sizeof(OK)-1 )){
	   fprintf(out,"Server: Chyba - prikaz nemuze byt splnen\n");
	   fprintf (out,"%s\n", buf+3);
	   return 0;
   }
   fprintf (out,"Server: Prikaz rozpoznan, proveden\n");
   fprintf (out,"%s\n", buf+3);
   fgets (buf, 1024, tmp);
   if(!strncmp(USER,buf,4) || !strncmp(PASS,buf,4) ||!strncmp(STAT,buf,4) ||!strncmp(LIST,buf,4) ||!strncmp(UIDL,buf,4) ||!strncmp(RETR,buf,4) ||!strncmp(QUIT,buf,4)||!strncmp(DELE,buf,4)||!strncmp(NOOP,buf,4)||!strncmp(RSET,buf,4)||!strncmp(TOP,buf,3)||!strncmp(APOP,buf,3) )  	return 1;

	while (strncmp(".",buf,1)) 
   { 
	   
	   fprintf(out,"%s",buf);	
	   if(NULL==fgets (buf, 1024, tmp)) break;
   }
   if(NULL==fgets (buf, 1024, tmp)) return 0;
   return 1;
}





