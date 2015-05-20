#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

//#define DEBUGOUT

#include "common.h"

#define DEFAULTPORT 8765
#define FBUFFSIZE 2048		/* velikost bufferu pro cteni ze souboru */
#define NBUFFSIZE 128		/* velikost bufferu pro zapis na sit */

/* Spolecne zdroje */
FILE *f  = NULL;            /* lokalni soubor */
SOCKET s = INVALID_SOCKET;	/* pouzivany socket */
void *buf = NULL;			/* buffer mezi souborem a siti */
#ifdef __WIN32__
int WSA_inited = 0;			/* bylo provedeno WSAStartup? (pouze Windows) */
#endif

void simplefail(char *msg); /* Ukonci program s chybou */
void fail(char *msg); /* Ukonci program s chybou a popisem standardni chyby */
void sockfail(char *msg); /* Ukonci program s chybou a popisem posledni socket chyby */

void terminate(int returncode) /* Ukonci program (s uklidem) */
{
	/* Uvolneni pameti bufferu */
	if (buf != NULL)
		free(buf);

	/* Uzavreni socketu */
	if (s != INVALID_SOCKET)
		if (closesocket(s)) {
			s = INVALID_SOCKET;
			sockfail("Error closing socket");
		}

	/* Uzavreni lokalniho souboru */
	if (f != NULL)
		if (fclose(f)) {
			f = NULL;
			fail("Error closing file");
		}

#ifdef __WIN32__
	/* Ukonceni Windows Sockets (pouze ve Windows) */
	if (WSA_inited)
		if (WSACleanup()) {
			WSA_inited = 0;
			sockfail("Error closing WinSock DLL");
		}
#endif

	exit(returncode);
}

size_t filesize(FILE *stream)
/* Pomocna funkce pro zjisteni delky souboru */
{
   long curpos, length;

   curpos = ftell(stream);
   fseek(stream, 0L, SEEK_END);
   length = ftell(stream);
   fseek(stream, curpos, SEEK_SET);
   return length;
}

void usage(void)
/* Vypise informace o pouziti programu */
{
	printf("Syntax: sender localfile address remotefile\n"
		   "		localfile  ... file on local machine to be sent\n"
		   "		address    ... internet address of the remote machine\n"
		   "		remotefile ... file on remote machine for this file to be transferred\n");
}

void processaddress(char *addr, struct sockaddr_in *sin)
/* Zpracuje retezec addr z prikazove radky do sin (vcetne DNS) */
{
	char *portpart;
	int	 port;
	struct hostent FAR *host;

	memset((void *)sin, '\0', sizeof(*sin));
	sin->sin_family = AF_INET;

	if ((portpart = strchr(addr, ':')) != NULL) {
		*portpart = '\0';	/* ukonci adresu na dvojtecce */
		portpart++;			/* portpart ted ukazuje na cislo portu */
		if ( (port = atoi(portpart)) != 0 )
			sin->sin_port = htons(port);
		else {
				printf("Invalid address syntax\n");
				usage();
				terminate(255);
		}
	} else {
		sin->sin_port = htons(DEFAULTPORT);
	}

	if ((sin->sin_addr.s_addr = inet_addr(addr)) == INADDR_NONE) {
#ifdef DEBUGOUT
		fprintf(stderr, "client:Looking up DNS \"%s\"...\n",addr);
#endif
		if ((host = gethostbyname(addr)) == NULL)
			sockfail("Could not locate remote server");

#ifdef DEBUGOUT
		fprintf(stderr, "client:DNS lookup complete: %i.%i.%i.%i\n",
				(unsigned char)(host->h_addr_list[0])[0],
				(unsigned char)(host->h_addr_list[0])[1],
				(unsigned char)(host->h_addr_list[0])[2],
				(unsigned char)(host->h_addr_list[0])[3]);
#endif

		if ( (host->h_addrtype != AF_INET) ||
			 (host->h_length != 4) )
			 simplefail("Remote server address not supported");

		memcpy(&(sin->sin_addr), host->h_addr_list[0], host->h_length);
	}
}

int main(int argc, char* argv[])
{
	struct sockaddr_in sin;
	fheader head;
	fhacknowledge hack;
	size_t remaining;

#ifdef __WIN32__
	WORD wVersionRequired = MAKEWORD( 1, 0 );
	WSADATA WSAData;
#endif

	/* kontrola prikazoveho radku */
	if (argc != 4) {
		usage();
		terminate(255);
	}

#ifdef __WIN32__
	/* Inicializace Windows Sockets (pouze ve Windows) */
	if (WSAStartup( wVersionRequired, &WSAData ))
		simplefail("No usable WinSock DLL found");
	WSA_inited = 1;
#endif

	/* import adresy z prikazoveho radku (vcetne DNS lookup) */
	processaddress(argv[2], &sin);

	/* vytvoreni socketu */
	if ((s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) == INVALID_SOCKET)
		sockfail("Error creating socket");

#ifdef DEBUGOUT
	fprintf(stderr, "client:Connecting to server ...\n");
#endif
	if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) == INVALID_SOCKET)
		sockfail("Could not connect to remote server");
#ifdef DEBUGOUT
	fprintf(stderr, "client:Connected\n");
#endif

	/* Spojeni navazano, nasleduje vlastni komunikace */

	/* Otevri lokalni soubor */
	if ((f = fopen(argv[1], "rb")) == NULL)
		fail("Cannot open local file");

	/* Komunikace, cast 1 - posli "hlavicku spojeni" */
	head.filesize = filesize(f);
	head.fnamelen = strlen(argv[3]);
    net_send(s, &head, sizeof(head));
    net_send(s, argv[3], head.fnamelen);

	/* Komunikace, cast 2 - precti "vysledek hlavicky" */
	net_receive(s, &hack, sizeof(hack));
	switch (hack.result) {
		case ACKR_OK: break;
		case ACKR_EXISTS:   {
								int c = '\n';
                                char confirmation;

								do {
								    if (c=='\n') printf("Remote file already exists! Overwrite? ");
									c = toupper(getchar());
								} while ((c != 'Y') && ( c != 'N'));
								if (c == 'N') {
									printf("OK, aborted.\n");
									terminate(1);
								}

                                confirmation = CONF_OVER;
                                net_send(s, &confirmation, 1);
								break;
							}
		case ACKR_NOACCESS: simplefail("Remote file cannot be created");
		default:			simplefail("Corrupted data received from network");
	}

	/* Komunikace, cast 3 - posilani souboru */

	/* Alokujeme prenosovy buffer */
	if ((buf = malloc(FBUFFSIZE)) == NULL)
		fail("Error allocating buffer");

#ifdef DEBUGOUT
	fprintf(stderr, "client:Sending file data...\n");
#endif
	for(remaining = head.filesize; remaining>0; ) {
		size_t toread = (remaining > FBUFFSIZE) ? FBUFFSIZE : remaining;
#ifdef DEBUGOUT
		fprintf(stderr, "client:%luB remaining\n", remaining);
#endif

		if (fread(buf, 1, toread, f) != toread)
			fail("Error reading local file");

		net_send(s, buf, toread);

		remaining -= toread;
	}
#ifdef DEBUGOUT
	fprintf(stderr, "client:Done.\n");
#endif

	printf("%luB transferred OK.\n", head.filesize);

	/* Uzavreni vseho potrebneho & konec */
	terminate(0);
    return 0;
}
