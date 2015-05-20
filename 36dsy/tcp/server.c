#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <sys/stat.h>

#ifdef __WIN32__

#include <io.h>
#include <windows.h>

#define F_OK 00
#define W_OK 02
#define getstat stat

#else

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>

#define getstat lstat

#endif // ifdef __WIN32__ else

//#define DEBUGOUT
#define FBUFFSIZE 2048		/* velikost bufferu pro zapis do souboru */
#define BACKLOG 5           /* delka fronty cekajicich spojeni */
#define MAXCONNS 10         /* maximalni pocet aktualne otevrenych spojeni */

#include "common.h"

#ifndef __WIN32__
#define stricmp strcmp
#endif

/* Spolecne zdroje */
SOCKET s = INVALID_SOCKET;	    /* pouzivany socket */
#ifdef __WIN32__
int WSA_inited = 0;				/* bylo provedeno WSAStartup? (pouze Windows) */
DWORD MainThreadId;				/* Thread ID hlavniho threadu */
#endif

/* Stavove promenne */
int terminated = 0;     /* zde se pri ukonceni (signalem) nastavi priznak na true */
unsigned connections = 0; /* tady se pocita pocet aktualne otevrenych spojeni */

/* Konfigurace */
int usedport = DEFAULTPORT; /* port, na kterem server ceka na klienty */
int autooverwrite = 0;      /* automaticky prepisovat existujici soubory? */

void terminate(int returncode) /* Ukonci program (s uklidem) */
{
	/* Uzavreni socketu */
#ifdef __WIN32__
  if (GetCurrentThreadId() == MainThreadId) {
#endif
	if (s != INVALID_SOCKET)
		if (closesocket(s)) {
			s = INVALID_SOCKET;
			sockfail("Error closing socket");
		}

#ifdef __WIN32__
	/* Ukonceni Windows Sockets (pouze ve Windows) */
	if (WSA_inited)
		if (WSACleanup()) {
			WSA_inited = 0;
			sockfail("Error closing WinSock DLL");
		}
#endif

#ifdef __WIN32__
	ExitThread(returncode);
  }
#else
	exit(returncode);
#endif
}

void usage(void)
/* Vypise informace o pouziti programu */
{
	printf("Syntax: server [options]\n"
		   "        -p port  ... changes the port on which the server waits (default %d)\n"
		   "        -o       ... automatically overwrite any file (do not ask the client)\n",DEFAULTPORT);
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
		fprintf(stderr, "server:Looking up DNS ...\n");
#endif
		if ((host = gethostbyname(addr)) == NULL)
			sockfail("Could not locate remote server");

#ifdef DEBUGOUT
		fprintf(stderr, "server:DNS lookup complete: %i.%i.%i.%i\n",
				(unsigned char)(host->h_addr_list[0])[0],
				(unsigned char)(host->h_addr_list[0])[1],
				(unsigned char)(host->h_addr_list[0])[2],
				(unsigned char)(host->h_addr_list[0])[3]);
#endif

		if ( (host->h_addrtype != AF_INET) ||
			 (host->h_length != 4) )
			 simplefail("Remote server address not supported");

		memcpy(&(sin->sin_addr), &(host->h_addr_list[0]), host->h_length);
	}
}

int processargs(int argc, char *argv[]) /* zpracuje prikazovy radek, vysledek: 0=OK */
{
  int i;
  char *err;

  for (i=1; i<argc; i++) {
    if (*argv[i] == '\0') return 1;

    if (!stricmp(argv[i], "-p")) {
        i++;
        if (i>=argc) return 1;

        usedport = strtoul(argv[i], &err, 10);
        if ((err != NULL) && (*err)) {
            struct servent *serv;

            if ((serv = getservbyname(argv[i], "tcp")) == NULL) {
                fflush(stdout);
				fprintf(stderr, "Unknown port name\n");
                return 1;
            }
            usedport = serv->s_port;
		}
		if (usedport > 65535) return 1;
    } else if (!stricmp(argv[i], "-o")) {
        autooverwrite = 1;
    } else return 1;
  }
  return 0;
}

/* toto je procedura pro jedno spojeni (je pro ni vzdy vytvoren zvlastni thread/proces) */
void processconnection(SOCKET s)
{
	FILE *f;
	void *buf;
	fheader head;
	fhacknowledge hack;
	size_t remaining;
	char *fname;
	char confirmation;
	int accessdenied = 0;

	/* Komunikace, cast 1 - prijmi "hlavicku spojeni" */
	net_receive(s, &head, sizeof(head));
	if ((fname = (char *)malloc(head.fnamelen+1)) == NULL) fail("Not enough memory");
	fname[head.fnamelen] = '\0'; /* ukoncovaci nula ve stringu */
	net_receive(s, fname, head.fnamelen);

	if (!access(fname, F_OK)) {
		struct stat info;

		if (access(fname, W_OK) || getstat(fname, &info) || (info.st_mode & S_IFMT != S_IFREG)) accessdenied = 1;
		else {
			if (!autooverwrite) {
				/* soubor existuje! */
				hack.result = ACKR_EXISTS;
				net_send(s, &hack, sizeof(hack));

				switch (recv(s, &confirmation, 1, 0)) {
					case INVALID_SOCKET: sockfail("Error reading from network");
					case 0: terminate(0); /* client nechce prepsat soubor, proto ukoncil spojeni */
					default: if (confirmation != CONF_OVER) simplefail("Corrupted data received from network");
				}
			} else {
				hack.result = ACKR_OK;
				net_send(s, &hack, sizeof(hack));
			}
		}
	} else {
		FILE *test;

		if ((test = fopen(fname, "ab")) == NULL) accessdenied = 1;
		else {
			if (fclose(test))
				fail("Error closing file");
		}
		hack.result = ACKR_OK;
		net_send(s, &hack, sizeof(hack));
	}

	if (accessdenied) {
		/* do souboru nelze zapisovat */
		hack.result = ACKR_NOACCESS;
		net_send(s, &hack, sizeof(hack));
		terminate(0);
	}

	if ((f = fopen(fname, "wb")) == NULL)
		fail("Error opening file");

	free(fname);

	/* Komunikace, cast 3 - prijimani souboru */

	/* Alokujeme prenosovy buffer */
	if ((buf = malloc(FBUFFSIZE)) == NULL) {
		fclose(f);
		fail("Error allocating buffer");
	}

#ifdef DEBUGOUT
	fprintf(stderr, "server:Receiving file data...\n");
#endif
	for(remaining = head.filesize; remaining>0; ) {
		size_t towrite = (remaining > FBUFFSIZE) ? FBUFFSIZE : remaining;

		net_receive(s, buf, towrite);
		if (fwrite(buf, 1, towrite, f) != towrite) {
			free(buf);
			fclose(f);
			fail("Error writing to the destination file");
		}

		remaining -= towrite;

#ifdef DEBUGOUT
		fprintf(stderr, "server:%luB remaining\n", remaining);
#endif
	}
#ifdef DEBUGOUT
	fprintf(stderr, "server:Done.\n");
#endif

	free(buf);
	fclose(f);
	terminate(0);
}

#ifdef __WIN32__
DWORD WINAPI ThreadFunc(LPVOID lparam)
{
	connections++;
	processconnection((SOCKET)lparam);
	connections--;
	terminate(1);
	return 255;
}

void launchprocess(SOCKET conn_sock)
{
	unsigned long ThreadID;

#if (sizeof(LPVOID) != sizeof(SOCKET))
#error Must change implementation!
#endif
	if (CreateThread(NULL, 0, ThreadFunc, (LPVOID)conn_sock, 0, &ThreadID) == NULL)
		simplefail("Error creating thread for the connection");
}
#else
/* Obsluha signalu SIGCHLD - potomek zemrel */
void childdead(int sig)
{
   union wait status;

   while (wait3(&status, 0, (struct rusage *)NULL)>0);
   connections--;

   signal(SIGCHLD, childdead);     // a priste znovu
}

void launchprocess(SOCKET conn_sock)
{
	connections++;
	switch(fork()) {
		case  0: break;
		case -1: fail("Error creating process for the connection");
		default: processconnection(conn_sock);
				 terminate(1);
				 break;
	}
}
#endif // ifdef __WIN32__ else

int main(int argc, char* argv[])
{
	struct sockaddr_in my_addr, remote_addr;

#ifdef __WIN32__
	WORD wVersionRequired = MAKEWORD( 1, 0 );
	WSADATA WSAData;

	MainThreadId = GetCurrentThreadId();
#endif

	/* kontrola prikazoveho radku */
	if (processargs(argc, argv)) {
		usage();
		terminate(255);
	}

#ifdef __WIN32__
	/* Inicializace Windows Sockets (pouze ve Windows) */
	if (WSAStartup( wVersionRequired, &WSAData ))
		simplefail("No usable WinSock DLL found");
	WSA_inited = 1;
#endif

	/* vytvoreni socketu */
	if ((s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) == INVALID_SOCKET)
		sockfail("Error creating socket");

    memset(&my_addr, 0, sizeof(my_addr));
    my_addr.sin_family = AF_INET;
    my_addr.sin_port = htons(usedport);

#ifdef DEBUGOUT
	fprintf(stderr, "server:Binding the socket to a local address ...\n");
#endif
	if (bind(s, (struct sockaddr *)&my_addr, sizeof(my_addr)) == INVALID_SOCKET)
        sockfail("Error binding the socket to a local address");
#ifdef DEBUGOUT
      fprintf(stderr, "server:Socket bind to port %i"/*%i.%i.%i.%i:%i*/"\n",
                /*(unsigned char)(my_addr.sin_addr.s_addr >> 24),
                (unsigned char)(my_addr.sin_addr.s_addr >> 16),
                (unsigned char)(my_addr.sin_addr.s_addr >> 8),
                (unsigned char)(my_addr.sin_addr.s_addr),*/
                usedport);
#endif

#ifdef DEBUGOUT
	fprintf(stderr, "server:Listening on the socket ...\n");
#endif
	if (listen(s, BACKLOG) == INVALID_SOCKET)
        sockfail("Error establishing the socket to listen for an incoming connection");
#ifdef DEBUGOUT
	fprintf(stderr, "server:OK, now awaiting connections ...\n");
#endif

#ifndef __WIN32__
	/* Zaregistruj obsluhu pri ukonceni child procesu */
	if (signal(SIGCHLD, childdead) == SIG_ERR)
		fail("Error registrating signal handler");
#endif

    /* ------------- Hlavni smycka serveru ------------ */
    while (!terminated)
	{
      int remote_addr_len = sizeof(remote_addr);
	  SOCKET conn_sock;

      while (connections >= MAXCONNS) {
        simplefail("server:DEBUG VERSION: Too many connections");
      }

#ifdef DEBUGOUT
      fprintf(stderr, "server:Awaiting connection ...\n");
#endif
      memset(&remote_addr, 0, sizeof(remote_addr));
      if ((conn_sock = accept(s, (struct sockaddr *)&remote_addr, &remote_addr_len)) == INVALID_SOCKET)
         sockfail("Error accepting connection");
#ifdef DEBUGOUT
	  remote_addr.sin_addr.s_addr = ntohl(remote_addr.sin_addr.s_addr);	
	  fprintf(stderr, "server:Accepted connection from %i.%i.%i.%i\n",
				(unsigned char)(remote_addr.sin_addr.s_addr >> 24),
				(unsigned char)(remote_addr.sin_addr.s_addr >> 16),
				(unsigned char)(remote_addr.sin_addr.s_addr >> 8),
				(unsigned char)(remote_addr.sin_addr.s_addr));
#endif

		launchprocess(conn_sock);
	}

	/* Uzavreni vseho potrebneho & konec */
	terminate(0);
	return 0;
}
