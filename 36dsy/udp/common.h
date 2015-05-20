#ifndef __COMMON_H
#define __COMMON_H

/********************************************************************
  Spolecne definice pro clienta i server pro posilani souboru
 ********************************************************************/

/******************************************************************************/

#ifdef __WIN32__

#include <winsock.h>
#define errno WSAGetLastError()

#define EWOULDBLOCK             WSAEWOULDBLOCK
#define EINPROGRESS             WSAEINPROGRESS
#define EALREADY                WSAEALREADY
#define ENOTSOCK                WSAENOTSOCK
#define EDESTADDRREQ            WSAEDESTADDRREQ
#define EMSGSIZE                WSAEMSGSIZE
#define EPROTOTYPE              WSAEPROTOTYPE
#define ENOPROTOOPT             WSAENOPROTOOPT
#define EPROTONOSUPPORT         WSAEPROTONOSUPPORT
#define ESOCKTNOSUPPORT         WSAESOCKTNOSUPPORT
#define EOPNOTSUPP              WSAEOPNOTSUPP
#define EPFNOSUPPORT            WSAEPFNOSUPPORT
#define EAFNOSUPPORT            WSAEAFNOSUPPORT
#define EADDRINUSE              WSAEADDRINUSE
#define EADDRNOTAVAIL           WSAEADDRNOTAVAIL
#define ENETDOWN                WSAENETDOWN
#define ENETUNREACH             WSAENETUNREACH
#define ENETRESET               WSAENETRESET
#define ECONNABORTED            WSAECONNABORTED
#define ECONNRESET              WSAECONNRESET
#define ENOBUFS                 WSAENOBUFS
#define EISCONN                 WSAEISCONN
#define ENOTCONN                WSAENOTCONN
#define ESHUTDOWN               WSAESHUTDOWN
#define ETOOMANYREFS            WSAETOOMANYREFS
#define ETIMEDOUT               WSAETIMEDOUT
#define ECONNREFUSED            WSAECONNREFUSED
#define ELOOP                   WSAELOOP
#define EHOSTDOWN               WSAEHOSTDOWN
#define EHOSTUNREACH            WSAEHOSTUNREACH
#define EPROCLIM                WSAEPROCLIM
#define EUSERS                  WSAEUSERS
#define EDQUOT                  WSAEDQUOT
#define ESTALE                  WSAESTALE
#define EREMOTE                 WSAEREMOTE

#else  /* ifdef __WIN32__ */

#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
/*#include <sys/ioctl.h>*/
#include <errno.h>
#define closesocket close
#define ioctlsocket ioctl
#define INVALID_SOCKET -1
#define SOCKET int
#define FAR

#endif /* ifdef __WIN32__ else */

/******************************************************************************/

#define DEFAULTPORT 8765

/* file header - hlavicka spojeni */
typedef struct tag_fheader {
		unsigned long int filesize;	/* (4B) delka souboru (byte) */
		unsigned char  	  fnamelen;	/* (1B) delka jmena souboru (znaku) */
		/* nyni nasleduje jmeno souboru, ASCIIZ */
	} fheader;

/* file acknowledge - potvrzeni prijeti hlavicky */
typedef struct tag_fhacknowledge {
		int result;	/* (4B) uspesnost spojeni (viz ACKR_*) */
	} fhacknowledge;

#define ACKR_OK			0	/* OK, muzete zacit posilat */
#define ACKR_EXISTS 	1	/* vzdaleny soubor jiz existuje, pokud nechcete prepsat, ukoncete spojeni, jinak zaslete CONF_OVER*/
#define	ACKR_NOACCESS	2	/* k zadanemu souboru nemate prava/pristup/etc */

/* pote, co prislo facknowledge s AKCR_EXISTS, bud poslu CONF_SKIP pro preskoceni, nebo poslu CONF_OVER pro prepsani */
#define CONF_OVER       1
#define CONF_SKIP       2

void psockerror(char *msg); /* Vytiskne posledni socket chybu na stderr */

void simplefail(char *msg); /* Ukonci program s chybou */
void fail(char *msg); /* Ukonci program s chybou a popisem standardni chyby */
void sockfail(char *msg); /* Ukonci program s chybou a popisem posledni socket chyby */

#endif
