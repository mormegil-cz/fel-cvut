#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include "common.h"

extern void terminate(int returncode); /* Ukonci program (s uklidem) */

void simplefail(char *msg) /* Ukonci program s chybou */
{
	fflush(stdout);
#ifdef DEBUGOUT
    fprintf(stderr, "client:");
#endif
	fprintf(stderr, msg);
	fputc('\n', stderr);
	terminate(2);
}

void fail(char *msg) /* Ukonci program s chybou a popisem standardni chyby */
{
	fflush(stdout);
#ifdef DEBUGOUT
    fprintf(stderr, "client:");
#endif
	perror(msg);
	terminate(2);
}

void sockfail(char *msg) /* Ukonci program s chybou a popisem posledni socket chyby */
{
	fflush(stdout);
#ifdef DEBUGOUT
    fprintf(stderr, "client:");
#endif
    psockerror(msg);
	terminate(2);
}

void net_receive(SOCKET s, void *buf, long int size)
/* Prijme size bajtu ze socketu s do buf */
{
	int recvlen;

	while(size) {
		if ((recvlen = recv(s, buf, size, 0)) <= 0)
			sockfail("Error reading from network");

		(char *)buf += recvlen;
		size -= recvlen;
		assert(size >= 0);
	}
}

void net_send(SOCKET s, void *buf, long int size)
/* Posle size bajtu z buf na socket s */
{
	while(size) {
		long int tosend = (size > NBUFFSIZE) ? NBUFFSIZE : size;

		if (send(s, buf, tosend, 0) != tosend)
			sockfail("Error writing to network");

		size -= tosend;
		(char *)buf += tosend;

		assert(size >= 0);
	}
}


void psockerror(char *msg)
{
	switch (errno) {
		case EACCES: fprintf(stderr, "%s: Permission denied.", msg); break;		case EADDRINUSE: fprintf(stderr, "%s: Address already in use.", msg); break;
		case EADDRNOTAVAIL: fprintf(stderr, "%s: Cannot assign requested address.", msg); break;
		case EAFNOSUPPORT: fprintf(stderr, "%s: Address family not supported by protocol family.", msg); break;
		case EALREADY: fprintf(stderr, "%s: Operation already in progress.", msg); break;
		case ECONNABORTED: fprintf(stderr, "%s: Software caused connection abort.", msg); break;
		case ECONNREFUSED: fprintf(stderr, "%s: Connection refused.", msg); break;
		case ECONNRESET: fprintf(stderr, "%s: Connection reset by peer.", msg); break;
		case EDESTADDRREQ: fprintf(stderr, "%s: Destination address required.", msg); break;
		case EFAULT: fprintf(stderr, "%s: Bad address.", msg); break;
		case EHOSTDOWN: fprintf(stderr, "%s: Host is down.", msg); break;
		case EHOSTUNREACH: fprintf(stderr, "%s: No route to host.", msg); break;
		case EINPROGRESS: fprintf(stderr, "%s: Operation now in progress.", msg); break;
		case EINTR: fprintf(stderr, "%s: Interrupted function call.", msg); break;
		case EINVAL: fprintf(stderr, "%s: Invalid argument.", msg); break;
		case EISCONN: fprintf(stderr, "%s: Socket is already connected.", msg); break;
		case EMFILE: fprintf(stderr, "%s: Too many open files.", msg); break;
		case EMSGSIZE: fprintf(stderr, "%s: Message too long.", msg); break;
		case ENETDOWN: fprintf(stderr, "%s: Network is down.", msg); break;
		case ENETRESET: fprintf(stderr, "%s: Network dropped connection on reset.", msg); break;
		case ENETUNREACH: fprintf(stderr, "%s: Network is unreachable.", msg); break;
		case ENOBUFS: fprintf(stderr, "%s: No buffer space available.", msg); break;
		case ENOPROTOOPT: fprintf(stderr, "%s: Bad protocol option.", msg); break;
		case ENOTCONN: fprintf(stderr, "%s: Socket is not connected.", msg); break;
		case ENOTSOCK: fprintf(stderr, "%s: Socket operation on non-socket.", msg); break;
		case EOPNOTSUPP: fprintf(stderr, "%s: Operation not supported.", msg); break;
		case EPFNOSUPPORT: fprintf(stderr, "%s: Protocol family not supported.", msg); break;
		case EPROTONOSUPPORT: fprintf(stderr, "%s: Protocol not supported.", msg); break;
		case EPROTOTYPE: fprintf(stderr, "%s: Protocol wrong type for socket.", msg); break;
		case ESHUTDOWN: fprintf(stderr, "%s: Cannot send after socket shutdown.", msg); break;
		case ESOCKTNOSUPPORT: fprintf(stderr, "%s: Socket type not supported.", msg); break;
		case ETIMEDOUT: fprintf(stderr, "%s: Connection timed out.", msg); break;
		case EWOULDBLOCK: fprintf(stderr, "%s: Resource temporarily unavailable.", msg); break;
#ifdef __WIN32__

		case EPROCLIM: fprintf(stderr, "%s: Too many processes.", msg); break;
		case HOST_NOT_FOUND: fprintf(stderr, "%s: Host not found.", msg); break;
		case NO_DATA: fprintf(stderr, "%s: Valid name, no data record of requested type.", msg); break;
		case NO_RECOVERY: fprintf(stderr, "%s: This is a non-recoverable error.", msg); break;
		case TRY_AGAIN: fprintf(stderr, "%s: Non-authoritative host not found.", msg); break;
		case WSAEDISCON: fprintf(stderr, "%s: Graceful shutdown in progress.", msg); break;

		case WSASYSNOTREADY: fprintf(stderr, "%s: Network subsystem is unavailable.", msg); break;
		case WSAVERNOTSUPPORTED: fprintf(stderr, "%s: WINSOCK.DLL version out of range.", msg); break;
		case WSANOTINITIALISED: fprintf(stderr, "%s: Successful WSAStartup not yet performed.", msg); break;
#else
		  /*		case EDISCON: fprintf(stderr, "%s: Graceful shutdown in progress.", msg); break;*/
#endif

		default: perror(msg);
	}
}


