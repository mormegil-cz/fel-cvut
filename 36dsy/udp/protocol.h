#ifndef __PROTOCOL_H
#define __PROTOCOL_H

#include <stdlib.h>
#include <time.h>
#include "common.h"

#define DATALEN 123			// length of data inside one message
#define WINDOWSIZE 4		// length of sliding window (messages)

typedef unsigned long msgordinal;	// message ordinal number (!unsigned!)
typedef unsigned char msgset;		// set of messages inside the sliding window

#define ALLSENT ((1 << WINDOWSIZE)-1)	// msgset for "all messages from window sent"

typedef struct messageTAG {		// one message
	msgordinal ordinal;		    // message ordinal number
	size_t	   datalen;	       	// length of the valid data
	char       data[DATALEN];	// message data
} message_t;

typedef struct acknowledgeTAG { // acknowledgement message
	msgordinal ordinal;			// ordinal number of the message that is being acknowledged
} acknowledge_t;

extern struct timeval timeout;
extern time_t quiet_timeout;
extern msgordinal ordinal;
extern unsigned max_repeats;

void net_send(SOCKET s, const char *data, size_t len, struct sockaddr* to, int tolen);
void net_receive(SOCKET s, const char *data, size_t len);

#endif // ifndef __PROTOCOL_H
