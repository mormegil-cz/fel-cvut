#ifndef __PROCESS_POP3_H_
#define __PROCESS_POP3_H_

#include <pcap.h>

#define SERVICE_POP3 110    // POP3 well-known port
extern FILE * tmp;

void process_incoming_POP3_data(const u_char *data, size_t size);
void process_outgoing_POP3_data(const u_char *data, size_t size);

#endif // ifndef  __PROCESS_POP3_H_
