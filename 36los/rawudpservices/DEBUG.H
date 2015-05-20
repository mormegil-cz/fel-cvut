#ifndef _DEBUG_H_
#define _DEBUG_H_

#include <iostream>
#include <pcap.h>

#define MSG_TRACE 0

void debug_output(int severity, const char *msg);
void print_ip_addr(std::ostream f, const u_char ip_addr[4]);

#endif	// ifndef _DEBUG_H_
