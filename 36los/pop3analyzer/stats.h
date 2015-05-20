#ifndef __STATS_H_
#define __STATS_H_

extern unsigned total_IP_fragments;
extern unsigned total_IP_packets;
extern unsigned total_TCP_packets;
extern unsigned ignored_TCP_packets;
extern unsigned discarded_TCP_packets;
extern unsigned retransmitted_TCP_packets;

void show_statistics();

#endif // ifndef __STATS_H_
