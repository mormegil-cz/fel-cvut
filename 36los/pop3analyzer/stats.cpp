#include <iostream>

#include "stats.h"

unsigned total_IP_fragments = 0;
unsigned total_IP_packets = 0;
unsigned total_TCP_packets = 0;
unsigned ignored_TCP_packets = 0;
unsigned discarded_TCP_packets = 0;
unsigned retransmitted_TCP_packets = 0;

using namespace std;

void show_statistics()
{
    cout << "STATISTICS:" << endl;
    cout << "\tIP packets: " << total_IP_packets << " in " << total_IP_fragments << " fragments";
    if (total_IP_packets) cout << " (" << (100.0f * total_IP_fragments / total_IP_packets - 100.0f) << "% fragmentation)";
    cout << endl;
    cout << "\tTCP packets: " << total_TCP_packets << " (" << ignored_TCP_packets << " of them ignored)" << endl;
    cout << "\tDiscarded TCP packets: " << discarded_TCP_packets << endl;
    cout << "\t\tRetransmitted: " << retransmitted_TCP_packets << endl;

    cout << endl;
}
