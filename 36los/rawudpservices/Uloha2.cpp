#include <iostream>
#include <iomanip>

#include <pcap.h>
#include <winsock.h>

#include "packets.h"
#include "debug.h"

using namespace std;

u_char my_mac[ETH_ALEN];
u_char my_ip[4];

/* mark a UDP port as bound (just to prevent Windows' IP stack from replying to
   packets sent to that port) */
void bind_port(u_short port)
{
    SOCKET s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    sockaddr_in sa = { AF_INET, htons(port) };
    if (bind(s, (const struct sockaddr *)&sa, sizeof(sa))) {
        cerr << "Unable to bind port " << port << endl;
        return;
    }
}

void received_packet(u_char *user, const struct pcap_pkthdr *pkt_header, const u_char *pkt_data)
{
    process_received_packet((pcap_t *)user, pkt_header, pkt_data, my_mac, my_ip);
}

int main(int argc, char *argv[])
{
    /* Tohle je jmeno interface, ktery je ve skole, kde nejde zjistit seznam zarizeni,
       takze ve skole staci zakomentovat cely ten cirkus s argc, argv (mezi //###)
       a nechat tam tuhle hodnotu. */
    char *intrf = "\\Device\\Packet_{B027D69E-2DDE-4452-9B3C-53ACC36E2147}";
    pcap_t *fp;
    char error[PCAP_ERRBUF_SIZE];
    
    srand(time(NULL));
    
    //###
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " interface" << endl;
        
        if (argc < 2) {
            typedef (*pcap_findalldevs_proc_t)(pcap_if_t **, char *);
            typedef (*pcap_freealldevs_proc_t)(pcap_if_t *);
            
            pcap_findalldevs_proc_t pcap_findalldevs_proc;
            pcap_freealldevs_proc_t pcap_freealldevs_proc;
            
            HMODULE hm;
            
            if ((hm = LoadLibrary("wpcap.dll")) == 0) {
                cerr << "Unable to load wpcap.dll! Check that WinPCap is installed!" << endl;
                return 2;
            }
            pcap_findalldevs_proc = (pcap_findalldevs_proc_t) GetProcAddress(hm, "pcap_findalldevs");
            pcap_freealldevs_proc = (pcap_freealldevs_proc_t) GetProcAddress(hm, "pcap_freealldevs");
            
            if (pcap_findalldevs_proc) {
                pcap_if_t *devlist, *d;
                int i;

                cerr << "\nSupported interfaces:" << endl;
                if (pcap_findalldevs_proc(&devlist, error) == -1) {
                    cerr << "\tUnable to retrieve the list: " << error << endl;
                    return 2;
                }
                
                for (i=0, d=devlist; d; d=d->next, i++) {
                    cerr << '\t' << d->name << " (";
                    if (d->description)
                        cerr << d->description << ")\n";
                    else
                        cerr << "No description available)\n";
                }
                if (i == 0) {
                    cerr << "\tNo interfaces found. Check that WinPCap is installed!" << endl;
                }
                if (pcap_freealldevs_proc)
                    pcap_freealldevs_proc(devlist);
            } else cerr << "\nThe installed wpcap.dll does not support device enumeration, get a new version\n"
                           "to get a list of interfaces." << endl;
            FreeLibrary(hm);
        }
        
        return 2;
    }
    intrf = argv[1];
    //###
    
    if((fp = pcap_open_live(intrf, 1024, 0, 100, error)) == NULL)
    {
        cerr << "Exiting, " << error << endl;
        return 1;
    }

    if (!get_MAC_addr(fp, my_mac)) {
        cerr << "Unable to retrieve my MAC address, exiting" << endl;
        return 1;
    }
    cout << "My MAC address: " << hex << (int)my_mac[0] << ":" << (int)my_mac[1] << ":" << (int)my_mac[2] << ":" << (int)my_mac[3] << ":" << (int)my_mac[4]<< ":" << (int)my_mac[5] << dec << endl;

    if (!retrieve_IP_addr_via_BOOTP(fp, my_mac, my_ip)) {
        cerr << "Unable to retrieve my IP address via BOOTP, exiting" << endl;
        return 1;
    }
    cout << "My IP address: ";
    print_ip_addr(cerr, my_ip);
    cout << endl;

    /* mark the server ports as listening...
       (NOT NECCESSARY, only prevents Windows' IP stack from replying to
       incoming packets with ICMP "Destination Unreachable/Port Unreachable" message) */
    WSADATA wsad;
    if (WSAStartup(0x0100, &wsad)) {
        cerr << "Unable to initialize Windows sockets\n";
    } else {
        bind_port(SERVICE_ECHO);
        bind_port(SERVICE_DISCARD);
        bind_port(SERVICE_CHARGEN);
        bind_port(SERVICE_TIME);
    }
    
    debug_output(MSG_TRACE, "Up and running\n");
    
    /* let's roll! */
    if (pcap_loop(fp, -1, received_packet, (u_char *)fp) < 0) {
        cerr << "Error in pcap_dispatch()" << endl;
        return 1;
    }

    return 0;
}
