#include <stdio.h>
#include <assert.h>
#include <pcap.h>

#include <stdexcept>
#include <iostream>

#include "process_IP.h"
#include "stats.h"
#include "POP_stream.h"

using namespace std;

FILE *tmp;

extern void process_packet_data(const u_char *, size_t size);

/**********************************************************************/

static void dispatcher_handler(u_char *, const struct pcap_pkthdr *, const u_char *);

int main(int argc, char **argv) {

    pcap_t *fp;
    char errbuf[PCAP_ERRBUF_SIZE];

    if(argc != 2){
        printf("usage: %s filename", argv[0]);
        return -1;
    }

    /* Open a capture file */
    if ( (fp = pcap_open_offline(argv[1], errbuf) ) == NULL)
    {
        fprintf(stderr,"\nError opening dump file\n");
        return -1;
    }

    /* read and dispatch packets until EOF is reached */

	tmp = fopen ("tempPOP3.tmp", "w");

    try {
      pcap_loop(fp, 500, dispatcher_handler, NULL);
    }
    catch (const exception &e) {
	cerr << "Error: " << e.what() << endl;
	return 1;
    }

    show_statistics();

   char buf[1024];

	fclose(tmp);
   

	tmp = fopen ("tempPOP3.tmp", "r");
	FILE * out;
	out = fopen ("POP3out.txt", "w");


	fgets (buf, 1024, tmp);
      
   if (!strncmp ( OK, buf, sizeof(OK)-1 )){
      fprintf (out,"Spojeni uspesne navazano...\n");
   } else  {
      fprintf (out,"Spojeni NEnavazano!\n");   
      return 2;
   };//if
   
   
   //smycka: prikaz - odezva


   fgets (buf, 1024, tmp);

   
   while(1) {
      //prikaz
      if (!parseCom(buf,out)) {
         fprintf (out,"Poslan neznamy prikaz\n"); 
         break; 
      }
      //odezva
      if (!parseRe(buf,tmp,out)) { 
		 fprintf (out,"Konec komunikace\n"); 
		 break;
         
         
      }
   }//while


   fclose(tmp);	
   fclose(out);


    return 0;
}

/**********************************************************************/

#define ETH_HEADER_SIZE 14

static void process_Ethernet_frame(const u_char *data, size_t size)
{
    #pragma pack(push, 1)
    typedef struct {
	u_char  from[6];
	u_char  to[6];
	u_short protocol;
    } eth_header_t;
    #pragma pack(pop)

    const eth_header_t *eh = (const eth_header_t *)data;

    assert(size >= ETH_HEADER_SIZE);
    if (eh->protocol == 8) {
	process_IP_fragment(data + ETH_HEADER_SIZE, size - ETH_HEADER_SIZE);
#ifndef NDEBUG
    } else {
	cerr << "Unknown Ethernet protocol, ignored\n" << endl;
#endif
    }
}

static void dispatcher_handler(u_char *,
                        const struct pcap_pkthdr *header, const u_char *pkt_data)
{
    process_Ethernet_frame(pkt_data, header->len);
}
