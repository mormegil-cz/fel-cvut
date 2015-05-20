#ifndef __PROCESS_TCP_H_
#define __PROCESS_TCP_H_

#include <pcap.h>

#include "process_IP.h"

void process_TCP_packet(IPHeader iph, const u_char *data, size_t length);

#define TCP_HEADER_LEN 20

// TCP packet header
class TCPHeader {
private:
    #pragma pack(push, 1)
    struct {
        u_short source_port;
        u_short dest_port;
        u_int   seqnum;
        u_int   acknum;
	u_char  data_ofs_res;
	u_char  flags;
        u_short window;
        u_short checksum;
        u_short urgent_ptr;
        /* u_int options:? */
    } data;
    #pragma pack(pop)
    short max_seg_size;

    inline void repair_endians();

public:
    TCPHeader(const u_char *from_data, size_t size);

    // note: the compiler-generated copy constructor is sufficient

    /*TCPHeader(const TCPHeader &from) {	// the copy constructor
	memcpy(&data, &from.data, sizeof(data));
	max_seg_size = from.max_seg_size;
    }

    TCPHeader & operator = (const TCPHeader &from) {
	memcpy(&data, &from.data, sizeof(data));
	max_seg_size = from.max_seg_size;
	return *this;
    }*/

    void dump() const;

    u_short source_port() const { return data.source_port; };
    u_short dest_port() const { return data.dest_port; };
    u_int   seqnum() const { return data.seqnum; };
    u_int   acknum() const { return data.acknum; };
    u_char  data_ofs() const { return u_char(data.data_ofs_res >> 4); };
    bool    urg() const { return (data.flags & 0x20) != 0; };
    bool    ack() const { return (data.flags & 0x10) != 0; };
    bool    psh() const { return (data.flags & 0x08) != 0; };
    bool    rst() const { return (data.flags & 0x04) != 0; };
    bool    syn() const { return (data.flags & 0x02) != 0; };
    bool    fin() const { return (data.flags & 0x01) != 0; };
    u_short window() const { return data.window; };
    u_short checksum() const { return data.checksum; };
    u_short urgent_ptr() const { return data.urgent_ptr; };

    short   maximum_seg_size() const { return max_seg_size; };
};

/*
 0                   1                   2                   3   
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |          Source Port          |       Destination Port        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                        Sequence Number                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                    Acknowledgment Number                      |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |  Data |           |U|A|P|R|S|F|                               |
   | Offset| Reserved  |R|C|S|S|Y|I|            Window             |
   |       |           |G|K|H|T|N|N|                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |           Checksum            |         Urgent Pointer        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                    Options                    |    Padding    |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                             data                              |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*/

#endif // ifndef  __PROCESS_TCP_H_
