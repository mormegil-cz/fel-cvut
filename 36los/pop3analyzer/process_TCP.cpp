#include <pcap.h>
#include <assert.h>

#include <set>
#include <stdexcept>
#include <iostream>

#include "util.h"
#include "stats.h"
#include "process_IP.h"
#include "process_TCP.h"
#include "filter.h"
#include "process_POP3.h"

using namespace std;

bool seqnum_less(u_int a, u_int b)
{
    return signed(a - b) < 0;
}

bool seqnum_lessequal(u_int a, u_int b)
{
    return signed(a - b) <= 0;
}

// ************** TCPPacket **************
class TCPPacket {
private:
    TCPHeader header;
    u_char *data;
    size_t datasize;
public:
    TCPPacket(const TCPHeader &header, const u_char *data, size_t datasize) : data(0), header(header), datasize(0) {
	assert(data);
        this->data = new u_char[datasize];
	this->datasize = datasize;
        memcpy(this->data, data, datasize);
    }
    
    TCPPacket(const TCPPacket &p) : data(0), header(p.header), datasize(0) { // copy constructor
	assert(p.data);
        data = new u_char[p.datasize];
        datasize = p.datasize;
        memcpy(data, p.data, datasize);
    }

    ~TCPPacket() {
	datasize = 0;
	delete[] data;
	data = 0;
    }
    
    const TCPHeader &get_header() const { return header; };
    const u_char *get_data() const { return data; };
    size_t get_datasize() const { return datasize; };
    
    bool operator < (const TCPPacket &to) const {
        return seqnum_less(header.seqnum(), to.header.seqnum());
    };

    TCPPacket& operator = (const TCPPacket &from) { // assignment operator
	if (this == &from) return *this;
	assert(from.data);
	datasize = 0;
	delete[] data;
	data = 0;
	datasize = from.datasize;
	data = new u_char[datasize];
	memcpy(data, from.data, datasize);
	return *this;
    }
};

// *********** TCPConnectionHalf ***********

class TCPConnectionHalf {
private:
    enum {
        TCPS_CLOSED, TCPS_LISTEN, TCPS_SYN_RCVD, TCPS_SYN_SENT,
            TCPS_ESTABLISHED, TCPS_FIN_WAIT1, TCPS_CLOSE_WAIT,
            TCPS_FIN_WAIT2, TCPS_CLOSING, TCPS_LAST_ACK, TCPS_TIME_WAIT
    } state;
    
    u_int seq, ack;
    u_int snd_una, snd_nxt, snd_wnd, snd_wl1, snd_wl2, snd_iss;
    u_int rcv_nxt, rcv_wnd, rcv_irs;

    u_int fin_seq;
    bool fin_acked;
    
    bool passive_connection;
    
    set<TCPPacket> *receive_window;

    void advance_snd_una(u_int to_value) {
        // TODO: advance_snd_una (?)
        // any segments on the retransmission queue which are thereby acknowledged should be removed.

        snd_una = to_value;
    }

    void segment_into_receive_window(const TCPHeader *h, const u_char *data, size_t datasize) {
	TCPPacket p(*h, data, datasize);
	receive_window->insert(p);

	while(!receive_window->empty()) {  // break inside
	    TCPPacket first = *receive_window->begin();
	    u_int seqnum = first.get_header().seqnum();
	    size_t seg_w_start, seg_w_len;

	    if (seqnum_less(seqnum, rcv_nxt)) {
                cout << "Segment crossing the window boundary (lower -- partial retransmission)" << endl;
		seg_w_start = rcv_nxt - seqnum;
		retransmitted_TCP_packets++;
	    }
	    else seg_w_start = 0;

	    if (seqnum_lessequal(seqnum + datasize, rcv_nxt + rcv_wnd)) seg_w_len = datasize - seg_w_start;
	    else {
		cout << "Segment crossing the window boundary (upper)" << endl;
		seg_w_len = rcv_nxt + rcv_wnd - seqnum + datasize - 1;
	    }
	    
	    if (seg_w_start > datasize) {
		cout << "Completely old TCP packet (retransmission), discarded" << endl;
		receive_window->erase(receive_window->begin());
		retransmitted_TCP_packets++;
		discarded_TCP_packets++;
		return;
	    }
	    
	    if (seqnum + seg_w_start == rcv_nxt) {
		process_packet(first, seg_w_start, seg_w_len);
		receive_window->erase(receive_window->begin());
	    } else break;
	}
    }

    // process received packet
    void process_packet(const TCPPacket &p, size_t start, size_t len) {
	const TCPHeader *h = &(p.get_header());
	//cout << "Processing " << len << "B ";
	//h->dump();
	//cout << "Current pointers: SND.NXT = " << snd_nxt << ", SND.UNA = " << snd_una << ", RCV.NXT = " << rcv_nxt << ", RCV.WND = " << rcv_wnd << endl;
	if (h->rst()) {
	    switch(state) {
	    case TCPS_SYN_RCVD:
		if (passive_connection) {
		    state = TCPS_LISTEN;
		    cout << "Connection attempt cancelled by RST" << endl;
		} else {
		    state = TCPS_CLOSED;
		    cout << "Connection refused" << endl;
		}
		// "delete the TCB"
		break;

	    case TCPS_ESTABLISHED:
	    case TCPS_FIN_WAIT1:
	    case TCPS_FIN_WAIT2:
	    case TCPS_CLOSE_WAIT:
		cout << "Connection reset by peer" << endl;
		state = TCPS_CLOSED;
		// "delete the TCB"
		break;

	    case TCPS_CLOSING:
	    case TCPS_LAST_ACK:
	    case TCPS_TIME_WAIT:
		state = TCPS_CLOSED;
		cout << "Connection closed by RST" << endl;
		// "delete the TCB"
		break;

	    default:
		assert(0);
	    }
	    return;
	}
	
	// (missing "check security and precedence")
	
	if (h->syn()) {
	    cout << "SYN received inside the window, connection closed" << endl;
	    state = TCPS_CLOSED;
	    // "delete the TCB"
	    discarded_TCP_packets++;
	    return;
	}
	
	if (!h->ack()) {
	    cout << "non-ACK segment received inside the window, discarded" << endl;
	    discarded_TCP_packets++;
	    return;
	}

	rcv_wnd = h->window();

	switch(state) {
	case TCPS_SYN_RCVD:
	    if (seqnum_lessequal(snd_una, h->acknum()) && seqnum_less(h->acknum(), snd_nxt)) {
		cout << "Connection established" << endl;
		state = TCPS_ESTABLISHED;
	    } else {
		cout << "non-acceptable ACK segment received inside the window" << endl;
		discarded_TCP_packets++;
		return;
	    }
	    //break;    // ??????

	case TCPS_ESTABLISHED:
	case TCPS_FIN_WAIT1:
	case TCPS_FIN_WAIT2:
	case TCPS_CLOSE_WAIT:
	case TCPS_CLOSING:
	case TCPS_TIME_WAIT:
	    if (seqnum_less(snd_una, h->acknum()) && seqnum_lessequal(h->acknum(), snd_nxt)) {
		advance_snd_una(h->acknum());
		if (seqnum_less(snd_wl1, h->seqnum()) ||
		    snd_wl1 == h->seqnum() && seqnum_lessequal(snd_wl2, h->acknum())) {
		    snd_wnd = h->window();
		    snd_wl1 = h->seqnum();
		    snd_wl2 = h->acknum();
		}
	    } else {
#ifndef NDEBUG
		if (!seqnum_less(snd_una, h->acknum())) {
		    cout << "Duplicate ACK, ignored" << endl;
		} else {
		    cout << "ACKing something I have not sent, ACK ignored" << endl;
		}
		break;
#endif
	    }
	    
	    switch(state) {
	    case TCPS_FIN_WAIT1:
		if (!fin_acked && seqnum_lessequal(fin_seq, h->acknum())) {
		    cout << "FIN acknowledged, going to FIN-WAIT-2" << endl;
		    state = TCPS_FIN_WAIT2;
		    fin_acked = true;
		}
		
	    case TCPS_FIN_WAIT2:
		// "if the retransmission queue is empty, the user's CLOSE can be acknowledged"
		break;
		
	    case TCPS_CLOSING:
		if (seqnum_lessequal(fin_seq, h->acknum())) {
		    cout << "FIN acknowledged, going to TIME-WAIt" << endl;
		    state = TCPS_TIME_WAIT;
		    fin_acked = true;
		} else {
		    cout << "Arriving segment when CLOSING, discarded" << endl;
		    discarded_TCP_packets++;
		}
		break;
		
	    case TCPS_LAST_ACK:
		if (seqnum_lessequal(fin_seq, h->acknum())) {
		    cout << "FIN acknowledged, connection closed" << endl;
		    // "delete the TCB"
		    fin_acked = true;
		    state = TCPS_CLOSED;
		} else {
		    cout << "Arriving segment when LAST-ACK, discarded" << endl;
		    discarded_TCP_packets++;
		}
		break;
		
	    case TCPS_TIME_WAIT:
		cout << "Retransmission (we are in TIME-WAIT), discarded" << endl;
		discarded_TCP_packets++;
		retransmitted_TCP_packets++;
		break;
	    }
	    break;
	    
	    default:
		assert(0);
	}

	if (h->urg()) {
	    // (missing check for state, URG should be processed only in ESTABLISHED, FIN_WAIT1, FIN_WAIT2 states)
	    cout << "Urgent data, pointer = " << h->urgent_ptr() << endl;
	}

	// process data
	if (len) {
	    switch(state) {
	    case TCPS_ESTABLISHED:
	    case TCPS_FIN_WAIT1:
	    case TCPS_FIN_WAIT2:
		// process packet data
		//cout << "Packet data delivered to user" << endl;
		if (passive_connection)
		    process_incoming_POP3_data(p.get_data() + start, len);
		else
		    process_outgoing_POP3_data(p.get_data() + start, len);
		rcv_nxt += len;
		// rcv_wnd .. ???
		break;
		
	    case TCPS_CLOSE_WAIT:
	    case TCPS_CLOSING:
	    case TCPS_LAST_ACK:
	    case TCPS_TIME_WAIT:
		// should not happen...
		// ???
		//ignored_TCP_packets++;
		break;
	    }
	}

	if (h->fin()) {
	    switch(state) {
	    case TCPS_CLOSED:
	    case TCPS_LISTEN:
	    case TCPS_SYN_SENT:
		cout << "Arriving FIN when CLOSED, LISTEN, or SYN-SENT, discarded" << endl;
		discarded_TCP_packets++;
		return;

	    case TCPS_SYN_RCVD:
	    case TCPS_ESTABLISHED:
		cout << "FIN received, going to CLOSE-WAIT" << endl;
		state = TCPS_CLOSE_WAIT;
		break;

	    case TCPS_FIN_WAIT1:
		if (fin_acked) {
		    state = TCPS_TIME_WAIT;
		    cout << "FIN received in FIN-WAIT-1, going to TIME-WAIT" << endl;
		}
		else {
		    state = TCPS_CLOSING;
		    cout << "FIN received in FIN-WAIT-1, going to CLOSING" << endl;
		}
		break;

	    case TCPS_FIN_WAIT2:
		state = TCPS_TIME_WAIT;
		cout << "FIN received in FIN-WAIT-2, going to TIME-WAIT" << endl;
		break;
	    }

	    cout << "Connection is closing..." << endl;
	    rcv_nxt++;
	}
    }
    
public:
    TCPConnectionHalf() {
        state = TCPS_CLOSED;
        passive_connection = false;
	fin_acked = false;
	receive_window = new set<TCPPacket>; 
	snd_una = 0;
    }
    
    void listen()
    {
        state = TCPS_LISTEN;
        passive_connection = true;
    }
    
    void outgoing_packet(const TCPHeader *h, const u_char *data, size_t datasize) {
        // TODO: outgoing_packet ..????
        
        // TCPS_SYN_RCVD_1 : check LISTEN/arriving segment, or SYN_SENT/arriving segment
        // TCPS_ESTABLISHED_1 : check SYN_SENT/arriving segment

	switch(state) {
	case TCPS_CLOSED:
	case TCPS_LISTEN:
	    if (!h->syn()) {
		cout << "WARNING: SYN segment expected to be sent!" << endl;
		return;
	    }
	    snd_iss = h->seqnum();
	    snd_una = snd_iss;
	    snd_nxt = snd_iss + 1;
	    state = TCPS_SYN_SENT;

	    //cout << "Outgoing first SYN packet, ISS = SND.UNA = " << snd_iss << ", SND.NXT = " << snd_nxt << endl;

	    if (seqnum_less(snd_nxt, h->seqnum() + datasize + 1)) {
		snd_nxt = h->seqnum() + datasize + 1;
	    }
	    return;

	case TCPS_SYN_SENT:
	case TCPS_SYN_RCVD:
	    state = TCPS_ESTABLISHED;

	case TCPS_ESTABLISHED:
	case TCPS_CLOSE_WAIT:
	    {
		rcv_nxt = h->acknum();
		
		u_int seq = h->seqnum() + datasize;
		
		if (h->syn()) {
		    seq++;
		    if (!snd_una) snd_una = h->seqnum();	// ...?? not exactly correct, but... :)
		}
		if (h->fin()) {
		    seq++;
		    fin_seq = h->seqnum();
		}
		
		if (seqnum_less(snd_nxt, seq)) snd_nxt = seq;
		
		break;
	    }

	default:
	    assert(0);

	}

	// ... ???

	// ????
	if (state == TCPS_CLOSED) state = TCPS_ESTABLISHED;
    }
    
    void incoming_packet(const TCPHeader *h, const u_char *data, size_t datasize) {
        switch(state) {
        case TCPS_CLOSED:
            if (h->rst()) break;   // discard incoming RST
            cout << "Incoming packet when CLOSED, discarded" << endl;
            discarded_TCP_packets++;
            break;
            
        case TCPS_LISTEN:
            if (h->rst()) break;   // discard incoming RST
            if (h->ack()) {
                cout << "Incoming ACK when LISTEN, discarded" << endl;
                discarded_TCP_packets++;
                break;
            }
            if (!(h->syn())) {
                cout << "Incoming non-SYN when LISTEN, discarded" << endl;
                discarded_TCP_packets++;
                break;
            }
            // (missing "check security")
            // (missing "check PRC")
            rcv_nxt = h->seqnum() + 1;
	    rcv_irs = h->seqnum();
	    // TODO: Process the data part of the segment now,
	    //	    SYN and ACK should not be processed again...!!!
	    //	    also, the seqnum should be now +1
            state = TCPS_SYN_RCVD;
	    if (datasize) {
		throw logic_error("Incoming SYN with data in LISTEN --- UNSUPPORTED!!!");
	    }
	    //segment_into_receive_window(h, data, datasize);
            break;
            
        case TCPS_SYN_SENT:
            {
                bool ack_acceptable = false;
                
                if (h->ack()) {
                    if (h->acknum() <= snd_iss || h->acknum() > snd_nxt) {
                        if (!(h->rst())) cout << "Incoming ACK when SYN-SENT, discarded" << endl;
                        discarded_TCP_packets++;
                        return;
                    }
                    if (snd_una <= h->acknum() && h->acknum() <= snd_nxt) {
                        ack_acceptable = true;
                        rcv_wnd = h->window();
                    }
                }
                
                if (h->rst()) {
                    if (ack_acceptable) {
                        cout << "Connection reset by peer" << endl;
                        state = TCPS_CLOSED;
                        // "delete the TCB"
                    } else discarded_TCP_packets++;
                    return;
                }
                
                // (missing "check security and precedence")
                
                if (h->syn()) {
                    assert(ack_acceptable || (!h->ack() && !h->rst()));
                    
                    rcv_nxt = h->seqnum() + 1;
                    rcv_irs = h->seqnum();

                    advance_snd_una(h->acknum());
                    
                    if (snd_una > snd_iss) {
                        state = TCPS_ESTABLISHED;
			cout << "TCP connection established" << endl;
                    } else {
                        state = TCPS_SYN_RCVD;
			cout << "SYN received, going to SYN-RECEIVED" << endl;
                    }
		    // TODO: continue processing with URG...???
    		    if (datasize) {
			throw logic_error("incoming SYN with data in LISTEN --- UNSUPPORTED!!!");
		    }
                    return;
                } else {
		    cout << "Non-SYN, non-RST incoming when SYN-SENT, discarded" << endl;
		    discarded_TCP_packets++;
		    return;
		}
            }
            
        case TCPS_SYN_RCVD:
        case TCPS_ESTABLISHED:
        case TCPS_FIN_WAIT1:
        case TCPS_FIN_WAIT2:
        case TCPS_CLOSE_WAIT:
        case TCPS_CLOSING:
        case TCPS_LAST_ACK:
        case TCPS_TIME_WAIT:
            {
                bool acceptable;
                
                if (datasize) {
                    if (rcv_wnd) {
                        acceptable =
                            (rcv_nxt <= h->seqnum() && h->seqnum() < rcv_nxt + rcv_wnd) ||
                            (rcv_nxt <= h->seqnum() + datasize - 1 && h->seqnum() + datasize - 1 < rcv_nxt + rcv_wnd);
                    } else {
                        acceptable = false;
                    }
                } else {
                    // datasize == 0
                    if (rcv_wnd) {
                        acceptable = rcv_nxt <= h->seqnum() && h->seqnum() < rcv_nxt + rcv_wnd;
                    } else {
                        acceptable = (h->seqnum() == rcv_nxt) || (rcv_nxt == 0);
                    }
                }
                
                if (!acceptable) {
                    if (!h->rst()) {
                        cout << "Nonacceptable segment, discarded" << endl;
                    }
                    discarded_TCP_packets++;
                    return;
                }
                
		segment_into_receive_window(h, data, datasize);
		break;
            }
            
        default:
	    assert(0);
        }
    }
};

// ************** TCPHeader **************

TCPHeader::TCPHeader(const u_char *from_data, size_t size) {
    assert(size >= sizeof(data));
    memcpy(&data, from_data, sizeof(data));
    repair_endians();
    
    // process options
    assert(size >= (data_ofs() << 2));
    size = data_ofs() << 2;
    const u_char *opt = from_data + sizeof(data);
    size_t remopts = size - sizeof(data);
    
    max_seg_size = -1;
    
    while (remopts && *opt) {
        switch(*opt) {
        case 1: // "no operation" option
            opt++;
            remopts--;
            break;
        case 2: // "maximum segment size" option
            memcpy(&max_seg_size, opt + 2, sizeof(max_seg_size));
            REPAIR_SHORT(max_seg_size);
        default:
            assert(remopts >= opt[1]);
            remopts -= opt[1];
            opt += opt[1];
        }
    }
}

void TCPHeader::repair_endians() {
    REPAIR_SHORT(data.source_port);
    REPAIR_SHORT(data.dest_port);
    REPAIR_LONG(data.seqnum);
    REPAIR_LONG(data.acknum);
    REPAIR_SHORT(data.window);
    REPAIR_SHORT(data.checksum);
    REPAIR_SHORT(data.urgent_ptr);
}

void TCPHeader::dump() const {
    //cout << "TCP Header:" << endl;
    //cout << "\tSource port: " << source_port() << endl;
    //cout << "\tDestination port: " << dest_port() << endl;
    cout << "\tSeq/Ack number: " << seqnum() << '/' << acknum() << endl;
    //cout << "\tHeader size: " << (data_ofs() << 2) << endl;
    cout << "\tFlags: ";
    if (urg()) cout << "URG "; else cout << "--- ";
    if (ack()) cout << "ACK "; else cout << "--- ";
    if (psh()) cout << "PSH "; else cout << "--- ";
    if (rst()) cout << "RST "; else cout << "--- ";
    if (syn()) cout << "SYN "; else cout << "--- ";
    if (fin()) cout << "FIN "; else cout << "--- ";
    cout << endl;
    //cout << "\tWindow: " << window() << endl;
    //cout << "\tChecksum: " << hex << checksum() << dec << endl;
    //cout << "\tUrgent pointer: " << urgent_ptr() << endl;
    if (maximum_seg_size() >= 0) cout << "\tMaximum segment size: " << maximum_seg_size() << endl;
}

TCPConnectionHalf server_halfconn, client_halfconn;

// ******** main processing function ********
void process_TCP_packet(IPHeader iph, const u_char *data, size_t length)
{
    TCPHeader tcph(data, length);
    const u_char *p = data;
    size_t tcphlen;
    
    assert(length >= TCP_HEADER_LEN);  // every fragment must contain at least the IP header
    
    tcphlen = tcph.data_ofs() << 2;
    assert(length >= tcphlen);      // check some basic validity constraints
    p += tcphlen;
    length -= tcphlen;
    
    total_TCP_packets++;
    
    if (!packet_filter.filter_enabled()) {
        // filter not yet set up
        if (tcph.dest_port() != SERVICE_POP3) {
            // not POP3 request
            ignored_TCP_packets++;
#ifndef NDEBUG
            cerr << "Not a POP3 request, ignored" << endl;
#endif
            return;
        }
        
        // set up the filter
        packet_filter.setup_filter(iph.source(), iph.dest(), tcph.source_port(), tcph.dest_port());
        server_halfconn.listen();
    }
    
    if (!packet_filter.pass(iph, tcph)) {
        ignored_TCP_packets++;
#ifndef NDEBUG
        cerr << "Not a part of the monitored connection, ignored" << endl;
#endif
        return;
    }

    //iph.dump();
    //tcph.dump();

    cout << length << "B TCP packet: ";
    print_ip_addr(cout, iph.source());
    cout << ":" << tcph.source_port() << " --> ";
    print_ip_addr(cout, iph.dest());
    cout << ":" << tcph.dest_port() << endl;
    tcph.dump();
    
    if (packet_filter.get_client() == iph.source()) {
	client_halfconn.outgoing_packet(&tcph, p, length);
	server_halfconn.incoming_packet(&tcph, p, length);
    } else {
	server_halfconn.outgoing_packet(&tcph, p, length);
	client_halfconn.incoming_packet(&tcph, p, length);
    }

    cout << endl;
}
