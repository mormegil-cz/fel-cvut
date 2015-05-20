#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>

#include <winsock.h>
#include "protocol.h"
#include "fails.h"

struct timeval timeout = {1, 0};
time_t quiet_timeout = 5;
msgordinal ordinal = 0;
unsigned max_repeats = 12;

// ############################################################################
// ####    Error simulation wrapper ###########################################
// ############################################################################
#define ERROR_RATE 0.0  //  (1.0 == (practically) all packets are lost)

int PASCAL FAR e_sendto (
                       IN SOCKET s,
                       IN const char FAR * buf,
                       IN int len,
                       IN int flags,
                       IN const struct sockaddr FAR *to,
                       IN int tolen)
{
  if ((rand()/(RAND_MAX+1.0)) >= ERROR_RATE)
    return sendto(s, buf, len, flags, to, tolen);
  else {
    fprintf(stderr, "packet %dB lost\n",len);
    fflush(stderr);
    return len;
  }
}
// ############################################################################
// ############################################################################
// ############################################################################

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

void net_send(SOCKET s, const char *data, size_t len, struct sockaddr* to, int tolen)
{
	msgordinal acked   = ordinal-1; // ordinal of the last acked message
	msgset     sent    = 0;         // set of messages inside the sliding window, that have been sent
    unsigned   repeats = 0;         // number of retransmissions of the first message

	while (1) {
		switch (sent) {
			case ALLSENT: {
				// state 1: All sent, we wait for acknowledge
				struct fd_set ack;

				FD_ZERO(&ack);
				FD_SET(s, &ack);

				switch (select(0, &ack, NULL, NULL, &timeout)) {
					case 0: {
								// timeout
								assert( sent & 1 );
								sent ^= 1;	// the first message is considered as not sent
                                if (++repeats > max_repeats)
                                  simplefail("Connection lost");
#ifdef DEBUGOUT
		                        fprintf(stderr, "sender:timeout #%d\n", repeats);
#endif

								break;
							} // case select == 0
					case 1: {
								// something has arrived
								acknowledge_t ack;
								//struct sockaddr from;
								//int fromlen = sizeof(from);
								int i;

                                //memset(&from, 0, sizeof(from));

                                i = recvfrom(s, (char *)&ack, sizeof(ack), 0, NULL, NULL);
								if (i != sizeof(ack)) {
                                    fprintf(stderr, "%dB data read instead of %dB\n", i, sizeof(ack));
									sockfail("Invalid data read from network");
                                }
								/*if (fromlen != tolen || !memcmp(&from, to, fromlen)) {
                                    printf("From: %d.%d.%d.%d/%d, To: %d.%d.%d.%d/%d\n",
				                        (unsigned char)(from.sa_data[2]),
				                        (unsigned char)(from.sa_data[3]),
				                        (unsigned char)(from.sa_data[4]),
				                        (unsigned char)(from.sa_data[5]),
                                        fromlen,
				                        (unsigned char)(to->sa_data[2]),
				                        (unsigned char)(to->sa_data[3]),
				                        (unsigned char)(to->sa_data[4]),
				                        (unsigned char)(to->sa_data[5]),
                                        tolen);
									simplefail("Unknown remote host responding");
                                }*/

								for (i=1; i<=WINDOWSIZE; i++)
									if (ack.ordinal == ((msgordinal)acked+i)) {
										size_t delta = (len > DATALEN) ? DATALEN : len;

										acked = ack.ordinal;
										sent >>= i;
										len -= delta;
										data += delta;
                                        repeats = 0;

										if (!len) {
											// ALL SENT => DONE
											return;
										}
										break;
									}

								break;
							} // case select == 1
					case SOCKET_ERROR: sockfail("Error in select()");
					default: assert(0);
				} // switch select
				break;
			} // case sent == ALLSENT

			case 0: {
				// state 0: Nothing sent, we just send the first message
				message_t msg;

				if (!len) {
					// ALL SENT => DONE
					return;
				}

				msg.ordinal = ordinal;
				if (len > DATALEN) msg.datalen = DATALEN;
				else {
					int i;
					msgset mask = 2;

					msg.datalen = len;

					// mark the rest of the window as sent - there is no more data
					for (i=1; i<WINDOWSIZE; i++, mask <<= 1)
						sent |= mask;
				}
				memcpy(msg.data, data, msg.datalen);

				if (e_sendto(s, (char *)&msg, sizeof(msg), 0, to, tolen) != sizeof(msg))
					sockfail("Error writing to network");

				sent |= 1; // the first message has been sent
				break;
			} // case sent == 0

			default: {
				// state 2: We have to send some messages from the window and wait for acks from the rest
				struct fd_set ack;
				struct fd_set out;

				if (!len) {
					// ALL SENT => DONE
					return;
				}

				FD_ZERO(&ack);   FD_ZERO(&out);
				FD_SET(s, &ack); FD_SET(s, &out);

				switch (select(2, &ack, &out, NULL, &timeout)) {
					case 0: {
								// timeout
								assert( sent & 1 );
								sent ^= 1;	// the first message is considered as not sent
                                if (++repeats > max_repeats)
                                  simplefail("Connection lost");
#ifdef DEBUGOUT
		                        fprintf(stderr, "sender:timeout #%d\n", repeats);
#endif
								break;
					} // case select == 0
					case 1:
					case 2: {
								// we can either send or receive (or both)
								if (FD_ISSET(s, &ack)) {
									// we can receive
								    acknowledge_t ack;
                                    //struct sockaddr from;
								    //int fromlen = sizeof(from);
								    int i;

                                    i = recvfrom(s, (char *)&ack, sizeof(ack), 0, NULL, NULL);
								    if (i != sizeof(ack)) {
                                        fprintf(stderr, "%dB data read instead of %dB\n", i, sizeof(ack));
									    sockfail("Invalid data read from network");
                                    }

									/*if (fromlen != tolen || !memcmp(&from, to, fromlen))
										simplefail("Unknown remote host responding");*/

									for (i=1; i<=WINDOWSIZE; i++)
										if (ack.ordinal == ((msgordinal)acked+i)) {
											size_t delta = (len > DATALEN) ? DATALEN : len;

											acked = ack.ordinal;
											sent >>= i;
											len -= delta;
											data += delta;
                                            repeats = 0;
											break;
										}
								} // if we can receive

								if (FD_ISSET(s, &out)) {
									// we can send
									message_t msg;
                                    msgset    mask = 1;
                                    int       i;
                                    size_t    remaining = len;
                                    char *p = (char *) data;

				                    if (!len) {
					                    // ALL SENT => DONE
					                    return;
                                    }

                                    for (i = 0; i<WINDOWSIZE; i++, mask<<=1, remaining -= DATALEN, p+= DATALEN)
                                        if (!(sent & mask)) {
				                            msg.ordinal = ordinal;
				                            if (remaining > DATALEN) msg.datalen = DATALEN;
				                            else {
				                    	        int j;
				                    	        msgset mask = 2;

				                    	        msg.datalen = remaining;

				                    	        // mark the rest of the window as sent - there is no more data
				                    	        for (j=i+1; i<WINDOWSIZE; j++, mask <<= 1)
				                    	    	    sent |= mask;

                                                if (!msg.datalen) break;
				                            }
				                            memcpy(msg.data, p, msg.datalen);

				                            if (e_sendto(s, (char *)&msg, sizeof(msg), 0, to, tolen) != sizeof(msg))
				                    	        sockfail("Error writing to network");

                                            sent |= mask; // the message has been sent
                                            break;
                                        } // if (!(sent & mask))
								} // if we can send

								break;
					} // case select == 1 or select == 2
					case SOCKET_ERROR: sockfail("Error in select()");
					default: assert(0);
				} // switch select
			} // case sent default
		} // switch sent
	} // while(1)
} // net_send

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

void net_receive(SOCKET s, const char *data, size_t len)
{
  message_t msg;
  acknowledge_t ack;
  unsigned long offset;
  msgset recved = 0;         // set of messages inside the sliding window, that have been received
  time_t quiettimeout;
  struct net

  while (len) {
    // (state 0) wait for message receival
    offset = recvfrom(s, (char *)&msg, sizeof(msg), 0, NULL, NULL);
    if (offset != sizeof(msg)) {
        fprintf(stderr, "%dB data read instead of %dB\n", offset, sizeof(msg));
        sockfail("Invalid data read from network");
    }

    if ((msgordinal)(msg.ordinal - ordinal) >= WINDOWSIZE &&
        (msgordinal)(ordinal - msg.ordinal) >= WINDOWSIZE) continue; // ignore messages outside the window

    offset = (msgordinal)(msg.ordinal - ordinal) * DATALEN;
    if (offset+msg.datalen > len || msg.datalen > DATALEN || msg.datalen == 0)
        simplefail("Corrupt data read from network");

    memcpy((void *)(data+offset), (void *)msg.data, msg.datalen);
    recved |= 1 << (msgordinal)(msg.ordinal - ordinal);

    // (state 1) acknowledge message receipt
    ack.ordinal = msg.ordinal;
    if (e_sendto(s, (char *)&ack, sizeof(ack), 0, from, fromlen) != sizeof(ack))
        sockfail("Error writing to network");

    // shift the sliding window if possible
    while (recved & 1) {
      offset = len > DATALEN ? DATALEN : len;
      data += offset;
      len -= offset;

      recved >>= 1;
    }

    if ((msg.datalen < DATALEN && len != 0)) simplefail("Corrupt data read from network");
  } // while (len)

#ifdef DEBUGOUT
  fprintf(stderr, "server: closing connection\n");
#endif

  // (state 2) connection close
  quiettimeout = time(NULL) + quiet_timeout;
  while (time(NULL) < quiettimeout) {
    struct fd_set data;
    struct timeval seltimeout;
    seltimeout.tv_sec = quiettimeout - time(NULL);
    seltimeout.tv_usec = 0;

    FD_ZERO(&data);
    FD_SET(s, &data);

    switch (select(1, &data, NULL, NULL, &seltimeout)) {
        case 0: // timeout ==> connection close
#ifdef DEBUGOUT
                fprintf(stderr, "server:quiet timeout done\n");
#endif
                return;
        case 1: // data received ==> acknowledge
                if ((msgordinal)(ordinal - msg.ordinal) >= WINDOWSIZE) continue; // ignore messages outside the window

                ack.ordinal = msg.ordinal;
                if (e_sendto(s, (char *)&ack, sizeof(ack), 0, from, fromlen) != sizeof(ack))
                    sockfail("Error writing to network");
                break;

        case SOCKET_ERROR: sockfail("Error in select()");
        default: assert(0);
    }
  } // while (time < quiettimeout)
} // net_receive

