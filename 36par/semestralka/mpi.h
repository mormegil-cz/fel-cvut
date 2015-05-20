#ifndef __MPI_H_
#define __MPI_H_

/* Error codes */
#define E_MPI_OK        0
#define E_MPI_BADARGS   1
#define E_MPI_WIN32FAIL 2
#define E_MPI_TOO_BIG   3

/* MPI datatypes */

#define MPI_CHAR (sizeof(char))
#define MPI_LONG (sizeof(long))

/* Generic constants */

#define MPI_COMM_WORLD (-1)
#define MPI_ANY_TAG    (-1)
#define MPI_ANY_SOURCE (-1)

#define MPI_TAG_BROADCAST (-2)

/* Structures */
typedef struct MPI_Status {
        int MPI_SOURCE;
        int MPI_TAG;
    } MPI_Status;

typedef struct MPI_Request {
        char *processed;
    } MPI_Request;

/* Functions */

int MPI_Init(int *argc, char ***argv);
int MPI_Finalize(void);

int MPI_Comm_rank(int comm, int *comm_rank);
int MPI_Comm_size(int comm, int *comm_size);

int MPI_Send(void *buff, size_t count, size_t type, int dest, int tag, int comm);
int MPI_Isend(void *buff, size_t count, size_t type, int dest, int tag, int comm, MPI_Request *request);

int MPI_Recv(void *buff, size_t count, size_t type, int from, int tag, int comm, MPI_Status *status);

int MPI_Bcast(void *buff, size_t count, size_t type, int broadcaster, int comm);

int MPI_Probe(int source, int tag, int comm, MPI_Status *status);
int MPI_Iprobe(int source, int tag, int comm, int *waiting, MPI_Status *status);

int MPI_Barrier(int comm);

double MPI_Wtime(void);

int MPI_ERROR_HANDLER_FAULT(int errcode);
int MPI_ERROR_HANDLER_RETURN(int errcode);

#endif

