/* (c) 2002-2009 Tom Schouten

   Part of this file is adapted from PacketForth (c) Tom Schouten,
   which is licenced under the GPL.  However, this file is part of
   core libprim and licenced under the LGPL. */

#include "config.h"
// #include <ulimit.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "port.h"
#include <leaf/bytes.h>
#include <leaf/error.h>




// UNIX socket code - adapted from PF.
#include <stdio.h>
#include <errno.h>

#include <fcntl.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>

/* An all-in-one constructor for TCP/UNIX server/client socket. */


// #define ERROR(...) { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); return -1; } // FIXME: leaf error handling

#define ERROR(...) { leaf_raise_error(ctx, "port", __VA_ARGS__); return -1; }


union addr {
    struct sockaddr_un un;
    struct sockaddr_in in;
};

/* This produces file descriptors.  It's a bit awkward to work with
   streams for listening sockets.  Use these in conjunction with
   fdopen to create port abstractions. */

// ((host port) mode kind -- stream )
int fd_socket(leaf_ctx *ctx,
              const char *sockname,  // hostname | filesystem node
              int port_number,       // only for TCP sockets
              int kind) {

    char *action;

    int sockfd = -1;
    int intarg = -1; // for setsockopt
    struct hostent *hp = 0; // name lookup
    union addr address;  // target/listen address
    socklen_t addrlen = 0;

    memset(&address, 0, sizeof(struct sockaddr_in));

    // UNIX socket
    if (kind & PORT_SOCKET_UNIX) {

	// create UNIX socket
 	sockfd = socket(PF_UNIX, SOCK_STREAM,0);
	if (sockfd == -1) ERROR("can't create socket");
	address.un.sun_family = AF_UNIX;
 	strcpy(address.un.sun_path, sockname); // FIXME: buffer overrun UNIX_PATH_MAX
	addrlen = sizeof(address.un.sun_family) 
	    + strlen(address.un.sun_path) + 1;
    }

    // TCP/UDP socket
    else{
        // invalid port number.
        if ((port_number < 0) || (port_number >= 0xFFFF)) {
            ERROR("invalid IP port number %d (valid: 1->65534)", port_number);
        }

	// lookup DNS name
	if (sockname[0]){
	    hp = gethostbyname(sockname);
	    if (!hp){
                ERROR("lookup failed for host %s", sockname);
	    }
	    memcpy((char *)&address.in.sin_addr, 
		   (char *)hp->h_addr, hp->h_length);	
	}
	else {
	    // only listening TCP socket can be anonymous.
	    sockname = "0.0.0.0";
	}

	// create socket
	address.in.sin_port = htons((u_short)port_number);
	address.in.sin_family = AF_INET;
	
	// UDP
	if (kind & PORT_SOCKET_UDP){
	    sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	    if (sockfd < -1) ERROR("can't create socket");
	    if (kind & PORT_SOCKET_BROADCAST){
		intarg =  1;
		if (setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST,
			       &intarg, sizeof(intarg)) < 0){
		    close(sockfd);
		    ERROR("can't set broadcast socket option");
		}
	    }
	}
	// TCP
	else {
	    sockfd = socket(AF_INET, SOCK_STREAM, 0);
	    if (sockfd < -1) ERROR("can't create socket");
	    if (setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY,
			   &intarg, sizeof(intarg)) < 0){
		close(sockfd);
		ERROR("setsockopt TCP_NODELAY error");
	    }
            /* Allow bind() when socket is in TIME_WAIT, which happens
               when server dies when clients are still connected. */
	    if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR,
			   &intarg, sizeof(intarg)) < 0){
		close(sockfd);
		ERROR("setsockopt SO_REUSEADDR error");
	    }

	}
	addrlen = sizeof(address.in);
    }
    // server socket
    if (kind & PORT_SOCKET_SERVER){
	if (-1 == bind(sockfd, (struct sockaddr *) &address, addrlen)){
	    action = "bind";
	    goto error;
	}
	if (!(kind & PORT_SOCKET_UDP)){
	    if (-1 == listen(sockfd, 5)) { // FIXME: magic number
		close(sockfd);
		ERROR("can't listen: %s", strerror(errno));
	    }
	}

	// need nonblocking IO
	//if (-1 == fd_nonblock(sockfd)) {
	//    ERROR("can't set nonblock mode on sockfd %d", sockfd);
	//}
    }

    // client socket
    else {
	if (-1 == connect(sockfd, (struct sockaddr *) &address, addrlen)){
	    action = "connect";
	    goto error;
	}
    }
    
    return sockfd;

    // connect / bind error handler
  error:
    close(sockfd);
    if (port_number) { 
	ERROR("%s to host %s, TCP port %d failed: %s", 
	      action, sockname, port_number, strerror(errno)); 
    }
    else {
	ERROR("%s to UNIX socket %s failed: %s", 
	      action, sockname, strerror(errno));
    }
}


int fd_accept(leaf_ctx *ctx, int server_fd) {
    union addr address;  // connection address
    socklen_t addrlen = sizeof(address);
    int fd_server = server_fd;
    int fd_con;
    if (-1 == (fd_con = accept(fd_server, 
			       (struct sockaddr *)&address,
			       &addrlen))){
	ERROR("can't accept connection: %s", strerror(errno));
    }
    return fd_con;
}


/* Open a uni-directional connection to an inferior process. 

   RV = fd, 

   connect_fd = 0 -> our side writes
   connect_fd = 1 -> our side reads

*/

static void close_fds_from(int from) {
    // close all other filedescriptors
    // int i = ulimit(4, 0);
    int i = getdtablesize();
    while (i-- > from) {
        // fprintf(stderr, "closing fd %d\n", i);
        int cr;
        do { cr = close(i); } while ((cr == -1) && (errno == EINTR));
    }
}

int fd_pipe(char **argv, int *_pid, int connect_fd) {

    int fd[2];
    pipe(fd);
    int pid = fork();
    
    /* CHILD */
    if (!pid){

	close(connect_fd);    // connect input (sink==1) or output (sink==0)
	dup(fd[connect_fd]);

	close(fd[0]);       // don't need these
	close(fd[1]); 
        close_fds_from(3);

	// try to execute child
	if (-1 == execvp(argv[0], argv)){
	    perror ("can't execute inferior process");
	    exit(1);
	}
    }

    /* PARENT */
    int filedes = fd[connect_fd ^ 1];
    close(fd[connect_fd]);
    if (_pid) *_pid = pid;

    return filedes;
}


int fd_pipe_2(char **argv, int *_pid, int *fd_to_stdin, int *fd_from_stdout) {
    int rv, pid, R=0, W=1;
    int stdin_pipe_fd[2];   // R,W
    int stdout_pipe_fd[2];
    if ((rv = pipe(stdin_pipe_fd))) return rv;
    if ((rv = pipe(stdout_pipe_fd))) return rv;

    /* CHILD */
    if (!(pid = fork())) {
        close(stdin_pipe_fd[W]);
        close(stdout_pipe_fd[R]);

        close(0); dup(stdin_pipe_fd[R]);
        close(1); dup(stdout_pipe_fd[W]);

        close_fds_from(3);

	// try to execute child
	if (-1 == execvp(argv[0], argv)){
	    perror ("can't execute inferior process");
	    exit(1);
	}
    }

    /* PARENT */
    close(stdin_pipe_fd[R]);
    close(stdout_pipe_fd[W]);

    *fd_to_stdin    = stdin_pipe_fd[W];
    *fd_from_stdout = stdout_pipe_fd[R];
    if (_pid) *_pid = pid;
    return 0;
}




