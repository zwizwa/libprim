/* (c) 2002-2009 Tom Schouten

   Part of this file is adapted from PacketForth which is licenced
   under the GPL.  However, this file is part of core libprim and
   licenced under the LGPL. */



#include <stdlib.h>
#include <string.h>
#include "port.h"
#include <leaf/bytes.h>



/* FILE ports */
int port_file_vprintf(port *p, const char *fmt, va_list ap) {
    va_list aq;
    int len;
    /* We might be called from error reporting with broken state.
       Allow for a default. */
    FILE *f = p ? p->stream.file : stderr;
    va_copy(aq, ap);
    len = vfprintf(f, fmt, aq);
    va_end(aq);
    return len;
}
int port_file_getc(port *p) {
    return fgetc(p->stream.file);
}
int port_file_putc(port *p, int c) {
    return fputc(c, p->stream.file);
}
int port_file_ungetc(port *p, int c) {
    return ungetc(c, p->stream.file);
}
int port_file_write(port *p, void *buf, size_t len) {
    return fwrite(buf, 1, len, p->stream.file);
}
void port_file_close(port *x) {
    fclose(x->stream.file);
    x->stream.file = NULL;
}
void port_file_flush(port *x) {
    fflush(x->stream.file);
}
bytes *port_file_bytes(port *x) { return NULL; }
void port_file_init(port *p) {
    p->vprintf = port_file_vprintf;
    p->get     = port_file_getc;
    p->unget   = port_file_ungetc;
    p->put     = port_file_putc;
    p->write   = port_file_write;
    p->close   = port_file_close;
    p->bytes   = port_file_bytes;
    p->flush   = port_file_flush;
}

/* Bytes ports */
int port_bytes_vprintf(port *p, const char *fmt, va_list ap) {
    if (!p->stream.b.bytes) return -1;
    va_list aq;
    int len;
    va_copy(aq, ap);
    len = vsnprintf(NULL, 0, fmt, aq);
    va_end(aq);
    if (len < 0) return len;
    void *data = bytes_allot(p->stream.b.bytes, len);
    len = vsprintf(data, fmt, ap);
    return len;
}
int port_bytes_getc(port *p) {
    if (!p->stream.b.bytes) return -1;
    int i;
    if ((i = p->stream.b.read_index) >= p->stream.b.bytes->size) return EOF;
    int c = p->stream.b.bytes->bytes[i];
    p->stream.b.read_index++;
    return c;
}
int port_bytes_ungetc(port *p, int c) {
    if (!p->stream.b.bytes) return -1;
    if (p->stream.b.read_index > 0) {
        p->stream.b.bytes->bytes[--(p->stream.b.read_index)] = c;
        return c;
    }
    else return EOF;
}
int port_bytes_putc(port *p, int c) {
    if (!p->stream.b.bytes) return -1;
    char *data = bytes_allot(p->stream.b.bytes, 1);
    data[0] = c;
    return c;
}
int port_bytes_write(port *p, void *buf, size_t len) {
    if (!p->stream.b.bytes) return -1;
    void *data = bytes_allot(p->stream.b.bytes, len);
    memcpy(data, buf, len);
    return len;
}
void port_bytes_close(port *p) {
    if (p->stream.b.bytes) leaf_free((leaf_object*)(p->stream.b.bytes));
    p->stream.b.bytes = NULL;
}
bytes *port_bytes_bytes(port *p) {
    bytes *b = p->stream.b.bytes; 
    p->stream.b.bytes = NULL;
    return b;
}
void port_bytes_flush(port *p) {}

void port_bytes_init(port *p) {
    p->vprintf = port_bytes_vprintf;
    p->get     = port_bytes_getc;
    p->unget   = port_bytes_ungetc;
    p->put     = port_bytes_putc;
    p->write   = port_bytes_write;
    p->close   = port_bytes_close;
    p->bytes   = port_bytes_bytes;
    p->flush   = port_bytes_flush;
}

/* Default */
static port *_default_out = NULL;
static port *default_out(void) {
    if (!_default_out) _default_out = port_file_new(stdout, "<stdout>");
    return _default_out;
}


/* Virtual */
int port_printf(port *p, const char *fmt, ...) {
    if (!p) p = default_out();
    int len;
    va_list ap;
    va_start (ap, fmt);
    len = p->vprintf(p, fmt, ap);
    va_end(ap);
    return len;
}
int port_vprintf(port *p, const char *fmt, va_list ap) {
    if (!p) p = default_out();
    return p->vprintf(p, fmt, ap);
}
int port_getc(port *p) {
    if (!p) return EOF;
    return p->get(p);
}
int port_ungetc(port *p, int c) {
    if (!p) return EOF;
    return p->unget(p, c);
}
int port_putc(port *p, int c) {
    if (!p) p = default_out();
    return p->put(p, c);
}
int port_write(port *p, void *buf, size_t len) {
    if (!p) p = default_out();
    return p->write(p, buf, len);
}
void port_close(port *p) {
    if (p) p->close(p);
}
void port_flush(port *p) {
    if (p) p->flush(p);
}
bytes *port_get_bytes(port *p) {
    if (!p) return NULL;
    return p->bytes(p);
}


void port_free(port *x) {
    /* fprintf(stderr, "port_free(%p)\n", x); */
    port_close(x);
    if (x->name) free (x->name);
    free(x);
}


static int port_write_info(port *x, port *p) {
    return port_printf(p, "#<port:%s>", x->name);
}

static port_class *type = NULL;
static port_class *port_class_new(void) {
    port_class *x = calloc(1, sizeof(*x));
    x->super.free = (leaf_free_m)port_free;
    x->super.write = (leaf_write_m)port_write_info;
    return x;
}
port_class *port_type(void) {
    if (!type) type = port_class_new();
    return type;
}
port *port_file_new(FILE *f, const char *name) {
    if (!f) return NULL;
    port *x = calloc(1, sizeof(*x));
    port_file_init(x);
    x->type = port_type();
    x->stream.file = f;
    x->name = NULL;
    if (name) {
        x->name = malloc(1 + strlen(name));
        strcpy(x->name, name);
    }
    return x;
}
port *port_bytes_new(bytes *b) {
    if (!b) return NULL;
    port *x = calloc(1, sizeof(*x));
    port_bytes_init(x);
    x->type = port_type();
    x->stream.b.bytes = b;
    x->stream.b.read_index = 0;
    x->name = malloc(9);
    strcpy(x->name, "<string>");
    return x;
}



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


#define ERROR(...) { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); return -1; } // FIXME: leaf error handling

union addr {
    struct sockaddr_un un;
    struct sockaddr_in in;
};

/* This produces file descriptors.  It's a bit awkward to work with
   streams for listening sockets.  Use these in conjunction with
   fdopen to create port abstractions. */

// ((host port) mode kind -- stream )
int fd_socket(const char *sockname,  // hostname | filesystem node
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
		ERROR("setsockopt error");
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


int fd_accept(int server_fd) {
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
