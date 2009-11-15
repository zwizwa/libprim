/*
 *   Pure Data Packet system module. - x window glue code (fairly tied to pd and pf)
 *   Copyright (c) by Tom Schouten <tom@zwizwa.be>
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */


// this code is fairly tied to pd and pf. serves mainly as reusable glue code
// for pf_xv, pf_glx, pf_3d_windowcontext, ...

#include <string.h>
#include <stdlib.h>
#include <stdio.h>


//#include <pf/stream.h>
//#include <pf/packet.h>
//#include <pf/packet_imp.h>
//#include <pf/image.h>
#include "xwindow.h"
#include "xv.h"

#define D if(0)




/************************************* XV ************************************/

static void xv_destroy_xvimage(xv_t *xvid);


/* this was a local function. */
static int errors;
static int error_handler(Display *dpy, XErrorEvent *e){errors++; return 0;}

static void xv_create_xvimage(xv_t *xvid, int width, int height)
{
    // int i;
    long size;
    errors = 0;

    xvid->width = width;
    xvid->height = height;
    size = (xvid->width * xvid->height + (((xvid->width>>1)*(xvid->height>>1))<<1));

    // don't use shared memory
    if (!xvid->shm){
      no_shm:	

	xvid->data = (unsigned char *)malloc(size + 4); // 4 bytes extra to get word size buffer for word bitflip
	//for (i=0; i<size; i++) xvid->data[i] = i;

	xvid->xvi = XvCreateImage(xvid->xdpy->dpy, xvid->xv_port, xvid->xv_format, 
				  (char *)xvid->data, xvid->width, xvid->height);
	xvid->last_encoding = -1;

	if ((!xvid->xvi) || (!xvid->data)) fprintf (stderr, "xv_create_xvimage: error creating xvimage");
	return;

    }
    // try shared memory
    else {
	void *old_handler = XSetErrorHandler(error_handler);

	// ASSERT!
	if (xvid->shminfo) {
	    fprintf(stderr, "have shminfo?"); exit(1);
	}

	xvid->shminfo = malloc(sizeof(XShmSegmentInfo));
	memset(xvid->shminfo, 0, sizeof(XShmSegmentInfo));
	
	xvid->xvi = XvShmCreateImage(xvid->xdpy->dpy, xvid->xv_port, xvid->xv_format,
				     0,  xvid->width, xvid->height, xvid->shminfo);
	if (!xvid->xvi) goto shm_error;
	if (-1 == (xvid->shminfo->shmid = shmget(IPC_PRIVATE, xvid->xvi->data_size,
					     IPC_CREAT | 0777))) goto shm_error;
	if (((void *)-1) == (xvid->shminfo->shmaddr = (char *) shmat(xvid->shminfo->shmid, 0, 0))) goto shm_error;
	
	xvid->data = xvid->xvi->data = xvid->shminfo->shmaddr;
	xvid->shminfo->readOnly = False;
	XShmAttach(xvid->xdpy->dpy, xvid->shminfo);
	XSync(xvid->xdpy->dpy, False);
	if (errors) goto shm_error;
	shmctl(xvid->shminfo->shmid, IPC_RMID, 0);
	XSetErrorHandler(old_handler);
	D fprintf(stderr, "xv: using shared memory\n");
	return; // ok

      shm_error:
	D fprintf(stderr, "xv: can't use shared memory\n");
	xv_destroy_xvimage(xvid);
	XSetErrorHandler(old_handler);
	xvid->shm = 0;
	goto no_shm;
    }
}

static void xv_destroy_xvimage(xv_t *xvid)
{
    if (xvid->xvi) XFree(xvid->xvi);
    if (xvid->shminfo){
	if ((void *)-1 != xvid->shminfo->shmaddr  &&  NULL != xvid->shminfo->shmaddr)
	    shmdt(xvid->shminfo->shmaddr);
	free(xvid->shminfo);
    }
    else {
	if (xvid->data) free(xvid->data);
    }
    xvid->shminfo = 0;
    xvid->xvi = 0;
    xvid->data = 0;
}

/* display */
void xv_image_display(xv_t *xvid, xwindow_t *xwin){

    if (!xvid->initialized) return;

    /* set aspect ratio here if necessary 
       currently we don't : client responsability */
    int a_x = 0;
    int a_y = 0;
    int b_x = xvid->width;
    int b_y = xvid->height;
    // int offset = 0;

    int w_w = xwin->winwidth;
    int w_h = xwin->winheight;

    if (xvid->shminfo){
	XvShmPutImage(xvid->xdpy->dpy,xvid->xv_port, xwin->win,xwin->gc,xvid->xvi, 
		      a_x, a_y, b_x, b_y, 0,0, w_w, w_h, True);
    }
    else {
	XvPutImage(xvid->xdpy->dpy,xvid->xv_port, xwin->win,xwin->gc,xvid->xvi, 
		   a_x, a_y, b_x, b_y, 0,0, w_w, w_h);
    }

    XFlush(xvid->xdpy->dpy);
}


void *xv_image_data(xv_t *xvid, xwindow_t *xwin, 
			unsigned int width, unsigned int height){

    if (!xvid->initialized) return 0;

    /* check if xvimage needs to be recreated */
    if ((width != xvid->width) || (height != xvid->height)){
	xv_destroy_xvimage(xvid);
	xv_create_xvimage(xvid, width, height);
    }

    return xvid->data;

}




void xv_close(xv_t* xvid)
{
    if (xvid->initialized){
	if (xvid->xvi) xv_destroy_xvimage(xvid);
	XvUngrabPort(xvid->xdpy->dpy, xvid->xv_port, CurrentTime);
	xvid->xv_port = 0;
	xvid->xdpy = 0;
	xvid->last_encoding = -1;
	xvid->initialized = 0;
    }
}

void xv_cleanup(xv_t* xvid)
{
    // close xv port (and delete XvImage)
    xv_close(xvid);

    // no more dynamic data to free

}

void xv_free(xv_t* xvid){
    xv_cleanup(xvid);
    free(xvid);
}

void xv_init(xv_t *xvid)
{

    xvid->xdpy = 0;

    xvid->xv_format = FOURCC_I420; // used to be FOURCC_YV12.  veejay compat.
    xvid->xv_port      = 0;

    xvid->shm = 1; // try to use shm
    //xvid->shm = 0; // don't
    xvid->shminfo = 0;

    xvid->width = 320;
    xvid->height = 240;

    xvid->data = 0;
    xvid->xvi = 0;

    xvid->initialized = 0;
    xvid->last_encoding = -1;

}
xv_t *xv_new(void)
{
    xv_t *xvid = malloc(sizeof(*xvid));
    xv_init(xvid);
    return xvid;
}

int xv_open_on_display(xv_t *xvid, xdisplay_t *d, int adaptor_start)
{
    unsigned int ver, rel, req, ev, err, i, j;
    unsigned int adaptors;
    // int formats;
    XvAdaptorInfo        *ai;

    if (xvid->initialized) return 1;
    if (!d) return 0;
    xvid->xdpy = d;
    
    if (Success != XvQueryExtension(xvid->xdpy->dpy,&ver,&rel,&req,&ev,&err))	return 0;

    /* find + lock port */
    if (Success != XvQueryAdaptors(xvid->xdpy->dpy,DefaultRootWindow(xvid->xdpy->dpy),&adaptors,&ai))
	return 0;
    for (i = adaptor_start; i < adaptors; i++) {
	if ((ai[i].type & XvInputMask) && (ai[i].type & XvImageMask)) {
	    for (j=0; j < ai[i].num_ports; j++){
		if (Success != XvGrabPort(xvid->xdpy->dpy,ai[i].base_id+j,CurrentTime)) {
		    D fprintf(stderr,"xv: Xv port %ld on adapter %d: is busy, skipping\n",ai[i].base_id+j, i);
		}
		else {
		    xvid->xv_port = ai[i].base_id + j;
		    goto breakout;
		}
	    }
	}
    }


 breakout:

    XFree(ai);
    if (0 == xvid->xv_port) return 0;
    D fprintf(stderr, "xv: grabbed port %d on adaptor %d\n", xvid->xv_port, i);
    xvid->initialized = 1;
    xv_create_xvimage(xvid, xvid->width, xvid->height);
    return 1;
}


