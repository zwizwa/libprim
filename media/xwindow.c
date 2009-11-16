/*
 *   Pure Data Packet system module. - x window glue code (fairly tied to pd and pf)
 *   Copyright (c) by Tom Schouten <tom@zwizwa.be>
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as publishd by
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

//#include <pf/stream.h> // no longer depending on pf
//#include <pf/debug.h>
//#include <pf/symbol.h>
//#include <pf/list.h>

#include "xwindow.h"
#include <stdlib.h>
#include <stdio.h>
#include <X11/Xutil.h>

#include <leaf/port.h>

#define D if(0)

// TODO: clean this up
#define ASSERT(x) {if (!(x)) {fprintf(stderr, "xwindow: assert failed\n"); exit(1);}}

/************************************* XDISPLAY ************************************/


int xdisplay_errorhandler(Display *dpy, XErrorEvent *ev){
    fprintf(stderr, "xdisplay_errorhandler\n");
    return 0xdeadf00d;
}

static int xdisplay_write(xdisplay *x, port *p) {
    return port_printf(p, "#<xdisplay:%p>", x);
}
xdisplay_class *xdisplay_type(void) {
    static xdisplay_class *type = NULL;
    if (!type) {
        type = calloc(1, sizeof(*type));
        type->super.free  = (leaf_free_m)xdisplay_free;
        type->super.write = (leaf_write_m)xdisplay_write;
    }
    return type;
}

xdisplay *xdisplay_new(char *dpy_string)
{

    /* open display */
    xdisplay *d = malloc(sizeof(*d));
    d->type = xdisplay_type();
    if (!(d->dpy = XOpenDisplay(dpy_string))){
	fprintf(stderr, "x11: can't open display %s\n", dpy_string);
	free(d);
	return (0);
    }

    /* install error handler */
    // XSetErrorHandler(&xdisplay_errorhandler);

    d->windowlist = buf_new(16);
    d->screen = DefaultScreen(d->dpy);
    d->dragbutton = -1;
    D fprintf (stderr, "x11: using display %s\n", dpy_string);
    return d;
}

void xdisplay_free(xdisplay *d)
{
    XCloseDisplay(d->dpy);
    ASSERT(0 == d->windowlist->elements); // make sure there are no dangling xwindow objects
    buf_free(d->windowlist);
    free(d);
}

/* some private members */
// static int _windowset_contains(xdisplay *d, xwindow *w){
//    return (buf_lookup(d->windowlist, w) >= 0);
//}


void xdisplay_register_window(xdisplay *d, xwindow *w)
{
    //pf_post("xdisplay: registering window %p", w);
    buf_add_to_set(d->windowlist, w);
    
    //if (!_windowset_contains(d, w)) _windowset_add(d, w);
}
void xdisplay_unregister_window(xdisplay *d, xwindow *w)
{
    //pf_post("xdisplay: unregistering window %p", w);
    buf_remove(d->windowlist, w);
    //if (_windowset_contains(d, w)) _windowset_remove(d, w);
}


// this reads all events from display and passes them on to client
// just raw x events. client needs to parse events.. since this is
// shared code, we keep it as simple as possible

void xdisplay_route_events(xdisplay *d){
    xwindow *w;
    int i;
    XEvent *e = 0;

    while (XPending(d->dpy)){
	if (!e) e = malloc(sizeof(XEvent));
	XNextEvent(d->dpy, e);	

	// find xwindow instance

	for (i = 0; i<d->windowlist->elements; i++){
	    w = (xwindow *)d->windowlist->data[i];
	    if (w->win == e->xany.window){

		// handle here
		if (e->type == ConfigureNotify){
		    w->winwidth = e->xconfigure.width;
		    w->winheight = e->xconfigure.height;
		}

		else {
		    xwindow_queue_event(w, e);
		    e = 0; // moved
		    break;
		}
	    }
	}
    }

    if (e) free(e);
}



/************************************* XWINDOW ************************************/

void xwindow_queue_event(xwindow *x, XEvent *e){
    buf_add(x->events, e);
}

void xwindow_drop_events(xwindow *x){
    void *e;
    while ((e = stack_pop(x->events))) free(e);
}


void xwindow_warppointer(xwindow *xwin, int x, int y)
{
    if (xwin->initialized){
	XWarpPointer(xwin->xdisplay->dpy, None, xwin->win, 0, 0, 0, 0, x, y);
    }
}




void xwindow_overrideredirect(xwindow *xwin, int b)
{
    XSetWindowAttributes new_attr;
    new_attr.override_redirect = b ? True : False;
    XChangeWindowAttributes(xwin->xdisplay->dpy, xwin->win, CWOverrideRedirect, &new_attr);
    //XFlush(xwin->xdisplay->dpy);

}


void xwindow_moveresize(xwindow *xwin, int xoffset, int yoffset, int width, int height)
{

    if ((width > 0) && (height > 0)){
	xwin->winwidth = width;
	xwin->winheight = height;
	xwin->winxoffset = xoffset;
	xwin->winyoffset = yoffset;

	if (xwin->initialized){
	    XMoveResizeWindow(xwin->xdisplay->dpy, xwin->win, xoffset, yoffset, width,  height);
	    XFlush(xwin->xdisplay->dpy);
	}
    }
}


void xwindow_fullscreen(xwindow *xwin)
{
    XWindowAttributes rootwin_attr;


    /* hmm.. fullscreen and xlib the big puzzle..
       if it looks like a hack it is a hack. */

    if (xwin->initialized){
        
        XGetWindowAttributes(xwin->xdisplay->dpy, RootWindow(xwin->xdisplay->dpy, xwin->xdisplay->screen), &rootwin_attr );

	//xwindow_overrideredirect(xwin, 0);
	xwindow_moveresize(xwin, 0, 0, rootwin_attr.width,  rootwin_attr.height);
	//xwindow_overrideredirect(xwin, 1);
	//XRaiseWindow(xwin->xdisplay->dpy, xwin->win);
	//xwindow_moveresize(xwin, 0, 0, rootwin_attr.width,   rootwin_attr.height);
	//xwindow_overrideredirect(xwin, 0);

 


    }
}


void xwindowile(xwindow *xwin, int x_tiles, int y_tiles, int i, int j)
{
    XWindowAttributes rootwin_attr;
    // XSetWindowAttributes new_attr;

    if (xwin->initialized){
	int tile_w;
	int tile_h;
        XGetWindowAttributes(xwin->xdisplay->dpy, RootWindow(xwin->xdisplay->dpy, xwin->xdisplay->screen), &rootwin_attr );

	tile_w = rootwin_attr.width / x_tiles;
	tile_h = rootwin_attr.height / y_tiles;

	xwin->winwidth = (x_tiles-1) ? rootwin_attr.width - (x_tiles-1)*tile_w : tile_w;
	xwin->winheight = (y_tiles-1) ? rootwin_attr.height - (y_tiles-1)*tile_h : tile_h;
	xwin->winxoffset = i * tile_w;
	xwin->winyoffset = j * tile_h;

        //new_attr.override_redirect = True;
        //XChangeWindowAttributes(xwin->xdisplay->dpy, xwin->win, CWOverrideRedirect, &new_attr );
	XMoveResizeWindow(xwin->xdisplay->dpy, xwin->win, xwin->winxoffset, xwin->winyoffset, xwin->winwidth,  xwin->winheight);

    }
}

/* resize window */
void xwindow_resize(xwindow *xwin, int width, int height)
{
    if ((width > 0) && (height > 0)){
	xwin->winwidth = width;
	xwin->winheight = height;
	if (xwin->initialized){
	    XResizeWindow(xwin->xdisplay->dpy, xwin->win,  width,  height);
	    XFlush(xwin->xdisplay->dpy);
	}
    }
    //_xwindow_moveresize(xwin, xwin->winxoffset, xwin->winyoffset, width, height);
}

/* move window */
void xwindow_move(xwindow *xwin, int xoffset, int yoffset)
{
    xwindow_moveresize(xwin, xoffset, yoffset, xwin->winwidth, xwin->winheight);
}




/* set an arbitrary cursor image */
void xwindow_cursor_image(xwindow *xwin, char *data, int width, int height)
{
    if (!xwin->initialized) return;

    Cursor cursor;
    Pixmap pm;
    XColor fg;
    XColor bg;

    fg.red = fg.green = fg.blue = 0xffff;
    bg.red = bg.green = bg.blue = 0x0000;

    pm = XCreateBitmapFromData(xwin->xdisplay->dpy, xwin->win, data, width, height);
    cursor = XCreatePixmapCursor(xwin->xdisplay->dpy, pm, pm, &fg,
				 &bg, width/2, height/2);
    XFreePixmap(xwin->xdisplay->dpy, pm);
    XDefineCursor(xwin->xdisplay->dpy, xwin->win,cursor);
}

/* enable / disable cursor */
void xwindow_cursor(xwindow *xwin, int i){
    if (!xwin->initialized) return;
    if (i == 0) {
        char data[] = {0};
	xwindow_cursor_image(xwin, data, 1, 1);
    }
    else
        XUndefineCursor(xwin->xdisplay->dpy, xwin->win);

    xwin->cursor = i;
}


void xwindowitle(xwindow *xwin, char *title)
{
    if (xwin->initialized)
	XStoreName(xwin->xdisplay->dpy, xwin->win, title);
}


/* create xwindow */
int xwindow_config(xwindow *xwin, xdisplay *d) 
{
    XEvent e;
    // unsigned int i;

    /* check if already opened */
    if(  xwin->initialized ){
	fprintf(stderr, "x11: window already created\n");
	goto exit;
    }

    xwin->xdisplay = d;
    ASSERT(xwin->xdisplay);

    /* create a window if xwin doesn't contain one yet:
       glx init will create its own window */
    if (!xwin->win){
	xwin->win = XCreateSimpleWindow(
	    xwin->xdisplay->dpy, 
	    RootWindow(xwin->xdisplay->dpy, xwin->xdisplay->screen), 
	    xwin->winxoffset, xwin->winyoffset, 
	    xwin->winwidth, xwin->winheight, 0, 
	    BlackPixel(xwin->xdisplay->dpy, xwin->xdisplay->screen),
	    BlackPixel(xwin->xdisplay->dpy, xwin->xdisplay->screen));

	D fprintf(stderr, "created window %x on screen %d\n", 
		  (unsigned int)xwin->win, xwin->xdisplay->screen);
    }
    else {
	D fprintf(stderr, "reused window %x on screen %d\n", 
		  (unsigned int)xwin->win, xwin->xdisplay->screen);
    }


    /* enable handling of close window event */
    xwin->WM_DELETE_WINDOW = XInternAtom(xwin->xdisplay->dpy,
					 "WM_DELETE_WINDOW", True);
    (void)XSetWMProtocols(xwin->xdisplay->dpy, xwin->win,
			  &xwin->WM_DELETE_WINDOW, 1);

    if(!(xwin->win)){
	/* clean up mess */
	fprintf(stderr, "x11: could not create window.\n");
	//XCloseDisplay(xwin->xdisplay->dpy); NOT OWNER
	xwin->xdisplay = 0;
	xwin->initialized = 0;
	goto exit;
    }

    /* select input events */
    XSelectInput(xwin->xdisplay->dpy, xwin->win, 
		 StructureNotifyMask  
		 | KeyPressMask 
		 | KeyReleaseMask 
		 | ButtonPressMask 
		 | ButtonReleaseMask 
		 | MotionNotify 
		 | PointerMotionMask);
    //	 | ButtonMotionMask);
    //XSelectInput(xwin->xdisplay->dpy, xwin->win, StructureNotifyMask);



    /* set WM_CLASS */
    XClassHint chint = {"pf","Pdp"};
    XSetClassHint(xwin->xdisplay->dpy, xwin->win, &chint);
    XSetCommand(xwin->xdisplay->dpy, xwin->win, 0, 0);

    /* map */
    XMapWindow(xwin->xdisplay->dpy, xwin->win);

    /* create graphics context */
    xwin->gc = XCreateGC(xwin->xdisplay->dpy, xwin->win, 0, 0);
    
    /* catch mapnotify */
    for(;;){
	XNextEvent(xwin->xdisplay->dpy, &e);
	if (e.type == MapNotify) break;
    }

    D fprintf(stderr, "mapped window %x\n", (unsigned int)xwin->win);


    /* we're done initializing */
    xwin->initialized = 1;

    /* disable/enable cursor */
    xwindow_cursor(xwin, xwin->cursor);

    /* set window title */
    xwindowitle(xwin, "pf");

    xdisplay_register_window(xwin->xdisplay, xwin);

 exit:
    return xwin->initialized;

}

void xwindow_init(xwindow *xwin)
{
    xwin->xdisplay = 0;
    xwin->win = 0;

    xwin->winwidth = 320;
    xwin->winheight = 240;
    xwin->winxoffset = 0;
    xwin->winyoffset = 0;

    xwin->initialized = 0;

    xwin->cursor = 0;
    //xwin->dragbutton = gensym("drag1");

    xwin->events = buf_new(16);

}
static int xwindow_write(xdisplay *x, port *p) {
    return port_printf(p, "#<xwindow:%p>", x);
}
xwindow_class *xwindow_type(void) {
    static xwindow_class *type = NULL;
    if (!type) {
        type = calloc(1, sizeof(*type));
        type->super.free  = (leaf_free_m)xwindow_free;
        type->super.write = (leaf_write_m)xwindow_write;
    }
    return type;
}
xwindow *xwindow_new(void)
{
    xwindow *xwin = malloc(sizeof(*xwin));
    xwin->type = xwindow_type();
    xwindow_init(xwin);
    return xwin;
}
    

void xwindow_close(xwindow *xwin)
{
    // void *v;
    xwindow_drop_events(xwin); 
    buf_free(xwin->events);
    xwin->events = 0;

    XEvent e;

    if (xwin->initialized){
	XFreeGC(xwin->xdisplay->dpy, xwin->gc);
	XDestroyWindow(xwin->xdisplay->dpy, xwin->win);
	while(XPending(xwin->xdisplay->dpy)) XNextEvent(xwin->xdisplay->dpy, &e);
	xdisplay_unregister_window(xwin->xdisplay, xwin);
	xwin->xdisplay = 0;
	xwin->initialized = 0;
    }

}

void xwindow_cleanup(xwindow *x)
{
    // close win
    xwindow_close(x);

    // no more dynamic data to free
}

void xwindow_free(xwindow *xwin)
{
    xwindow_cleanup(xwin);
    free(xwin);
}


