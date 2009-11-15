
/*
 *   Pure Data Packet header file: xwindow glue code
 *   Copyright (c) by Tom Schouten <tom@zwizwa.be>
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,x
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef __xwindow_h__
#define __xwindow_h__


// x stuff
#include <X11/Xlib.h>
#include <X11/Xatom.h> 

#include "util.h" // small utility library

#include <leaf/leaf.h>

/* x display class */
typedef struct {
    leaf_class super;
} xdisplay_class;

typedef struct _xdisplay
{
    xdisplay_class *type;

    Display *dpy;              // the display connection
    int screen;                // the screen
    buf_t *windowlist;         // all windows belonging to this connection
                               // this contains (id, eventlist)

    int dragbutton;            // current drag button

} xdisplay;

/* x window class */
typedef struct {
    leaf_class super;
} xwindow_class;

typedef struct _xwindow
{
    xwindow_class *type;
    //Display *dpy;
    //int screen;
    xdisplay *xdisplay; // the display object
    Window win;               // window reference
    GC gc;                    // graphics context
    Atom WM_DELETE_WINDOW;

    buf_t *events;

    int winwidth;             // dim states
    int winheight;
    int winxoffset;
    int winyoffset;

    int  initialized;
    int  autocreate;

    char lastbut; // last button pressed (for drag)

    //t_symbol *dragbutton;
    
    float cursor;

} xwindow;

/* handle event */
void xwindow_queue_event(xwindow *xwin, XEvent *e);





/* cons */
xdisplay *xdisplay_new(char *dpy_string);

/* des */
void xdisplay_free(xdisplay *d);


void xdisplay_register_window(xdisplay *d,  xwindow *w);
void xdisplay_unregister_window(xdisplay *d, xwindow *w);

void xdisplay_route_events(xdisplay *d);

/* cons */
void xwindow_init(xwindow *b);
xwindow *xwindow_new(void);

/* des */    
void xwindow_cleanup(xwindow *b);
void xwindow_free(xwindow *b);

/* move the pointer */
void xwindow_warppointer(xwindow *xwin, int x, int y);


/* fullscreen message */
void xwindow_fullscreen(xwindow *xwin);

/* resize window */
void xwindow_resize(xwindow *b, int width, int height);

/* resize window */
void xwindow_moveresize(xwindow *b, int xoffset, int yoffset, int width, int height);

/* fill a tile of the screen */
void xwindowile(xwindow *xwin, int x_tiles, int y_tiles, int i, int j);

/* move window */
void xwindow_move(xwindow *xwin, int xoffset, int yoffset);

/* enable/disable cursor */
void xwindow_cursor(xwindow *b, int flag);

/* create xwindow. return code != NULL on succes */
int xwindow_config(xwindow *b, xdisplay *d);

/* close window */    
void xwindow_close(xwindow *b);

/* set title */
void xwindowitle(xwindow *xwin, char *title);

/* clear event queue */
void xwindow_drop_events(xwindow *x);


// dequeueing needs to be implemented by client
// all events are in the events buffer. each pointer
// is an XEvent allocated with malloc

xwindow_class *xwindow_type(void);
xdisplay_class *xdisplay_type(void);

#endif
