
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

//#include <pf/list.h> // no longer depending on pf
//#include <pf/mem.h>

/* x display class */
typedef struct _xdisplay
{
    Display *dpy;              // the display connection
    int screen;                // the screen
    buf_t *windowlist;         // all windows belonging to this connection
                               // this contains (id, eventlist)

    int dragbutton;            // current drag button

} xdisplay_t;


/* cons */
xdisplay_t *xdisplay_new(char *dpy_string);

/* des */
void xdisplay_free(xdisplay_t *d);

struct _xwindow;

void xdisplay_register_window(xdisplay_t *d, struct _xwindow *w);
void xdisplay_unregister_window(xdisplay_t *d, struct _xwindow *w);

void xdisplay_route_events(xdisplay_t *d);

/* x window class */
typedef struct _xwindow
{
    //Display *dpy;
    //int screen;
    xdisplay_t *xdisplay; // the display object
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

} xwindow_t;

/* cons */
void xwindow_init(xwindow_t *b);
xwindow_t *xwindow_new(void);

/* des */    
void xwindow_cleanup(xwindow_t *b);
void xwindow_free(xwindow_t *b);

/* move the pointer */
void xwindow_warppointer(xwindow_t *xwin, int x, int y);


/* fullscreen message */
void xwindow_fullscreen(xwindow_t *xwin);

/* resize window */
void xwindow_resize(xwindow_t *b, int width, int height);

/* resize window */
void xwindow_moveresize(xwindow_t *b, int xoffset, int yoffset, int width, int height);

/* fill a tile of the screen */
void xwindow_tile(xwindow_t *xwin, int x_tiles, int y_tiles, int i, int j);

/* move window */
void xwindow_move(xwindow_t *xwin, int xoffset, int yoffset);

/* enable/disable cursor */
void xwindow_cursor(xwindow_t *b, int flag);

/* create xwindow. return code != NULL on succes */
int xwindow_config(xwindow_t *b, xdisplay_t *d);

/* close window */    
void xwindow_close(xwindow_t *b);

/* set title */
void xwindow_title(xwindow_t *xwin, char *title);

/* handle event */
void xwindow_queue_event(xwindow_t *xwin, XEvent *e);

/* clear event queue */
void xwindow_drop_events(xwindow_t *x);


// dequeueing needs to be implemented by client
// all events are in the events buffer. each pointer
// is an XEvent allocated with malloc

#endif
