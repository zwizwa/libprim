
/*
 *   Pure Data Packet header file: glx glue code
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


// x stuff
#define GLX_GLXEXT_PROTOTYPES

#include "xwindow.h"
#include <string.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>



// does not work on Darwin
//#include <GL/glxext.h>

/* glx class */
typedef struct _glx
{

    xdisplay *xdpy; //mother display object
    int  initialized;

    /* texture */
    XVisualInfo *vis_info;
    GLXContext glx_context;
    GLuint texture;
    GLsizei tex_width;
    GLsizei tex_height;
    GLuint format;

    /* image */
    void *data;
    int image_width;
    int image_height;


} glx_t;


/* cons */
void glx_init(glx_t *x);
glx_t *glx_new(void);

/* des */
void glx_cleanup(glx_t* x);
void glx_free(glx_t* x);


/* open an opengl context */
int glx_open_on_display(glx_t *x, xwindow *w, xdisplay *d);

/* close an opengl context */
void glx_close(glx_t* x);

/* display a packet */
//void glx_display_packet(glx_t *x, xwindow *xwin, pf_packet_t packet);

/* get texture data buffer */
void *glx_image_data(glx_t *x, xwindow *xwin, 
			 unsigned int width, unsigned int height);

/* display the texture */
void glx_image_display(glx_t *x, xwindow *xwin);


/* opengl specific stuff*/
void glx_swapbuffers(glx_t *x, xwindow *xwin);
void glx_makecurrent(glx_t *x, xwindow *xwin);


/* opengl sync module 
   this uses GLX_SGI_video_sync to sync to retrace
*/

//typedef struct {
//    xdisplay *xdpy;
//    Window dummy_window;
//   GLXContext context;
//} glx_sync_t;

// glx_sync_t *glx_sync_new(void);
// oid glx_sync_free(glx_sync_t *x);
// void glx_sync_wait(glx_sync_t *x);
// int glx_sync_open_on_display(glx_sync_t *x, xdisplay *xdpy);
