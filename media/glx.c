/*
 *   Pure Data Packet support file: glx glue code
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

//#include <pf/typedefs.h>

#include "glx.h"
//#include <pf/image.h>
//#include <pf/packet.h>
//#include <pf/packet_imp.h>
//#include <pf/stream.h>

#define D if (0)


static glx_t *current_context = 0;

/* cons */
void glx_init(glx_t *x)
{
    memset(x, 0, sizeof(*x));
    x->tex_width = 1; // smallest nonzero (for mul 2) dummy size
    x->tex_height = 1;
}

glx_t *glx_new(void){
    glx_t *x = malloc(sizeof(*x));
    glx_init(x);
    return x;
}

/* des */
void glx_cleanup(glx_t* x)
{
    // XEvent e;

    if (x->initialized){
	glXDestroyContext(x->xdpy->dpy, x->glx_context);
	x->xdpy = 0;
	x->initialized = 0;
    }
}
void glx_free(glx_t* x){
    
    // fprintf(stderr, "glx_free: FIXME: free texture\n");
    if (glIsTexture(x->texture)){glDeleteTextures(1, &(x->texture));}
    free(x);
}


void glx_swapbuffers(glx_t *x, xwindow_t *xwin)
{
    glXSwapBuffers(x->xdpy->dpy, xwin->win);
}
void glx_makecurrent(glx_t *x, xwindow_t *xwin)
{
    if (x != current_context){

	D fprintf(stderr, "glXMakeCurrent(%p,%p,%p)\n", 
                  (void*)x->xdpy->dpy, 
                  (void*)xwin->win, 
                  (void*)x->glx_context);
	glXMakeCurrent(x->xdpy->dpy, xwin->win, x->glx_context);

	current_context = x;

    }
}


void *glx_image_data(glx_t* x, xwindow_t *xwin,
			unsigned int w, unsigned int h){

    if (!x->initialized) return 0;

    x->image_width = w;
    x->image_height = h;
    
    int tex_width = x->tex_width;
    int tex_height = x->tex_height;

    /* set window context current
       just to make sure we don't conflict with other contexts */
    glx_makecurrent(x, xwin);

       /* setup texture if necesary */
    if ((x->image_width > tex_width) || (x->image_height > tex_height)) {
	while (x->image_width > tex_width) tex_width *= 2;
	while (x->image_height > tex_height) tex_height *= 2;
	if (!glIsTexture(x->texture)){glGenTextures(1, &(x->texture));}
	glBindTexture(GL_TEXTURE_2D, x->texture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_width, tex_height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
	x->tex_width = tex_width;
	x->tex_height = tex_height;
	fprintf(stderr, "glx_image_data: creating texture %dx%d", tex_width, tex_height);
	if (x->data) free (x->data);
	x->data = malloc(4 * x->image_width * x->image_height);
    }
    else {
	glBindTexture(GL_TEXTURE_2D, x->texture);
    }
    return x->data;
 
}
/* display the texture */
void glx_image_display(glx_t *x, xwindow_t *xwin){

    /* set window context current
       just to make sure we don't conflict with other contexts */
    glx_makecurrent(x, xwin);

    /* get fractional dimensions */
    float fx = (float)x->image_width / (float)x->tex_width;
    float fy = (float)x->image_height / (float)x->tex_height;

    /* upload subtexture */
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, x->image_width, x->image_height, 
		    x->format, GL_UNSIGNED_BYTE, x->data);
	    
    /* setup viewport, projection and modelview */
    glViewport(0, 0, xwin->winwidth, xwin->winheight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0.0, xwin->winwidth, 0.0, xwin->winheight);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    /* enable default texture */
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, x->texture);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    /* display texture */  
    glBegin(GL_QUADS);
        glTexCoord2f(fx, fy);
        glVertex2i(xwin->winwidth,0);
        glTexCoord2f(fx, 0);
	glVertex2i(xwin->winwidth, xwin->winheight);
        glTexCoord2f(0.0, 0.0);
	glVertex2i(0, xwin->winheight);
        glTexCoord2f(0, fy);
	glVertex2i(0,0);
    glEnd();


    glFlush();
    glXSwapBuffers(x->xdpy->dpy, xwin->win);

}


/* open an opengl context */
int glx_open_on_display(glx_t *x, xwindow_t *w, xdisplay_t *d)
{
    static int vis_attr[] = {GLX_RGBA, 
			     GLX_RED_SIZE, 4, 
			     GLX_GREEN_SIZE, 4,
			     GLX_BLUE_SIZE, 4, 
			     GLX_DEPTH_SIZE, 16, 
			     GLX_DOUBLEBUFFER, None};


    /* store mother */
    x->xdpy = d;

    /* create a glx visual */
    if (!(x->vis_info = glXChooseVisual(x->xdpy->dpy,
					x->xdpy->screen,
					vis_attr))){
	fprintf(stderr, "glx_open_on_display: can't find appropriate visual\n");
	goto error;
    }
    else {
	D fprintf(stderr, "glx_open_on_display: using visual 0x%x\n", 
		(unsigned int)x->vis_info->visualid);
    }

    /* create the rendering context */
    if (!(x->glx_context = glXCreateContext(x->xdpy->dpy,
					    x->vis_info,
					    0 /*share list*/,
					    GL_TRUE))){
	fprintf(stderr, "glx_open_on_display: can't create render context\n");
	goto error;
    }
    else {
	D fprintf(stderr, 
                  "glx_open_on_display: created render "
                  "context %p on screen %d\n", 
                  (void*)x->glx_context, x->xdpy->screen);
    }

    
    /* following is from glxgears.c */

    XSetWindowAttributes swa;
    Colormap cmap;
    /* create an X colormap since probably not using default visual */
    cmap = XCreateColormap(d->dpy, RootWindow(d->dpy, x->vis_info->screen),
			   x->vis_info->visual, AllocNone);
    swa.colormap = cmap;
    swa.border_pixel = 0;
    swa.event_mask = ExposureMask | ButtonPressMask | StructureNotifyMask;



    /* create a window using this visual */
    w->win = XCreateWindow(
	d->dpy,
	RootWindow(d->dpy, x->vis_info->screen), 
	w->winxoffset, w->winyoffset, 
	w->winwidth, w->winheight, 0,
	x->vis_info->depth,
	InputOutput, x->vis_info->visual,
	CWBorderPixel | CWColormap | CWEventMask, &swa);

    x->initialized = 1;

    return 1;

  error:
    
    return 0;
}



int GLXExtensionSupported(Display *dpy, const char *extension)
{
    const char *extensionsString;
    char *pos;
    extensionsString = glXQueryExtensionsString(dpy, DefaultScreen(dpy));
    pos = strstr(extensionsString, extension);
    return pos!=NULL && (pos==extensionsString || pos[-1]==' ') &&
        (pos[strlen(extension)]==' ' || pos[strlen(extension)]=='\0');
}

void handle_error(char *msg){
    fprintf(stderr, "%s\n", msg);
    exit(1);
}





