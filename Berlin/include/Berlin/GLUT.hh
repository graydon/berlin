/*$Id: GLUT.hh,v 1.7 2001/03/03 00:09:54 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Niklas Elmqvist <elm@3dwm.org>
 * http://www.berlin-consortium.org
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

#ifndef _GLUT_hh
#define _GLUT_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Input.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadQueue.hh>
#include <vector>

using namespace Warsaw;

/* This is a console implementation based on GLUT, meaning that all
 * that is required is an OpenGL installation complete with GLUT, the
 * GL utility toolkit. In other words, it should be possible to run
 * Berlin on a Win32 system using this console, or at least on top of
 * X on Linux system and take advantage of hardware acceleration. Of
 * course, since we are using GLUT, we only support the GLDrawingKit.
 *
 * GLUT was not really meant to be used in a realistic application
 * like Berlin, and is used to getting control of the main loop and
 * stuff like when to redraw, poll for input, shut down, etc.  This is
 * of course not possible in Berlin, so I've designed the GLUTConsole
 * around a producer/consumer scheme with a dedicated thread that
 * makes GLUT happy (then it can make-believe that it retains control
 * of the main loop).
 *
 * However, there are some nasty problems with GLUT not being
 * thread-safe nor instance-safe.  We're currently being plagued by
 * deadlocks between the GLUT thread and the drawing/event polling
 * thread, and GLUT sometimes rolls itself into a little ball and
 * shuts out the world when it is forced to play with the other
 * children. Nevertheless, we do get graphical output between lockups,
 * so the main concept is sound!
 * 
 * I've received word of new implementations of GLUT (called freeglut
 * and slimglut) that reportedly will be thread-safe. I will likely go
 * back and revisit the GLUTConsole then.
 * 
 * Still, this is a sample console implementation that could serve as
 * a useful template for budding console writers, especially when you
 * have to take to dirty tricks like I had to. :)
 * 
 * To activate the GLUT console, configure the server with
 * --with-console=GLUT, and the rest should be automatic. You also
 * need to comment out the calls to readBuffer/writeBuffer in
 * Pointer.cc, since those calls are not (and cannot) be implemented
 * in the GLUTDrawable.
 * 
 * /elm, 2000-08-29
 **/

// @@@ FIXME! Need a better way for this (can't hardcode it!)
// Screen resolution (in pixels)
const Coord screenx = 1024;
const Coord screeny = 786;

// Screen dimensions (physical size in millimeters)
const Coord screendimx = 360.0f;
const Coord screendimy = 270.0f;


using namespace Prague;

// -- Forward Declarations
class GLUTConsole;
class GLUTHandler;

// -- Class Declarations

/**
 * GLUT drawable implementation. This drawable will be used by the
 * GLDrawingKit only.
 **/
class GLUTDrawable {
  
private:
  
    friend class GLUTConsole;
    friend class GLUTHandler;
    friend class DrawableTie<GLUTDrawable>;
  
public:
    typedef long Pixel;
    DrawableTie<GLUTDrawable>::PixelFormat pixel_format();
    Drawable::BufferFormat buffer_format();
    PixelCoord width() const { return _width; }
    PixelCoord height() const { return _height; }
    PixelCoord vwidth() const { return _width;}
    PixelCoord vheight() const { return _height;}
    Coord resolution(Axis a) const;
    Coord dpi(Axis a) const { return resolution(a) * 254.0; }
    PixelCoord row_length() { return 0; }
    Pixel map(const Color &) { return 0; }
    void *read_buffer() { return 0; }
    void *write_buffer() { return 0; }
  
    /*
     * Read one or more pixels from framebuffer
     */
    void read_pixel(PixelCoord x, PixelCoord y, Pixel &p) { }
    void read_pixels(PixelCoord, PixelCoord, PixelCoord, PixelCoord, void *) { }
  
    /*
     * Draw primitives with the current color (Pixel)
     */
    void set_color(Pixel p) { } 
    void draw_pixel(PixelCoord x, PixelCoord y) { }
    void draw_hline(PixelCoord x, PixelCoord y, PixelCoord w) { }
    void draw_vline(PixelCoord x, PixelCoord y, PixelCoord h) { }
    void draw_line(PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h) { }
    void draw_box(PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h) { }
  
    /*
     * Draw primitives with the given color (Pixel). 
     */
    void put_pixel(PixelCoord x, PixelCoord y, Pixel p) { }
    void put_hline(PixelCoord x, PixelCoord y, PixelCoord w, void *p) { }
    void put_vline(PixelCoord x, PixelCoord y, PixelCoord h, void *p) { }
    void draw_pixels(PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h, void *p) { }
  
    /*
     * Fast blits
     */
    void blit(PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord);
    void blit(const GLUTDrawable &, PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord);
  
    void flush() { }
    void flush(PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h) { }
  
    void init();
    void finish();
  
    /**
     * Render the "cached" scene (i.e. the display lists comprising
     * the scene). This is a slight breach of abstraction, but it is
     * by far the easiest solution.
     **/
    void render();
  
  
    /**
     * GLUT window may be reshaped by the user.
     **/
    void reshape(int width, int height);
  
private:
  
    // Constructor and destructor
    GLUTDrawable();
    ~GLUTDrawable();
  
    /// Display width
    int _width;
  
    /// Display height
    int _height;
  
    /// Main display list
    unsigned int _displist;

    /// Is the drawable dirty? (i.e. do we need to redraw?)
    bool _dirty;

    /// Access mutex (to control access to display lists)
    Mutex _mutex;
};


/**
 * GLUT console implementation.
 **/
class GLUTConsole {
  
    friend class GLUTHandler;
  
public:
  
    typedef GLUTDrawable Drawable;
    GLUTConsole(int &argc, char **argv, PortableServer::POA_ptr);
    ~GLUTConsole();
    static DrawableTie<Drawable> *drawable() { return _drawable; }
    static DrawableTie<Drawable> *create_drawable(PixelCoord, PixelCoord, PixelCoord) { return 0; }
    static DrawableTie<Drawable> *create_drawable(Drawable *) { return 0; }
    
    Input::Event *next_event();
    void wakeup();
    void activate_autoplay() { _autoplay = true; }
  
private:
  
    bool _autoplay;
  
    /// Capacity of event queue
    static const int eventQueueCapacity = 100;
  
    /// Drawable vector
    //static vector<DrawableTie<Drawable> *> _drawables;
    static DrawableTie<Drawable> *_drawable;
  
    /// Event producer-consumer queue (for polling threads)
    Thread::Queue<Input::Event *> _eventQueue;
  
};

#endif /* GLUT.hh */
