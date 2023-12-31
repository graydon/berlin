/*$Id: CAVE.hh,v 1.5 2000/09/28 15:46:25 stefan Exp $
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

#ifndef _CAVE_hh
#define _CAVE_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/Input.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadQueue.hh>
#include <vector>

using namespace Prague;

// Cavelib OpenGL bindings
extern "C" {
    //#include <cave_ogl.h>
}

// -- Forward Declarations
class CAVEConsole;
class CAVEEventListener;

// -- Class Declarations

/**
 * Cavelib drawable implementation (used as a tie implementation).  A
 * large part of the cavelib drawable has been left out since it is
 * not needed (and indeed, not supported) by the OpenGL CAVELib.  The
 * CAVEConsole/CAVEDrawable will be used by the GLDrawingKit
 * exclusively, so this is not an issue.
 **/
class CAVEDrawable {

private:
    
    friend class CAVEConsole;
    friend class DrawableTie<CAVEDrawable>;
  
public:
    typedef long Pixel;
    DrawableTie<CAVEDrawable>::PixelFormat pixel_format();
    
    PixelCoord width() const { return _width; }
    PixelCoord height() const { return _height; }
    PixelCoord vwidth() const { return _width;}
    PixelCoord vheight() const { return _height;}
    Coord resolution(Axis a) const { return _resolution; }
    Coord dpi(Axis a) const { return resolution(a) * 254.0; }
    PixelCoord row_length() { return 0; }
    Pixel map(const Color &) { return 0; }
    void *read_buffer() { return 0; }
    void *write_buffer() { return 0; }
    
    /*
     * Read one or more pixels from framebuffer
     */
    void readPixel(PixelCoord x, PixelCoord y, Pixel &p) { }
    void readPixels(PixelCoord, PixelCoord, PixelCoord, PixelCoord, void *) { }
    
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
    void blit(PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord) { }
    void blit(const CAVEDrawable &, PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord, PixelCoord) { }
    
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
    
private:
    
    // Constructor and destructor
    CAVEDrawable();
    ~CAVEDrawable();
    
    // Display width (set once from a rendering process)
    int _width;
    
    // Display height (set once from a rendering process)
    int _height;
  
    // Resolution 
    float _resolution;
    
    /// Main display list
    unsigned int _displist;

    /// Access mutex (to control access to display lists)
    Mutex _mutex;

};


/**
 * CAVE event listener (runs in its own thread and listens for CAVE
 * events to add to the CAVEConsole event queue).
 **/
class CAVEEventListener {

public:
    
    CAVEEventListener(CAVEConsole *console, Thread::Queue<Input::Event *> &queue) 
	: _listener(proc, this), _queue(queue), _console(console) { }
    ~CAVEEventListener() { cancel(); }
    void run() { _listener.start(); }
    void cancel() { _listener.join(0); }

private:
    
    /**
     * Listener thread bootstrapping function.
     **/
    static void *proc(void *X) {
	CAVEEventListener *listener = reinterpret_cast<CAVEEventListener *>(X);
	listener->start();
	return 0;
    }

    /**
     * Main listening function.
     **/
    void start();

    // Listener thread
    Thread _listener;

    // Event queue
    Thread::Queue<Input::Event *> &_queue;

    // Console handle
    CAVEConsole *_console;
};

/**
 * Cavelib console implementation (also a tie implementation).
 **/
class CAVEConsole {
  
public:
    
    typedef CAVEDrawable Drawable;
    CAVEConsole(int &argc, char **argv);
    ~CAVEConsole();
    static DrawableTie<Drawable> *drawable() { return _drawable; }
    static DrawableTie<Drawable> *create_drawable(PixelCoord, PixelCoord, PixelCoord);
    static DrawableTie<Drawable> *create_drawable(Drawable *);
    
    Input::Event *next_event();
    void wakeup();
    void activate_autoplay() { _autoplay = true; }
    
private:

    /// Cavelib rendering callback
    static void render();

    /// Automatic replay of events?
    bool _autoplay;

    /// Capacity of event queue
    static const int eventQueueCapacity = 100;
    
    /// Single CAVE drawable instance
    static DrawableTie<Drawable> *_drawable;

    /// Event producer-consumer queue (for polling threads)
    Thread::Queue<Input::Event *> _eventQueue;

    /// Event listening thread
    CAVEEventListener _listener;
    
};

#endif /* CAVE.hh */
