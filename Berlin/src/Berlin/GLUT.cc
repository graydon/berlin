/*$Id: GLUT.cc,v 1.7 2001/03/03 00:09:54 tobias Exp $
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

#include "Berlin/Console.hh"
#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Thread.hh>
#include <Prague/Sys/ThreadQueue.hh>

#include <cmath>

// -- OpenGL Includes

extern "C" {
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
}

// Local GLUT handler. GLUT 
class GLUTHandler {
    struct arg_t {
	arg_t(int c, char **v, const char *n, int ww, int hh)
	    : argc(c), argv(v), name(n), w(ww), h(hh)
	{ }
	int argc;
	char **argv;
	const char *name;
	int w, h;
    };
    friend class GLUTDrawable;
public:
    static void init(GLUTConsole *console, int &argc, char **argv, const char *name, int w, int h);    
private:
    
    static void *start(void *);
    
    static void render();
    static void reshape(int width, int height);
    static void keyboard(unsigned char key, int x, int y);
    static void special(int key, int x, int y);
    static void mouse(int button, int state, int x, int y);
    static void mouseMotion(int x, int y);
    static void idle();
    
    static Thread *_glutThread;
    static GLUTDrawable *drawable;
    static GLUTConsole *console;
    
    static const int numbuttons = 3;
    static int button_state[numbuttons];
};

// -- Static member variable initializations
Thread *GLUTHandler::_glutThread = 0;
int GLUTHandler::button_state[3];
GLUTConsole *GLUTHandler::console = 0;
GLUTDrawable *GLUTHandler::drawable = 0;
DrawableTie<GLUTDrawable> *GLUTConsole::_drawable = 0;

// -- Code Segment

void GLUTHandler::init(GLUTConsole *c, int &argc, char **argv, const char *name, int w, int h)
{
    console = c;
    drawable = &console->drawable()->impl();
    
    // Initialize state
    for (int i = 0; i < numbuttons; i++) button_state[i] = GLUT_UP;
    arg_t *arg = new arg_t(argc, argv, name, w, h);

    // Start the GLUT thread
    _glutThread = new Thread(&GLUTHandler::start, arg);
    _glutThread->start();
}

void *GLUTHandler::start(void *X)
{
    arg_t *arg = static_cast<arg_t *>(X);
    
    // Initialize the GLUT library
    glutInit(&arg->argc, arg->argv);
    
    // Initialize display mode and the window
    //glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH);
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);
    glutInitWindowSize(arg->w, arg->h);
    
    // Okay, create the window
    glutCreateWindow(arg->name);
    
    // Specify the display callback functions
    glutDisplayFunc(&GLUTHandler::render);
    glutReshapeFunc(&GLUTHandler::reshape);
    
    // Install input handlers
    glutKeyboardFunc(&GLUTHandler::keyboard);
    glutSpecialFunc(&GLUTHandler::special);
    glutMouseFunc(&GLUTHandler::mouse);
    glutMotionFunc(&GLUTHandler::mouseMotion);
    glutPassiveMotionFunc(&GLUTHandler::mouseMotion);
    glutIdleFunc(&GLUTHandler::idle);

    // @@@ Remove the GLUT mouse pointer here later
    
    glutMainLoop();
    return 0;
}

void GLUTHandler::render()
{
    Trace trace("GLUTHandler::render");
    cerr << "GLUTHandler::render() -- enter" << endl;
    
    // Clear the canvas
    //glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
    
    // Render the Berlin scene graph!
    drawable->render();
    
    // Need to flip, we're double-buffered!
    glutSwapBuffers();
    cerr << "GLUTHandler::render() -- done" << endl;
}

void GLUTHandler::reshape(int width, int height)
{
    Trace trace("GLUTHandler::reshape");
    drawable->reshape(width, height);
}

void GLUTHandler::keyboard(unsigned char key, int x, int y)
{
    Trace trace("GLUTHandler::keyboard");
    Input::Event_var event = new Input::Event;
    
    // Keyboard event
    Input::Toggle toggle;
    toggle.actuation = Input::Toggle::press;
    toggle.number = key;
    event->length(1);
    event[0].dev = 0;
    event[0].attr.selection(toggle); event[0].attr._d(Input::key);
    
    // Add the event to the queue
    console->_eventQueue.push(event._retn());
}

void GLUTHandler::special(int key, int x, int y)
{
    Trace trace("GLUTHandler::special");
    // @@@ How to handle special keystrokes?
}

void GLUTHandler::mouse(int button, int state, int x, int y)
{
    Trace trace("GLUTHandler::mouse");
    // Is this change in button state?
    if (button_state[button] != state) {
	Input::Event_var event = new Input::Event;
	
	// Fill out the event object
	Input::Toggle toggle;
	if (state == GLUT_DOWN)
	    toggle.actuation = Input::Toggle::press;
	else
	    toggle.actuation = Input::Toggle::release;
	toggle.number = button;
	Input::Position position;
	position.x = x / drawable->resolution(xaxis);
	position.y = y / drawable->resolution(yaxis);
	position.z = 0;
	event->length(2);
	event[0].dev = 1;
	event[0].attr.selection(toggle); event[0].attr._d(Input::button);
	event[1].dev = 1;
	event[1].attr.location(position);
	
	// Add the event to the queue and update state info
	console->_eventQueue.push(event._retn());
	button_state[button] = state;
	
	cerr << "button " << button
	     << " in pos (" 
	     << position.x << ", " << position.y << ")" << endl;
    }
}

void GLUTHandler::mouseMotion(int x, int y)
{
    Trace trace("GLUTHandler::mouseMotion");
    Input::Event_var event = new Input::Event;
    
    // Mouse movement function (we rely on this being called only when
    // changes in the cursor are detected).
    Input::Position position;
    position.x = x / drawable->resolution(xaxis);
    position.y = y / drawable->resolution(yaxis);
    position.z = 0;
    event->length(1);
    event[0].dev = 1;
    event[0].attr.location(position);    
    
    console->_eventQueue.push(event._retn());
}


void GLUTHandler::idle()
{
    Trace trace("GLUTHandler::idle");
    Prague::Guard<Mutex> guard(drawable->_mutex);
    if (drawable->_dirty) {
	drawable->_dirty = false;
	glutPostRedisplay();
    }
    else Thread::delay(500);
}


// -- GLUTDrawable implementation

GLUTDrawable::GLUTDrawable()
    : _displist(0)
{
    // empty (can't allocate display lists before GLUT is initialized)
}

GLUTDrawable::~GLUTDrawable()
{
    Prague::Guard<Mutex> guard(_mutex);
    glDeleteLists(_displist, 1);
    _displist = 0;
}

Coord GLUTDrawable::resolution(Axis a) const
{
    // Return the resolution as dots/pixels per tenth of a millimeter    
    // @@@ The 25 below is a 'magic' number. We need to tweak it!
    return a == xaxis 
	? screenx / (25.0 * screendimx) 
	: screeny / (25.0 * screendimy);
}

void GLUTDrawable::init()
{
    // Start the definition of the display list
    _mutex.lock();
    if (_displist == 0) {
	_displist = glGenLists(1);
	if (_displist == 0) {
	    _mutex.unlock();
	    throw 0;
	}
    }
    cerr << "Locking and rendering in display list " << _displist << endl; 
    glNewList(_displist, GL_COMPILE);
}

void GLUTDrawable::finish()
{
    // End display list definition
    glEndList();
    _dirty = true;
    cerr << "Display list finished and unlocked." << endl;
    _mutex.unlock();
}

void GLUTDrawable::render()
{
    Trace trace("GLUTDrawable::render");
    Prague::Guard<Mutex> guard(_mutex);
    
    // Call display list 
    if (_displist != 0) {
	glCallList(_displist);
	cerr << "Rendered display list " << _displist << endl;
    }
}

void GLUTDrawable::reshape(int width, int height)
{
    Trace trace("GLUTDrawable::reshape");
    _width = width; _height = height;
    cerr << "Reshape to dimensions (" << width << "x" << height 
	 << ") [ " << _width / resolution(xaxis) << "x"
	 << _height / resolution(yaxis) <<"]" << endl;
  
    // Reset the viewport
    glViewport(0, 0, (GLsizei) _width, (GLsizei) _height);
  
    // Set up a new projection mode
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, _width / resolution(xaxis), _height / resolution(yaxis), 0, -1000.0, 1000.0); 
    glTranslatef(0.375, 0.375, 0.);
    glMatrixMode(GL_MODELVIEW);
}

DrawableTie<GLUTDrawable>::PixelFormat GLUTDrawable::pixel_format() {
    // Load dummy values (@@@ How do we fill in this correctly?)
    DrawableTie<GLUTDrawable>::PixelFormat pft = {
	32, 32, 0xff000000, 24, 0x00ff0000, 16, 0x0000ff00, 8, 0x000000ff, 0
    };
    return pft;
}

Drawable::BufferFormat GLUTDrawable::buffer_format() {
  Warsaw::Drawable::BufferFormat format;
  format.skip_width = 0;
  format.width = width();
  format.skip_height = 0;
  format.height = height();
  format.row_length = row_length();
  return format;
}


// -- GLUTConsole implementation 

GLUTConsole::GLUTConsole(int &argc, char **argv, PortableServer::POA_ptr poa)
    : _eventQueue(eventQueueCapacity)
{
    _drawable = new DrawableTie<Drawable>(new GLUTDrawable);
  
    // @@@ Hardcoded stuff for simplicity!
    int winwidth = 640;
    int winheight = 480;
  
    // Initialize and launch the GLUT handling thread
    GLUTHandler::init(this, argc, argv, "Berlin-on-glut", winwidth, winheight);
}

GLUTConsole::~GLUTConsole()
{
    // @@@ Stop the GLUT thread somehow
    delete _drawable;
}

Input::Event *GLUTConsole::next_event()
{
    Trace trace("GLUTConsole::next_event");
    Input::Event_var e = _eventQueue.pop();
    return e._retn();
}

void GLUTConsole::wakeup()
{
    Trace trace("GLUTConsole::wakeup");
    // Add an empty event to the event queue to wake up the combined
    // rendering/input thread. In the event manager, an empty event
    // pointer (NULL or 0) will cause a window repair, so this is
    // correct behavior.
    _eventQueue.push(0);
}


