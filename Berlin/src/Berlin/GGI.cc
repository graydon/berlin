/*$Id: GGI.cc,v 1.22 2001/04/18 06:07:26 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Prague/Sys/FdSet.hh>
#include <Prague/Sys/Tracer.hh>
#include "Berlin/Console.hh"

using namespace Prague;
using namespace Warsaw;

GGIConsole::dlist_t GGIConsole::_drawables;
PortableServer::POA_var GGIConsole::_poa;

GGIConsole::GGIConsole(int &argc, char **argv, PortableServer::POA_ptr poa)
  : _autoplay(false)
{
  ggiInit();
  GGIDrawable *drawable = new GGIDrawable(0);
  _visual = drawable->_visual;
  _size[0] = drawable->_mode.visible.x;
  _size[1] = drawable->_mode.visible.y;
  _position[0] = 0;
  _position[1] = 0;
  _resolution[0] = 0.1 * drawable->_mode.visible.x / drawable->_mode.size.x;
  _resolution[1] = 0.1 * drawable->_mode.visible.y / drawable->_mode.size.y;

  _drawables.push_back(new DrawableTie<Drawable>(drawable));
  _poa = PortableServer::POA::_duplicate(poa);
  pipe(_wakeupPipe);
}

GGIConsole::~GGIConsole()
{
  for (dlist_t::iterator i = _drawables.begin(); i != _drawables.end(); i++) delete *i;
  close(_wakeupPipe[0]);
  close(_wakeupPipe[1]);
  ggiExit();
}

DrawableTie<GGIDrawable> *GGIConsole::drawable()
{
  Trace trace("GGIConsole::drawable");
  assert(_drawables.size());
  return _drawables.front();
}

DrawableTie<GGIDrawable> *GGIConsole::create_drawable(PixelCoord w, PixelCoord h, PixelCoord d)
{
  _drawables.push_back(new DrawableTie<GGIDrawable>(new GGIDrawable("display-memory", w, h, d)));
  return _drawables.back();
}

DrawableTie<GGIDrawable> *GGIConsole::create_drawable(GGIDrawable *drawable)
{
  _drawables.push_back(new DrawableTie<GGIDrawable>(drawable));
  return _drawables.back();
}

Warsaw::Drawable_ptr GGIConsole::activate_drawable(DrawableTie<GGIDrawable> *d)
{
  Trace trace("GGIConsole::activate_drawable");
  PortableServer::ObjectId *oid = _poa->activate_object(d);
  d->_remove_ref();
  delete oid;
  return d->_this();
}

DrawableTie<GGIDrawable> *GGIConsole::reference_to_servant(Warsaw::Drawable_ptr drawable)
{
  Trace trace("GGIConsole::reference_to_servant");
  try
    {
      PortableServer::Servant servant = _poa->reference_to_servant(drawable);
      for (dlist_t::iterator i = _drawables.begin(); i != _drawables.end(); ++i)
	if (*i == servant) return *i;
    }
  catch (const PortableServer::POA::ObjectNotActive &) {}
  catch (const PortableServer::POA::WrongAdapter &) {}
  catch (const CORBA::OBJECT_NOT_EXIST &) {}
  return 0;
}

static void readEvent(ggi_event &e)
{
  unsigned int t;
  cin >> t;
  e.any.type = (char)t;
  switch (e.any.type)
    {
    case evKeyPress:
    case evKeyRepeat:
      {
	cin >> t;
	e.key.sym = t;
	break;
      }
    case evPtrRelative:
    case evPtrAbsolute:
      {
	cin >> e.pmove.x
	    >> e.pmove.y;
	break;
      }
    case evPtrButtonPress:
    case evPtrButtonRelease:
      {
	break;
      }
  }
}

static void writeEvent(ggi_event &e)
{
  cout << ((unsigned int)(e.any.type)) << ' ';
  switch (e.any.type)
    {
    case evKeyPress:
    case evKeyRepeat:
      {
	cout << ((unsigned int)(e.key.sym));
	break;
      }
    case evPtrRelative:
    case evPtrAbsolute:
      {
	cout << e.pmove.x << ' '
	     << e.pmove.y;
	break;
      }
    case evPtrButtonPress:
    case evPtrButtonRelease:
      {
	break;
      }
    }
  cout << endl;
}

Input::Event *GGIConsole::next_event()
{
  Prague::Trace trace("GGI::Console::next_event");
  ggi_event event;
  ggi_event_mask mask;
  ggi_event_mask move_mask = ggi_event_mask(emPtrMove | emValuator);

  int input = fileno(stdin);
  Prague::FdSet rfdset;

  int nfds = -1;
  do
    {
      mask = ggi_event_mask(emKeyboard | emPtrMove | emPtrButton | emValuator);
      rfdset.set (_wakeupPipe[0]);
      if (_autoplay)
	rfdset.set (input);
      nfds = ggiEventSelect(_visual, &mask, rfdset.max() + 1, rfdset, 0, 0, 0);
    }
  while (nfds == -1 && errno == EINTR);

  if (nfds == 0)
    {
      // no input from the outside world
      ggiEventRead(_visual, &event, mask); 
      if (event.any.type == evPtrRelative || event.any.type == evPtrAbsolute)// ||
	//	  event.any.type == evValAbsolute)
	{
	  int m = ggiEventsQueued(_visual, mask);
	  int n = ggiEventsQueued(_visual, move_mask);
	  //	  cout << "events " << m << ' ' << n << endl;
	  if (m == n)  // nothing but a bunch of moves queued up
	    {
	      int x = event.pmove.x, y = event.pmove.y;
	      for (int i = 0; i < n; ++i)
		{
		  // consume them all
		  ggiEventRead(_visual, &event, move_mask); 	  
		  if (event.any.type == evPtrRelative)
		    {
		      x += event.pmove.x;
		      y += event.pmove.y;
		    }
		}
	      if (event.any.type == evPtrRelative)
		{
		  event.pmove.x = x;
		  event.pmove.y = y;
		}
	    }
	}
      if (_autoplay) writeEvent(event);
      return synthesize(event);
    }
  else if (nfds > 0)
    {
      if (_autoplay && rfdset.isset(input))
	{
	  readEvent(event);
	  return synthesize(event);
	}
      else
	{
	  if (rfdset.isset(_wakeupPipe[0]))
	    {
	      char c; read(_wakeupPipe[0], &c, 1);
	      return 0;
	    }
	}
    }
  return 0;
}

void GGIConsole::wakeup() { char c = 'z'; write(_wakeupPipe[1],&c,1);}

Input::Event *GGIConsole::synthesize(const ggi_event &e)
{
  Input::Event_var event = new Input::Event;
  switch (e.any.type)
    {
    case evKeyPress:
      {
	Input::Toggle toggle;
	toggle.actuation = Input::Toggle::press;
	toggle.number = e.key.sym;
	event->length(1);
	event[0].dev = 0;
	event[0].attr.selection(toggle); event[0].attr._d(Input::key);
	break;
      }
    case evKeyRepeat:
      {
	Input::Toggle toggle;
	toggle.actuation = Input::Toggle::hold;
	toggle.number = e.key.sym;
	event->length(1);
	event[0].dev = 0;
	event[0].attr.selection(toggle); event[0].attr._d(Input::key);
	break;
      }
    case evPtrRelative:
      {
	if (_position[0] + e.pmove.x >= 0 && _position[0] + e.pmove.x < _size[0]) _position[0] += e.pmove.x;
	if (_position[1] + e.pmove.y >= 0 && _position[1] + e.pmove.y < _size[1]) _position[1] += e.pmove.y;	  
	Input::Position position;
	position.x = _position[0]/_resolution[0];
	position.y = _position[1]/_resolution[1];
	position.z = 0;
	event->length(1);
	event[0].dev = 1;
	event[0].attr.location(position);
	break;
      }
    case evPtrAbsolute:
      {
	_position[0] = e.pmove.x;
	_position[1] = e.pmove.y;
	Input::Position position;
	position.x = _position[0]/_resolution[0];
	position.y = _position[1]/_resolution[1];
	position.z = 0;
	event->length(1);
	event[0].dev = 1;
	event[0].attr.location(position);
	break;
      }
    case evPtrButtonPress:
    case evPtrButtonRelease:
      {
	Input::Toggle toggle;
	if (e.any.type == evPtrButtonPress)
	  toggle.actuation = Input::Toggle::press;
	else
	  toggle.actuation = Input::Toggle::release;
 	toggle.number = e.pbutton.button;	  
	Input::Position position;
	position.x = _position[0]/_resolution[0];
	position.y = _position[1]/_resolution[1];
	position.z = 0;
	event->length(2);
	event[0].dev = 1;
	event[0].attr.selection(toggle); event[0].attr._d(Input::button);
	event[1].dev = 1;
	event[1].attr.location(position);
	break;
      }
    case evValAbsolute:
      {
	// wacom intuo (quick hack):
	// stylus uses val numbers 0 and 1 for x/y
	for (int i = 0; i != e.val.count; ++i)
	  {
	    int number = e.val.first + i;
	    if (number == 0) _position[0] = (e.val.value[i]*800)/20320;
	    else if (number == 1) _position[1] = (e.val.value[i]*600)/15040;;
	  }
	Input::Position position;
	position.x = _position[0]/_resolution[0];
	position.y = _position[1]/_resolution[1];
	position.z = 0;
	event->length(1);
	event[0].dev = 1;
	event[0].attr.location(position);
	break;
      }
    }
  return event._retn();
}

GGIDrawable::GGIDrawable(const char *display, PixelCoord w, PixelCoord h, PixelCoord d) // throw (exception)
{
  if (display) _visual = ggiOpen(display, 0);
  else _visual = ggiOpen(0);
  if (!_visual) throw std::exception();

  long depth = GGI_AUTO;
  switch (d)
    {
    case 0: depth = GGI_AUTO; break;
    case 1: depth = GT_8BIT; break;
    case 2: depth = GT_16BIT; break;
    case 3: depth = GT_24BIT; break;
    case 4: depth = GT_32BIT; break;
    default:
      cerr << "GGIDrawable: Warning: " << d << " bytes per pixel not supported" << endl;
      break;
    };
  _mode.visible.x = _mode.virt.x = w ? w : GGI_AUTO;
  _mode.visible.y = _mode.virt.y = h ? h : GGI_AUTO;
  _mode.size.x = GGI_AUTO;
  _mode.size.y = GGI_AUTO;
  _mode.dpp.x = _mode.dpp.y = 1;
  _mode.graphtype = depth;
  _mode.frames = 1;
  if (ggiCheckMode(_visual, &_mode) == 0)
    {
      if (ggiSetMode(_visual, &_mode) != 0) throw std::exception();
    }
  else
    {
      if (ggiCheckMode(_visual, &_mode) != 0 || ggiSetMode(_visual, &_mode) != 0) throw std::exception();
    }
  _mode.size = _mode.visible; // awful hack around a ggi bug...
  ggiAddFlags(_visual, GGIFLAG_ASYNC);
}

GGIDrawable::~GGIDrawable()
{
  ggiClose(_visual);
}

Warsaw::Drawable::PixelFormat GGIDrawable::pixel_format()
{
  Warsaw::Drawable::PixelFormat format;
  const ggi_pixelformat *pf = ggiGetPixelFormat(_visual);
  format.depth       = pf->depth;
  format.size        = pf->size;
  format.red_mask    = pf->red_mask;
  format.red_shift   = pf->red_shift;
  format.green_mask  = pf->green_mask;
  format.green_shift = pf->green_shift;
  format.blue_mask   = pf->blue_mask;
  format.blue_shift  = pf->blue_shift;
  format.alpha_mask  = pf->alpha_mask;
  format.alpha_shift = pf->alpha_shift;
  return format;
}

Warsaw::Drawable::BufferFormat GGIDrawable::buffer_format()
{
  Warsaw::Drawable::BufferFormat format;
  format.skip_width = 0;
  format.width = width();
  format.skip_height = 0;
  format.height = height();
  format.row_length = row_length();
  return format;
}
