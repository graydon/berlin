/*$Id: WindowImpl.cc,v 1.12 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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

#include "Berlin/Vertex.hh"
#include "Berlin/Logger.hh"
#include "Desktop/WindowImpl.hh"

using namespace Prague;

class Mover : public WindowImpl::Manipulator
{
public:
  virtual void execute(const CORBA::Any &any)
    {
      if (CORBA::is_nil(handle)) return;
      Vertex *delta;
      if (any >>= delta)
	{
 	  Vertex p = handle->position();
	  handle->position(p + *delta);
	}
      else  cerr << "Mover::execute : wrong message type !" << endl;
    }
};

class Resizer : public WindowImpl::Manipulator
{
public:
  virtual void execute(const CORBA::Any &any)
    {
      if (CORBA::is_nil(handle)) return;
      Vertex *delta;
      if (any >>= delta)
	{
 	  Vertex s = handle->size();
          Graphic::Requisition r;
          handle->child()->request(r);
          if (r.x.defined)
            {
              if (delta->x > 0.) s.x = min(s.x + delta->x, r.x.maximum);
              else s.x = max(s.x + delta->x, r.x.minimum);
            }
          else s.x += delta->x;
          if (r.y.defined)
            {
              if (delta->y > 0.) s.y = min(s.y + delta->y, r.y.maximum);
              else s.y = max(s.y + delta->y, r.y.minimum);
            }
          else s.y += delta->y;
	  handle->size(s);
	}
      else cerr << "Resizer::execute : wrong message type !" << endl;
    }
};

class MoveResizer : public WindowImpl::Manipulator
{
public:
  MoveResizer(Alignment x, Alignment y, CORBA::Short b) : xalign(x), yalign(y), border(b) {}
  virtual void execute(const CORBA::Any &any)
    {
      SectionLog section("MoveResizer::execute");
      if (CORBA::is_nil(handle)) return;
      Vertex *vertex;
      if (any >>= vertex)
	{
          Graphic::Requisition r;
          handle->child()->request(r);
 	  Vertex pos = handle->position();
 	  Vertex size = handle->size();
	  Vertex p = pos, s = size;
	  if (border & Window::left && xalign != 0.)
	    {
	      s.x = min(r.x.maximum, max(r.x.minimum, size.x - vertex->x/xalign));
	      p.x = pos.x - xalign * (s.x - size.x);
	    }
	  else if (border & Window::right && xalign != 1.)
	    {
	      s.x = min(r.x.maximum, max(r.x.minimum, size.x + vertex->x/(1.-xalign)));
	      p.x = pos.x - xalign * (s.x - size.x);
	    }
	  if (border & Window::top && yalign != 0.)
	    {
	      s.y = min(r.y.maximum, max(r.y.minimum, size.y - vertex->y/yalign));
	      p.y = pos.y - yalign * (s.y - size.y);
	    }
	  else if (border & Window::bottom && yalign != 1.)
	    {
	      s.y = min(r.y.maximum, max(r.y.minimum, size.y + vertex->y/(1.-yalign)));
	      p.y = pos.y - yalign * (s.y - size.y);
	    }
	  handle->parent()->begin();
	  handle->position(p);
	  handle->size(s);
	  handle->parent()->end();
	}
      else cerr << "MoveResizer::execute : wrong message type !" << endl;
    }
private:
  Alignment xalign, yalign;
  CORBA::Short border;
};

class Relayerer : public WindowImpl::Manipulator
{
public:
  virtual void execute(const CORBA::Any &any)
    {
      if (CORBA::is_nil(handle)) return;
      Stage::Index i;
      if (any >>= i)
	{
	  handle->layer(i);
	}
      else cerr << "Relayerer::execute : wrong message type !" << endl;
    }
};

void WindowImpl::Mapper::execute(const CORBA::Any &)
{
  if (flag) window->map();
  else window->unmap();
}

WindowImpl::WindowImpl()
  : ControllerImpl(false), unmapped(0), manipulators(3), mapper(0)
{
  manipulators[0] = new Mover;
  manipulators[0]->_obj_is_ready(_boa());
  manipulators[1] = new Resizer;
  manipulators[1]->_obj_is_ready(_boa());
  manipulators[2] = new Relayerer;
  manipulators[2]->_obj_is_ready(_boa());
  mapper = new Mapper(this, true);
  mapper->_obj_is_ready(_boa());
  unmapper = new Mapper(this, false);
  unmapper->_obj_is_ready(_boa());
}

WindowImpl::~WindowImpl()
{
  for (mtable_t::iterator i = manipulators.begin(); i != manipulators.end(); i++)
    (*i)->_dispose();
  mapper->_dispose();
  unmapper->_dispose();
}

void WindowImpl::insert(Desktop_ptr desktop, bool mapped)
{
  SectionLog section("WindowImpl::insert");
  Vertex position, size;
  position.x = position.y = 100., position.z = 0.;
  Graphic::Requisition r;
  request(r);
  size.x = r.x.natural, size.y = r.y.natural, size.z = 0;
  unmapped = new UnmappedStageHandle(desktop, Graphic_var(_this()), position, size, 0);
  unmapped->_obj_is_ready(_boa());
  handle = StageHandle_var(unmapped->_this());
  if (mapped) map();
}

Command_ptr WindowImpl::move() { return manipulators[0]->_this();}
Command_ptr WindowImpl::resize() { return manipulators[1]->_this();}
Command_ptr WindowImpl::moveResize(Alignment x, Alignment y, CORBA::Short b)
{
  manipulators.push_back(new MoveResizer(x, y, b));
  manipulators.back()->_obj_is_ready(_boa());
  return manipulators.back()->_this();
}
Command_ptr WindowImpl::relayer() { return manipulators[2]->_this();}
Command_ptr WindowImpl::map(CORBA::Boolean f)
{
  return f ? mapper->_this() : unmapper->_this();
}

// void WindowImpl::pick(PickTraversal_ptr traversal)
// {
//   SectionLog section("WindowImpl::pick");
//   traversal->enterController(Controller_var(_this()));
//   MonoGraphic::traverse(traversal);
//   traversal->leaveController();
// }

void WindowImpl::map()
{
  MutexGuard guard(mutex);
  if (!unmapped) return;
  Stage_var stage = handle->parent();
  stage->begin();
  StageHandle_var tmp = stage->insert(Graphic_var(_this()), handle->position(), handle->size(), handle->layer()); 
  stage->end();
  handle = tmp;
  for (mtable_t::iterator i = manipulators.begin(); i != manipulators.end(); i++)
    (*i)->bind(handle);
  unmapped->_dispose();
  unmapped = 0;
}

void WindowImpl::unmap()
{
  MutexGuard guard(mutex);
  if (unmapped) return;
  unmapped = new UnmappedStageHandle(handle);
  unmapped->_obj_is_ready(_boa());
  Stage_var stage = handle->parent();
  stage->begin();
  stage->remove(handle); 
  stage->end();
}
