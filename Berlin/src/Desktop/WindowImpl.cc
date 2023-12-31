/*$Id: WindowImpl.cc,v 1.33 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include "Desktop/WindowImpl.hh"
#include <Berlin/Vertex.hh>
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/IO.hh>

using namespace Prague;
using namespace Warsaw;
using namespace Layout;

class WindowImpl::UnmappedStageHandle : public virtual POA_Layout::StageHandle,
		                        public virtual ServantBase
{
public:
  UnmappedStageHandle(Stage_ptr, Graphic_ptr, const Vertex &, const Vertex &, Stage::Index);
  UnmappedStageHandle(StageHandle_ptr);
  virtual ~UnmappedStageHandle();
  virtual Stage_ptr parent() { return Stage::_duplicate(_parent);}
  virtual Graphic_ptr child() { return Warsaw::Graphic::_duplicate(_child);}
  virtual void remove() {}
  virtual Vertex position() { return _position;}
  virtual void position(const Vertex &pp) { _position = pp;}
  virtual Vertex size() { return _size;}
  virtual void size(const Vertex &ss) { _size = ss;}
  virtual Stage::Index layer() { return _layer;}
  virtual void layer(Stage::Index ll) { _layer = ll;}
private:
  Stage_var    _parent;
  Graphic_var  _child;
  Vertex       _position;
  Vertex       _size;
  Stage::Index _layer;
};

WindowImpl::UnmappedStageHandle::UnmappedStageHandle(Stage_ptr par, Graphic_ptr cc,
						     const Vertex &pp, const Vertex &ss, Stage::Index ll)
  : _parent(Stage::_duplicate(par)), _child(Warsaw::Graphic::_duplicate(cc)), _position(pp), _size(ss), _layer(ll) {}
WindowImpl::UnmappedStageHandle::UnmappedStageHandle(StageHandle_ptr handle)
  : _parent(handle->parent()),
    _child(handle->child()),
    _position(handle->position()),
    _size(handle->size()),
    _layer(handle->layer())
{}
WindowImpl::UnmappedStageHandle::~UnmappedStageHandle() { Trace trace("UnmappedStageHandle::~UnmappedStageHandle");}

WindowImpl::WindowImpl()
  : ControllerImpl(false)
{
  Trace trace("WindowImpl::WindowImpl");
}

WindowImpl::~WindowImpl()
{
  Trace trace("WindowImpl::~WindowImpl");
  mapped(false);
}

void WindowImpl::need_resize()
{
  Trace trace("WindowImpl::need_resize");
  Vertex size = _handle->size();
  Warsaw::Graphic::Requisition r;
  request(r);
  if (r.x.minimum <= size.x && r.x.maximum >= size.x &&
      r.y.minimum <= size.y && r.y.maximum >= size.y &&
      r.z.minimum <= size.z && r.z.maximum >= size.z)
    need_redraw();
  else
    {
      size.x = std::min(r.x.maximum, std::max(r.x.minimum, size.x));
      size.y = std::min(r.y.maximum, std::max(r.y.minimum, size.y));
      size.z = std::min(r.z.maximum, std::max(r.z.minimum, size.z));
      _handle->size(size);
    }
}

/*
 * cache the focus holding controllers so we can restore them when the window
 * receives focus again...
 */
CORBA::Boolean WindowImpl::request_focus(Controller_ptr c, Warsaw::Input::Device d)
{
  if (_unmapped) return false;
  Controller_var parent = parent_controller();
  if (CORBA::is_nil(parent)) return false;
  if (parent->request_focus(c, d))
    {
      if (_focus.size() <= d) _focus.resize(d + 1);
      _focus[d] = Warsaw::Controller::_duplicate(c);
      return true;
    }
  else return false;
}

void WindowImpl::insert(Desktop_ptr desktop)
{
  Trace trace("WindowImpl::insert");
  Vertex position, size;
  position.x = position.y = 1000., position.z = 0.;
  Warsaw::Graphic::Requisition r;
  request(r);
  size.x = r.x.natural, size.y = r.y.natural, size.z = 0;
  _unmapped = new UnmappedStageHandle(desktop, Graphic_var(_this()), position, size, 0);
  _handle = _unmapped->_this();
}

Vertex WindowImpl::position()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _handle->position();
}

void WindowImpl::position(const Vertex &p)
{
  Trace trace("WindowImpl::position");
  Prague::Guard<Mutex> guard(_mutex);
  _handle->position(p);
}

Vertex WindowImpl::size()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _handle->size();
}

void WindowImpl::size(const Vertex &s)
{
  Trace trace("WindowImpl::size");
  Prague::Guard<Mutex> guard(_mutex);
  _handle->size(s);
}

Stage::Index WindowImpl::layer()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _handle->layer();
}

void WindowImpl::layer(Stage::Index l)
{
  Trace trace("WindowImpl::layer");
  Prague::Guard<Mutex> guard(_mutex);
  _handle->layer(l);
}

CORBA::Boolean WindowImpl::mapped()
{
  Prague::Guard<Mutex> guard(_mutex);
  return !_unmapped;
}

void WindowImpl::mapped(CORBA::Boolean flag)
{
  Trace trace("WindowImpl::mapped");
  if (flag)
    /*
     * map
     */
    {
      Prague::Guard<Mutex> guard(_mutex);
      if (!_unmapped) return;
      Stage_var stage = _handle->parent();
      stage->begin();
      StageHandle_var tmp = stage->insert(Graphic_var(_this()), _handle->position(), _handle->size(), _handle->layer()); 
      stage->end();
      _handle = tmp;
      _unmapped = 0;
    }
  else
    /*
     * unmap
     */
    {
      Prague::Guard<Mutex> guard(_mutex);
      if (_unmapped) return;
      _unmapped = new UnmappedStageHandle(_handle);
      _handle->remove();
      _handle = _unmapped->_this();
    }
}
