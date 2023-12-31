/*$Id: MonoGraphic.cc,v 1.33 2001/02/06 19:46:16 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Prague/Sys/Tracer.hh>
#include "Berlin/MonoGraphic.hh"
#include "Berlin/TransformImpl.hh"
#include "Warsaw/Traversal.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Provider.hh"
#include "Berlin/RefCountVar.hh"

using namespace Prague;
using namespace Warsaw;

MonoGraphic::MonoGraphic() { _child.peer = Warsaw::Graphic::_nil();}
MonoGraphic::~MonoGraphic()
{
  Trace trace("MonoGraphic::~MonoGraphic");
  Prague::Guard<Mutex> guard(_mutex);
  if (!CORBA::is_nil(_child.peer))
    try
      {
	_child.peer->decrement();
	_child.peer->remove_parent_graphic(_child.peerId);
      }
    catch(const CORBA::OBJECT_NOT_EXIST &) {}
    catch (const CORBA::COMM_FAILURE &) {}
}

Graphic_ptr MonoGraphic::body()
{
  Trace trace("MonoGraphic::body");
  Prague::Guard<Mutex> guard(_mutex);
  return RefCount_var<Warsaw::Graphic>::increment(_child.peer);
}

void MonoGraphic::body(Graphic_ptr c)
{
  Prague::Guard<Mutex> guard(_mutex);
  if (!CORBA::is_nil(_child.peer))
    try
      {
	_child.peer->remove_parent_graphic(_child.peerId);
	_child.peer->decrement();
      }
    catch(const CORBA::OBJECT_NOT_EXIST &) {}
    catch (const CORBA::COMM_FAILURE &) {}
  _child.peer = Warsaw::Graphic::_duplicate(c);
  if (!CORBA::is_nil(_child.peer))
    try
      {
	_child.peerId = _child.peer->add_parent_graphic(Graphic_var(_this()), 0);
	_child.peer->increment();
      }
    catch(const CORBA::OBJECT_NOT_EXIST &) { _child.peer = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { _child.peer = Warsaw::Graphic::_nil();}
  //   needResize();
}

void MonoGraphic::append_graphic(Graphic_ptr c)
{
  Prague::Guard<Mutex> guard(_mutex);
  if (!CORBA::is_nil(_child.peer))
    try { _child.peer->append_graphic(c);}
    catch (const CORBA::OBJECT_NOT_EXIST &) { _child.peer = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { _child.peer = Warsaw::Graphic::_nil();}
}

void MonoGraphic::prepend_graphic(Graphic_ptr c)
{
  Prague::Guard<Mutex> guard(_mutex);
  if (!CORBA::is_nil(_child.peer))
    try { _child.peer->prepend_graphic(c);}
    catch (const CORBA::OBJECT_NOT_EXIST &) { _child.peer = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { _child.peer = Warsaw::Graphic::_nil();}
}

void MonoGraphic::remove_graphic(Tag localId)
{
  Trace trace("MonoGraphic::remove_graphic");
  Prague::Guard<Mutex> guard(_mutex);
  if (!CORBA::is_nil(_child.peer))
    try { _child.peer->remove_graphic(localId);}
    catch (const CORBA::OBJECT_NOT_EXIST &) { _child.peer = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { _child.peer = Warsaw::Graphic::_nil();}
}

void MonoGraphic::remove_child_graphic(Tag localId)
{
  Trace trace("MonoGraphic::remove_child");
  {
    Prague::Guard<Mutex> guard(_mutex);
    if (localId == 0) _child.peer = Warsaw::Graphic::_nil();
  }
  need_resize();
}

Warsaw::Graphic::Iterator_ptr MonoGraphic::first_child_graphic()
{
  Prague::Guard<Mutex> guard(_mutex);
  return (CORBA::is_nil (_child.peer)
	  ? Warsaw::Graphic::Iterator::_nil()
	  : _child.peer->first_child_graphic());
}

Warsaw::Graphic::Iterator_ptr MonoGraphic::last_child_graphic()
{
  Prague::Guard<Mutex> guard(_mutex);
  return (CORBA::is_nil (_child.peer)
	  ? Warsaw::Graphic::Iterator::_nil()
	  : _child.peer->last_child_graphic());
}

Transform_ptr MonoGraphic::transformation()
{
  Graphic_var child = body();
  return CORBA::is_nil(child) ? Transform::_nil() : child->transformation();
}

void MonoGraphic::request(Warsaw::Graphic::Requisition &r)
{
  Trace trace("MonoGraphic::request");
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  try { child->request(r);}
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
  if (!CORBA::is_nil(child)) child->request(r);
}

void MonoGraphic::extension(const Warsaw::Allocation::Info &info, Region_ptr region)
{
  Trace trace("MonoGraphic::extension");
  Graphic_var child = body();
  if (!CORBA::is_nil(child))
    {
      Lease_var<RegionImpl> tmp(Provider<RegionImpl>::provide());
      tmp->clear();
      Lease_var<TransformImpl> transform(Provider<TransformImpl>::provide());
      transform->load_identity();
      Allocation::Info i;
      i.allocation = tmp->_this();
      i.allocation->copy(info.allocation);
      i.transformation = transform->_this();
      i.transformation->copy(info.transformation);
      allocate(0, i);
      child->extension(i, region);
    }
}

void MonoGraphic::shape(Region_ptr region)
{
  Graphic_var child = body();
  if (!CORBA::is_nil(child)) child->shape(region);
}

void MonoGraphic::traverse(Traversal_ptr traversal)
{
  Trace trace("MonoGraphic::traverse");
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  try { traversal->traverse_child (child, 0, Region::_nil(), Transform::_nil());}
  catch (const CORBA::OBJECT_NOT_EXIST &) { body (Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
}
