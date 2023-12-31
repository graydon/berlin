/*$Id: PolyGraphic.cc,v 1.38 2001/04/24 03:26:50 stefan Exp $
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
#include "Berlin/PolyGraphic.hh"
#include "Berlin/RefCountVar.hh"
#include <iostream>

using namespace Prague;
using namespace Warsaw;

class PolyGraphic::Iterator : public virtual POA_Warsaw::GraphicIterator,
		              public virtual GraphicIteratorImpl
{
public:
  Iterator(PolyGraphic *p, Tag c) : _parent(p), _cursor(c) { Trace trace("PolyGraphic::Iterator::Iterator"); _parent->_add_ref();}
  virtual ~Iterator() { Trace trace("PolyGraphic::Iterator::~Iterator"); _parent->_remove_ref();}
  virtual Warsaw::Graphic_ptr child()
  {
    Trace trace("PolyGraphic::Iterator::child");
    Prague::Guard<Mutex> guard(_parent->_mutex);
    if (_cursor > _parent->_children.size()) return Warsaw::Graphic::_nil();
    return RefCount_var<Warsaw::Graphic>::increment(_parent->_children[_cursor].peer);
  }
  virtual void next() { _cursor++;}
  virtual void prev() { _cursor--;}
  virtual void insert(Graphic_ptr child)
  {
    Trace trace("PolyGraphic::Iterator::insert");
    _parent->_mutex.lock();
    Edge edge;
    edge.peer = RefCount_var<Warsaw::Graphic>::increment(child);
    edge.localId = _parent->unique_child_id();
    edge.peerId = child->add_parent_graphic(Graphic_var(_parent->_this()), edge.localId);
    _parent->_children.insert(_parent->_children.begin() + _cursor, edge);
    _parent->_mutex.unlock();
    _parent->need_resize();
  }
  virtual void replace(Graphic_ptr child)
  {
    Trace trace("PolyGraphic::Iterator::replace");
    {
      Prague::Guard<Mutex> guard(_parent->_mutex);
      if (_cursor > _parent->_children.size()) return;
      Edge &edge = _parent->_children[_cursor];
      if (!CORBA::is_nil(edge.peer))
	try
	  {
	    edge.peer->remove_parent_graphic(edge.peerId);
	    edge.peer->decrement();
	  }
	catch(const CORBA::OBJECT_NOT_EXIST &) {}
	catch (const CORBA::COMM_FAILURE &) {}
      edge.peer = RefCount_var<Warsaw::Graphic>::increment(child);
      edge.peerId = child->add_parent_graphic(Graphic_var(_parent->_this()), edge.localId);
    }
    _parent->need_resize();
  }

  virtual void remove()
  {
    Trace trace("PolyGraphic::Iterator::remove");
    {
      Prague::Guard<Mutex> guard(_parent->_mutex);
      if (_cursor >= _parent->_children.size()) return;

      GraphicImpl::glist_t::iterator i = _parent->_children.begin() + _cursor;
      try
	{
	  (*i).peer->remove_parent_graphic((*i).peerId);
	  (*i).peer->decrement();
	}
      catch(const CORBA::OBJECT_NOT_EXIST &) {}
      catch (const CORBA::COMM_FAILURE &) {}
      _parent->_children.erase(i);
    }
    _parent->need_resize();
  }
private:
  PolyGraphic *_parent;
  size_t       _cursor;
};

Pool<Warsaw::Graphic::Requisition> PolyGraphic::_pool;

PolyGraphic::PolyGraphic() {}
PolyGraphic::~PolyGraphic()
{
  Trace trace("PolyGraphic::~PolyGraphic");
  Prague::Guard<Mutex> guard(_mutex);
  for (glist_t::iterator i = _children.begin(); i != _children.end(); i++)
    {
      if (!CORBA::is_nil((*i).peer))
	try
	  {
	    (*i).peer->remove_parent_graphic((*i).peerId);
	    (*i).peer->decrement();
	  }
	catch(const CORBA::OBJECT_NOT_EXIST &) {}
	catch (const CORBA::COMM_FAILURE &) {}
    }
}

void PolyGraphic::append_graphic(Graphic_ptr child)
{
  Trace trace("PolyGraphic::append_graphic");
  _mutex.lock();
  Edge edge;
  edge.peer = RefCount_var<Warsaw::Graphic>::increment(child);
  edge.localId = unique_child_id();
  edge.peerId = child->add_parent_graphic(Graphic_var(_this()), edge.localId);
  _children.push_back(edge);
  _mutex.unlock();
  need_resize();
}

void PolyGraphic::prepend_graphic(Graphic_ptr child)
{
  Trace trace("PolyGraphic::prepend_graphic");
  _mutex.lock();
  Edge edge;
  edge.peer = RefCount_var<Warsaw::Graphic>::increment(child);
  edge.localId = unique_child_id();
  edge.peerId = child->add_parent_graphic(Graphic_var(_this()), edge.localId);
  _children.insert(_children.begin(), edge);
  _mutex.unlock();
  need_resize();
}

void PolyGraphic::remove_graphic(Tag localId)
{
  Trace trace("PolyGraphic::remove_graphic");
  _mutex.lock();
  glist_t::iterator i = child_id_to_iterator(localId);
  try
    {
      (*i).peer->remove_parent_graphic((*i).peerId);
      (*i).peer->decrement();
    }
  catch(const CORBA::OBJECT_NOT_EXIST &) {}
  catch (const CORBA::COMM_FAILURE &) {}
  _children.erase(i);
  _mutex.unlock();
  need_resize();
}

void PolyGraphic::remove_child_graphic(Tag localId)
{
  Trace trace("PolyGraphic::remove_child_graphic");
  _mutex.lock();
  glist_t::iterator i = child_id_to_iterator(localId);
  _children.erase(i);
  _mutex.unlock();
  need_resize();
}

Warsaw::Graphic::Iterator_ptr PolyGraphic::first_child_graphic()
{
  Trace trace("PolyGraphic::first_child_graphic");
  Iterator *iterator = new Iterator(this, 0);
  activate(iterator);
  return iterator->_this();
}

Warsaw::Graphic::Iterator_ptr PolyGraphic::last_child_graphic()
{
  Trace trace("PolyGraphic::last_child_graphic");
  Iterator *iterator = new Iterator(this, num_children());
  activate(iterator);
  return iterator->_this();
}

void PolyGraphic::need_resize() { GraphicImpl::need_resize();}
void PolyGraphic::need_resize(Tag) { GraphicImpl::need_resize();}

long PolyGraphic::num_children()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _children.size();
}

Warsaw::Graphic::Requisition *PolyGraphic::children_requests()
{
  Trace trace("PolyGraphic::children_requests");
  Prague::Guard<Mutex> guard(_mutex);
  Warsaw::Graphic::Requisition *requisitions = _pool.allocate(_children.size());
  Warsaw::Graphic::Requisition *r = requisitions;
  for (glist_t::iterator i = _children.begin(); i != _children.end(); i++)
    {
      GraphicImpl::init_requisition(*r);
      if (!CORBA::is_nil((*i).peer))
	try { (*i).peer->request(*r);}
	catch (const CORBA::OBJECT_NOT_EXIST &) { (*i).peer = Warsaw::Graphic::_nil();}
	catch (const CORBA::COMM_FAILURE &) { (*i).peer = Warsaw::Graphic::_nil();}
      ++r;
    }
  return requisitions;
}

void PolyGraphic::deallocate_requisitions(Warsaw::Graphic::Requisition *r)
{
  Prague::Guard<Mutex> guard(_mutex);
  _pool.deallocate(r);
}

void PolyGraphic::child_extension(size_t i, const Warsaw::Allocation::Info &info, Region_ptr region)
{
  Prague::Guard<Mutex> guard(_mutex);
  Graphic_var child = _children[i].peer;
  if (!CORBA::is_nil(child))
    try { child->extension(info, region);}
    catch (const CORBA::OBJECT_NOT_EXIST &) { _children[i].peer = Warsaw::Graphic::_nil();}
    catch (const CORBA::COMM_FAILURE &) { _children[i].peer = Warsaw::Graphic::_nil();}
}
