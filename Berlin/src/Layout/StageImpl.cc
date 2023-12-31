/*$Id: StageImpl.cc,v 1.45 2001/04/18 06:07:27 stefan Exp $
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

#include "Layout/StageImpl.hh"
#include <Berlin/Provider.hh>
#include <Warsaw/config.hh>
#include <Warsaw/Screen.hh>
#include <Berlin/AllocationImpl.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/DebugGraphic.hh>
#include <Berlin/QuadTree.hh>
#include <Berlin/Math.hh>
#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Profiler.hh>

using namespace Geometry;
using namespace Prague;
using namespace Warsaw;
using namespace Layout;

class StageImpl::Sequence : public std::vector<StageHandleImpl *>
{
  typedef std::vector<StageHandleImpl *> parent_t;
public:
  Sequence() : _cursor(0) {}
  ~Sequence() {}
  
  void insert(StageHandleImpl *);
  void remove(StageHandleImpl *);
  
  StageHandleImpl *find(Layout::Stage::Index layer) { iterator i = lookup(layer); return i == end() ? 0 : *i;}
  StageHandleImpl *front() { return size() ? parent_t::front() : 0;}
  StageHandleImpl *back() { return size() ? parent_t::back() : 0;}
  StageHandleImpl *current() { return _cursor < size() ? *(begin() + _cursor) : 0;}
private:
  iterator lookup(Layout::Stage::Index layer);
  size_t _cursor;
};
 
namespace
{
class Finder
{
public:
  virtual ~Finder() {}
  virtual void found(StageHandleImpl *) = 0;
};

class Quad : public QTNode<Coord, StageHandleImpl *>
{
  typedef QTNode<Coord, StageHandleImpl *> parent_t;
public:
  Quad(const Geometry::Rectangle<Coord> &);
  Quad(const Geometry::Rectangle<Coord> &, Quad *);
  Quad *node(int i) { return static_cast<Quad *>(parent_t::node(static_cast<index>(i)));}
  void within(const Geometry::Rectangle<Coord> &, Finder &);
  void contains(const Geometry::Point<Coord> &, Finder &);
  void intersects(const Geometry::Rectangle<Coord> &, Finder &);
  void intersects(const Geometry::Rectangle<Coord> &, const Geometry::Polygon<Coord> &, Finder &);
};
};

class StageImpl::QuadTree : public ::QuadTree<Coord, StageHandleImpl *>
{
  typedef ::QuadTree<Coord, StageHandleImpl *> parent_t;
public:
  QuadTree() : _transaction(0), _operations(0) {}
  Quad *node() { return static_cast<Quad *>(parent_t::node());}

  void begin(){ _transaction++;}
  void insert(StageHandleImpl *);
  void remove(StageHandleImpl *);
  void end();
  StageHandleImpl *contains(const Geometry::Point<Coord> &);
  void within(const Geometry::Rectangle<Coord> &r, Finder &f) { if (node()) node()->within(r, f);}
  void intersects(const Geometry::Rectangle<Coord> &r, Finder &f) { if (node()) node()->intersects(r, f);}
  void intersects(const Geometry::Polygon<Coord> &, Finder &);
private:
  unsigned int _transaction;
  unsigned int _operations;
};

StageImpl::Sequence::iterator StageImpl::Sequence::lookup(Layout::Stage::Index layer)
{
  Trace trace("StageImpl::Sequence::lookup");
  if (layer == front()->_layer) return begin();
  if (layer == back()->_layer) return end() - 1;
  if (layer == current()->_layer) return begin() + _cursor;
  /*
   * start searching from the closest item
   */
  Layout::Stage::Index fdist = front()->_layer - layer;
  Layout::Stage::Index bdist = layer;
  Layout::Stage::Index cdist = Math::abs(current()->_layer - layer);
  if (fdist < bdist)
    {
      if (fdist < cdist) _cursor = 0;
    }
  else
    {
      if (bdist < cdist) _cursor = size() - 1;
    }
  _cursor += layer - current()->_layer;
  return begin() + _cursor;
}

void StageImpl::Sequence::insert(StageHandleImpl *handle)
{
  Trace trace("StageImpl::Sequence::insert");
  Layout::Stage::Index layer = handle->_layer;
  iterator i;
  if (!size() || layer == 0) i = begin();
  else if (front()->_layer < layer) i = end();
  else i = lookup(layer);
  for (iterator j = i; j != end(); j++) (*j)->_layer = ++layer;
  parent_t::insert(i, handle);
}
 
void StageImpl::Sequence::remove(StageHandleImpl *handle)
{
  Trace trace("StageImpl::Sequence::remove");
  Layout::Stage::Index layer = handle->_layer;
  iterator old = lookup(layer);
  if (old == begin() + _cursor)
    if (current()->_layer <= (front()->_layer / 2)) _cursor++;
    else _cursor--;
  for (iterator i = old++; i != end(); i++) (*i)->_layer = layer++;
  parent_t::erase(--old);
}

Quad::Quad(const Rectangle<Coord> &region) : parent_t(region) {}

/*
 * one child node is given; the other three are added inside
 */
Quad::Quad(const Rectangle<Coord> &r, Quad *node)
 : parent_t(r)
{
//   SectionLog section("StageQuad::StageQuad(rectangle, StageQuad *)");
  elements = node->elements;
  boundingbox = node->boundingbox;

  const Rectangle<Coord> &b = node->region;
  int idx = none;

  /*
   * now determine in which directions we have to extend.
   * node becomes one of our children, the other three are added
   */
  Coord dl = b.l - r.l;
  Coord dr = r.r - b.r;
  if (dl < dr) { idx |= left; region.l = b.l; region.r = b.r + b.w();}
  else { idx |= right; region.l = b.l - b.w(); region.r = b.r;}

  Coord dt = b.t - r.t;
  Coord db = r.b - b.b;
  if (dt < db) { idx |= top; region.t = b.t; region.b = b.b + b.h();}
  else { idx |= bottom; region.t = b.t - b.h(); region.b = b.b;}

  quadrants[lefttop] =
    idx == lefttop ? node :
    new Quad(Rectangle<Coord>(region.l, region.t, region.cx(), region.cy()));
  quadrants[righttop] =
    idx == righttop ? node :
    new Quad(Rectangle<Coord>(region.cx(), region.t, region.r, region.cy()));
  quadrants[leftbottom] =
    idx == leftbottom ? node :
    new Quad(Rectangle<Coord>(region.l, region.cy(), region.cx(), region.b));
  quadrants[rightbottom] =
    idx == rightbottom ? node :
    new Quad(Rectangle<Coord>(region.cx(), region.cy(), region.r, region.b));
}

void Quad::within(const Rectangle<Coord> &r, Finder &finder)
{
  index idx = where(region);
  if (idx == fence)
    {
      for (list::iterator i = items.begin(); i != items.end(); i++)
	if ((*i)->bbox().within(r)) finder.found(*i);
      if (!leaf())
	{
	  if (r.r <= region.cx())
	    {
	      node(lefttop)->within(r, finder);
	      node(leftbottom)->within(r, finder);
	    }
	  else if (r.l > region.cx())
	    {
	      node(righttop)->within(r, finder);
	      node(rightbottom)->within(r, finder);
	    }
	  else if (r.b <= region.cy())
	    {
	      node(lefttop)->within(r, finder);
	      node(righttop)->within(r, finder);
	    }
	  else if (r.t > region.cy())
	    {
	      node(leftbottom)->within(r, finder);
	      node(rightbottom)->within(r, finder);
	    }
	  else for (int i = 0; i < 4; ++i) node(i)->within(r, finder);
	}
    }
  else node(idx)->within(r, finder);
}

void Quad::contains(const Point<Coord> &point, Finder &finder)
{
  for (list::iterator i = items.begin(); i != items.end(); i++)
    if ((*i)->bbox().contains(point))
      {
	/*
	  RegionImpl region;
	  (*i)->c->shape(region);
	  if (region->contains(point.x, point.y)) finder.found(*i);
	*/
	finder.found(*i);
      }
  if (!leaf()) node(where(point))->contains(point, finder);
}

void Quad::intersects(const Rectangle<Coord> &r, Finder &finder)
{
  for (list::iterator i = items.begin(); i != items.end(); i++)
    {
      if ((*i)->bbox().intersects(r))
	{
	  /*
	    RegionImpl shape;
	    (*i)->c->shape(region);
	    if (shape->intersects(region.l, region.b, region.r, region.t)) finder.found(item);
	  */
	  finder.found(*i);
	}
    }
  index idx = where(r);
  if (idx == fence)
    {
      if (!leaf())
	{
	  if (r.r <= region.cx())
	    {
	      node(leftbottom)->intersects(r, finder);
	      node(lefttop)->intersects(r, finder);
	    }
	  else if (r.l > region.cx())
	    {
	      node(rightbottom)->intersects(r, finder);
	      node(righttop)->intersects(r, finder);
	    }
	  else if (r.b <= region.cy())
	    {
	      node(lefttop)->intersects(r, finder);
	      node(righttop)->intersects(r, finder);
	    }
	  else if (r.t > region.cy())
	    {
	      node(leftbottom)->intersects(r, finder);
	      node(rightbottom)->intersects(r, finder);
	    }
	  else for (int i = 0; i < 4; ++i) node(i)->intersects(r, finder);
	}
    }
  else node(idx)->intersects(r, finder);
}

void Quad::intersects(const Rectangle<Coord> &r, const Polygon<Coord> &polygon, Finder &finder)
{
  for (list::iterator i = items.begin(); i != items.end(); i++)
    if (polygon.intersects((*i)->bbox()))
      {
	/*
	  RegionImpl shape;
	  Graphic_var(item->child())->shape(region);
	  if (shape->intersects(Geometry::Polygon)) finder.found(item);
	*/
	finder.found(*i);
      }
  index idx = where(r);
  if (idx == fence)
    {
      if (!leaf())
	{
	  /*
	   * shouldn't this read node(...)->intersects(s, polygon, finder) ??? -stefan
	   */
	  if (r.r <= region.cx())
	    {
	      node(leftbottom)->intersects(r, finder);
	      node(lefttop)->intersects(r, finder);
	    }
	  else if (r.l > region.cx())
	    {
	      node(rightbottom)->intersects(r, finder);
	      node(righttop)->intersects(r, finder);
	    }
	  else if (r.b <= region.cy())
	    {
	      node(lefttop)->intersects(r, finder);
	      node(righttop)->intersects(r, finder);
	    }
	  else if (r.t > region.cy())
	    {
	      node(leftbottom)->intersects(r, finder);
	      node(rightbottom)->intersects(r, finder);
	    }
	  else for (int i = 0; i < 4; i++) node(i)->intersects(r, finder);
	}
    }
  else node(idx)->intersects(r, finder);
}

void StageImpl::QuadTree::insert(StageHandleImpl *handle)
{
  const Rectangle<Coord> &bbox = handle->bbox();
  if (!node()) quad = new Quad(bbox);
  /*
   * FIXME: currently, this code inserts new nodes as long as the outermost
   *        doesn't completely contain the handle's boundingbox. What if the
   *        given bbox is infinity ?? On the other hand, guessing too large
   *        values is tricky since we don't know the scale of this coordinate 
   *        system, so may be a limiting depth of the tree would be a solution.
   *                -stefan
   */
  else while (!bbox.within(node()->extension())) quad = new Quad(bbox, node());
  node()->insert(handle);
}

void StageImpl::QuadTree::remove(StageHandleImpl *handle)
{
  node()->remove(handle);
}

void StageImpl::QuadTree::end()
{
  _transaction--;
  if (_transaction == 0)
    {
      /*
       * ??? every 32 operations adjust the StageQuad tree -denis
       */
      _operations++;
      if (_operations & 0x1f == 0)
	{
	  /*
	   * ??? desire min of 8, max of 32, smallest span of 16 -denis
	   */
	  node()->adjust(8, 32, 16, 16);
	}
    }
}

class StageQuadTreeContains : public Finder
{
public:
  StageQuadTreeContains(Traversal::order o) : handle(0), order(o) {}
  virtual void found(StageHandleImpl *h)
    {
      if (!handle || (order == Traversal::down && handle->_layer > h->_layer) || handle->_layer < h->_layer)
	handle = h;
    }
  StageHandleImpl *handle;
  Traversal::order order;
};

StageHandleImpl *StageImpl::QuadTree::contains(const Point<Coord> &point)
{
  StageHandleImpl *handle = 0;
  if (node())
    {
      StageQuadTreeContains finder(Traversal::up);
      node()->contains(point, finder);
      handle = finder.handle;
    }
  return handle;
}

void StageImpl::QuadTree::intersects(const Polygon<Coord> &polygon, Finder &finder)
{
  if (node())
    {
      Rectangle<Coord> bb;
      polygon.boundingBox(bb);
      node()->intersects(bb, polygon, finder);
    }
}

class StageTraversal : public Finder
{
public:
  StageTraversal(Traversal_ptr t);
  virtual ~StageTraversal();
  virtual void found(StageHandleImpl *h) { _buffer.push_back(h);}
  void execute();
private:
  void traverse(StageHandleImpl *);
  Traversal_ptr                  _traversal;
  std::vector<StageHandleImpl *> _buffer;
};

StageTraversal::StageTraversal(Traversal_ptr t) : _traversal(t) {}
StageTraversal::~StageTraversal() {}

namespace std
{
template <>
struct greater<StageHandleImpl *> : public binary_function<StageHandleImpl *, StageHandleImpl *, bool>
{ bool operator() (StageHandleImpl *a, StageHandleImpl *b) const { return a->_layer > b->_layer;}};

template <>
struct less<StageHandleImpl *> : public binary_function<StageHandleImpl *, StageHandleImpl *, bool>
{ bool operator() (StageHandleImpl *a, StageHandleImpl *b) const { return a->_layer < b->_layer;}};
};

void StageTraversal::execute()
{
  if (_traversal->direction() == Traversal::down)
    sort(_buffer.begin(), _buffer.end(), std::less<StageHandleImpl *>());
  else
    sort(_buffer.begin(), _buffer.end(), std::greater<StageHandleImpl *>());
  for (std::vector<StageHandleImpl *>::iterator i = _buffer.begin(); i != _buffer.end() && _traversal->ok(); ++i)
    {
      if (!_traversal->ok()) break;
      traverse(*i);
    }
}

void StageTraversal::traverse(StageHandleImpl *handle)
{
  Trace trace("StageTraversal::traverse");
  if (CORBA::is_nil(handle->_child)) return;
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  handle->bbox(*region);
  Vertex origin;
  region->normalize(origin);
  Lease_var<TransformImpl> transformation(Provider<TransformImpl>::provide());
  transformation->load_identity();
  transformation->translate(origin);
  try { _traversal->traverse_child(handle->_child, handle->_tag, Region_var(region->_this()), Transform_var(transformation->_this()));}
  catch (const CORBA::OBJECT_NOT_EXIST &) { handle->_child = Warsaw::Graphic::_nil();}
  catch (const CORBA::COMM_FAILURE &) { handle->_child = Warsaw::Graphic::_nil();}
}

StageImpl::StageImpl()
  : _children(new Sequence),
    _tree(new QuadTree),
    _nesting(0),
    _damage(new RegionImpl),
    _bbregion(new RegionImpl),
    _need_redraw(false),
    _need_resize(false)
{
}

StageImpl::~StageImpl()
{
  delete _tree;
  delete _children;
}

void StageImpl::request(Warsaw::Graphic::Requisition &r)
{
  GraphicImpl::init_requisition(r);
  if (_tree->size() > 0)
    {
      Geometry::Rectangle<Coord> b = _tree->bbox();
      Coord w = b.r - b.l;
      Coord h = b.b - b.t;
      Coord ax = (Math::equal(w, 0., epsilon) ? 0 : (-b.l / w));
      Coord ay = (Math::equal(h, 0., epsilon) ? 0 : (-b.t / h));
      GraphicImpl::require(r.x, w, 0, 0, ax);
      GraphicImpl::require(r.y, h, 0, 0, ay);
    }
}

void StageImpl::traverse(Traversal_ptr traversal)
{
  Trace trace("StageImpl::traverse");
  Prague::Guard<Mutex> guard(_mutex);
//   Profiler prf("StageImpl::traverse");
  RegionImpl region(Region_var(traversal->current_allocation()));
  Geometry::Rectangle<Coord> rectangle;
  rectangle.l = region.lower.x;
  rectangle.t = region.lower.y;
  rectangle.r = region.upper.x;
  rectangle.b = region.upper.y;
//   dumpQuadTree(*tree);
  StageTraversal st(traversal);
  _tree->intersects(rectangle, st);
  st.execute();
}

void StageImpl::allocate(Tag tag, const Allocation::Info &a)
{
  StageHandleImpl *handle = tag_to_handle(tag);
  if (handle)
    {
      Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
      Lease_var<TransformImpl> transform(Provider<TransformImpl>::provide());
      transform->load_identity();
      Vertex origin;
      handle->bbox(*region);
      region->normalize(origin);
      transform->translate(handle->_position);
      a.allocation->copy(Region_var(region->_this()));
      a.transformation->premultiply(Transform_var(transform->_this()));
    }
}

void StageImpl::need_redraw()
{
  Trace trace("StageImpl::needRedraw");
  Lease_var<AllocationImpl> allocation(Provider<AllocationImpl>::provide());
  allocations(Allocation_var(allocation->_this()));
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  for (CORBA::Long i = 0; i < allocation->size(); i++)
    {
      const Allocation::Info_var info = allocation->get(i);
      region->valid = false;
      extension(info, Region_var(region->_this()));
      if (region->valid)
	{
	  Vertex origin;
	  info->allocation->origin(origin);
	  tx->load_identity();
	  tx->translate(origin);
	  region->apply_transform(Transform_var(tx->_this()));
	  if (region->valid) info->root->damage(Region_var(region->_this()));
	}
    }
}

void StageImpl::need_redraw_region(Region_ptr region)
{
  Trace trace("StageImpl::need_redraw_region");
  Lease_var<AllocationImpl> allocation(Provider<AllocationImpl>::provide());
  allocations(Allocation_var(allocation->_this()));
  CORBA::Long size = allocation->size();
  Lease_var<RegionImpl> tmp(Provider<RegionImpl>::provide());
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  for (CORBA::Long i = 0; i < size; i++)
    {
      const Allocation::Info_var info = allocation->get(i);
      tmp->copy(region);
      tmp->apply_transform(info->transformation);
      Vertex origin;
      info->allocation->origin(origin);
      tx->load_identity();
      tx->translate(origin);
      tmp->apply_transform(Transform_var(tx->_this()));
      if (tmp->valid) info->root->damage(Region_var(tmp->_this()));
    }
}

void StageImpl::need_resize()
{
  Trace trace("StageImpl::need_resize");
  /*
   * FIXME !!!: need to work out how to process this. (which sub region to damage etc...)
   */
  GraphicImpl::need_resize();
}

Region_ptr StageImpl::bbox()
{
  Prague::Guard<Mutex> guard(_mutex);
  Geometry::Rectangle<Coord> bb = _tree->bbox();
  _bbregion->valid = true;
  _bbregion->lower.x = bb.l;
  _bbregion->lower.y = bb.t;
  _bbregion->lower.z = 0.;
  _bbregion->upper.x = bb.r;
  _bbregion->upper.y = bb.b;
  _bbregion->upper.z = 0.;
  return _bbregion->_this();
}

CORBA::Long StageImpl::layers()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _tree->size();
}

StageHandle_ptr StageImpl::layer(Layout::Stage::Index i)
{
  Prague::Guard<Mutex> guard(_mutex);
  StageHandleImpl *handle = _children->find(i);
  return handle ? handle->_this() : StageHandle::_nil();
}

void StageImpl::begin()
{
  if (!_nesting++)
    {
      Geometry::Rectangle<Coord> bb = _tree->bbox();
      _bbregion->lower.x = bb.l;
      _bbregion->lower.y = bb.t;
      _bbregion->upper.x = bb.r;
      _bbregion->upper.y = bb.b;
      _tree->begin();
    }
}

void StageImpl::end()
{
  Trace trace("StageImpl::end");
  Prague::Guard<Mutex> guard(_mutex);
  if (!--_nesting)
    {
      _tree->end();
      if (_need_redraw)
	{
	  need_redraw_region(Region_var(_damage->_this()));
	  _need_redraw = false;
	}
      if (_need_resize)
	{
 	  Geometry::Rectangle<Coord> bb = _tree->bbox();
 	  if (! Math::equal(_bbregion->lower.x, bb.l, epsilon) ||
 	      ! Math::equal(_bbregion->lower.y, bb.t, epsilon) ||
 	      ! Math::equal(_bbregion->upper.x, bb.r, epsilon) ||
 	      ! Math::equal(_bbregion->upper.y, bb.b, epsilon))
 	    GraphicImpl::need_resize();
 	  _need_resize = false;
	}
    }
}

StageHandle_ptr StageImpl::insert(Graphic_ptr g, const Vertex &position, const Vertex &size, Layout::Stage::Index layer)
{
  Trace trace("StageImpl::insert");
  Prague::Guard<Mutex> guard(_mutex);
  StageHandleImpl *handle = new StageHandleImpl(this, g, unique_tag(), position, size, layer);
  _tree->insert(handle);
//   dumpQuadTree(*tree);
  _children->insert(handle);
  damage(handle);
  return handle->_this();
}

void StageImpl::remove(StageHandle_ptr h)
{
  Trace trace("StageImpl::remove");
  Prague::Guard<Mutex> guard(_mutex);
  StageHandleImpl *handle = _children->find(h->layer());
  if (!handle) return;
  _tree->remove(handle);
//   dumpQuadTree(*tree);
  _children->remove(handle);

  damage(handle);
//  handle->_dispose();
  _need_resize = true;
}

void StageImpl::move(StageHandleImpl *handle, const Vertex &p)
{
  Trace trace("StageImpl::move");
//   Prague::Profiler prf("StageImpl::move");
  Prague::Guard<Mutex> guard(_mutex);
  _tree->remove(handle);

  damage(handle);
  _need_resize = true;

  
  Coord dx = p.x - handle->_position.x;
  Coord dy = p.y - handle->_position.y;
  handle->_bbox.l += dx;
  handle->_bbox.t += dy;
  handle->_bbox.r += dx;
  handle->_bbox.b += dy;
  handle->_position = p;
  _tree->insert(handle);
//   dumpQuadTree(*tree);

  damage(handle);
  _need_resize = true;
}

void StageImpl::resize(StageHandleImpl *handle, const Vertex &s)
{
  Trace trace("StageImpl::resize");
  Prague::Guard<Mutex> guard(_mutex);
  _tree->remove(handle);

  damage(handle);
  _need_resize = true;
  
  handle->_bbox.r = handle->_bbox.l + s.x;
  handle->_bbox.b = handle->_bbox.t + s.y;
  handle->_size = s;
  _tree->insert(handle);
//   dumpQuadTree(*tree);

  damage(handle);
  _need_resize = true;
}

void StageImpl::relayer(StageHandleImpl *handle, Layout::Stage::Index l)
{
  Trace trace("StageImpl::relayer");
  Prague::Guard<Mutex> guard(_mutex);
  _children->remove(handle);
  handle->_layer = l;
  _children->insert(handle);
  damage(handle);
}

Tag StageImpl::unique_tag()
{
  Tag t = 0;
  do
    {
      Sequence::iterator i;
      for (i = _children->begin(); i != _children->end(); i++)
	if ((*i)->_tag == t) break;
      if (i == _children->end()) return t;
    }
  while (++t);
  return 0;
}

StageHandleImpl *StageImpl::tag_to_handle(Tag tag)
{
  for (Sequence::iterator i = _children->begin(); i != _children->end(); i++)
    if ((*i)->_tag == tag) return *i;
  return 0;
}

void StageImpl::damage(StageHandleImpl *handle)
{
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  handle->bbox(*region);
  if (_need_redraw) _damage->merge_union(Region_var(region->_this()));
  else
    {
      _need_redraw = true;
      _damage->copy(Region_var(region->_this()));
    }
}

StageHandleImpl::StageHandleImpl(StageImpl *stage, Graphic_ptr g, Tag t, const Vertex &p, const Vertex &s, Layout::Stage::Index l)
  : _parent(stage), _child(Graphic::_duplicate(g)), _tag(t), _position(p), _size(s), _layer(l)
{
  _child->add_parent_graphic(Stage_var(_parent->_this()), _tag);
  cache_bbox();
}

Layout::Stage_ptr StageHandleImpl::parent()
{
  return _parent->_this();
}
Warsaw::Graphic_ptr StageHandleImpl::child()
{
  return Warsaw::Graphic::_duplicate(_child);
}

void StageHandleImpl::remove()
{
  _parent->begin();
  _parent->remove(StageHandle_var(_this()));
  _parent->end();
}

Warsaw::Vertex StageHandleImpl::position()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _position;
}

void StageHandleImpl::position(const Vertex &pp)
{
  _parent->begin();
  _parent->move(this, pp);
  _parent->end();
}

Warsaw::Vertex StageHandleImpl::size()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _size;
}

void StageHandleImpl::size(const Vertex &ss)
{
  _parent->begin();
  _parent->resize(this, ss);
  _parent->end();
}

Layout::Stage::Index StageHandleImpl::layer() 
{
  Prague::Guard<Mutex> guard(_mutex);
  return _layer;
}

void StageHandleImpl::layer(Layout::Stage::Index ll)
{
  _parent->begin();
  _parent->relayer(this, ll);
  _parent->end();
}

const Geometry::Rectangle<Warsaw::Coord> &StageHandleImpl::bbox()
{
  Prague::Guard<Mutex> guard(_mutex);
  return _bbox;
}

void StageHandleImpl::bbox(RegionImpl &region)
{
  Prague::Guard<Mutex> guard(_mutex);
  region.valid   = true;
  region.lower.x = _bbox.l;
  region.upper.x = _bbox.r;
  region.xalign  = _xalign;
  region.lower.y = _bbox.t;
  region.upper.y = _bbox.b;
  region.yalign  = _yalign;
}

void StageHandleImpl::cache_bbox()
{
  Trace trace("StageHandleImpl::cache_bbox");
  Graphic::Requisition r;
  GraphicImpl::init_requisition(r);    
  _child->request(r);
  if (r.x.defined && r.y.defined)
    {
      _xalign = r.x.align;
      _yalign = r.y.align;
      if (_size.x != 0)
	{
	  _bbox.l = _position.x;
	  _bbox.r = _position.x + _size.x;
	}
      else
	{
	  _bbox.l = _position.x - r.x.natural * r.x.align;
	  _bbox.r = _position.x + r.x.natural * (1. - r.x.align);
	}
      if (_size.y != 0)
	{
	  _bbox.t = _position.y;
	  _bbox.b = _position.y + _size.y;
	}
      else
	{
	  _bbox.t = _position.y - r.y.natural * r.y.align;
	  _bbox.b = _position.y + r.y.natural * (1. - r.y.align);
	}
    }
  else
    {
      _xalign = 0.;
      _yalign = 0.;
      _bbox.l = _position.x;
      _bbox.r = _position.x;
      _bbox.t = _position.y;
      _bbox.b = _position.y;
    }
}

