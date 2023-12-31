/*$Id: StageImpl.cc,v 1.27 1999/11/06 20:23:08 stefan Exp $
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

#include "Warsaw/config.hh"
#include "Warsaw/Screen.hh"
#include "Berlin/AllocationImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/DebugGraphic.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Logger.hh"
#include "Berlin/QuadTree.hh"
#include "Layout/StageImpl.hh"

using namespace Geometry;
using namespace Prague;

class StageImpl::Sequence : public list<StageHandleImpl *>
{
  typedef list<StageHandleImpl *> parent_t;
  iterator lookup(Stage::Index layer);
public:
  Sequence() : cursor(begin()) {}
  ~Sequence() {}
  
  void insert(StageHandleImpl *);
  void remove(StageHandleImpl *);
  
  StageHandleImpl *find(Stage::Index layer) { return *lookup(layer);}
  StageHandleImpl *front() { return parent_t::front();}
  StageHandleImpl *back() { return parent_t::back();}
private:
  iterator cursor;
};
 
class StageImpl::Finder
{
public:
  virtual ~Finder() {}
  virtual void found(StageHandleImpl *) = 0;
};

class StageImpl::Quad : public QTNode<Coord, StageHandleImpl *>
{
  typedef QTNode<Coord, StageHandleImpl *> parent_t;
public:
  Quad(const Geometry::Rectangle<Coord> &);
  Quad(const Geometry::Rectangle<Coord> &, StageImpl::Quad *);
  Quad *node(int i) { return static_cast<StageImpl::Quad *>(parent_t::node(static_cast<index>(i)));}
  void within(const Geometry::Rectangle<Coord> &, StageImpl::Finder &);
  void contains(const Geometry::Point<Coord> &, StageImpl::Finder &);
  void intersects(const Geometry::Rectangle<Coord> &, StageImpl::Finder &);
  void intersects(const Geometry::Rectangle<Coord> &, const Geometry::Polygon<Coord> &, StageImpl::Finder &);
};

class StageImpl::QuadTree : public ::QuadTree<Coord, StageHandleImpl *>
{
  typedef ::QuadTree<Coord, StageHandleImpl *> parent_t;
public:
  QuadTree() : transaction(0), operations(0) {}
  StageImpl::Quad *node() { return static_cast<StageImpl::Quad *>(parent_t::node());}

  void begin(){ transaction++;}
  void insert(StageHandleImpl *);
  void remove(StageHandleImpl *);
  void end();
  StageHandleImpl *contains(const Geometry::Point<Coord> &);
  void within(const Geometry::Rectangle<Coord> &r, StageImpl::Finder &f) { if (node()) node()->within(r, f);}
  void intersects(const Geometry::Rectangle<Coord> &r, StageImpl::Finder &f) { if (node()) node()->intersects(r, f);}
  void intersects(const Geometry::Polygon<Coord> &, StageImpl::Finder &);
private:
  unsigned transaction;
  unsigned operations;
};

StageImpl::Sequence::iterator StageImpl::Sequence::lookup(Stage::Index layer)
{
  if (layer == front()->l) return begin();
  if (layer == back()->l) return --end();
  if (layer == (*cursor)->l) return cursor;
  /*
   * start searching from the closest item
   */
  int fdist = front()->l - layer;
  int bdist = layer;
  int cdist = Math::abs((*cursor)->l - layer);
  if (fdist < bdist)
    {
      if (fdist < cdist) cursor = begin();
    }
  else
    {
      if (bdist < cdist) cursor = --end();
    }

  int delta = layer - (*cursor)->l;
  while (delta > 0) delta--, cursor++;
  while (delta < 0) delta++, cursor--;
  return cursor;
}

void StageImpl::Sequence::insert(StageHandleImpl *handle)
{
  int layer = handle->l;
  iterator i;
  if (!size() || layer == 0) i = begin();
  else if (front()->l < layer) i = end();
  else i = lookup(layer);
  parent_t::insert(i++, handle);
  for (i++; i != end(); i++) (*i)->l = layer++;
}
 
void StageImpl::Sequence::remove(StageHandleImpl *handle)
{
  int layer = handle->l;
  iterator old = lookup(layer);
  if (old == cursor)
    if ((*cursor)->l <= (front()->l / 2)) cursor++;
    else cursor--;
  for (iterator i = old++; i != end(); i++) (*i)->l = layer++;
  parent_t::erase(--old);
}

StageImpl::Quad::Quad(const Rectangle<Coord> &region) : parent_t(region)
{
//   SectionLog section("StageQuad::StageQuad(rectangle)");
}

/*
 * one child node is given; the other three are added inside
 */
StageImpl::Quad::Quad(const Rectangle<Coord> &r, StageImpl::Quad *node)
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
    new StageImpl::Quad(Rectangle<Coord>(region.l, region.t, region.cx(), region.cy()));
  quadrants[righttop] =
    idx == righttop ? node :
    new StageImpl::Quad(Rectangle<Coord>(region.cx(), region.t, region.r, region.cy()));
  quadrants[leftbottom] =
    idx == leftbottom ? node :
    new StageImpl::Quad(Rectangle<Coord>(region.l, region.cy(), region.cx(), region.b));
  quadrants[rightbottom] =
    idx == rightbottom ? node :
    new StageImpl::Quad(Rectangle<Coord>(region.cx(), region.cy(), region.r, region.b));
}

void StageImpl::Quad::within(const Rectangle<Coord> &r, StageImpl::Finder &finder)
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

void StageImpl::Quad::contains(const Point<Coord> &point, StageImpl::Finder &finder)
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

void StageImpl::Quad::intersects(const Rectangle<Coord> &r, StageImpl::Finder &finder)
{
//   cout << "StageImpl::Quad::intersects " << r << endl;
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
//       else cout << "don't intersect for " << (*i)->bbox() << endl;
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

void StageImpl::Quad::intersects(const Rectangle<Coord> &r, const Polygon<Coord> &polygon, StageImpl::Finder &finder)
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
  if (!node()) quad = new StageImpl::Quad(bbox);
  /*
   * FIXME: currently, this code inserts new nodes as long as the outermost
   *        doesn't completely contain the handle's boundingbox. What if the
   *        given bbox is infinity ?? On the other hand, guessing too large
   *        values is tricky since we don't know the scale of this coordinate 
   *        system, so may be a limiting depth of the tree would be a solution.
   *                -stefan
   */
  else while (!bbox.within(node()->extension())) quad = new StageImpl::Quad(bbox, node());
  node()->insert(handle);
}

void StageImpl::QuadTree::remove(StageHandleImpl *handle)
{
  node()->remove(handle);
}

void StageImpl::QuadTree::end()
{
  transaction--;
  if (transaction == 0)
    {
      /*
       * ??? every 32 operations adjust the StageQuad tree -denis
       */
      operations++;
      if (operations & 0x1f == 0)
	{
	  /*
	   * ??? desire min of 8, max of 32, smallest span of 16 -denis
	   */
	  node()->adjust(8, 32, 16, 16);
	}
    }
}

class StageQuadTreeContains : public StageImpl::Finder
{
public:
  StageQuadTreeContains(Traversal::order o) : handle(0), order(o) {}
  virtual void found(StageHandleImpl *h)
    {
      if (!handle || (order == Traversal::down && handle->l > h->l) || handle->l < h->l)
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

void StageImpl::QuadTree::intersects(const Polygon<Coord> &polygon, StageImpl::Finder &finder)
{
  if (node())
    {
      Rectangle<Coord> bb;
      polygon.boundingBox(bb);
      node()->intersects(bb, polygon, finder);
    }
}

class StageTraversal : public StageImpl::Finder
{
public:
  StageTraversal(Traversal_ptr t);
  virtual ~StageTraversal();
  virtual void found(StageHandleImpl *h) { buffer.push_back(h);}
  void execute();
private:
  void traverse(StageHandleImpl *);
  Traversal_ptr traversal;
  vector<StageHandleImpl *> buffer;
};

StageTraversal::StageTraversal(Traversal_ptr t) : traversal(t) {}
StageTraversal::~StageTraversal() {}

template <>
struct greater<StageHandleImpl *> : public binary_function<StageHandleImpl *, StageHandleImpl *, bool>
{ bool operator() (StageHandleImpl *a, StageHandleImpl *b) const { return a->l > b->l;}};

template <>
struct less<StageHandleImpl *> : public binary_function<StageHandleImpl *, StageHandleImpl *, bool>
{ bool operator() (StageHandleImpl *a, StageHandleImpl *b) const { return a->l < b->l;}};

void StageTraversal::execute()
{
  if (traversal->direction() == Traversal::down)
    sort(buffer.begin(), buffer.end(), less<StageHandleImpl *>());
  else
    sort(buffer.begin(), buffer.end(), greater<StageHandleImpl *>());
  for (vector<StageHandleImpl *>::iterator i = buffer.begin(); i != buffer.end() && traversal->ok(); i++)
    {
      if (!traversal->ok()) break;
      traverse(*i);
    }
}

void StageTraversal::traverse(StageHandleImpl *handle)
{
  Impl_var<RegionImpl> region(new RegionImpl);
  handle->bbox(*region);
  Vertex origin;
  region->normalize(origin);
  Impl_var<TransformImpl> transformation(new TransformImpl);
  transformation->translate(origin);
  traversal->traverseChild(handle->c, handle->tag, Region_var(region->_this()), Transform_var(transformation->_this()));
}

StageImpl::StageImpl()
  : children(new Sequence),
    tree(new QuadTree),
    nesting(0),
    damage_(new RegionImpl),
    bbregion(new RegionImpl),
    need_redraw(false),
    need_resize(false)
{
}

StageImpl::~StageImpl()
{
  delete tree;
  delete tree;
}

void StageImpl::request(Requisition &r)
{
  GraphicImpl::initRequisition(r);
  if (tree->size() > 0)
    {
      Geometry::Rectangle<Coord> b = tree->bbox();
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
  SectionLog section("StageImpl::traverse");
  RegionImpl region(Region_var(traversal->allocation()));
  Geometry::Rectangle<Coord> rectangle;
  rectangle.l = region.lower.x;
  rectangle.t = region.lower.y;
  rectangle.r = region.upper.x;
  rectangle.b = region.upper.y;
//   dumpQuadTree(*tree);
  StageTraversal st(traversal);
  tree->intersects(rectangle, st);
  st.execute();
}

void StageImpl::allocate(Tag tag, const Allocation::Info &a)
{
  StageHandleImpl *handle = tag2handle(tag);
  if (handle)
    {
      Impl_var<RegionImpl> region(new RegionImpl);
      Impl_var<TransformImpl> transform(new TransformImpl);
      Vertex origin;
      handle->bbox(*region);
      region->normalize(origin);
      transform->translate(handle->p);
      a.allocation->copy(Region_var(region->_this()));
      a.transformation->premultiply(Transform_var(transform->_this()));
    }
//   else cerr << "StageImpl::allocate : child not found ! (tag is " << tag << ')' << endl;
}

void StageImpl::needRedraw()
{
  SectionLog section("StageImpl::needRedraw");
  Impl_var<AllocationImpl> allocation(new AllocationImpl);
  allocations(Allocation_var(allocation->_this()));
  Impl_var<RegionImpl> region(new RegionImpl);
  Impl_var<TransformImpl> tx(new TransformImpl);
  for (CORBA::Long i = 0; i < allocation->size(); i++)
    {
      const Allocation::Info_var info = allocation->get(i);
      region->valid = false;
      extension(info, Region_var(region->_this()));
      if (region->valid)
	{
	  Vertex origin;
	  info->allocation->origin(origin);
	  tx->loadIdentity();
	  tx->translate(origin);
	  region->applyTransform(Transform_var(tx->_this()));
	  if (region->valid) info->root->damage(Region_var(region->_this()));
	}
    }
}

void StageImpl::needRedrawRegion(Region_ptr region)
{
  SectionLog section("StageImpl::needRedrawRegion");
  Impl_var<AllocationImpl> allocation(new AllocationImpl);
  allocations(Allocation_var(allocation->_this()));
  CORBA::Long size = allocation->size();
  Impl_var<RegionImpl> tmp(new RegionImpl);
  Impl_var<TransformImpl> tx(new TransformImpl);
  for (CORBA::Long i = 0; i < size; i++)
    {
      const Allocation::Info_var info = allocation->get(i);
      tmp->copy(region);
      tmp->applyTransform(info->transformation);
      Vertex origin;
      info->allocation->origin(origin);
      tx->loadIdentity();
      tx->translate(origin);
      tmp->applyTransform(Transform_var(tx->_this()));
      if (tmp->valid) info->root->damage(Region_var(tmp->_this()));
    }
}

void StageImpl::needResize()
{
  SectionLog section("StageImpl::needResize");
  /*
   * FIXME !!!: need to work out how to process this. (which sub region to damage etc...)
   */
  GraphicImpl::needResize();
}

Region_ptr StageImpl::bbox()
{
  Geometry::Rectangle<Coord> bb = tree->bbox();
  bbregion->valid = true;
  bbregion->lower.x = bb.l;
  bbregion->lower.y = bb.t;
  bbregion->lower.z = 0.;
  bbregion->upper.x = bb.r;
  bbregion->upper.y = bb.b;
  bbregion->upper.z = 0.;
  return bbregion->_this();
}

CORBA::Long StageImpl::layers() { return tree->size();}

StageHandle_ptr StageImpl::layer(Stage::Index i)
{
  StageHandleImpl *handle = children->find(i);
  return handle ? handle->_this() : StageHandle::_nil();
}

void StageImpl::begin()
{
  if (!nesting++)
    {
      Geometry::Rectangle<Coord> bb = tree->bbox();
      bbregion->lower.x = bb.l;
      bbregion->lower.y = bb.t;
      bbregion->upper.x = bb.r;
      bbregion->upper.y = bb.b;
      tree->begin();
    }
}

void StageImpl::end()
{
  SectionLog section("StageImpl::end");
  MutexGuard guard(childMutex);
  if (!--nesting)
    {
      tree->end();
      if (need_redraw)
	{
	  needRedrawRegion(Region_var(damage_->_this()));
	  need_redraw = false;
	}
      if (need_resize)
	{
 	  Geometry::Rectangle<Coord> bb = tree->bbox();
 	  if (! Math::equal(bbregion->lower.x, bb.l, epsilon) ||
 	      ! Math::equal(bbregion->lower.y, bb.t, epsilon) ||
 	      ! Math::equal(bbregion->upper.x, bb.r, epsilon) ||
 	      ! Math::equal(bbregion->upper.y, bb.b, epsilon))
 	    GraphicImpl::needResize();
 	  need_resize = false;
	}
    }
}

StageHandle_ptr StageImpl::insert(Graphic_ptr g, const Vertex &position, const Vertex &size, Index layer)
{
  SectionLog section("StageImpl::insert");
  MutexGuard guard(childMutex);
  StageHandleImpl *handle = new StageHandleImpl(this, g, tag(), position, size, layer);
  handle->_obj_is_ready(_boa());
  tree->insert(handle);
//   dumpQuadTree(*tree);
  children->insert(handle);
  damage(handle);
  return handle->_this();
}

void StageImpl::remove(StageHandle_ptr h)
{
  SectionLog section("StageImpl::remove");
  MutexGuard guard(childMutex);
  StageHandleImpl *handle = children->find(h->layer());
  if (!handle) return;
  tree->remove(handle);
//   dumpQuadTree(*tree);
  children->remove(handle);

  damage(handle);
  handle->_dispose();
  need_resize = true;
}

void StageImpl::move(StageHandleImpl *handle, const Vertex &p)
{
  SectionLog section("StageImpl::move");
  MutexGuard guard(childMutex);
  tree->remove(handle);

  damage(handle);
  need_resize = true;
  
  Coord dx = p.x - handle->p.x;
  Coord dy = p.y - handle->p.y;
  handle->boundingbox.l += dx;
  handle->boundingbox.t += dy;
  handle->boundingbox.r += dx;
  handle->boundingbox.b += dy;
  handle->p = p;
  tree->insert(handle);
//   dumpQuadTree(*tree);

  damage(handle);
  need_resize = true;
}

void StageImpl::resize(StageHandleImpl *handle, const Vertex &s)
{
  SectionLog section("StageImpl::resize");
  MutexGuard guard(childMutex);
  tree->remove(handle);

  damage(handle);
  need_resize = true;
  
  handle->boundingbox.r = handle->boundingbox.l + s.x;
  handle->boundingbox.b = handle->boundingbox.t + s.y;
  handle->s = s;
  tree->insert(handle);
//   dumpQuadTree(*tree);

  damage(handle);
  need_resize = true;
}

void StageImpl::relayer(StageHandleImpl *handle, Stage::Index l)
{
  SectionLog section("StageImpl::relayer");
  MutexGuard guard(childMutex);
  children->remove(handle);
  handle->l = l;
  children->insert(handle);
  damage(handle);
}

Tag StageImpl::tag()
{
  Tag t = 0;
  do
    {
      Sequence::iterator i;
      for (i = children->begin(); i != children->end(); i++)
	if ((*i)->tag == t) break;
      if (i == children->end()) return t;
    }
  while (++t);
  return 0;
}

StageHandleImpl *StageImpl::tag2handle(Tag tag)
{
  for (Sequence::iterator i = children->begin(); i != children->end(); i++)
    if ((*i)->tag == tag) return *i;
  return 0;
}

void StageImpl::damage(StageHandleImpl *handle)
{
  Impl_var<RegionImpl> region(new RegionImpl);
  handle->bbox(*region);
  if (need_redraw) damage_->mergeUnion(Region_var(region->_this()));
  else
    {
      need_redraw = true;
      damage_->copy(Region_var(region->_this()));
    }
}

StageHandleImpl::StageHandleImpl(StageImpl *pa, Graphic_ptr g, Tag t, const Vertex &pp, const Vertex &ss, Stage::Index ll)
  : stage(pa), c(Graphic::_duplicate(g)), tag(t), p(pp), s(ss), l(ll)
{
  c->addParent(Stage_var(stage->_this()), tag);
  cacheBBox();
}

void StageHandleImpl::position(const Vertex &pp)
{
  stage->begin();
  stage->move(this, pp);
  stage->end();
}

void StageHandleImpl::size(const Vertex &ss)
{
  stage->begin();
  stage->resize(this, ss);
  stage->end();
}

void StageHandleImpl::layer(Stage::Index ll)
{
  stage->begin();
  stage->relayer(this, ll);
  stage->end();
}

void StageHandleImpl::cacheBBox()
{
  SectionLog section("StageHandleImpl::cacheBBox");
  Graphic::Requisition r;
  GraphicImpl::initRequisition(r);    
  c->request(r);
  if (r.x.defined && r.y.defined)
    {
      xalign = r.x.align;
      yalign = r.y.align;
      if (s.x != 0)
	{
	  boundingbox.l = p.x;
	  boundingbox.r = p.x + s.x;
	}
      else
	{
	  boundingbox.l = p.x - r.x.natural * r.x.align;
	  boundingbox.r = p.x + r.x.natural * (1. - r.x.align);
	}
      if (s.y != 0)
	{
	  boundingbox.t = p.y;
	  boundingbox.b = p.y + s.y;
	}
      else
	{
	  boundingbox.t = p.y - r.y.natural * r.y.align;
	  boundingbox.b = p.y + r.y.natural * (1. - r.y.align);
	}
    }
  else
    {
      xalign = 0.;
      yalign = 0.;
      boundingbox.l = p.x;
      boundingbox.r = p.x;
      boundingbox.t = p.y;
      boundingbox.b = p.y;
    }
}

