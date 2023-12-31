/*$Id: StageImpl.hh,v 1.12 1999/10/21 20:23:51 gray Exp $
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
#ifndef _StageImpl_hh
#define _StageImpl_hh

#include "Warsaw/config.hh"
#include "Warsaw/Stage.hh"
#include "Warsaw/Traversal.hh"
#include "Berlin/GraphicImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Geometry.hh"
#include "Prague/Sys/Thread.hh"
#include <list>

class StageHandleImpl;

class StageImpl : implements(Stage), public GraphicImpl
{
  class Sequence;
  class Finder;
  class Quad;
  class QuadTree;
 public:
  StageImpl();
  ~StageImpl();

  virtual void request(Requisition &);

  virtual void traverse(Traversal_ptr);

  virtual void allocate(Tag, const Allocation::Info &);
  virtual void needRedraw();
  virtual void needRedrawRegion(Region_ptr);
  virtual void needResize();
  //. relayout the children. If the bounding box changes call needResize on the parent
  
  virtual Region_ptr bbox();
  virtual CORBA::Long layers();
  virtual StageHandle_ptr layer(Stage::Index);
  /*
   * begin() and end() 'lock' the stage
   * in that only after the last end() conditions for needRedraw() & needResize() are done
   */
  virtual void begin();
  virtual void end();
  virtual StageHandle_ptr insert(Graphic_ptr, const Vertex &, const Vertex &, Index);
  virtual void remove(StageHandle_ptr);

  void move(StageHandleImpl *, const Vertex &);
  void resize(StageHandleImpl *, const Vertex &);
  void relayer(StageHandleImpl *, Stage::Index);
private:
  Tag tag();
  StageHandleImpl *tag2handle(Tag);
  void damage(StageHandleImpl *);

  Sequence *children;
  QuadTree *tree;
  long nesting;
  Impl_var<RegionImpl> damage_;
  Impl_var<RegionImpl> bbregion;
  bool need_redraw : 1;
  bool need_resize : 1;
  Prague::Mutex childMutex;
};

class StageHandleImpl : implements(StageHandle)
{
 public:
  StageHandleImpl(StageImpl *, Graphic_ptr, Tag, const Vertex &, const Vertex &, Stage::Index);
  virtual Stage_ptr parent() { return stage->_this();}
  virtual Graphic_ptr child() { return Graphic::_duplicate(c);}
  virtual Vertex position() { Prague::MutexGuard guard(mutex); return p;}
  virtual void position(const Vertex &);
  virtual Vertex size() { Prague::MutexGuard guard(mutex); return s;}
  virtual void size(const Vertex &);
  virtual Stage::Index layer() { Prague::MutexGuard guard(mutex); return l;}
  virtual void layer(Stage::Index);

  const Geometry::Rectangle<Coord> &bbox() { return boundingbox;}
  void bbox(RegionImpl &region)
    {
      region.valid   = true;
      region.lower.x = boundingbox.l;
      region.upper.x = boundingbox.r;
      region.xalign  = xalign;
      region.lower.y = boundingbox.t;
      region.upper.y = boundingbox.b;
      region.yalign  = yalign;
    }
//  private:
  void cacheBBox();
  StageImpl *stage;
  Graphic_var c;
  Tag tag;
  Vertex p;
  Vertex s;
  Stage::Index l;
  Geometry::Rectangle<Coord> boundingbox;
  Alignment xalign;
  Alignment yalign;
  Prague::Mutex mutex;
};

#endif
