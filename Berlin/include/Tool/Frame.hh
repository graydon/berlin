/*$Id: Frame.hh,v 1.8 2000/10/20 17:45:01 stefan Exp $
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
#ifndef _Frame_hh
#define _Frame_hh

#include <Warsaw/config.hh>
#include <Warsaw/Telltale.hh>
#include <Warsaw/Region.hh>
#include "Berlin/ViewImpl.hh"
#include "Berlin/MonoGraphic.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/RefCountVar.hh"

class Frame : public MonoGraphic
{
public:
  class Renderer
  {
  public:
    Renderer(Warsaw::Coord t, bool f) : _thickness(t), _fill(f) {}
    virtual void draw(Warsaw::DrawTraversal_ptr) = 0;
    Warsaw::Coord _thickness;
    bool          _fill;  
  };
  Frame(Warsaw::Coord, Renderer *);
  virtual ~Frame();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);

  virtual void draw(Warsaw::DrawTraversal_ptr traversal) { if (_renderer) _renderer->draw(traversal);}
protected:
  void allocate_span(const Warsaw::Graphic::Requirement &, Warsaw::Region::Allotment &, Warsaw::Coord, Warsaw::Alignment);
  Warsaw::Coord        _thickness;
  Impl_var<RegionImpl> _allocation;
  Renderer            *_renderer;
};

class DynamicFrame : public virtual ViewImpl,
		     public Frame
{
 public:
  DynamicFrame(Warsaw::Coord t, Warsaw::Telltale::Mask, Frame::Renderer *, Frame::Renderer *);
  virtual ~DynamicFrame();
  virtual void attach(Warsaw::Telltale_ptr);
  virtual void update(const CORBA::Any &);
 protected:
  RefCount_var<Warsaw::Telltale> _telltale;
  Frame::Renderer               *_renderer1;
  Frame::Renderer               *_renderer2;
  bool                           _on;
  Warsaw::Telltale::Mask         _mask;
};

class InvisibleFrame : public Frame::Renderer
{
public:
  InvisibleFrame(Warsaw::Coord t, bool f) : Frame::Renderer(t, f) {}
  virtual void draw(Warsaw::DrawTraversal_ptr);
};

class Bevel : public Frame::Renderer
{
public:
  enum type { inset, outset, convex, concav};
  Bevel(Warsaw::Coord t, type s, Warsaw::Coord b, bool f) : Frame::Renderer(t, f), _style(s), _bright(b) {}
  virtual void draw(Warsaw::DrawTraversal_ptr);
protected:
  type          _style;
  Warsaw::Coord _bright;
};

class ColoredFrame : public Frame::Renderer
{
public:
  ColoredFrame(Warsaw::Coord t, const Warsaw::Color &c, bool f) : Frame::Renderer(t, f), _color(c) {}
  virtual void draw(Warsaw::DrawTraversal_ptr);
protected:
  Warsaw::Color _color;
};

#endif
