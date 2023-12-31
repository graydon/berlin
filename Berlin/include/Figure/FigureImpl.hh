/*$Id: FigureImpl.hh,v 1.2 1999/10/13 21:32:31 gray Exp $
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
#ifndef _FigureImpl_hh
#define _FigureImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Figure.hh>
#include <Berlin/GraphicImpl.hh>
#include <Berlin/ImplVar.hh>

class TransformImpl;
class RegionImpl;

class TransformFigure : implements (Figure), public GraphicImpl
{
 public:
  TransformFigure();
  ~TransformFigure();
  virtual Transform_ptr transformation();
  virtual void request(Requisition &);
  virtual void extension(const Allocation::Info &, Region_ptr r);
  virtual void pick(PickTraversal_ptr);
  virtual void needRedraw();
  
  Mode type() { return mode;}
  void type(Mode m) { mode = m; needRedraw();}
  Color foreground() { return fg;}
  void foreground(const Color &f) { fg = f; needRedraw();}
  Color background() { return bg;}
  void background(const Color &b) { bg = b; needRedraw();}

  virtual void resize();

  void copy(const TransformFigure &);
 protected:
  Mode mode;
  Color fg, bg;
  Impl_var<TransformImpl> tx;
  Impl_var<RegionImpl> ext;
};

class FigureImpl : public TransformFigure
{
public:
  FigureImpl();
  virtual ~FigureImpl();

  void addPoint(Coord, Coord);
  void reset();
  virtual void resize();

  virtual void extension(const Allocation::Info &, Region_ptr);
  virtual void draw(DrawTraversal_ptr);
  virtual void pick(PickTraversal_ptr);

  void copy(const FigureImpl &);
protected:
  Vertices_var path;
  Vertices_var handle;
};

#endif /* _FigureImpl_hh */
