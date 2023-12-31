/*$Id: FigureImpl.hh,v 1.8 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _FigureImpl_hh
#define _FigureImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Figure.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/GraphicImpl.hh>

class TransformImpl;
class RegionImpl;

class TransformFigure : public virtual POA_Figure::FigureBase,
			public GraphicImpl
{
 public:
  TransformFigure();
  ~TransformFigure();
  virtual Warsaw::Transform_ptr transformation();
  virtual void request(Warsaw::Graphic::Requisition &);
  virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);
  virtual void need_redraw();
  
  Figure::Mode type() { return _mode;}
  void type(Figure::Mode m) { _mode = m; need_redraw();}
  Warsaw::Color foreground() { return _fg;}
  void foreground(const Warsaw::Color &f) { _fg = f; need_redraw();}
  Warsaw::Color background() { return _bg;}
  void background(const Warsaw::Color &b) { _bg = b; need_redraw();}

  virtual void resize();

  void copy(const TransformFigure &);
 protected:
  Figure::Mode            _mode;
  Warsaw::Color           _fg, _bg;
  Impl_var<TransformImpl> _tx;
  Impl_var<RegionImpl>    _ext;
};

class FigureImpl : public TransformFigure
{
public:
  FigureImpl();
  virtual ~FigureImpl();

  void add_point(Warsaw::Coord, Warsaw::Coord);
  void reset();
  virtual void resize();

  virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);

  void copy(const FigureImpl &);
protected:
  Warsaw::Path_var _path;
};

#endif
