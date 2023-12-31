/*$Id: Figures.hh,v 1.1 1999/10/04 22:57:10 gray Exp $
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
#ifndef _Figures_hh
#define _Figures_hh

#include <Figure/FigureImpl.hh>

namespace Figures
{
  class PointImpl : implements(Point), public FigureImpl
  {
  public:
    PointImpl();
    PointImpl(const Vertex &);
    PointImpl(const PointImpl &);
    virtual ~PointImpl();
    virtual Vertex pt();
    virtual void pt(const Vertex &);
  };
  
  class LineImpl : implements(Line), public FigureImpl
  {
  public:
    LineImpl();
    LineImpl(const Vertex &, const Vertex &);
    LineImpl(const LineImpl &);
    virtual ~LineImpl();
    virtual Vertex pt1();
    virtual void pt1(const Vertex &);
    virtual Vertex pt2();
    virtual void pt2(const Vertex &);
  };

  class RectangleImpl : implements(Rectangle), public FigureImpl
  {
  public:
    RectangleImpl();
    RectangleImpl(const Vertex &, const Vertex &);
    RectangleImpl(const RectangleImpl &);
    virtual ~RectangleImpl();
    virtual Vertex pt1();
    virtual void pt1(const Vertex &);
    virtual Vertex pt2();
    virtual void pt2(const Vertex &);
  };

  class CircleImpl : implements(Circle), public FigureImpl
  {
  public:
    CircleImpl();
    CircleImpl(const Vertex &, Coord);
    CircleImpl(const CircleImpl &);
    virtual ~CircleImpl();
    virtual void resize();
    virtual Vertex center();
    virtual void center(const Vertex &);
    virtual Coord radius();
    virtual void radius(Coord);
  protected:
    Vertex center_;
    Coord radius_;
  };

  class EllipseImpl : implements(Ellipse), public FigureImpl
  {
  public:
    EllipseImpl();
    EllipseImpl(const Vertex &, Coord, Coord);
    EllipseImpl(const EllipseImpl &);
    virtual ~EllipseImpl();
    virtual void resize();
    virtual Vertex center();
    virtual void center(const Vertex &);
    virtual Coord radius1();
    virtual void radius1(Coord);
    virtual Coord radius2();
    virtual void radius2(Coord);
  protected:
    Vertex center_;
    Coord radius1_, radius2_;
  };

  class PathImpl : implements(Path), public FigureImpl
  {
  public:
    PathImpl();
    PathImpl(const Vertices &);
    PathImpl(const PathImpl &);
    virtual ~PathImpl();
    virtual void resize();
    Vertices *handles();
  };

};

#endif /* _Figures_hh */
