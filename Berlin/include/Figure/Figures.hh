/*$Id: Figures.hh,v 1.7 2000/12/21 21:05:43 stefan Exp $
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
#ifndef _Figures_hh
#define _Figures_hh

#include <Figure/FigureImpl.hh>

class PointImpl : public virtual POA_Figure::Point,
                  public FigureImpl
{
public:
  PointImpl();
  PointImpl(const Warsaw::Vertex &);
  PointImpl(const PointImpl &);
  virtual ~PointImpl();
  virtual Warsaw::Vertex pt();
  virtual void pt(const Warsaw::Vertex &);
};
  
class LineImpl : public virtual POA_Figure::Line,
                 public FigureImpl
{
public:
  LineImpl();
  LineImpl(const Warsaw::Vertex &, const Warsaw::Vertex &);
  LineImpl(const LineImpl &);
  virtual ~LineImpl();
  virtual Warsaw::Vertex pt1();
  virtual void pt1(const Warsaw::Vertex &);
  virtual Warsaw::Vertex pt2();
  virtual void pt2(const Warsaw::Vertex &);
};

class RectangleImpl : public virtual POA_Figure::Rectangle,
                      public FigureImpl
{
public:
  RectangleImpl();
  RectangleImpl(const Warsaw::Vertex &, const Warsaw::Vertex &);
  RectangleImpl(const RectangleImpl &);
  virtual ~RectangleImpl();
  virtual Warsaw::Vertex pt1();
  virtual void pt1(const Warsaw::Vertex &);
  virtual Warsaw::Vertex pt2();
  virtual void pt2(const Warsaw::Vertex &);
};

class CircleImpl : public virtual POA_Figure::Circle,
                   public FigureImpl
{
public:
  CircleImpl();
  CircleImpl(const Warsaw::Vertex &, Warsaw::Coord);
  CircleImpl(const CircleImpl &);
  virtual ~CircleImpl();
  virtual void resize();
  virtual Warsaw::Vertex center();
  virtual void center(const Warsaw::Vertex &);
  virtual Warsaw::Coord radius();
  virtual void radius(Warsaw::Coord);
protected:
  Warsaw::Vertex _center;
  Warsaw::Coord  _radius;
};

class EllipseImpl : public virtual POA_Figure::Ellipse,
                    public FigureImpl
{
public:
  EllipseImpl();
  EllipseImpl(const Warsaw::Vertex &, Warsaw::Coord, Warsaw::Coord);
  EllipseImpl(const EllipseImpl &);
  virtual ~EllipseImpl();
  virtual void resize();
  virtual Warsaw::Vertex center();
  virtual void center(const Warsaw::Vertex &);
  virtual Warsaw::Coord radius1();
  virtual void radius1(Warsaw::Coord);
  virtual Warsaw::Coord radius2();
  virtual void radius2(Warsaw::Coord);
protected:
  Warsaw::Vertex _center;
  Warsaw::Coord  _radius1;
  Warsaw::Coord  _radius2;
};

class PathImpl : public virtual POA_Figure::Path,
                 public FigureImpl
{
public:
  PathImpl(bool);
  PathImpl(const Warsaw::Path &, bool);
  PathImpl(const PathImpl &);
  virtual ~PathImpl();
  virtual void resize();
  Warsaw::Path *handles();
  void handles(const Warsaw::Path &);
  CORBA::Boolean closed();
private:
  Warsaw::Path_var _handles;
  bool             _closed;
};

#endif
