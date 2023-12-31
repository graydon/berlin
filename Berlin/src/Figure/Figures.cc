/*$Id: Figures.cc,v 1.2 1999/10/13 21:32:32 gray Exp $
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

#include <Figure/Figures.hh>

// using namespace Geometry;
using namespace Figures;

PointImpl::PointImpl() { addPoint(0., 0.);}
PointImpl::PointImpl (const Vertex &v) { addPoint(v.x, v.y);}
PointImpl::PointImpl (const PointImpl &f) { copy(f);}
PointImpl::~PointImpl() {}
Vertex PointImpl::pt() { return path[0];}
void PointImpl::pt(const Vertex &v) { path[0] = v; resize();}

LineImpl::LineImpl()
{
  addPoint(0., 0.);
  addPoint(0., 0.);
}

LineImpl::LineImpl (const Vertex &v1, const Vertex &v2)
{
  addPoint(v1.x, v1.y);
  addPoint(v2.x, v2.y);
}

LineImpl::LineImpl (const LineImpl &line) { copy(line);}
LineImpl::~LineImpl() {}
Vertex LineImpl::pt1() { return path[0];}
void LineImpl::pt1(const Vertex &v) { path[0] = v; resize();}
Vertex LineImpl::pt2() { return path[1];}
void LineImpl::pt2(const Vertex &v) { path[1] = v; resize();}

RectangleImpl::RectangleImpl()
{
  addPoint(0., 0.);
  addPoint(0., 0.);
  addPoint(0., 0.);
  addPoint(0., 0.);
}

RectangleImpl::RectangleImpl(const Vertex &v1, const Vertex &v2)
{
  addPoint(v1.x, v1.y);
  addPoint(v1.x, v2.y);
  addPoint(v2.x, v2.y);
  addPoint(v2.x, v1.y);
  resize();
}

RectangleImpl::RectangleImpl (const RectangleImpl &rectangle) { copy(rectangle);}
RectangleImpl::~RectangleImpl() {}
Vertex RectangleImpl::pt1() { return path[0];}
void RectangleImpl::pt1(const Vertex &v1)
{
  Vertex v2 =  pt2();
  path[0] = v1;
  path[1].x = v1.x;
  path[1].y = v2.y;
  path[3].x = v2.x;
  path[3].y = v1.y;
  resize();
}

Vertex RectangleImpl::pt2() { return path[2];}
void RectangleImpl::pt2(const Vertex &v2)
{
  Vertex v1 =  pt1();
  path[2] = v2;
  path[1].x = v1.x;
  path[1].y = v2.y;
  path[3].x = v2.x;
  path[3].y = v1.y;
  resize();
}

static const float magic = 0.5522847498307934f; // 4/3 * (sqrt(2) - 1)

CircleImpl::CircleImpl() {}

CircleImpl::CircleImpl (const Vertex &c, Coord r)
  : center_(c), radius_(r)
{
  resize();
}

CircleImpl::CircleImpl(const CircleImpl &circle)
{
  copy(circle);
  center_ = circle.center_;
  radius_ = circle.radius_;
}

CircleImpl::~CircleImpl() {}

void CircleImpl::resize()
{
  Vertex &c = center_;
  Coord &r = radius_;

//   float r0 = magic * r;
  reset();
  addPoint(c.x + r, c.y);
//   add_curve(c.x, c.y - r, c.x + r, c.y - r0, c.x + r0, c.y - r);
//   add_curve(c.x - r, c.y, c.x - r0, c.y - r, c.x - r, c.y - r0);
//   add_curve(c.x, c.y + r, c.x - r, c.y + r0, c.x - r0, c.y + r);
//   add_curve(c.x + r, c.y, c.x + r0, c.y + r, c.x + r, c.y + r0);
  cerr << "sorry, CircleImpl::resize not implemented" << endl;
}

Vertex CircleImpl::center() { return center_;}
void CircleImpl::center(const Vertex &c) { center_ = c; resize();}
Coord CircleImpl::radius() { return radius_;}
void CircleImpl::radius(Coord r) { radius_ = r; resize();}

static const float p0 = 1.00000000f;
static const float p1 = 0.89657547f;   // cos 30 * sqrt(1 + tan 15 * tan 15)
static const float p2 = 0.70710678f;   // cos 45
static const float p3 = 0.51763809f;   // cos 60 * sqrt(1 + tan 15 * tan 15)
static const float p4 = 0.26794919f;   // tan 15

EllipseImpl::EllipseImpl() {}
EllipseImpl::EllipseImpl(const Vertex &c, Coord r1, Coord r2)
  : center_(c), radius1_(r1), radius2_(r2)
{
  resize();
}

EllipseImpl::EllipseImpl(const EllipseImpl &ellipse)
  : center_(ellipse.center_), radius1_(ellipse.radius1_), radius2_(ellipse.radius2_)
{
  copy(ellipse);
}

EllipseImpl::~EllipseImpl() {}

void EllipseImpl::resize()
{
  Coord &r1 = radius1_;
  Coord &r2 = radius2_;
  Coord &x = center_.x;
  Coord &y = center_.y;

//   float px0 = p0 * r1, py0 = p0 * r2;
//   float px1 = p1 * r1, py1 = p1 * r2;
//   float px2 = p2 * r1, py2 = p2 * r2;
//   float px3 = p3 * r1, py3 = p3 * r2;
//   float px4 = p4 * r1, py4 = p4 * r2;

  reset();
  addPoint(x + r1, y);
//     add_curve(x + px2, y + py2, x + px0, y + py4, x + px1, y + py3);
//     add_curve(x, y + r2, x + px3, y + py1, x + px4, y + py0);
//     add_curve(x - px2, y + py2, x - px4, y + py0, x - px3, y + py1);
//     add_curve(x - r1, y, x - px1, y + py3, x - px0, y + py4);
//     add_curve(x - px2, y - py2, x - px0, y - py4, x - px1, y - py3);
//     add_curve(x, y - r2, x - px3, y - py1, x - px4, y - py0);
//     add_curve(x + px2, y - py2, x + px4, y - py0, x + px3, y - py1);
//     add_curve(x + r1, y, x + px1, y - py3, x + px0, y - py4);
  cerr << "sorry, EllipseImpl::resize not implemented" << endl;
}

Vertex EllipseImpl::center() { return center_;}
void EllipseImpl::center(const Vertex &c) { center_ = c; resize();}
Coord EllipseImpl::radius1() { return radius1_;}
void EllipseImpl::radius1(Coord r) { radius1_ = r; resize();}
Coord EllipseImpl::radius2() { return radius2_;}
void EllipseImpl::radius2(Coord r) { radius2_ = r; resize();}

PathImpl::PathImpl() { FigureImpl::handle = new Vertices;}
PathImpl::PathImpl (const Vertices &v)
{
  handle = new Vertices(v);
  resize();
}

PathImpl::PathImpl(const PathImpl &path)
{
  copy(path);
  handle = new Vertices(path.handle);
}

void PathImpl::resize()
{
//   FigureKit::Vertices& vv = *handles_;
//   long n = vv.length();
  reset();
//   if (!closed_ && curved_) {
//         Bspline_move_to(vv[0].x, vv[0].y, vv[0].x, vv[0].y, vv[0].x, vv[0].y);
//         Bspline_curve_to(vv[0].x, vv[0].y, vv[0].x, vv[0].y, vv[1].x, vv[1].y);
//         for (long i = 1; i < n - 1; ++i) {
//             Bspline_curve_to(
//                 vv[i].x, vv[i].y, vv[i-1].x, vv[i-1].y, vv[i+1].x, vv[i+1].y
//             );
//         }
//         Bspline_curve_to(
// 	    vv[n-1].x, vv[n-1].y, vv[n-2].x, vv[n-2].y, vv[n-1].x, vv[n-1].y
//         );
//         Bspline_curve_to(
// 	    vv[n-1].x, vv[n-1].y, vv[n-1].x, vv[n-1].y, vv[n-1].x, vv[n-1].y
//         );
//     } else if (closed_ && curved_) {
//         Bspline_move_to(
// 	    vv[0].x, vv[0].y, vv[n-1].x, vv[n-1].y, vv[1].x, vv[1].y
//         );
//         for (long i = 1; i < n - 1; ++i) {
//             Bspline_curve_to(
//                 vv[i].x, vv[i].y, vv[i-1].x, vv[i-1].y, vv[i+1].x, vv[i+1].y
//             );
//         }
//         Bspline_curve_to(
//             vv[n-1].x, vv[n-1].y, vv[n-2].x, vv[n-2].y, vv[0].x, vv[0].y
//         );
//         Bspline_curve_to(
//             vv[0].x, vv[0].y, vv[n-1].x, vv[n-1].y, vv[1].x, vv[1].y
//         );
//     } else {
  for (CORBA::ULong i = 0; i < handle->length(); ++i) addPoint(handle[i].x, handle[i].y);
//         }
//     }
//   cerr << "sorry, PathImpl::resize not implemented" << endl;
}

PathImpl::~PathImpl () {}

Figure::Vertices *PathImpl::handles() { Figure::Vertices *ret = new Figure::Vertices(handle); return ret;}
