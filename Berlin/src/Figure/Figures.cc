/*$Id: Figures.cc,v 1.9 2001/04/18 06:07:27 stefan Exp $
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

#include <Figure/Figures.hh>

// using namespace Geometry;
using namespace Warsaw;

PointImpl::PointImpl() { add_point(0., 0.);}
PointImpl::PointImpl (const Vertex &v) { add_point(v.x, v.y);}
PointImpl::PointImpl (const PointImpl &f) { copy(f);}
PointImpl::~PointImpl() {}
Vertex PointImpl::pt() { return _path[0];}
void PointImpl::pt(const Vertex &v) { _path[0] = v; resize();}

LineImpl::LineImpl()
{
  add_point(0., 0.);
  add_point(0., 0.);
}

LineImpl::LineImpl (const Vertex &v1, const Vertex &v2)
{
  add_point(v1.x, v1.y);
  add_point(v2.x, v2.y);
}

LineImpl::LineImpl (const LineImpl &line) { copy(line);}
LineImpl::~LineImpl() {}
Vertex LineImpl::pt1() { return _path[0];}
void LineImpl::pt1(const Vertex &v) { _path[0] = v; resize();}
Vertex LineImpl::pt2() { return _path[1];}
void LineImpl::pt2(const Vertex &v) { _path[1] = v; resize();}

RectangleImpl::RectangleImpl()
{
  add_point(0., 0.);
  add_point(0., 0.);
  add_point(0., 0.);
  add_point(0., 0.);
}

RectangleImpl::RectangleImpl(const Vertex &v1, const Vertex &v2)
{
  add_point(v1.x, v1.y);
  add_point(v1.x, v2.y);
  add_point(v2.x, v2.y);
  add_point(v2.x, v1.y);
  resize();
}

RectangleImpl::RectangleImpl (const RectangleImpl &rectangle) { copy(rectangle);}
RectangleImpl::~RectangleImpl() {}
Vertex RectangleImpl::pt1() { return _path[0];}
void RectangleImpl::pt1(const Vertex &v1)
{
  Vertex v2 =  pt2();
  _path[0] = v1;
  _path[1].x = v1.x;
  _path[1].y = v2.y;
  _path[3].x = v2.x;
  _path[3].y = v1.y;
  resize();
}

Vertex RectangleImpl::pt2() { return _path[2];}
void RectangleImpl::pt2(const Vertex &v2)
{
  Vertex v1 =  pt1();
  _path[2] = v2;
  _path[1].x = v1.x;
  _path[1].y = v2.y;
  _path[3].x = v2.x;
  _path[3].y = v1.y;
  resize();
}

static const float magic = 0.5522847498307934f; // 4/3 * (sqrt(2) - 1)

CircleImpl::CircleImpl() {}

CircleImpl::CircleImpl (const Vertex &c, Coord r)
  : _center(c), _radius(r)
{
  resize();
}

CircleImpl::CircleImpl(const CircleImpl &circle)
{
  copy(circle);
  _center = circle._center;
  _radius = circle._radius;
}

CircleImpl::~CircleImpl() {}

void CircleImpl::resize()
{
  Vertex &c = _center;
  Coord &r = _radius;

//   float r0 = magic * r;
  reset();
  add_point(c.x + r, c.y);
//   add_curve(c.x, c.y - r, c.x + r, c.y - r0, c.x + r0, c.y - r);
//   add_curve(c.x - r, c.y, c.x - r0, c.y - r, c.x - r, c.y - r0);
//   add_curve(c.x, c.y + r, c.x - r, c.y + r0, c.x - r0, c.y + r);
//   add_curve(c.x + r, c.y, c.x + r0, c.y + r, c.x + r, c.y + r0);
  std::cerr << "sorry, CircleImpl::resize not implemented" << std::endl;
}

Vertex CircleImpl::center() { return _center;}
void CircleImpl::center(const Vertex &c) { _center = c; resize();}
Coord CircleImpl::radius() { return _radius;}
void CircleImpl::radius(Coord r) { _radius = r; resize();}

static const float p0 = 1.00000000f;
static const float p1 = 0.89657547f;   // cos 30 * sqrt(1 + tan 15 * tan 15)
static const float p2 = 0.70710678f;   // cos 45
static const float p3 = 0.51763809f;   // cos 60 * sqrt(1 + tan 15 * tan 15)
static const float p4 = 0.26794919f;   // tan 15

EllipseImpl::EllipseImpl() {}
EllipseImpl::EllipseImpl(const Vertex &c, Coord r1, Coord r2)
  : _center(c), _radius1(r1), _radius2(r2)
{
  resize();
}

EllipseImpl::EllipseImpl(const EllipseImpl &ellipse)
  : _center(ellipse._center), _radius1(ellipse._radius1), _radius2(ellipse._radius2)
{
  copy(ellipse);
}

EllipseImpl::~EllipseImpl() {}

void EllipseImpl::resize()
{
  Coord &r1 = _radius1;
  Coord &r2 = _radius2;
  Coord &x = _center.x;
  Coord &y = _center.y;

//   float px0 = p0 * r1, py0 = p0 * r2;
//   float px1 = p1 * r1, py1 = p1 * r2;
//   float px2 = p2 * r1, py2 = p2 * r2;
//   float px3 = p3 * r1, py3 = p3 * r2;
//   float px4 = p4 * r1, py4 = p4 * r2;

  reset();
  add_point(x + r1, y);
//     add_curve(x + px2, y + py2, x + px0, y + py4, x + px1, y + py3);
//     add_curve(x, y + r2, x + px3, y + py1, x + px4, y + py0);
//     add_curve(x - px2, y + py2, x - px4, y + py0, x - px3, y + py1);
//     add_curve(x - r1, y, x - px1, y + py3, x - px0, y + py4);
//     add_curve(x - px2, y - py2, x - px0, y - py4, x - px1, y - py3);
//     add_curve(x, y - r2, x - px3, y - py1, x - px4, y - py0);
//     add_curve(x + px2, y - py2, x + px4, y - py0, x + px3, y - py1);
//     add_curve(x + r1, y, x + px1, y - py3, x + px0, y - py4);
  std::cerr << "sorry, EllipseImpl::resize not implemented" << std::endl;
}

Vertex EllipseImpl::center() { return _center;}
void EllipseImpl::center(const Vertex &c) { _center = c; resize();}
Coord EllipseImpl::radius1() { return _radius1;}
void EllipseImpl::radius1(Coord r) { _radius1 = r; resize();}
Coord EllipseImpl::radius2() { return _radius2;}
void EllipseImpl::radius2(Coord r) { _radius2 = r; resize();}

PathImpl::PathImpl(bool flag) : _handles(new Warsaw::Path()), _closed(flag) {}
PathImpl::PathImpl (const Warsaw::Path &path, bool flag) : _handles(new Warsaw::Path(path)), _closed(flag) { resize();}
PathImpl::PathImpl(const PathImpl &path) : _handles(new Warsaw::Path(path._handles)), _closed(path._closed) { copy(path);}

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
  for (CORBA::ULong i = 0; i < _handles->length(); ++i) add_point(_handles[i].x, _handles[i].y);
  if (_closed && _handles->length()) add_point(_handles[0].x, _handles[0].y);
//         }
//     }
//   cerr << "sorry, PathImpl::resize not implemented" << endl;
}

PathImpl::~PathImpl () {}

Warsaw::Path *PathImpl::handles() { return new Warsaw::Path(_handles);}
void PathImpl::handles(const Warsaw::Path &path) { _handles = new Warsaw::Path(path); resize();}
CORBA::Boolean PathImpl::closed() { return _closed;}
