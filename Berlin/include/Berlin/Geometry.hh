/*$Id: Geometry.hh,v 1.5 1999/10/21 20:23:50 gray Exp $
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
#ifndef Geometry_hh
#define Geometry_hh

#include <Berlin/Math.hh>

namespace Geometry
{
  
const double epsilon = 10e-6;

template <class T>
class Point
{
public:
  Point() : x(0), y(0) {}
  Point(T xx, T yy) : x(xx), y(yy) {}
  T distance(const Point &p) const { return static_cast<T>(sqrt(static_cast<double>((x - p.x)*(x - p.x) + (y - p.y)*(y - p.y))));}
  T x;
  T y;
};

template <class T>
class Line
{
public:
  Line() {}
  Line(T ux, T uy, T vx, T vy) : u(ux, uy), v(vx, vy) {}
  Line(const Point<T> &uu, const Point<T> &vv) : u(uu), v(vv) {}
  T l() const { return u.x < v.x ? u.x : v.x;}
  T b() const { return u.y < v.y ? u.y : v.y;}
  T r() const { return u.x > v.x ? u.x : v.x;}
  T t() const { return u.y > v.y ? u.y : v.y;}
  bool contains(const Point<T> &, T bloat) const;
  bool intersects(const Line<T> &) const;
  Point<T> u;
  Point<T> v;
};

template <class T>
class Rectangle
{
public:
  Rectangle() : l(0), t(0), r(0), b(0) {}
  Rectangle(const Rectangle &rect) : l(rect.l), t(rect.t), r(rect.r), b(rect.b) {}
  Rectangle(T ll, T tt, T rr, T bb) : l(ll), t(tt), r(rr), b(bb) {}
  T w() const {	return r - l;}
  T h() const { return b - t;}
  T cx() const { return (l + r) / 2.;}
  T cy() const { return (b + t) / 2.;}

  bool contains(const Point<T> &p) const { return p.x >= l && p.x <= r && p.y >= t && p.y <= b;}
  bool contains(const Line<T> &c) const { return c.l() >= l && c.t() >= t && c.r() <= r && c.b() <= b;}
  bool intersects(const Line<T> &) const;
  bool contains(const Rectangle<T> &) const;
  bool intersects(const Rectangle<T> &) const;
  bool within(const Rectangle<T> &) const;
  bool touches(const Rectangle<T> &, T epsilon) const;

  void clear() { l = b = r = t = 0;}
  void merge(const Rectangle<T> &);

  T l;
  T t;
  T r;
  T b;
private:
  bool intersectsLine(const Line<T> &) const;
  static bool equal(T x, T y, T epsilon) { return x - y < epsilon && y - x < epsilon;}
};

template <class T>
class Polygon
{
public:
  Polygon(Point<T> *p, int c) : points(p), count(c), norm(0), normCount(0) {}
  ~Polygon() { delete [] norm;}
  
  int size() const { return count;}
  const Point<T> &operator[](int i) const { return points[i];}
  Point<T> &operator[](int i) { return points[i];}
 
  void boundingBox(Rectangle<T> &) const;
  bool contains(const Point<T> &, T) const;
  bool intersects(const Line<T> &) const;
  bool intersects(const Rectangle<T> &) const;
  bool intersects(const Polygon<T> &) const;
  bool within(const Rectangle<T> &) const;
protected:
  void normalize() const;
  Point<T> *points;
  int       count;
  mutable Point<T> *norm;
  mutable int normCount;
};

template <class T>
inline void rotate(double radians, T x, T y, T &tx, T &ty)
{
  T c = static_cast<T>(cos(radians));
  T s = static_cast<T>(sin(radians));
  tx = x * c + y * (-s);
  ty = x * s + y * c;
}

template <class T>
inline int signum(T a)
{
  if (a < 0) return -1;
  else if (a > 0) return 1;
  else return 0;
}

template <class T>
inline int same(const Point<T> &u, const Point<T> &v)
{
  T dx, dx1, dx2;
  T dy, dy1, dy2;
		
  dx = v.x - u.x;
  dy = v.y - u.y;
  dx1 = u.x - u.x;
  dy1 = u.y - u.y;
  dx2 = v.x - v.x;
  dy2 = v.y - v.y;
  return signum(dx*dy1 - dy*dx1) * signum(dx*dy2 - dy*dx2);
}

template <class T>
inline bool Line<T>::contains(const Point<T> &p, T bloat) const
{
  T l = Math::min(u.x, v.x) - bloat;
  T t = Math::min(u.y, v.y) - bloat;
  T r = Math::max(u.x, v.x) + bloat;
  T b = Math::max(u.y, v.y) + bloat;
  if (! (l <= p.x) && (p.x <= r) && (t <= p.y) && (p.y <= b)) return false;

  T dx = u.x - v.x;
  T dy = u.y - v.y;
  T length = static_cast<T>(sqrt(dx*dx + dy*dy));
  T radians = length <= epsilon ? 0 : static_cast<T>(acos(dx/length));
  if (dy > 0) radians = -radians;

  T x, y;
  rotate(radians, p.x - u.x, p.y - u.y, x, y);
  l = -length - bloat;
  t = -bloat;
  r = bloat;
  b = bloat;
  return l <= x && x <= r && t <= y && y <= b;
}

template <class T>
inline bool Line<T>::intersects(const Line &i) const
{
  return l() <= i.r() && i.l() <= r() && t() <= i.b() && i.t() <= b() && same(i.u, i.v) <= 0 && same(u, v) <= 0;
}

template <class T>
inline bool Rectangle<T>::intersects(const Line<T> &i) const
{
  return l <= i.r() && i.l() <= r && t <= i.b() && i.t() <= b && intersectsLine(i);
}
  
template <class T>
inline bool Rectangle<T>::contains(const Rectangle<T> &c) const
{
  return l <= c.l && t <= c.t && r >= c.r && b >= c.b;
}

template <class T>
inline bool Rectangle<T>::intersects(const Rectangle<T> &i) const
{
  return l <= i.r && r >= i.l && t <= i.b && b >= i.t;
}

template <class T>
inline bool Rectangle<T>::within(const Rectangle<T> &w) const
{
  return l >= w.l && t >= w.t && r <= w.r && b <= w.b;
}

template <class T>
inline bool Rectangle<T>::touches(const Rectangle<T> &a, T epsilon) const
{
  return equal(l, a.l, epsilon) || equal(r, a.r, epsilon) || equal(t, a.t, epsilon) || equal(b, a.b, epsilon);
}

template <class T>
inline void Rectangle<T>::merge(const Rectangle<T> &m)
{
  if (m.l < l) l = m.l;
  if (m.t < t) t = m.t;
  if (m.r > r) r = m.r;
  if (m.b > b) b = m.b;
}

template <class T>
inline bool Rectangle<T>::intersectsLine(const Line<T> &i) const
{
  return (contains(i.u) || contains(i.v) ||
	  i.intersects(Line<T>(l, t, r, t)) ||
	  i.intersects(Line<T>(r, t, r, b)) ||
	  i.intersects(Line<T>(r, b, l, b)) ||
	  i.intersects(Line<T>(l, b, l, t)));
}

template <class T>
ostream &operator << (ostream &os, const Rectangle<T> &rectangle)
{
  return os << '(' << rectangle.l << ',' << rectangle.t << ',' << rectangle.r << ',' << rectangle.b << ')';
}

template <class T>
inline void Polygon<T>::boundingBox(Rectangle<T> &b) const
{
  b.l = b.r = points[0].x;
  b.t = b.b = points[0].y;
  for (int i = 1; i < count; i++)
    {
      b.l = Math::min(b.l, points[i].x);
      b.t = Math::min(b.t, points[i].y);
      b.r = Math::max(b.r, points[i].x);
      b.b = Math::max(b.b, points[i].y);
    }
}

template <class T>
inline bool Polygon<T>::contains(const Point<T> &p, T bloat) const
{
  if (normCount == 0) normalize();
  int count = 0;
  Point<T> p0;
  bool cury_sign = norm[0].y >= p.y;
  for (int i = 0; i < normCount - 2; i++)
    {
      Line<T> l(norm[i].x - p.x, norm[i].y - p.y, norm[i+1].x - p.x, norm[i+1].y - p.y);
      if (l.contains(p0, bloat)) return true;
      bool nexty_sign = l.v.y >= 0.;
      if (nexty_sign != cury_sign)
	{
	  bool curx_sign = l.u.x >= 0;
	  bool nextx_sign = l.v.x >= 0;
	  if (curx_sign && nextx_sign) count++;
	  else if (curx_sign || nextx_sign)
	    {
	      T dx = l.v.x - l.u.x;
	      T dy = l.v.y - l.u.y;
	      if (dy >= 0)
		{
		  if (l.u.x * dy > l.u.y * dx) count++;
		}
	      else
		{
		  if (l.u.x * dy < l.u.y * dx) count++;
		}
	    }
	}
      cury_sign = nexty_sign;
    }
  return count % 2 == 1;
}

template <class T>
inline bool Polygon<T>::intersects(const Line<T> &l) const
{
  Rectangle<T> b;
  bool intersects = false;
  if (normCount == 0) normalize();
  boundingBox(b);
  if (b.intersects(l))
    {
      const Polygon<T> p(norm, normCount - 1);
      intersects = p.intersects(l) || contains(l.u, 0) || contains(l.v, 0);
    }
  return intersects;
}

template <class T>
inline bool Polygon<T>::intersects(const Rectangle<T> &ub) const
{
  Rectangle<T> b;
  boundingBox(b);
  if (!b.intersects(ub)) return false;
  if (b.within(ub)) return true;

  Line<T> bottom(ub.l, ub.b, ub.r, ub.b);
  if (intersects(bottom)) return true;
 
  Line<T> right(ub.r, ub.b, ub.r, ub.t);
  if (intersects(right)) return true;
 
  Line<T> top(ub.r, ub.t, ub.l, ub.t);
  if (intersects(top)) return true;

  Line<T> left(ub.l, ub.t, ub.l, ub.b);
  return intersects(left);
}
 
template <class T>
inline bool Polygon<T>::intersects(const Polygon<T> &p) const
{
  int s = p.size();
  if ((s > 0) && contains(p[0], 0)) return true;
  if ((size() > 0) && p.contains(points[0], 0)) return true;
  for (int i = 0; i < s; i++)
    if (intersects(Line<T>(p[i], p[(i + 1) % s]))) return true;
  return false;
}

template <class T>
inline int lowestleft(const Point<T> *points, int count)
{
  int ll = 0;
  T lx = points[0].x;
  T ly = points[0].y;
  for (int i = 1; i < count; i++)
    if (points[i].y < ly || (points[i].y == ly && points[i].x < lx))
      {
	ll = i;
	lx = points[i].x;
	ly = points[i].y;
      }
  return ll;
}

template<class T>
void Polygon<T>::normalize() const
{
  if (size())
    {
      int i, newcount = 1;
      int ll, limit = size();
 
      if (points[0].x == points[size() - 1].x && points[0].y == points[size() - 1].y) limit--;
      ll = lowestleft(points, limit);
      normCount = limit + 2;
      norm = new Point<T>[normCount];
 
      for (i = ll; i < limit; i++, newcount++)
	{
	  norm[newcount].x = points[i].x;
	  norm[newcount].y = points[i].y;
	}
      for (i = 0; i < ll; i++, newcount++)
	{
	  norm[newcount].x = points[i].x;
	  norm[newcount].y = points[i].y;
	}
 
      norm[newcount].x = norm[1].x;
      norm[newcount].y = norm[1].y;
      newcount--;
      norm[0].x = norm[newcount].x;
      norm[0].y = norm[newcount].y;
    }
}

}

#endif /* _Geometry_hh */
