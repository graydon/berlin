/*$Id: PickTraversalImpl.cc,v 1.17 1999/11/12 16:41:32 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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
#include "Berlin/PickTraversalImpl.hh"

PickTraversalImpl::PickTraversalImpl(Graphic_ptr g, Region_ptr r, Transform_ptr t, const Event::Pointer &p, Focus_ptr f)
  : TraversalImpl(g, r, t),
    pointer(p),
    focus(Focus::_duplicate(f)),
    mem(0)
{}

PickTraversalImpl::PickTraversalImpl(const PickTraversalImpl &t)
  : TraversalImpl(t),
    controllers(t.controllers),
    positions(t.positions),
    pointer(t.pointer),
    focus(t.focus),
    mem(0)
{
  SectionLog log("PickTraversal::PickTraversal");
}

PickTraversalImpl::~PickTraversalImpl()
{
  delete mem;
}

CORBA::Boolean PickTraversalImpl::intersectsRegion(Region_ptr region)
{
  Transform::Matrix matrix;
  Transform_var transform = transformation();
  transform->storeMatrix(matrix);
  Coord d = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
  if (d == 0.) return false;
  Coord x = pointer.location.x - matrix[0][3];
  Coord y = pointer.location.y - matrix[1][3];
  Vertex local;
  local.x = (matrix[1][1] * x - matrix[0][1] * y)/d;
  local.y = (matrix[0][0] * y - matrix[1][0] * x)/d;
  Vertex lower, upper;
  region->bounds(lower, upper);
//   cout << "PickTraversalImpl::intersectsRegion " << local << endl;
  return lower.x <= local.x && local.x <= upper.x && lower.y <= local.y && local.y <= upper.y;
}

CORBA::Boolean PickTraversalImpl::intersectsAllocation()
{
  Region_var region = allocation();
  return intersectsRegion(region);
}

void PickTraversalImpl::enterController(Controller_ptr c)
{
  SectionLog log("PickTraversal::enterController");
  controllers.push_back(Controller::_duplicate(c));
  positions.push_back(size());
}

void PickTraversalImpl::leaveController()
{
  SectionLog log("PickTraversal::leaveController");
  controllers.pop_back();
  positions.pop_back();
}

void PickTraversalImpl::hit()
{
  SectionLog log("PickTraversal::hit");
  delete mem;
  mem = new PickTraversalImpl(*this);
}

ostream &operator << (ostream &os, const Transform::Matrix &m)
{
  os << '[' << m[0][0] << ',' << m[0][1] << ',' << m[0][2] << ',' << m[0][3] << "]\n"
     << '[' << m[1][0] << ',' << m[1][1] << ',' << m[1][2] << ',' << m[1][3] << "]\n"
     << '[' << m[2][0] << ',' << m[2][1] << ',' << m[2][2] << ',' << m[2][3] << "]\n"
     << '[' << m[3][0] << ',' << m[3][1] << ',' << m[3][2] << ',' << m[3][3] << ']' << endl;
  return os;
};

void PickTraversalImpl::debug()
{
  cout << "PickTraversal::debug : stack size = " << size() << '\n';
  cout << "Controllers at ";
  for (size_t i = 0; i != positions.size(); i++) cout << positions[i] << ' ';
  cout << endl;
  Region_var r = allocation();
  Transform_var t = transformation();
  RegionImpl region(r, t);
  cout << "current allocation is " << region << endl;
  cout << "pointer is " << pointer.location << endl;
  Vertex local = pointer.location;
  Transform::Matrix matrix;
  t->storeMatrix(matrix);
  cout << "current trafo \n" << matrix;
  t->inverseTransformVertex(local);
  region.copy(r);
  cout << "local CS: current allocation is " << region << endl;
  cout << "local CS: pointer is " << local << endl;      
}
