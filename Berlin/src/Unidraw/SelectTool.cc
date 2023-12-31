/*$Id: SelectTool.cc,v 1.2 2001/04/18 06:07:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/PickTraversal.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/PickTraversalImpl.hh>
#include <Berlin/Provider.hh>
#include "Unidraw/SelectTool.hh"
#include <vector>

using namespace Prague;
using namespace Warsaw;
using namespace Unidraw;

class SelectTraversal : public PickTraversalImpl
{
public:
  SelectTraversal(Warsaw::Graphic_ptr g, Warsaw::Region_ptr a, Warsaw::Transform_ptr t, PositionalFocus *f)
    : PickTraversalImpl(g, a, t, f) {}
  ~SelectTraversal()
  { for (std::vector<SelectTraversal *>::iterator i = _selected.begin(); i != _selected.end(); ++i) (*i)->deactivate();}
  size_t selected() const { return _selected.size();}
  SelectTraversal *operator [](size_t i) { return _selected[i];}
  virtual CORBA::Boolean intersects_region(Warsaw::Region_ptr);
  virtual void hit() { _selected.push_back(new SelectTraversal(*this)); activate(_selected.back());}
  virtual CORBA::Boolean ok() { return true;}
  virtual CORBA::Boolean picked() { return _selected.size();}
private:
  std::vector<SelectTraversal *> _selected;
};

CORBA::Boolean SelectTraversal::intersects_region(Region_ptr region)
{
//   const Transform::Matrix &matrix = get_transformation(current())->matrix();
//   Coord d = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
//   if (d == 0.) return false;
//   Coord x = _pointer.x - matrix[0][3];
//   Coord y = _pointer.y - matrix[1][3];
//   Vertex local;
//   local.x = (matrix[1][1] * x - matrix[0][1] * y)/d;
//   local.y = (matrix[0][0] * y - matrix[1][0] * x)/d;
//   Vertex lower, upper;
//   region->bounds(lower, upper);
//   return lower.x <= local.x && local.x <= upper.x && lower.y <= local.y && local.y <= upper.y;
  return false;
}

class SelectCommand : public virtual POA_Unidraw::Command,
		      public ServantBase
{
public:
  SelectCommand();
  virtual void execute();
  virtual void store(Unidraw::Model_ptr, const CORBA::Any &) {}
  virtual CORBA::Any *recall(Unidraw::Model_ptr) { return new CORBA::Any();}
  virtual void destroy() { deactivate();}
private:
};

SelectTool::SelectTool(Graphic_ptr graphic) : _graphic(Graphic::_duplicate(graphic)) {}
SelectTool::~SelectTool() {}
CORBA::Boolean SelectTool::grasp(Warsaw::Controller_ptr controller, Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  _root = Controller::_duplicate(controller);
  _iterator = _root->last_child_graphic();
  _begin = event[1].attr.location();
  Transform_var trafo = traversal->current_transformation();
  trafo->inverse_transform_vertex(_begin);
  _matrix[0][0] = _matrix[0][1] = _matrix[0][2] = 0.;
  _matrix[1][0] = _matrix[1][1] = _matrix[1][2] = 0.;
  _matrix[2][0] = _matrix[2][1] = _matrix[2][2] = 0.;
  _matrix[3][0] = _matrix[3][1] = _matrix[3][2] = 0.;
  _matrix[0][3] = _begin.x;
  _matrix[1][3] = _begin.y;
  _matrix[2][3] = _begin.z;
  Transform_var transform = _graphic->transformation();
  transform->load_matrix(_matrix);
  _iterator->insert(_graphic);
  return true;
}

CORBA::Boolean SelectTool::manipulate(Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  if (event[0].attr._d() == Warsaw::Input::button) return false;
  _end = event[0].attr.location();
  Transform_var trafo = traversal->current_transformation();
  trafo->inverse_transform_vertex(_end);
  _matrix[0][0] = _end.x - _begin.x;
  _matrix[1][1] = _end.y - _begin.y;
  _matrix[2][2] = _end.z - _begin.z;
  Transform_var transform = _graphic->transformation();
  transform->load_matrix(_matrix);
  _graphic->need_resize();
  return true;
}

Unidraw::Command_ptr SelectTool::effect(Warsaw::PickTraversal_ptr traversal, const Warsaw::Input::Event &event)
{
  /*
   * traverse the viewer's children and pick all graphics that intersect with the selected region 
   */
  Lease_var<RegionImpl> _allocation(Provider<RegionImpl>::provide());
  _allocation->valid = true;
  _allocation->lower = _begin;
  _allocation->upper = _end;
  Impl_var<SelectTraversal> select(new SelectTraversal(_root, Region_var(_allocation->_this()), Warsaw::Transform::_nil(), 0));
  _root->traverse(Traversal_var(select->_this()));
  /*
   * now walk down the picked trail and find 'Viewer' objects.
   */

  _iterator->remove();
  _iterator->destroy();
  SelectCommand *command = new SelectCommand();
  activate(command);
  return command->_this();
}

SelectCommand::SelectCommand() {}
void SelectCommand::execute() {}
