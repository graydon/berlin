/*$Id: Slider.cc,v 1.23 2001/04/18 06:07:28 stefan Exp $
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
#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/PickTraversal.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Berlin/CommandImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/Provider.hh>
#include <Berlin/TransformImpl.hh>
#include "Widget/Motif/Slider.hh"

using namespace Warsaw;
using namespace Motif;

class Slider::Observer : public ObserverImpl
{
public:
  Observer(Slider *s) : _parent(s) { _parent->_add_ref();}
  ~Observer() { _parent->_remove_ref();}
  void update(const CORBA::Any &any) { _parent->update(any);}
private:
  Slider *_parent;
};

class Slider::Drag : public CommandImpl
{
public:
  Drag(Slider *s) : _parent(s) { _parent->_add_ref();}
  ~Drag() { _parent->_remove_ref();}
  virtual void execute(const CORBA::Any &any)
  {
    Vertex *delta;
    if (any >>= delta)
      {
	if (_parent->_axis == xaxis && delta->x != 0.) _parent->_value->adjust(_parent->_scale * delta->x);
	else if (_parent->_axis == yaxis && delta->y != 0.) _parent->_value->adjust(_parent->_scale * delta->y);
      }
    else  std::cerr << "Slider::Drag::execute : wrong message type !" << std::endl;
  }
private:
  Slider *_parent;
};

Slider::Slider(BoundedValue_ptr v, Axis a, const Warsaw::Graphic::Requisition &r)
  : ControllerImpl(false),
    _requisition(r),
    _translate(new Observer(this)),
    _value(RefCount_var<BoundedValue>::increment(v)),
    _offset((_value->value() - _value->lower())/(_value->upper() - _value->lower())),
    _axis(a)
{
  _value->attach(Observer_var(_translate->_this()));
}

void Slider::init(Controller_ptr t)
{
  body(t);
  t->add_parent_graphic(Graphic_var(_this()), 0);
  append_controller(t);
}

void Slider::update(const CORBA::Any &any)
{
//   need_redraw();
  any >>= _offset;
  _offset -= _value->lower();
  _offset /= (_value->upper() - _value->lower());
  need_redraw();
}

void Slider::draw(DrawTraversal_ptr traversal)
{
  traverse_thumb(traversal);
}

void Slider::pick(PickTraversal_ptr traversal)
{
  if (traversal->intersects_allocation())
    {
      traversal->enter_controller(Controller_var(_this()));
      MonoGraphic::traverse(traversal);
      traverse_thumb(traversal);
      if (!traversal->picked()) traversal->hit();
      traversal->leave_controller();
    }
}

void Slider::allocate(Tag, const Allocation::Info &info)
{
  Lease_var<RegionImpl> allocation(Provider<RegionImpl>::provide());
  allocation->copy(info.allocation);
  if (_axis == xaxis)
    {
      Coord length = allocation->upper.x - allocation->lower.x - 240.;
      allocation->lower.x = _offset * length;
      allocation->upper.x = _offset * length + 240.;
    }
  else
    {
      Coord length = allocation->upper.y - allocation->lower.y - 240.;
      allocation->lower.y = _offset * length;
      allocation->upper.y = _offset * length + 240.;
    }
  allocation->lower.z = allocation->upper.z = 0.;
  allocation->normalize(info.transformation);
}

void Slider::extension(const Allocation::Info &a, Region_ptr r) { GraphicImpl::default_extension(a, r);}

Command_ptr Slider::create_drag_command()
{
  Drag *d = new Drag(this);
  activate(d);
  return d->_this();
}

void Slider::traverse_thumb(Traversal_ptr traversal)
{
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  Lease_var<RegionImpl> allocation(Provider<RegionImpl>::provide());
  allocation->copy(Region_var(traversal->current_allocation()));
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  Coord length;
  if (_axis == xaxis)
    {
      length = allocation->upper.x - allocation->lower.x - 240.;
      allocation->lower.x = _offset * length;
      allocation->upper.x = _offset * length + 240.;
    }
  else if (_axis == yaxis)
    {
      length = allocation->upper.y - allocation->lower.y - 240.;
      allocation->lower.y = _offset * length;
      allocation->upper.y = _offset * length + 240.;
    }
  allocation->lower.z = allocation->upper.z = 0.;
  allocation->normalize(Transform_var(tx->_this()));
  traversal->traverse_child(child, 0, Region_var(allocation->_this()), Transform_var(tx->_this()));
  _scale = (_value->upper() - _value->lower())/length;
}
