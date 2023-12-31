/*$Id: Scrollbar.cc,v 1.18 2001/04/18 06:07:28 stefan Exp $
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
#include <Berlin/Provider.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/CommandImpl.hh>
#include "Widget/Motif/Scrollbar.hh"

using namespace Prague;
using namespace Warsaw;
using namespace Motif;

class Scrollbar::Observer : public ObserverImpl
{
public:
  Observer(Scrollbar *s) : _scrollbar(s) {}
  void update(const CORBA::Any &any) { _scrollbar->update(any);}
private:
  Scrollbar *_scrollbar;
};

class Scrollbar::Drag : public CommandImpl
{
public:
  Drag(Scrollbar *s) : _parent(s) { _parent->_add_ref();}
  ~Drag() { _parent->_remove_ref();}
  virtual void execute(const CORBA::Any &any)
  {
    Vertex *delta;
    if (any >>= delta)
      {
	if (_parent->_axis == xaxis && delta->x != 0.) _parent->_value->adjust(delta->x);
	else if (_parent->_axis == yaxis && delta->y != 0.) _parent->_value->adjust(delta->y);
      }
    else  std::cerr << "Drag::execute : wrong message type !" << std::endl;
  }
private:
  Scrollbar *_parent;
};

Scrollbar::Scrollbar(BoundedRange_ptr v, Axis a, const Warsaw::Graphic::Requisition &r)
  : ControllerImpl(false),
    _requisition(r),
    _translate(new Observer(this)),
    _value(RefCount_var<BoundedRange>::increment(v)),
    _axis(a)
{
  Trace trace("Scrollbar::Scrollbar");
  BoundedRange::Settings settings = _value->state();
  _offset.lower = settings.lvalue/(settings.upper - settings.lower);
  _offset.upper = settings.uvalue/(settings.upper - settings.lower);
  _value->attach(Observer_var(_translate->_this()));
}

void Scrollbar::init(Controller_ptr t)
{
  Trace trace("Scrollbar::init");
  body(t);
  t->add_parent_graphic(Graphic_var(_this()), 0);
  append_controller(t);
}

void Scrollbar::update(const CORBA::Any &any)
{
  BoundedRange::Settings *settings;
  any >>= settings;
  _offset.lower = (settings->lvalue - settings->lower)/(settings->upper - settings->lower);
  _offset.upper = (settings->uvalue - settings->lower)/(settings->upper - settings->lower);
  need_redraw();
}

void Scrollbar::draw(DrawTraversal_ptr traversal)
{
  traverse_thumb(traversal);
}

void Scrollbar::pick(PickTraversal_ptr traversal)
{
//   if (grabbed(traversal->device()) || traversal->intersects_allocation())
  if (traversal->intersects_allocation())
    {
      traversal->enter_controller(Controller_var(_this()));
      MonoGraphic::traverse(traversal);
//       if (!grabbed(traversal->device())) traverse_thumb(traversal);
      traverse_thumb(traversal);
      if (!traversal->picked()) traversal->hit();
      traversal->leave_controller();
    }
}

void Scrollbar::allocate(Tag, const Allocation::Info &info)
{
  Lease_var<RegionImpl> allocation(Provider<RegionImpl>::provide());
  allocation->copy(info.allocation);
  if (_axis == xaxis)
    {
      Coord lower = allocation->lower.x;
      Coord scale = allocation->upper.x - allocation->lower.x;
      allocation->lower.x = lower + scale*_offset.lower;
      allocation->upper.x = lower + scale*_offset.upper;
    }
  else
    {
      Coord lower = allocation->lower.y;
      Coord scale = allocation->upper.y - allocation->lower.y;
      allocation->lower.y = lower + scale*_offset.lower;
      allocation->upper.y = lower + scale*_offset.upper;
    }
  allocation->lower.z = allocation->upper.z = 0.;
  allocation->normalize(info.transformation);
}

Command_ptr Scrollbar::create_drag_command()
{
  Drag *d = new Drag(this);
  activate(d);
  return d->_this();
}

void Scrollbar::traverse_thumb(Traversal_ptr traversal)
{
  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  Lease_var<RegionImpl> allocation(Provider<RegionImpl>::provide());
  allocation->copy(Region_var(traversal->current_allocation()));
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  if (_axis == xaxis)
    {
      Coord lower = allocation->lower.x;
      Coord scale = allocation->upper.x - allocation->lower.x;
      allocation->lower.x = lower + scale*_offset.lower;
      allocation->upper.x = lower + scale*_offset.upper;
      allocation->lower.z = allocation->upper.z = 0.;
    }
  else if (_axis == yaxis)
    {
      Coord lower = allocation->lower.y;
      Coord scale = allocation->upper.y - allocation->lower.y;
      allocation->lower.y = lower + scale*_offset.lower;
      allocation->upper.y = lower + scale*_offset.upper;
    }
  allocation->lower.z = allocation->upper.z = 0.;
  allocation->normalize(Transform_var(tx->_this()));
  try { traversal->traverse_child (child, 0, Region_var(allocation->_this()), Transform_var(tx->_this()));}
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
}
