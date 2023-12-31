/*$Id: Frame.cc,v 1.13 2001/04/18 06:07:28 stefan Exp $
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
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Subject.hh>
#include <Berlin/TransformImpl.hh>
#include <Berlin/RegionImpl.hh>
#include <Berlin/Provider.hh>
#include <Berlin/Color.hh>
#include <Prague/Sys/Tracer.hh>
#include "Tool/Frame.hh"
#include "Tool/Beveler.hh"

using namespace Prague;
using namespace Warsaw;

Frame::Frame(Coord t, Frame::Renderer *r) : _thickness(t), _allocation(new RegionImpl), _renderer(r) {}
Frame::~Frame() { Trace trace("Frame::~Frame");}
void Frame::request(Warsaw::Graphic::Requisition &requisition)
{
  MonoGraphic::request(requisition);
  Coord t = _thickness + _thickness;
  if (requisition.x.defined)
    {
      requisition.x.natural += t;
      requisition.x.maximum += t;
      requisition.x.minimum += t;
    }
  if (requisition.y.defined)
    {
      requisition.y.natural += t;
      requisition.y.maximum += t;
      requisition.y.minimum += t;
    }
}

void Frame::traverse(Traversal_ptr traversal)
{
  Trace trace("Frame::traverse");
  if (!traversal->intersects_allocation()) return;
  traversal->visit(Graphic_var(_this()));
  Lease_var<RegionImpl> allocation(Provider<RegionImpl>::provide());
  allocation->copy(Region_var(traversal->current_allocation()));
  Lease_var<TransformImpl> tx(Provider<TransformImpl>::provide());
  tx->load_identity();
  
  Allocation::Info info;
  info.allocation = allocation->_this();
  info.transformation = tx->_this();
  allocate(0, info);

  Graphic_var child = body();
  if (CORBA::is_nil(child)) return;
  try { traversal->traverse_child (child, 0, info.allocation, info.transformation);}
  catch (const CORBA::OBJECT_NOT_EXIST &) { body(Warsaw::Graphic::_nil());}
  catch (const CORBA::COMM_FAILURE &) { body(Warsaw::Graphic::_nil());}
}

void Frame::extension(const Allocation::Info &info, Region_ptr region)
{
  if (!CORBA::is_nil(info.allocation)) default_extension(info, region);
  else MonoGraphic::extension(info, region);
}

void Frame::allocate(Tag, const Allocation::Info &info)
{
  Warsaw::Graphic::Requisition req;
  GraphicImpl::init_requisition(req);
  MonoGraphic::request(req);
  _allocation->valid = true;
  Region::Allotment a;
  Vertex o;
  Lease_var<RegionImpl> region(Provider<RegionImpl>::provide());
  region->copy(info.allocation);

  region->normalize(o);
  info.transformation->translate(o);
  info.allocation->copy(Region_var(region->_this()));
  
  Vertex delta;
  info.allocation->span(xaxis, a);
  allocate_span(req.x, a, _thickness, 0.);
  _allocation->lower.x = -(a.end - a.begin) * a.align;
  _allocation->upper.x = _allocation->lower.x + (a.end - a.begin);
  _allocation->xalign = a.align;
  delta.x = a.begin - _allocation->lower.x;

  info.allocation->span(yaxis, a);
  allocate_span(req.y, a, _thickness, 0.);
  _allocation->lower.y = -(a.end - a.begin) * a.align;
  _allocation->upper.y = _allocation->lower.y + (a.end - a.begin);
  _allocation->yalign = a.align;
  delta.y = a.begin - _allocation->lower.y;
  delta.z = 0;
  
  info.allocation->copy(Region_var(_allocation->_this()));
  info.transformation->translate(delta);
}

void Frame::allocate_span(const Warsaw::Graphic::Requirement &r, Region::Allotment &a, Coord margin, Alignment align)
{
  a.begin += margin;
  a.end -= margin;
}

DynamicFrame::DynamicFrame(Coord t, Telltale::Mask m, Frame::Renderer *r1, Frame::Renderer *r2)
  : Frame(t, r2), _renderer1(r1), _renderer2(r2), _on(true), _mask(m)
{
  Trace trace("DynamicFrame::DynamicFrame");
}

DynamicFrame::~DynamicFrame()
{
  Trace trace("DynamicFrame::~DynamicFrame");
  delete _renderer1;
  delete _renderer2;
}

void DynamicFrame::attach(Telltale_ptr subject)
{
  Trace trace("DynamicFrame::attach");
  if (!CORBA::is_nil(_telltale)) _telltale->detach(Observer_var(_this()));
  if (!CORBA::is_nil(subject))
    {
      _telltale = RefCount_var<Telltale>::increment(subject);
      _telltale->attach(Observer_var(_this()));
      bool flag = _telltale->test(_mask);
      if (flag == _on) return;
      _on = flag;
      _renderer = _on ? _renderer1 : _renderer2;
      need_redraw();
    }
  else _telltale = Telltale::_nil();
}

void DynamicFrame::update(const CORBA::Any &)
{
  Trace trace("DynamicFrame::update");
  bool flag = _telltale->test(_mask);
  if (flag == _on) return;
  _on = flag;
  _renderer = _on ? _renderer1 : _renderer2;
  need_redraw();
}

void InvisibleFrame::draw(DrawTraversal_ptr traversal)
{
  Region_var allocation = traversal->current_allocation();
  Vertex l, u;
  allocation->bounds(l, u);
  DrawingKit_var drawing = traversal->drawing();
  DrawingKit::Fillstyle style = drawing->surface_fillstyle();
  if (style != DrawingKit::outlined && _fill) drawing->draw_rectangle(l, u);
  else if (_fill)
    {
      drawing->save();
      drawing->surface_fillstyle(DrawingKit::solid);
      drawing->draw_rectangle(l, u);
      drawing->restore();
    }
  else
    {
      Vertex ltmp = l, utmp = u;
      utmp.y = ltmp.y + _thickness;
      drawing->draw_rectangle(ltmp, utmp);
      ltmp.x = utmp.x - _thickness, ltmp.y = utmp.y;
      utmp.y = u.y - _thickness;
      drawing->draw_rectangle(ltmp, utmp);
      ltmp.x = l.x, utmp.x = l.x + _thickness;
      drawing->draw_rectangle(ltmp, utmp);
      ltmp.y = u.y - _thickness;
      utmp = u;
      drawing->draw_rectangle(ltmp, utmp);
    }
}

void Bevel::draw(DrawTraversal_ptr traversal)
{
  Region_var allocation = traversal->current_allocation();
  Vertex u, l;
  allocation->bounds(l, u);
  DrawingKit_var drawing = traversal->drawing();
  Color color = drawing->foreground();
  Color light = brightness(color, _bright);
  Color dark  = brightness(color,-_bright);
  switch (_style)
    {
    case inset:
      Beveler::rect(traversal, _thickness, color, dark, light, l.x, u.x, l.y, u.y, _fill);
      break;
    case outset:
      Beveler::rect(traversal, _thickness, color, light, dark, l.x, u.x, l.y, u.y, _fill);
      break;
    case convex:
      Beveler::rect(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, false);
      l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
      Beveler::rect(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, _fill);
      break;
    case concav:
      Beveler::rect(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, false);
      l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
      Beveler::rect(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, _fill);
      break;
    }
}

void ColoredFrame::draw(DrawTraversal_ptr traversal)
{
  Region_var allocation = traversal->current_allocation();
  Vertex l, u;
  allocation->bounds(l, u);
  DrawingKit_var drawing = traversal->drawing();
  DrawingKit::Fillstyle style = drawing->surface_fillstyle();
  drawing->save();
  Color tmp = drawing->foreground();
  tmp.red = _color.red;
  tmp.green = _color.green;
  tmp.blue = _color.blue;
  drawing->foreground(tmp);
  if (style == DrawingKit::outlined) drawing->surface_fillstyle(DrawingKit::solid);
  if (_fill) drawing->draw_rectangle(l, u);
  else
    {
      Vertex ltmp = l, utmp = u;
      utmp.y = ltmp.y + _thickness;
      drawing->draw_rectangle(ltmp, utmp);
      ltmp.x = utmp.x - _thickness, ltmp.y = utmp.y;
      utmp.y = u.y - _thickness;
      drawing->draw_rectangle(ltmp, utmp);
      ltmp.x = l.x, utmp.x = l.x + _thickness;
      drawing->draw_rectangle(ltmp, utmp);
      ltmp.y = u.y - _thickness;
      utmp = u;
      drawing->draw_rectangle(ltmp, utmp);
    }
  drawing->restore();
}

