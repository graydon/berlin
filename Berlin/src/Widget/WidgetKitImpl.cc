/*$Id: WidgetKitImpl.cc,v 1.27 1999/10/21 20:23:51 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#include "Warsaw/config.hh"
#include "Warsaw/LayoutKit.hh"
#include "Widget/WidgetKitImpl.hh"
#include "Widget/TelltaleImpl.hh"
#include "Widget/BoundedValueImpl.hh"
#include "Widget/BoundedRangeImpl.hh"
#include "Widget/TextBufferImpl.hh"
#include "Widget/StreamBufferImpl.hh"
#include "Widget/Filler.hh"
#include "Widget/Indicator.hh"
#include "Widget/Frame.hh"
#include "Widget/ButtonImpl.hh"
#include "Widget/Toggle.hh"
#include "Widget/Dragger.hh"
#include "Widget/Stepper.hh"
#include "Widget/Gauge.hh"
#include "Widget/Slider.hh"
#include "Berlin/DebugGraphic.hh"
#include "Berlin/Plugin.hh"

WidgetKitImpl::WidgetKitImpl()
{
}

WidgetKitImpl::~WidgetKitImpl()
{
}

void WidgetKitImpl::bind(ServerContext_ptr sc)
{
  CloneableImpl::bind(sc);
  lk = obtain(context, LayoutKit);
}

TelltaleConstraint_ptr WidgetKitImpl::exclusive()
{
  ExclusiveChoice *constraint = new ExclusiveChoice();
  constraint->_obj_is_ready(_boa());
//   subjects.push_back(constraint);
  return constraint->_this();
}

TelltaleConstraint_ptr WidgetKitImpl::selectionRequired()
{
  SelectionRequired *constraint = new SelectionRequired;
  constraint->_obj_is_ready(_boa());
//   subjects.push_back(constraint);
  return constraint->_this();
}

Telltale_ptr WidgetKitImpl::normalTelltale()
{
  TelltaleImpl *telltale = new TelltaleImpl(TelltaleConstraint::_nil());
  telltale->_obj_is_ready(_boa());
  subjects.push_back(telltale);
  return telltale->_this();
}

Telltale_ptr WidgetKitImpl::constrainedTelltale(TelltaleConstraint_ptr constraint)
{
    TelltaleImpl *telltale = new TelltaleImpl(constraint);
    telltale->_obj_is_ready(_boa());
    subjects.push_back(telltale);
    constraint->add(telltale->_this());
    return telltale->_this();
}

BoundedValue_ptr WidgetKitImpl::bvalue(Coord l, Coord u, Coord v, Coord s, Coord p)
{
  BoundedValueImpl *bounded = new BoundedValueImpl(l, u, v, s, p);
  bounded->_obj_is_ready(_boa());
  subjects.push_back(bounded);
  return bounded->_this();  
}

BoundedRange_ptr WidgetKitImpl::brange(Coord l, Coord u, Coord lv, Coord uv, Coord s, Coord p)
{
  BoundedRangeImpl *bounded = new BoundedRangeImpl(l, u, lv, uv, s, p);
  bounded->_obj_is_ready(_boa());
  subjects.push_back(bounded);
  return bounded->_this();  
}

TextBuffer_ptr WidgetKitImpl::text()
{
  TextBufferImpl *buffer = new TextBufferImpl();
  buffer->_obj_is_ready(_boa());
  subjects.push_back(buffer);
  return buffer->_this();  
}

StreamBuffer_ptr WidgetKitImpl::stream()
{
  StreamBufferImpl *buffer = new StreamBufferImpl(80);
  buffer->_obj_is_ready(_boa());
  subjects.push_back(buffer);
  return buffer->_this();  
}

Graphic_ptr WidgetKitImpl::debugger(Graphic_ptr g, const char *s)
{
  DebugGraphic *debug = new DebugGraphic(s);
  debug->_obj_is_ready(_boa());
  graphics.push_back(debug);
  debug->body(g);
  return debug->_this();
};

Graphic_ptr WidgetKitImpl::inset(Graphic_ptr g, const Color &c, CORBA::Boolean fill)
{
  Frame *frame = new Frame(1, c, Frame::concav, fill);
  frame->_obj_is_ready(_boa());
  graphics.push_back(frame);
  frame->body(g);
  return frame->_this();
}

Graphic_ptr WidgetKitImpl::outset(Graphic_ptr g, const Color &c, CORBA::Boolean fill)
{
  Frame *frame = new Frame(1, c, Frame::convex, fill);
  frame->_obj_is_ready(_boa());
  graphics.push_back(frame);
  frame->body(g);
  return frame->_this();
}

Graphic_ptr WidgetKitImpl::filler(Graphic_ptr g, const Color &c)
{
  Filler *f = new Filler(c);
  f->_obj_is_ready(_boa());
  graphics.push_back(f);
  f->body(g);
  return f->_this();
}

Graphic_ptr WidgetKitImpl::indicator(Graphic_ptr g, const Color &c, Telltale_ptr t)
{
  Indicator *i = new Indicator(c);
  i->_obj_is_ready(_boa());
  i->attach(t);
  i->body(g);
  graphics.push_back(i);
  return i->_this();
}

View_ptr WidgetKitImpl::pushButtonFrame(Graphic_ptr g, const Color &c, Telltale_ptr t)
{
  DynamicFrame *frame = new DynamicFrame(1, c, Frame::concav, Frame::convex, Telltale::toggle, true);
  frame->_obj_is_ready(_boa());
  graphics.push_back(frame);
  frame->body(g);
  frame->attach(t);
  return frame->_this();
}

Button_ptr WidgetKitImpl::pushButton(Graphic_ptr g, const Color &b, Command_ptr c)
{
  ButtonImpl *button = new ButtonImpl;
  button->_obj_is_ready(_boa());
  button->action(c);
  graphics.push_back(button);

  DynamicFrame *frame1 = new DynamicFrame(1, b, Frame::black, Frame::flat, Telltale::active, false);
  frame1->_obj_is_ready(_boa());
  graphics.push_back(frame1);
  frame1->body(g);
  frame1->attach(Controller_var(button->_this()));

  DynamicFrame *frame2 = new DynamicFrame(1, b, Frame::concav, Frame::convex, Telltale::toggle, true);
  frame2->_obj_is_ready(_boa());
  graphics.push_back(frame2);
  frame2->body(Graphic_var(frame1->_this()));
  frame2->attach(Controller_var(button->_this()));

  button->body(Graphic_var(frame2->_this()));
  return button->_this();
}

Controller_ptr WidgetKitImpl::toggle(Graphic_ptr g, const Color &b)
{
  Toggle *t = new Toggle;
  t->_obj_is_ready(_boa());
  graphics.push_back(t);
  DynamicFrame *frame = new DynamicFrame(1, b, Frame::concav, Frame::convex, Telltale::chosen, true);
  frame->_obj_is_ready(_boa());
  graphics.push_back(frame);
  frame->attach(Controller_var(t->_this()));
  frame->body(g);

  t->body(Graphic_var(frame->_this()));
  return t->_this();
}

Controller_ptr WidgetKitImpl::dragger(Graphic_ptr g, Command_ptr command)
{
  Dragger *dragger = new Dragger(command);
  dragger->_obj_is_ready(_boa());
  dragger->body(g);
  graphics.push_back(dragger);
  return dragger->_this();
}

Controller_ptr WidgetKitImpl::stepper(Graphic_ptr g, Command_ptr command)
{
  Stepper *stepper = new Stepper;
  stepper->_obj_is_ready(_boa());
  stepper->body(g);
  stepper->action(command);
  graphics.push_back(stepper);
  return stepper->_this();
}

Graphic_ptr WidgetKitImpl::gauge(const Color &color, BoundedValue_ptr value)
{
  Gauge *g = new Gauge(value, color);
  g->_obj_is_ready(_boa());
  value->attach(g);
  graphics.push_back(g);

  Frame *frame = new Frame(1, color, Frame::concav, false);
  frame->_obj_is_ready(_boa());
  graphics.push_back(frame);
  frame->body(g);
  return frame->_this();
}

Controller_ptr WidgetKitImpl::slider(const Color &color, BoundedValue_ptr value)
{
//   Graphic::Requisition req;
//   req.x.defined = true;
//   req.x.minimum = 10.;
//   req.x.natural = 10.;
//   req.x.maximum = 10.;
//   req.x.align = 0.;
//   req.y.defined = true;
//   req.y.minimum = 10.;
//   req.y.natural = 10.;
//   req.y.maximum = 10.;
//   req.y.align = 0;
//   Controller_var thumb;// = dragger(Graphic_var(outset(lk->glueRequisition(req), color, true)));
//   Slider *s = new Slider(xaxis);
//   s->_obj_is_ready(_boa());
//   s->init(thumb, value);
//   req.x.minimum = 100.;
//   req.x.natural = 100.;
//   req.x.maximum = 100.;
//   req.x.align = 0.;
//   req.y.minimum = 12.;
//   req.y.natural = 12.;
//   req.y.maximum = 12.;
//   req.y.align = 0;
//   s->body(Graphic_var(inset(Graphic_var(lk->glueRequisition(req)), color, true)));
//   graphics->push_back(s);
//   return s->_this();
  return Controller::_nil();
}

EXPORT_PLUGIN(WidgetKitImpl,interface(WidgetKit))
