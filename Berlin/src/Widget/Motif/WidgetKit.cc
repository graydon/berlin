/*$Id: WidgetKit.cc,v 1.21 2001/04/18 06:07:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/Server.hh>
#include <Warsaw/resolve.hh>
#include <Warsaw/Trigger.hh>
#include <Warsaw/Command.hh>
#include <Warsaw/Viewport.hh>
#include <Warsaw/Selection.hh>
#include <Berlin/CommandImpl.hh>
#include "Widget/Motif/WidgetKit.hh"
#include "Widget/Motif/Gauge.hh"
#include "Widget/Motif/Slider.hh"
#include "Widget/Motif/Panner.hh"
#include "Widget/Motif/Scrollbar.hh"
#include "Widget/Motif/Choice.hh"
#include "Widget/Motif/Terminal.hh"

using namespace Warsaw;
namespace Motif
{

class Forward : public CommandImpl
{
public:
  Forward(BoundedRange_ptr m) : model(RefCount_var<BoundedRange>::increment(m)) {}
  virtual void execute(const CORBA::Any &) { model->forward();}
private:
  RefCount_var<BoundedRange> model;
};

class Backward : public CommandImpl
{
public:
  Backward(BoundedRange_ptr m) : model(RefCount_var<BoundedRange>::increment(m)) {}
  virtual void execute(const CORBA::Any &) { model->backward();}
private:
  RefCount_var<BoundedRange> model;
};

// template <class Observer, class Argument>
// class Adapter1 : public WidgetKit::CommandImpl
// {
// public:
//   typedef void (Observer::*Method)();
//   Adapter1(typename Observer::_ptr_type o, Method m) : observer(o), method(m) {}
//   virtual void execute(const CORBA::Any &) { (observer->*method)();}
// private:
//   typename Observer::_ptr_type observer;
//   Method method;
// };

WidgetKit::WidgetKit(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
WidgetKit::~WidgetKit() {}

void WidgetKit::bind(ServerContext_ptr context)
{
  KitImpl::bind(context);
  Warsaw::Kit::PropertySeq props;
  props.length(0);
  command = resolve_kit<CommandKit>(context, "IDL:Warsaw/CommandKit:1.0", props);
  layout = resolve_kit<LayoutKit>(context, "IDL:Warsaw/LayoutKit:1.0", props);
  tool = resolve_kit<ToolKit>(context, "IDL:Warsaw/ToolKit:1.0", props);
  text = resolve_kit<TextKit>(context, "IDL:Warsaw/TextKit:1.0", props);
}

Trigger_ptr WidgetKit::button(Graphic_ptr g, Command_ptr c)
{
  Trigger_var trigger = tool->button(g, c);
  Warsaw::ToolKit::FrameSpec s1, s2;
  s1.brightness(0.5); s1._d(Warsaw::ToolKit::inset);
  s2.brightness(0.5); s2._d(Warsaw::ToolKit::outset);
  Graphic_var frame = tool->dynamic(g, 20., Warsaw::Controller::pressed, s1, s2, true, trigger);
  trigger->body(frame);
  return trigger._retn();
}

Controller_ptr WidgetKit::toggle(Graphic_ptr g)
{
  Controller_var toggle = tool->toggle(Warsaw::Graphic::_nil());
  Warsaw::ToolKit::FrameSpec s1, s2;
  s1.brightness(0.5); s1._d(ToolKit::inset);
  s2.brightness(0.5); s2._d(ToolKit::outset);
  Graphic_var frame = tool->dynamic(g, 20., Warsaw::Controller::toggled, s1, s2, true, toggle);
  toggle->body(frame);
  return toggle._retn();
}

Graphic_ptr WidgetKit::gauge(BoundedValue_ptr value)
{
  Color gray = {0.5, 0.5, 0.5, 1.0};
  Gauge *g = new Gauge(value, gray);
  activate(g);
  value->attach(Observer_var(g->_this()));

  Warsaw::ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(Warsaw::ToolKit::outset);

  Graphic_var frame = tool->frame(Graphic_var(g->_this()), 20., spec, false);
  return frame._retn();
}

Controller_ptr WidgetKit::slider(BoundedValue_ptr value, Axis axis)
{
  Warsaw::ToolKit::FrameSpec spec;
  /*
   * the bar
   */
  Warsaw::Graphic::Requirement fixed;
  Warsaw::Graphic::Requirement flexible;
  flexible.defined = true;
  flexible.minimum = 2000.;
  flexible.natural = 2000.;
  flexible.maximum = 2000.;
  flexible.align = 0.;
  fixed.defined = true;
  fixed.minimum = 120.;
  fixed.natural = 120.;
  fixed.maximum = 120.;
  fixed.align = 0.;
  Warsaw::Graphic::Requisition req;
  if (axis == xaxis) req.x = flexible, req.y = fixed, req.z.defined = false;
  else               req.x = fixed, req.y = flexible, req.z.defined = false;
  Slider *slider = new Slider(value, axis, req);
  activate(slider);
  /*
   * the thumb
   */
  Graphic_var box = axis == xaxis ? layout->hbox() : layout->vbox();
  spec.brightness(0.5); spec._d(Warsaw::ToolKit::outset);
  Graphic_var quad = layout->fixed_size(Warsaw::Graphic::_nil(), 80., 80.);
  box->append_graphic(Graphic_var(tool->frame(quad, 20., spec, true)));
  box->append_graphic(Graphic_var(tool->frame(quad, 20., spec, true)));
  Controller_var thumb = tool->dragger(box, Command_var(slider->create_drag_command()));
  slider->init(thumb);
  /*
   * now put it into an inset
   */
  spec.brightness(0.5); spec._d(Warsaw::ToolKit::inset);
  Graphic_var inset = tool->frame(Graphic_var(slider->_this()), 20., spec, false);
  Controller_var root = tool->group(Graphic_var(layout->align_axis(inset, axis == xaxis ? yaxis : xaxis, 1.0)));
  /*
   * now wire up the control structure
   */
  root->append_controller(Controller_var(slider->_this()));
  return root._retn();
}

Controller_ptr WidgetKit::panner(BoundedRange_ptr x, BoundedRange_ptr y)
{
  Panner *panner = new Panner(x, y);
  activate(panner);
  Warsaw::ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(Warsaw::ToolKit::outset);
  Graphic_var outset = tool->frame(Warsaw::Graphic::_nil(), 20., spec, true);
  Controller_var thumb = tool->dragger(outset, Command_var(panner->create_drag_command()));
  panner->init(thumb);

  spec.brightness(0.5); spec._d(Warsaw::ToolKit::inset);
  Graphic_var fixed = layout->fixed_size(Graphic_var(panner->_this()), 1000., 1000.);
  Graphic_var inset = tool->frame(fixed, 20., spec, true);
  Controller_var root = tool->group(inset);
  /*
   * now wire up the control structure
   */
  root->append_controller(Controller_var(panner->_this()));
  return root._retn();
}

Controller_ptr WidgetKit::scrollbar(BoundedRange_ptr x, Axis a)
{
  Warsaw::ToolKit::FrameSpec spec;
  /*
   * the bar
   */
  Warsaw::Graphic::Requirement fixed;
  Warsaw::Graphic::Requirement flexible;
  flexible.defined = true;
  flexible.minimum = 0.;
  flexible.natural = 0.;
  flexible.maximum = layout->fill();
  flexible.align = 0.;
  fixed.defined = true;
  fixed.minimum = 120.;
  fixed.natural = 120.;
  fixed.maximum = 120.;
  fixed.align = 0;
  Warsaw::Graphic::Requisition req;
  if (a == xaxis) req.x = flexible, req.y = fixed, req.z.defined = false;
  else            req.x = fixed, req.y = flexible, req.z.defined = false;
  Scrollbar *scrollbar = new Scrollbar(x, a, req);
  activate(scrollbar);
  /*
   * the thumb
   */
  spec.brightness(0.5); spec._d(Warsaw::ToolKit::outset);
  Graphic_var outset = tool->frame(Warsaw::Graphic::_nil(), 20., spec, true);
  Controller_var thumb = tool->dragger(outset, Command_var(scrollbar->create_drag_command()));
  scrollbar->init(thumb);
  /*
   * the triangles
   */
  Warsaw::ToolKit::FrameSpec in, out;
  in.brightness(0.5); in._d(Warsaw::ToolKit::inset);
  out.brightness(0.5); out._d(Warsaw::ToolKit::outset);
  CommandImpl *backward = new Backward(x);
  activate(backward);
  Controller_var lower = tool->stepper(Graphic::_nil(), Command_var(backward->_this()));
  outset = layout->fixed_size(Graphic_var(tool->dynamic_triangle(Graphic::_nil(), 20.,
							         Warsaw::Controller::pressed, in, out, true,
							         a == xaxis ? Warsaw::ToolKit::left : Warsaw::ToolKit::up, lower)), 120., 120.);
  lower->body(outset);

  CommandImpl *forward = new Forward(x);
  activate(forward);
  Controller_var upper = tool->stepper(Graphic::_nil(), Command_var(forward->_this()));
  outset = layout->fixed_size(Graphic_var(tool->dynamic_triangle(Warsaw::Graphic::_nil(), 20.,
							         Warsaw::Controller::pressed, in, out, true,
							         a == xaxis ? Warsaw::ToolKit::right : Warsaw::ToolKit::down, upper)), 120., 120.);
  upper->body(outset);

  Graphic_var box = a == xaxis ? layout->hbox() : layout->vbox();
  box->append_graphic(lower);
  box->append_graphic(Controller_var(scrollbar->_this()));
  box->append_graphic(upper);
  /*
   * now put it into an inset
   */
  spec.brightness(0.5); spec._d(Warsaw::ToolKit::inset);
  Graphic_var inset = tool->frame(box, 20., spec, false);
  Controller_var root = tool->group(inset);
  /*
   * now wire up the control structure
   */
  root->append_controller(lower);
  root->append_controller(Controller_var(scrollbar->_this()));
  root->append_controller(upper);
  return root._retn();
}

Choice_ptr WidgetKit::toggle_choice()
{
  RefCount_var<Selection> selection = command->group(Selection::exclusive);
  Choice *choice = new ToggleChoice(selection, layout, tool, WidgetKit_var(_this()));
  activate(choice);
  choice->body(Graphic_var(layout->vbox()));
  return choice->_this();
}

Choice_ptr WidgetKit::checkbox_choice()
{
  RefCount_var<Selection> selection = command->group(0);
  Choice *choice = new CheckboxChoice(selection, layout, tool, WidgetKit_var(_this()));
  activate(choice);
  choice->body(Graphic_var(layout->vbox()));
  return choice->_this();
}

Choice_ptr WidgetKit::toolbar()
{
  RefCount_var<Selection> selection = command->group(Selection::exclusive|Selection::required);
  Choice *choice = new ToolChoice(selection, layout, tool, WidgetKit_var(_this()));
  activate(choice);
  choice->body(Graphic_var(layout->vbox()));
  return choice->_this();
}

Controller_ptr WidgetKit::terminal()
{
  Terminal *terminal = new Terminal(command);
  activate(terminal);
  Graphic_var view = text->terminal(StreamBuffer_var(terminal->output()));
  terminal->body(view);
  Controller_var input = tool->terminal(Graphic_var(terminal->_this()), StreamBuffer_var(terminal->input()));
//   input->appendController(Controller_var(terminal->_this()));
  return input._retn();
}

Controller_ptr WidgetKit::scrollable(Graphic_ptr g)
{
  Layout::Viewport_var viewport = layout->scrollable(g);
  Controller_var xscroller = scrollbar(viewport->adjustment(xaxis), xaxis);
  Controller_var yscroller = scrollbar(viewport->adjustment(yaxis), yaxis);
  Graphic_var hbox1 = layout->hbox();
  Warsaw::ToolKit::FrameSpec inset;
  inset.brightness(0.5); inset._d(Warsaw::ToolKit::inset);
  hbox1->append_graphic(Graphic_var(tool->frame(viewport, 20, inset, false)));
  hbox1->append_graphic(yscroller);
  Graphic_var hbox2 = layout->hbox();
  hbox2->append_graphic(xscroller);
  hbox2->append_graphic(Graphic_var(layout->fixed_size(Warsaw::Graphic::_nil(), 160., 160.)));
  Graphic_var vbox = layout->vbox();
  vbox->append_graphic(hbox1);
  vbox->append_graphic(hbox2);
  Controller_var group = tool->group(vbox);
  group->append_controller(xscroller);
  group->append_controller(yscroller);
  return group._retn();
}

};

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "Motif::WidgetKit", "style", "Motif"};
  return new KitFactoryImpl<Motif::WidgetKit> ("IDL:Warsaw/WidgetKit:1.0", properties, 2);
}
