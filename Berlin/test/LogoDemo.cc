/*$Id: LogoDemo.cc,v 1.2 1999/11/30 20:03:51 tobias Exp $
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

#include "LogoDemo.hh"
#include "Warsaw/config.hh"
#include "Warsaw/Command.hh"
#include "Warsaw/Desktop.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/ImplVar.hh"

Rotator::Rotator(BoundedValue_ptr v, Transformator_ptr t, Graphic_ptr p, Coord d)
  : value(BoundedValue::_duplicate(v)),
    transformator(Transformator::_duplicate(t)),
    parent(Graphic::_duplicate(p)),
    zdegree(d)
{
  CORBA::Any dummy;
  update(Subject_var(Subject::_nil()), dummy);
}

void Rotator::update(Subject_ptr, const CORBA::Any &)
{
  Coord ydegree = value->value();
  Transform_var tx = transformator->transformation();
  tx->loadIdentity();
  tx->rotate(ydegree, yaxis);
  tx->rotate(zdegree, zaxis);
  parent->needRedraw();
}

LogoDemo::LogoDemo(Application *a)
  : Demo(a),
    tx1(new TransformImpl),
    tx2(new TransformImpl),
    tx3(new TransformImpl)
{
  LayoutKit_var layout = application->layout();
  WidgetKit_var widget = application->widget();
  FigureKit_var figure = application->figure();
  CommandKit_var command = application->command();
  
  bv1 = widget->bvalue(0., 360., 0., 5., 5.);
  bv2 = widget->bvalue(0., 360., 0., 5., 5.);
  bv3 = widget->bvalue(0., 360., 0., 5., 5.);
  
  tx1->rotate(10., zaxis);
  tx2->rotate(-10., zaxis);
  tx3->rotate(-20., zaxis);
  
  Coord a = 200.;
  Vertex offset;
  offset.x = -a/2., offset.y = -3./2.*a, offset.z = 0.;
  Figure::Vertices path; path.length(3);
  path[0].x = a/2 + offset.x, path[0].y = + offset.y, path[0].z = offset.z;
  path[1].x = a + offset.x, path[1].y = 0.866*a + offset.y, path[1].z = offset.z;
  path[2].x = offset.x, path[2].y = 0.866*a + offset.y, path[2].z = offset.z;
  
  Color red = {1.0, 0.5, 0.5, 0.5};
  Color green = {0.5, 1.0, 0.5, 0.5};
  Color blue = {0.5, 0.5, 1.0, 0.5};
  Color white = {1.0, 1.0, 1.0, 1.0};
  
  Figures::Path_var triangle1 = figure->polygon(path);
  triangle1->type(Figure::fill);
  triangle1->foreground(green);
  Figures::Path_var triangle2 = figure->polygon(path);
  triangle2->type(Figure::fill);
  triangle2->foreground(blue);
  Figures::Path_var triangle3 = figure->polygon(path);
  triangle3->type(Figure::fill);
  triangle3->foreground(red);
  Transformator_var transformator1 = figure->projection(triangle1);
  Transformator_var transformator2 = figure->projection(triangle2);
  Transformator_var transformator3 = figure->projection(triangle3);
  
  Graphic_var group = figure->group();
  
  rotator1 = new Rotator(bv1, transformator1, group, 20.);
  bv1->attach(Observer_var(rotator1->_this()));
  rotator2 = new Rotator(bv2, transformator2, group, 10.);
  bv2->attach(Observer_var(rotator2->_this()));
  rotator3 = new Rotator(bv3, transformator3, group, -10.);
  bv3->attach(Observer_var(rotator3->_this()));
  
  Graphic_var root = figure->root(group);
  group->append(transformator1);
  group->append(transformator2);
  group->append(transformator3);
  
  Graphic_var box = layout->vbox();
  box->append(Graphic_var(layout->align(group, 0., 0.)));
  box->append(Graphic_var(makeController(bv1, green)));
  box->append(Graphic_var(makeController(bv2, blue)));
  box->append(Graphic_var(makeController(bv3, red)));
  application->append(Graphic_var(widget->outset(box, white, true)),
		      Unicode::String("MVC demo"));
}

Graphic_ptr LogoDemo::makeController(BoundedValue_ptr value, const Color &color)
{
  WidgetKit_var widget = application->widget();
  LayoutKit_var layout = application->layout();
  Graphic_var gauge = widget->gauge(color, value);
  Forward *forward = new Forward(value);
  forward->_obj_is_ready(CORBA::BOA::getBOA());
  Backward *backward = new Backward(value);
  backward->_obj_is_ready(CORBA::BOA::getBOA());
  Graphic_var rectangle = layout->fixed(Graphic_var(Graphic::_nil()), 20, 20);
  Controller_var begin = widget->stepper(Graphic_var(widget->inset(rectangle, color, true)), Command_var(backward->_this()));
  Controller_var end = widget->stepper(Graphic_var(widget->inset(rectangle, color, true)), Command_var(forward->_this()));
  Graphic_var box = layout->hbox();
  box->append(begin);
  box->append(gauge);
  box->append(end);
  return Graphic::_duplicate(box);
}
