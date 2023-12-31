/*$Id: Bevel.cc,v 1.15 1999/11/10 21:57:36 stefan Exp $
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

#include "Widget/Bevel.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/Pencil.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Berlin/Color.hh"
#include "Berlin/Logger.hh"

Bevel::Bevel(Coord t, Alignment x, Alignment y, bool h, bool v)
  : thickness(t), xalign(x), yalign(y), hmargin(h), vmargin(v)
{
  allocation = new RegionImpl;
  allocation->_obj_is_ready(_boa());
}

Bevel::~Bevel()
{
  allocation->_dispose();
}

void Bevel::request(Requisition &requisition)
{
  MonoGraphic::request(requisition);
  if (hmargin || vmargin)
    {
      Coord t = thickness + thickness;
      if (hmargin && requisition.x.defined)
	{
	  requisition.x.natural += t;
	  requisition.x.maximum += t;
	  requisition.x.minimum += t;
// 	  requisition.x.align = ((requisition.x.natural - 2*t)*requisition.x.align + t)/requisition.x.natural;
	}
      if (vmargin && requisition.y.defined)
	{
	  requisition.y.natural += t;
	  requisition.y.maximum += t;
	  requisition.y.minimum += t;
// 	  requisition.y.align = ((requisition.y.natural - 2*t)*requisition.y.align + t)/requisition.y.natural;
	}
    }
//   cout << "Bevel::request " << requisition.x.align << ' ' << requisition.y.align << endl;
}

void Bevel::traverse(Traversal_ptr traversal)
{
  SectionLog section("Bevel::traverse");
  /*
   * cheap and dirty cull test -stefan
   */
//   if (!traversal->intersectsAllocation()) return;
  traversal->visit(Graphic_var(_this()));
  Graphic_var child = body();
  if (!CORBA::is_nil(child))
    {
      if (hmargin || vmargin)
	{
	  Allocation::Info info;
	  Impl_var<RegionImpl> allocation(new RegionImpl(Region_var(traversal->allocation())));
	  info.allocation = allocation->_this();
	  Impl_var<TransformImpl> tx(new TransformImpl);
	  info.transformation = tx->_this();
	  allocate(0, info);
	  traversal->traverseChild(child, 0, info.allocation, info.transformation);
	}
      else MonoGraphic::traverse(traversal);
    }
}

void Bevel::extension(const Allocation::Info &info, Region_ptr region)
{
  if (!CORBA::is_nil(info.allocation)) defaultExtension(info, region);
  else MonoGraphic::extension(info, region);
}

void Bevel::allocate(Tag, const Allocation::Info &info)
{
  Requisition req;
  GraphicImpl::initRequisition(req);
  MonoGraphic::request(req);
  allocation->valid = true;
  Region::Allotment a;
  /*
   * same as Placement::normalTransform...
   */
  Vertex o;
  Impl_var<RegionImpl> region(new RegionImpl(info.allocation));
  region->normalize(o);
  info.transformation->translate(o);
  info.allocation->copy(Region_var(region->_this()));
  
  Vertex delta;
  info.allocation->span(xaxis, a);
  allocateSpan(req.x, a, hmargin ? thickness : 0, xalign);
  allocation->lower.x = -(a.end - a.begin) * a.align;
  allocation->upper.x = allocation->lower.x + (a.end - a.begin);
  allocation->xalign = a.align;
  delta.x = a.begin - allocation->lower.x;

  info.allocation->span(yaxis, a);
  allocateSpan(req.y, a, vmargin ? thickness : 0, yalign);
  allocation->lower.y = -(a.end - a.begin) * a.align;
  allocation->upper.y = allocation->lower.y + (a.end - a.begin);
  allocation->yalign = a.align;
  delta.y = a.begin - allocation->lower.y;
  delta.z = 0;
  
  info.allocation->copy(Region_var(allocation->_this()));
  info.transformation->translate(delta);
}

void Bevel::allocateSpan(const Requirement &r, Region::Allotment &a, Coord margin, Alignment align)
{
//   a.align = ((a.end - a.begin)*a.align - margin)/(a.end - a.begin - 2*margin);
  a.begin += margin;
  a.end -= margin;
}

void Bevel::rect(DrawTraversal_ptr dt, Coord thickness, const Color &medium, const Color &light, const Color &dark,
		 Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Coord lefti = left + thickness;
  Coord righti = right - thickness;
  Coord topi = top + thickness;
  Coord bottomi = bottom - thickness;
  DrawingKit_var dk = dt->kit();
  Transform_var tx = dt->transformation();
  Style::Spec style;
  style.length(1);
  style[0].a = Style::fillcolor;
  Pencil_var pen;
  Path path;
  path.p.length(5);
  if (fill)
    {
      style[0].val <<= medium;
      pen = dk->getPencil(style);
      path.p[0].x = left, path.p[0].y = top, path.p[0].z = 0.;
      path.p[1].x = right, path.p[1].y = top, path.p[1].z = 0.;
      path.p[2].x = right, path.p[2].y = bottom, path.p[2].z = 0.;
      path.p[3].x = left, path.p[3].y = bottom, path.p[3].z = 0.;
      path.p[4].x = left, path.p[4].y = top, path.p[4].z = 0.;
      pen->drawPath(path);
    }
  /*
   * light edges
   */
  style[0].val <<= light;
  pen = dk->getPencil(style);
  /*
   * left edge
   */
  path.p[0].x = left, path.p[0].y = top, path.p[0].z = 0.;
  path.p[1].x = lefti, path.p[1].y = topi, path.p[1].z = 0.;
  path.p[2].x = lefti, path.p[2].y = bottomi, path.p[2].z = 0.;
  path.p[3].x = left, path.p[3].y = bottom, path.p[3].z = 0.;
  path.p[4].x = left, path.p[4].y = top, path.p[4].z = 0.;
  pen->drawPath(path);
  /*
   * top edge
   */
  path.p[0].x = left, path.p[0].y = top, path.p[0].z = 0.;
  path.p[1].x = right, path.p[1].y = top, path.p[1].z = 0.;
  path.p[2].x = righti, path.p[2].y = topi, path.p[2].z = 0.;
  path.p[3].x = lefti, path.p[3].y = topi, path.p[3].z = 0.;
  path.p[4].x = left, path.p[4].y = top, path.p[4].z = 0.;
  pen->drawPath(path);
  /*
   * dark edges
   */
  style[0].val <<= dark;
  pen = dk->getPencil(style);
  /*
   * right edge
   */
  path.p[0].x = right, path.p[0].y = top, path.p[0].z = 0.;
  path.p[1].x = right, path.p[1].y = bottom, path.p[1].z = 0.;
  path.p[2].x = righti, path.p[2].y = bottomi, path.p[2].z = 0.;
  path.p[3].x = righti, path.p[3].y = topi, path.p[3].z = 0.;
  path.p[4].x = right, path.p[4].y = top, path.p[4].z = 0.;
  pen->drawPath(path);
  /*
   * bottom edge
   */
  path.p[0].x = left, path.p[0].y = bottom, path.p[0].z = 0.;
  path.p[1].x = lefti, path.p[1].y = bottomi, path.p[1].z = 0.;
  path.p[2].x = righti, path.p[2].y = bottomi, path.p[2].z = 0.;
  path.p[3].x = right, path.p[3].y = bottom, path.p[3].z = 0.;
  path.p[4].x = left, path.p[4].y = bottom, path.p[4].z = 0.;
  pen->drawPath(path);
}

void Bevel::leftArrow(DrawTraversal_ptr dt, Coord thickness, const Color &light, const Color &medium, const Color &dark,
		      Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
}

void Bevel::rightArrow(DrawTraversal_ptr dt, Coord thickness, const Color &light, const Color &medium, const Color &dark,
		       Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
}

void Bevel::upArrow(DrawTraversal_ptr, Coord thickness, const Color &light, const Color &medium, const Color &dark,
		    Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
}

void Bevel::downArrow(DrawTraversal_ptr, Coord thickness, const Color &light, const Color &medium, const Color &dark,
		      Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
}

void Bevel::diamond(DrawTraversal_ptr, Coord thickness, const Color &light, const Color &medium, const Color &dark,
		    Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
}

void Bevel::circle(DrawTraversal_ptr, Coord thickness, const Color &light, const Color &medium, const Color &dark,
		   Coord left, Coord bottom, Coord right, Coord top, bool fill)
{
}
