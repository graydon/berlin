/*$Id: Triangle.cc,v 1.8 2001/04/18 06:07:28 stefan Exp $
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
#include <Berlin/Logger.hh>
#include <Berlin/Color.hh>
#include "Tool/Triangle.hh"
#include "Tool/Beveler.hh"

using namespace Warsaw;

void InvisibleTriangle::draw(DrawTraversal_ptr traversal)
{
  Region_var allocation = traversal->current_allocation();
  Vertex l, u;
  allocation->bounds(l, u);
  DrawingKit_var drawing = traversal->drawing();
  DrawingKit::Fillstyle style = drawing->surface_fillstyle();
  if (style != DrawingKit::outlined && _fill)
    {
      Path path;
      path.length(4);
      switch (_direction)
	{
	case ToolKit::left:
	  path[0].x = l.x, path[0].y = (l.y + u.y)/2, path[0].z = 0;
	  path[1].x = u.x, path[1].y = l.y, path[1].z = 0;
	  path[2].x = u.x, path[2].y = u.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	case ToolKit::right:
	  path[0].x = l.x, path[0].y = l.y, path[0].z = 0;
	  path[1].x = u.x, path[1].y = (l.y + u.y)/2, path[1].z = 0;
	  path[2].x = l.x, path[2].y = u.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	case ToolKit::up:
	  path[0].x = l.x, path[0].y = u.y, path[0].z = 0;
	  path[1].x = (l.x + u.x)/2, path[1].y = l.y, path[1].z = 0;
	  path[2].x = u.x, path[2].y = u.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	case ToolKit::down:
	  path[0].x = l.x, path[0].y = l.y, path[0].z = 0;
	  path[1].x = (l.x + u.x)/2, path[1].y = u.y, path[1].z = 0;
	  path[2].x = l.x, path[2].y = l.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	}
      drawing->draw_path(path);
    }
  else if (_fill)
    {
      drawing->save();
      drawing->surface_fillstyle(DrawingKit::solid);
      Path path;
      path.length(4);
      switch (_direction)
	{
	case ToolKit::left:
	  path[0].x = l.x, path[0].y = (l.y + u.y)/2, path[0].z = 0;
	  path[1].x = u.x, path[1].y = l.y, path[1].z = 0;
	  path[2].x = u.x, path[2].y = u.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	case ToolKit::right:
	  path[0].x = l.x, path[0].y = l.y, path[0].z = 0;
	  path[1].x = u.x, path[1].y = (l.y + u.y)/2, path[1].z = 0;
	  path[2].x = l.x, path[2].y = u.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	case ToolKit::up:
	  path[0].x = l.x, path[0].y = u.y, path[0].z = 0;
	  path[1].x = (l.x + u.x)/2, path[1].y = l.y, path[1].z = 0;
	  path[2].x = u.x, path[2].y = u.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	case ToolKit::down:
	  path[0].x = l.x, path[0].y = l.y, path[0].z = 0;
	  path[1].x = (l.x + u.x)/2, path[1].y = u.y, path[1].z = 0;
	  path[2].x = l.x, path[2].y = l.y, path[2].z = 0;
	  path[3] = path[0];
	  break;
	}      
      drawing->draw_path(path);
      drawing->restore();
    }
  else
    {
      Color color = drawing->foreground();
      switch (_direction)
	{
	case ToolKit::left:
	  Beveler::leftArrow(traversal, _thickness, color, color, color, l.x, u.x, l.y, u.y, _fill);
	  break;
	case ToolKit::right:
	  break;
	  Beveler::leftArrow(traversal, _thickness, color, color, color, l.x, u.x, l.y, u.y, _fill);
	case ToolKit::up:
	  break;
	  Beveler::leftArrow(traversal, _thickness, color, color, color, l.x, u.x, l.y, u.y, _fill);
	case ToolKit::down:
	  break;
	  Beveler::leftArrow(traversal, _thickness, color, color, color, l.x, u.x, l.y, u.y, _fill);
	}
    }
}

void BeveledTriangle::draw(DrawTraversal_ptr traversal)
{
  Region_var allocation = traversal->current_allocation();
  Vertex u, l;
  allocation->bounds(l, u);
  DrawingKit_var drawing = traversal->drawing();
  Color color = drawing->foreground();
  Color light = brightness(color,-_bright);
  Color dark  = brightness(color, _bright);

  drawing->save();
  if (drawing->surface_fillstyle() == DrawingKit::outlined)
    drawing->surface_fillstyle(DrawingKit::solid);

  switch (_direction)
    {
    case ToolKit::left:
      switch (_style)
	{
	case inset:
	  Beveler::leftArrow(traversal, _thickness, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	case outset:
	  Beveler::leftArrow(traversal, _thickness, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	case convex:
	  Beveler::leftArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::leftArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	case concav:
	  Beveler::leftArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::leftArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	}
      break;
    case ToolKit::right:
      switch (_style)
	{
	case inset:
	  Beveler::rightArrow(traversal, _thickness, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	case outset:
	  Beveler::rightArrow(traversal, _thickness, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	case convex:
	  Beveler::rightArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::rightArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, _fill);
      break;
	case concav:
	  Beveler::rightArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::rightArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	}
      break;
    case ToolKit::up:
      switch (_style)
	{
	case inset:
	  Beveler::upArrow(traversal, _thickness, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	case outset:
	  Beveler::upArrow(traversal, _thickness, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	case convex:
	  Beveler::upArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::upArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	case concav:
	  Beveler::upArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::upArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	}
      break;
    case ToolKit::down:
      switch (_style)
	{
	case inset:
	  Beveler::downArrow(traversal, _thickness, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	case outset:
	  Beveler::downArrow(traversal, _thickness, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	case convex:
	  Beveler::downArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::downArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, _fill);
	  break;
	case concav:
	  Beveler::downArrow(traversal, _thickness/2, color, dark, light, l.x, u.x, l.y, u.y, false);
	  l.x += _thickness/2, u.x -= _thickness/2, l.y += _thickness/2, u.y -= _thickness/2;
	  Beveler::downArrow(traversal, _thickness/2, color, light, dark, l.x, u.x, l.y, u.y, _fill);
	  break;
	}
      break;
    }
  drawing->restore();
}

void ColoredTriangle::draw(DrawTraversal_ptr traversal)
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

