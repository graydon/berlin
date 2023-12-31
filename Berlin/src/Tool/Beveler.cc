/*$Id: Beveler.cc,v 1.10 2001/04/18 06:07:28 stefan Exp $
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
#include <Berlin/TransformImpl.hh>
#include <Berlin/Color.hh>
#include <Berlin/Logger.hh>
#include "Tool/Beveler.hh"

using namespace Warsaw;

namespace Beveler
{

void rect(DrawTraversal_ptr traversal, Coord thickness, const Color &medium, const Color &light, const Color &dark,
	  Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Coord lefti = left + thickness;
  Coord righti = right - thickness;
  Coord topi = top + thickness;
  Coord bottomi = bottom - thickness;
  DrawingKit_var drawing = traversal->drawing();
  drawing->save();
  Path path;
  path.length(7);
  Vertex lower = {left, top, 0.};
  Vertex upper = {right, bottom, 0.};

  if (fill)
    {
      drawing->foreground(medium);
      drawing->draw_rectangle(lower, upper);
    }
  drawing->surface_fillstyle(DrawingKit::solid);
  /*
   * light (left and top) edges
   */
  drawing->foreground(light);
  if (light.alpha == 1.) {
    // fast path
    lower.x = left; lower.y = top;
    upper.x = lefti; upper.y = bottom;
    drawing->draw_rectangle(lower,upper);    
    lower.x = left; lower.y = top;
    upper.x = right; upper.y = topi;
    drawing->draw_rectangle(lower,upper);  
  } else {
    path[0].x = left, path[0].y = top, path[0].z = 0.;
    path[1].x = left, path[1].y = bottom, path[1].z = 0.;
    path[2].x = lefti, path[2].y = bottomi, path[2].z = 0.;
    path[3].x = lefti, path[3].y = topi, path[3].z = 0.;
    path[4].x = righti, path[4].y = topi, path[4].z = 0.;
    path[5].x = right, path[5].y = top, path[5].z = 0.;
    path[6].x = left, path[6].y = top, path[6].z = 0.;
    drawing->draw_path(path);
  }

  /*
   * right and top (dark) edges
   */
  drawing->foreground(dark);
  path[0].x = right, path[0].y = top, path[0].z = 0.;
  path[1].x = right, path[1].y = bottom, path[1].z = 0.;
  path[2].x = left, path[2].y = bottom, path[2].z = 0.;
  path[3].x = lefti, path[3].y = bottomi, path[3].z = 0.;
  path[4].x = righti, path[4].y = bottomi, path[4].z = 0.;
  path[5].x = righti, path[5].y = topi, path[5].z = 0.;
  path[6].x = right, path[6].y = top, path[6].z = 0.;
  drawing->draw_path(path);
  drawing->restore();
}

void leftArrow(DrawTraversal_ptr traversal, Coord thickness, const Color &medium, const Color &light, const Color &dark,
	       Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Vertex center = {(right + left)/2, (bottom + top)/2, 0.};
  Vertex length = {(right - left), (bottom - top), 0.};

  Coord slope = length.y / length.x / 2;
  Coord dx = thickness * std::sqrt(1/(slope * slope) + 1.0);
  Coord dy = thickness * (1 + slope);

  DrawingKit_var drawing = traversal->drawing();
  drawing->save();
  
  Path path;
  if (fill)
    {
      path.length(4);
      drawing->foreground(medium);
      path[0].x = right, path[0].y = bottom, path[0].z = 0;
      path[1].x = right, path[1].y = top, path[1].z = 0;
      path[2].x = left, path[2].y = center.y, path[2].z = 0;
      path[3] = path[0];
      drawing->draw_path(path);
    }
  path.length(5);
  /*
   * top
   */
  drawing->foreground(dark);
  path[0].x = left, path[0].y = center.y, path[0].z = 0.;
  path[1].x = left + dx, path[1].y = center.y, path[1].z = 0.;
  path[2].x = right - thickness, path[2].y = top + dy, path[2].z = 0;
  path[3].x = right, path[3].y = top, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * right
   */
  drawing->foreground(light);
  path[0].x = right, path[0].y = bottom, path[0].z = 0;
  path[1].x = right, path[1].y = top, path[1].z = 0;
  path[2].x = right - thickness, path[2].y = top + dy, path[2].z = 0;
  path[3].x = right - thickness, path[3].y = bottom - dy, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * bottom
   */
  path[0].x = left, path[0].y = center.y, path[0].z = 0;
  path[1].x = right, path[1].y = bottom, path[1].z = 0;
  path[2].x = right - thickness, path[2].y = bottom - dy, path[2].z = 0;
  path[3].x = left + dx, path[3].y = center.y, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  drawing->restore();
}

void rightArrow(DrawTraversal_ptr traversal, Coord thickness, const Color &medium, const Color &light, const Color &dark,
		Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Vertex center = {(right + left)/2, (bottom + top)/2, 0.};
  Vertex length = {(right - left), (bottom - top), 0.};
  Coord slope = length.y / length.x / 2;
  Coord dx = thickness * std::sqrt(1/(slope * slope) + 1.0);
  Coord dy = thickness * (1 + slope);

  DrawingKit_var drawing = traversal->drawing();
  drawing->save();

  Path path;
  if (fill)
    {
      path.length(4);
      drawing->foreground(medium);
      path[0].x = right, path[0].y = center.y, path[0].z = 0;
      path[1].x = left, path[1].y = top, path[1].z = 0;
      path[2].x = left, path[2].y = bottom, path[2].z = 0;
      path[3] = path[0];
      drawing->draw_path(path);
    }
  path.length(5);
  /*
   * bottom
   */
  drawing->foreground(light);
  path[0].x = left, path[0].y = bottom, path[0].z = 0;
  path[1].x = right, path[1].y = center.y, path[1].z = 0.;
  path[2].x = right - dx, path[2].y = center.y, path[2].z = 0.;
  path[3].x = left + thickness, path[3].y = bottom - dy, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * left
   */
  drawing->foreground(dark);
  path[0].x = left, path[0].y = top, path[0].z = 0;
  path[1].x = left, path[1].y = bottom, path[1].z = 0;
  path[2].x = left + thickness, path[2].y = bottom - dy, path[2].z = 0;
  path[3].x = left + thickness, path[3].y = top + dy, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * top
   */
  path[0].x = right, path[0].y = center.y, path[0].z = 0;
  path[1].x = left, path[1].y = top, path[1].z = 0;
  path[2].x = left + thickness, path[2].y = top + dy, path[2].z = 0;
  path[3].x = right - dx, path[3].y = center.y, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  drawing->restore();
}

void upArrow(DrawTraversal_ptr traversal, Coord thickness, const Color &medium, const Color &light, const Color &dark,
	     Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Vertex center = {(right + left)/2, (bottom + top)/2, 0.};
  Vertex length = {(right - left), (bottom - top), 0.};
  Coord slope = 2 * length.y / length.x;
  Coord dy = thickness * std::sqrt(slope * slope + 1.0);
  Coord dx = thickness * (1 + 1 / slope);

  DrawingKit_var drawing = traversal->drawing();
  drawing->save();

  Path path;
  if (fill)
    {
      path.length(4);
      drawing->foreground(medium);
      path[0].x = left, path[0].y = bottom, path[0].z = 0;
      path[1].x = right, path[1].y = bottom, path[1].z = 0;
      path[2].x = center.x, path[2].y = top, path[2].z = 0;
      path[3] = path[0];
      drawing->draw_path(path);
    }
  path.length(5);
  /*
   * left
   */
  drawing->foreground(light);
  path[0].x = center.x, path[0].y = top, path[0].z = 0;
  path[1].x = left, path[1].y = bottom, path[1].z = 0;
  path[2].x = left + dx, path[2].y = bottom - thickness, path[2].z = 0;
  path[3].x = center.x, path[3].y = top + dy, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * right
   */
  drawing->foreground(dark);
  path[0].x = center.x, path[0].y = top, path[0].z = 0;
  path[1].x = center.x, path[1].y = top + dy, path[1].z = 0;
  path[2].x = right - dx, path[2].y = bottom - thickness, path[2].z = 0;
  path[3].x = right, path[3].y = bottom, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * bottom
   */
  path[0].x = left, path[0].y = bottom, path[0].z = 0;
  path[1].x = right, path[1].y = bottom, path[1].z = 0;
  path[2].x = right - dx, path[2].y = bottom - thickness, path[2].z = 0;
  path[3].x = left + dx, path[3].y = bottom - thickness, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  drawing->restore();
}

void downArrow(DrawTraversal_ptr traversal, Coord thickness, const Color &medium, const Color &light, const Color &dark,
	       Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Vertex center = {(right + left)/2, (bottom + top)/2, 0.};
  Vertex length = {(right - left), (bottom - top), 0.};
  Coord slope = 2 * length.y / length.x;
  Coord dy = thickness * std::sqrt(slope * slope + 1.0);
  Coord dx = thickness * (1 + 1 / slope);

  DrawingKit_var drawing = traversal->drawing();
  drawing->save();

  Path path;
  if (fill)
    {
      path.length(4);
      drawing->foreground(medium);
      path[0].x = right, path[0].y = top, path[0].z = 0;
      path[1].x = left, path[1].y = top, path[1].z = 0;
      path[2].x = center.x, path[2].y = bottom, path[2].z = 0;
      path[3] = path[0];
      drawing->draw_path(path);
    }
  path.length(5);
  /*
   * right
   */
  drawing->foreground(light);
  path[0].x = right, path[0].y = top, path[0].z = 0;
  path[1].x = right - dx, path[1].y = top + thickness, path[1].z = 0;
  path[2].x = center.x, path[2].y = bottom - dy, path[2].z = 0;
  path[3].x = center.x, path[3].y = bottom, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * top
   */
  drawing->foreground(dark);
  path[0].x = right, path[0].y = top, path[0].z = 0;
  path[1].x = left, path[1].y = top, path[1].z = 0;
  path[2].x = left + dx, path[2].y = top + thickness, path[2].z = 0;
  path[3].x = right - dx, path[3].y = top + thickness, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * left
   */
  path[0].x = left, path[0].y = top, path[0].z = 0;
  path[1].x = center.x, path[1].y = bottom, path[1].z = 0;
  path[2].x = center.x, path[2].y = bottom - dy, path[2].z = 0;
  path[3].x = left + dx, path[3].y = top + thickness, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  drawing->restore();
}

void diamond(DrawTraversal_ptr traversal, Coord thickness, const Color &medium, const Color &light, const Color &dark,
	     Coord left, Coord right, Coord top, Coord bottom, bool fill)
{
  Vertex center = {(right + left)/2, (bottom + top)/2, 0.};
  Vertex length = {(right - left), (bottom - top), 0.};
  Coord dx = thickness * std::sqrt(length.x * length.x / (length.y * length.y) + 1.0);
  Coord dy = thickness * std::sqrt(length.y * length.y / (length.x * length.x) + 1.0);

  DrawingKit_var drawing = traversal->drawing();
  drawing->save();

  Path path;
  path.length(5);
  if (fill)
    {
      drawing->foreground(medium);
      path[0].x = center.x, path[0].y = top, path[0].z = 0;
      path[1].x = left, path[1].y = center.y, path[1].z = 0;
      path[2].x = center.x, path[2].y = bottom, path[2].z = 0;
      path[3].x = right, path[3].y = center.y, path[3].z = 0;
      path[4] = path[0];
      drawing->draw_path(path);
    }
  /*
   * top left
   */
  drawing->foreground(dark);
  path[0].x = center.x, path[0].y = top, path[0].z = 0;
  path[1].x = left, path[1].y = center.y, path[1].z = 0;
  path[2].x = left + dx, path[2].y = center.y, path[2].z = 0;
  path[3].x = center.x, path[3].y = top + dy, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * top right
   */
  path[0].x = center.x, path[0].y = top, path[0].z = 0;
  path[1].x = center.x, path[1].y = top + dy, path[1].z = 0;
  path[2].x = right - dx, path[2].y = center.y, path[2].z = 0;
  path[3].x = right, path[3].y = center.y, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * bottom left
   */
  drawing->foreground(light);
  path[0].x = left, path[0].y = center.y, path[0].z = 0;
  path[1].x = center.x, path[1].y = bottom, path[1].z = 0;
  path[2].x = center.x, path[2].y = bottom - dy, path[2].z = 0;
  path[3].x = left + dx, path[3].y = center.y, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  /*
   * bottom right
   */
  path[0].x = center.x, path[0].y = bottom, path[0].z = 0;
  path[1].x = right, path[1].y = center.y, path[1].z = 0;
  path[2].x = right - dx, path[2].y = center.y, path[2].z = 0;
  path[3].x = center.x, path[3].y = bottom - dy, path[3].z = 0;
  path[4] = path[0];
  drawing->draw_path(path);
  drawing->restore();
}

void circle(DrawTraversal_ptr, Coord thickness, const Color &medium, const Color &light, const Color &dark,
	    Coord left, Coord bottom, Coord right, Coord top, bool fill)
{
}

};
