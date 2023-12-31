/*$Id: GLPencil.hh,v 1.5 1999/04/14 02:51:26 gray Exp $
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
#ifndef _GLPencil_hh
#define _GLPencil_hh

#include "Warsaw/config.hh"
#include "Warsaw/Pencil.hh"
#include "Warsaw/Style.hh"
#include "Warsaw/Types.hh"
#include "Drawing/openGL/GLDrawable.hh"

class GLPencil : implements(Pencil)
{
public:
  GLPencil(const Style::Spec &, GLDrawable *);

  void drawPath(const Path &);
  void drawPatch(const Patch &);
  void drawRect(const Vertex &lower, const Vertex &upper);
  void drawEllipse(const Vertex &lower, const Vertex &upper);

protected:
  GLDrawable *myDrawable;
  GLfloat myLineColor[4];
  GLfloat myFillColor[4];
  Coord myThickness;
  Style::Fillmode myFillMode;
};

#endif /* _GLPencil_hh */
