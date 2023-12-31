/*$Id: Diamond.hh,v 1.2 2000/08/31 18:52:32 stefan Exp $
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
#ifndef _Diamond_hh
#define _Diamond_hh

#include <Warsaw/config.hh>
#include <Warsaw/ToolKit.hh>
#include "Tool/Frame.hh"

class InvisibleDiamond : public Frame::Renderer
{
public:
  InvisibleDiamond(Warsaw::Coord t, bool f) : Frame::Renderer(t, f) {}
  virtual void draw(Warsaw::DrawTraversal_ptr);
};

class BeveledDiamond : public Bevel
{
public:
  BeveledDiamond(Warsaw::Coord t, type s, Warsaw::Coord b, bool f) : Bevel(t, s, b, f) {}
  virtual void draw(Warsaw::DrawTraversal_ptr);
};

class ColoredDiamond : public ColoredFrame
{
public:
  ColoredDiamond(Warsaw::Coord t, const Warsaw::Color &c, bool f) : ColoredFrame(t, c, f) {}
  virtual void draw(Warsaw::DrawTraversal_ptr);
};

#endif
