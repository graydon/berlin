/*$Id: DrawingStateImpl.hh,v 1.1 2000/11/14 21:25:23 stefan Exp $
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
#ifndef _DrawingStateImpl_hh
#define _DrawingStateImpl_hh

#include <Prague/Sys/Thread.hh>
#include <Warsaw/config.hh>
#include <Warsaw/State.hh>
#include <Berlin/MonoGraphic.hh>

class DrawingStateImpl : public virtual POA_Warsaw::DrawingState,
			 public MonoGraphic
{
public:
  DrawingStateImpl();
  virtual ~DrawingStateImpl();

  virtual void traverse(Warsaw::Traversal_ptr);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void pick(Warsaw::PickTraversal_ptr);

  virtual Warsaw::Color foreground();
  virtual void foreground(const Warsaw::Color &);
  virtual Warsaw::Color lighting();
  virtual void lighting(const Warsaw::Color &);
  virtual Warsaw::Coord point_size();
  virtual void point_size(Warsaw::Coord);
  virtual Warsaw::Coord line_width();
  virtual void line_width(Warsaw::Coord);
  virtual Warsaw::DrawingKit::Endstyle line_endstyle();
  virtual void line_endstyle(Warsaw::DrawingKit::Endstyle);
  virtual Warsaw::DrawingKit::Fillstyle surface_fillstyle();
  virtual void surface_fillstyle(Warsaw::DrawingKit::Fillstyle);
  virtual Warsaw::Raster_ptr texture();
  virtual void texture(Warsaw::Raster_ptr);
private:
  enum {color = 0x1, light = 0x2, point = 0x4, line = 0x8, estyle = 0x10, fstyle = 0x20, tex = 0x40};
  long                          _enabled;
  Warsaw::Color                 _color;
  Warsaw::Color                 _light;
  Warsaw::Coord                 _point;
  Warsaw::Coord                 _line;
  Warsaw::DrawingKit::Endstyle  _estyle;
  Warsaw::DrawingKit::Fillstyle _fstyle;
  Warsaw::Raster_var            _texture;
};

#endif
