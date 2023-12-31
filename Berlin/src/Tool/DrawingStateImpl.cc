/*$Id: DrawingStateImpl.cc,v 1.2 2000/12/21 21:05:44 stefan Exp $
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

#include <Warsaw/config.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/PickTraversal.hh>
#include <Warsaw/Raster.hh>
#include <Warsaw/IO.hh>
#include "Tool/DrawingStateImpl.hh"

using namespace Prague;
using namespace Warsaw;

DrawingStateImpl::DrawingStateImpl() : _enabled(0) {}
DrawingStateImpl::~DrawingStateImpl() {}

void DrawingStateImpl::traverse(Traversal_ptr traversal)
{
  Trace trace("DrawingStateImpl::traverse");
  traversal->visit(Graphic_var(_this()));
}

void DrawingStateImpl::draw(DrawTraversal_ptr traversal)
{
  Trace trace("DrawingStateImpl::draw");
  DrawingKit_var drawing = traversal->drawing();
  drawing->save();
  if (_enabled & color) drawing->foreground(_color);
  if (_enabled & light) drawing->lighting(_light);
  if (_enabled & point) drawing->point_size(_point);
  if (_enabled & line) drawing->line_width(_line);
  if (_enabled & estyle) drawing->line_endstyle(_estyle);
  if (_enabled & fstyle) drawing->surface_fillstyle(_fstyle);
  if (_enabled & tex) drawing->texture(_texture);
  MonoGraphic::traverse(traversal);
  drawing->restore();
}

void DrawingStateImpl::pick(PickTraversal_ptr traversal)
{
  Trace trace("DrawingStateImpl::pick");
  MonoGraphic::traverse(traversal);
}

Color DrawingStateImpl::foreground() { return _color;}
void DrawingStateImpl::foreground(const Color &c) { _color = c; _enabled |= color;}
Color DrawingStateImpl::lighting() { return _light;}
void DrawingStateImpl::lighting(const Color &l) { _light = l; _enabled |= light;}
Coord DrawingStateImpl::point_size() { return _point;}
void DrawingStateImpl::point_size(Coord p) { _point = p; _enabled |= point;}
Coord DrawingStateImpl::line_width() { return _line;}
void DrawingStateImpl::line_width(Coord l) { _line = l; _enabled |= line;}
DrawingKit::Endstyle DrawingStateImpl::line_endstyle() { return _estyle;}
void DrawingStateImpl::line_endstyle(DrawingKit::Endstyle s)  { _estyle = s; _enabled |= estyle;}
DrawingKit::Fillstyle DrawingStateImpl::surface_fillstyle() { return _fstyle;}
void DrawingStateImpl::surface_fillstyle(DrawingKit::Fillstyle s) { _fstyle = s; _enabled |= fstyle;}
Raster_ptr DrawingStateImpl::texture() { return Raster::_duplicate(_texture);}
void DrawingStateImpl::texture(Raster_ptr t) { _texture = Raster::_duplicate(t); _enabled |= tex;}
