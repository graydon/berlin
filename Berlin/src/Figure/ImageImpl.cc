/*$Id: ImageImpl.cc,v 1.4 1999/11/10 21:57:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
 * Copyright (C) 1999 Stefan Seefeld <seefelds@MAGELLAN.UMontreal.CA>
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
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/DrawingKit.hh"
#include "Berlin/Logger.hh"
#include "Figure/ImageImpl.hh"

ImageImpl::ImageImpl(Raster_ptr r)
  : raster(Raster::_duplicate(r))
{
  Raster::Info info = raster->header();
  width = info.width;
  height = info.height;
}
ImageImpl::~ImageImpl() {}

void ImageImpl::request(Requisition &r)
{
  r.x.defined = true;
  r.x.natural = r.x.maximum = r.x.minimum = width;
  r.x.align = 0.;
  r.y.defined = true;
  r.y.natural = r.y.maximum = r.y.minimum = height;
  r.y.align = 0.;
}

void ImageImpl::draw(DrawTraversal_ptr traversal)
{
  SectionLog section("Image::draw");
  if (!traversal->intersectsAllocation()) return;
  DrawingKit_var dk = traversal->kit();
  dk->image(raster);
}

