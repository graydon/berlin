/*$Id: ImageKitImpl.cc,v 1.12 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent A. Fulgham <bfulgham@debian.org>
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#include <Berlin/ImplVar.hh>
#include "Image/ImageKitImpl.hh"
#include "Image/RasterImpl.hh"
#include <Prague/Sys/Tracer.hh>

using namespace Prague;
using namespace Warsaw;

ImageKitImpl::ImageKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
ImageKitImpl::~ImageKitImpl() {}

Raster_ptr ImageKitImpl::empty()
{
  Trace trace("ImageKitImpl::empty");
  RasterImpl *raster = new RasterImpl();
  activate(raster);
  return raster->_this();
}

Raster_ptr ImageKitImpl::create(const char *file)
{
  Trace trace("ImageKitImpl::create");
  RasterImpl *raster = new RasterImpl(file);
  activate(raster);
  return raster->_this();
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "ImageKitImpl"};
  return new KitFactoryImpl<ImageKitImpl> ("IDL:Warsaw/ImageKit:1.0", properties, 1);
}
