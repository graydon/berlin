/*$Id: ImageKitImpl.hh,v 1.9 2000/08/31 18:52:32 stefan Exp $
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

#ifndef _ImageKitImpl_hh
#define _ImageKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/ImageKit.hh>
#include <Warsaw/Raster.hh>
#include <Berlin/KitImpl.hh>
#include <vector>

class RasterImpl;

class ImageKitImpl : public virtual POA_Warsaw::ImageKit,
		     public KitImpl
{
public:
  ImageKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~ImageKitImpl();

  Warsaw::Raster_ptr empty();
  Warsaw::Raster_ptr create(const char *file);
protected:
};

#endif
