/*$Id: LibArtRaster.hh,v 1.3 2000/08/31 18:52:32 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 graydon hoare <graydon@pobox.com>
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
#ifndef _LibArtRaster_hh
#define _LibArtRaster_hh

#include <Warsaw/config.hh>
#include <Warsaw/Raster.hh>
#include <libart_lgpl/art_misc.h>
#include <libart_lgpl/art_pixbuf.h>

struct LibArtRaster
{
  LibArtRaster(Warsaw::Raster_var r);
  virtual LibArtRaster::~LibArtRaster();  
  Warsaw::Raster_var remote;
  art_u8 *pixels;
public:
  ArtPixBuf *pixbuf;
};

#endif 
