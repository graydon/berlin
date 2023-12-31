/*$Id: RasterImpl.cc,v 1.29 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
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

#include <Prague/Sys/Tracer.hh>
#include <Prague/Sys/Path.hh>
#include <Berlin/RCManager.hh>
#include "Image/RasterImpl.hh"
#include <string>

using namespace Prague;
using namespace Warsaw;

RasterImpl::RasterImpl() : _rows(0) {}
RasterImpl::RasterImpl(const char *file) : _rows(0)
{
  Prague::Path path = RCManager::get_path("rasterpath");
  std::string pngfile = path.lookup_file(file);
  if (pngfile.empty())
    {
      std::cerr << "RasterImpl Warning : can't find '" << file << "' in current rasterpath" << std::endl;
      pngfile = path.lookup_file("berlin-128.png");
    }
  _rows = _png.read(pngfile);
  if (!_rows)
    {
      std::cerr << "RasterImpl fatal error: can't read fallback raster berlin-128.png" << std::endl;
      exit(-1);
    }
}
RasterImpl::~RasterImpl() { Trace trace("RasterImpl::~RasterImpl");}
void RasterImpl::clear()
{
  delete [] _rows;
  _rows = 0;
  _png.clear();
}

Warsaw::Raster::Info RasterImpl::header()
{
  Warsaw::Raster::Info info;
  _png.header(info);
  return info;
}

void RasterImpl::load_data(const Warsaw::Raster::Data &data)
{
  Trace trace("RasterImpl::load_data");
  clear();
  _rows = _png.demarshal(data);
}

void RasterImpl::store_data(Warsaw::Raster::Data_out data)
{
  Trace trace("RasterImpl::store_data");
  delete data;
  data = 0;
  data = _png.marshal(_rows);
}

void RasterImpl::store_pixel(const Warsaw::Raster::Index &index, Color &color)
{
  Trace trace("RasterImpl::store_pixel");
  color = _png.pixel(index.x, index.y, _rows);
}

void RasterImpl::load_pixel(const Warsaw::Raster::Index &index, const Color &color)
{
  Trace trace("RasterImpl::load_pixel");
  _png.pixel(index.x, index.y, color, _rows);
}

void RasterImpl::store_pixels(const Warsaw::Raster::Index &lower, const Warsaw::Raster::Index &upper, Warsaw::Raster::ColorSeq_out pixels)
{
  Trace trace("RasterImpl::store_pixels");
  delete pixels;
  pixels = 0;
  pixels = _png.pixels(lower.x, lower.y, upper.x, upper.y, _rows);
}

void RasterImpl::load_pixels(const Warsaw::Raster::Index &lower, const Warsaw::Raster::Index &upper, const Warsaw::Raster::ColorSeq &pixels)
{
  Trace trace("RasterImpl::load_pixels");
  _png.pixels(lower.x, lower.y, upper.x, upper.y, pixels, _rows);
}

void RasterImpl::write(const char *file)
{
  Trace trace("RasterImpl::write");
  _png.write(file, _rows);
}
