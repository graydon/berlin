/*$Id: PNG.hh,v 1.1 1999/10/19 21:07:52 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Brent Fulgham <bfulgham@debian.org>
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
#ifndef _PNG_hh
#define _PNG_hh

#include <Warsaw/Raster.hh>
#include <png.h>

class PNG
{
  class ibuf;
  class obuf;
  class Encoder;
  class Decoder;
public:
  enum color_t { gray = PNG_COLOR_TYPE_GRAY,
		 grayalpha = PNG_COLOR_TYPE_GRAY_ALPHA,
		 palette = PNG_COLOR_TYPE_PALETTE,
		 rgb = PNG_COLOR_TYPE_RGB,
		 rgbalpha = PNG_COLOR_TYPE_RGB_ALPHA,
		 maskpalette = PNG_COLOR_MASK_PALETTE,
		 maskcolor = PNG_COLOR_MASK_COLOR,
		 maskalpha = PNG_COLOR_MASK_ALPHA};
  enum interlace_t { none = PNG_INTERLACE_NONE,
		     adam7 = PNG_INTERLACE_ADAM7,
		     last = PNG_INTERLACE_LAST};
  PNG();
  ~PNG();
  void clear();
  void header(Raster::Info &);
  Raster::Data *marshal(unsigned char *const *);
  unsigned char **demarshal(const Raster::Data &);
  Color pixel(unsigned long, unsigned long, unsigned char *const *);
  void pixel(unsigned long, unsigned long, const Color &, unsigned char **);
  Raster::ColorSeq *pixels(unsigned long, unsigned long, unsigned long, unsigned long, unsigned char *const *);
  void pixels(unsigned long, unsigned long, unsigned long, unsigned long, const Raster::ColorSeq &, unsigned char **);
  unsigned char **read(const string &);
  void write(const string &, unsigned char *const *);
private:
  void expand(const unsigned char *, const unsigned char *, unsigned char *);
  png_structp rpng;
  png_infop   rinfo;
  png_infop   rend;
};

#endif /* _PNG_hh */
