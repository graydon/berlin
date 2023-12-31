/*$Id: Pixmap.cc,v 1.1 1999/12/13 21:14:34 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
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

#include "Drawing/FT/Pixmap.hh"
#include <cstring>

using namespace FT;

template <class T> T inline floor(T t) { return t & -64;}
template <class T> T inline ceiling(T t) { return (t + 63) & -64;}

Pixmap::Pixmap(const Glyph &glyph) throw (exception)
  : w(0), h(0), cols(0), bitmap(0), pixmap(0),
    a(0), dx(0), dy(0)
{
  Glyph::Metrics metrics = glyph.metrics();
  TT_BBox &bbox = metrics.bbox; // glyph bounding box
  a  = metrics.advance;
  dx = bbox.xMin;
  dy = bbox.yMin;

  bbox.xMin = floor(bbox.xMin);
  bbox.yMin = floor(bbox.yMin);
  bbox.xMax = ceiling(bbox.xMax);
  bbox.yMax = ceiling(bbox.yMax);

  w = (bbox.xMax - bbox.xMin)/64;
  h = (bbox.yMax - bbox.yMin)/64;

  cols= (w + 3) & -4;
  int size= cols * h;

  if(size <= 0) return;

  bitmap = new unsigned char [size];
  memset(bitmap, 0, size);

  TT_Raster_Map map;

  map.width = w;
  map.cols  = cols;
  map.rows  = h;
  map.flow  = TT_Flow_Up;
  map.size  = size;
  map.bitmap= bitmap;

  if (TT_Get_Glyph_Pixmap(glyph, &map, -bbox.xMin, -bbox.yMin)) throw exception();
}

Pixmap::~Pixmap()
{
  delete [] bitmap;
  delete [] pixmap;
}


unsigned char *Pixmap::data(unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
  if(bitmap == 0) return 0;
  if(pixmap != 0 && r == color.r && g == color.g && b == color.b && a == color.a) return pixmap;

  // This is another color! We need to recolor our pixmap from the
  // grayscale bitmap

  if(pixmap == 0) pixmap= new unsigned char [cols * h * 4];

  unsigned char palette[5][4];

  for(int j= 0; j < 5; j++)
    {
      palette[j][0]= (unsigned char)(int(r) * j / 4);
      palette[j][1]= (unsigned char)(int(g) * j / 4);
      palette[j][2]= (unsigned char)(int(b) * j / 4);
      palette[j][3]= (unsigned char)(int(a) * j / 4);
    }

  unsigned char *bline= bitmap;
  unsigned char *pline= pixmap;
  if (sizeof(int) == 4)
    {
      for(int j = 0; j < h; j++)
	{
	  for(int i = 0; i < w; i++)
	    {
	      unsigned char k = bline[i];
	      // assuming sizeof(int) == 4 ...
	      *(int*)(pline + i*4)= *(int*) &(palette[k][0]);
	    }
	  bline += cols;
	  pline += cols * 4;
	}
    }
  else
    {
      for(int j = 0; j < h; j++)
	{
	  for(int i = 0; i < w; i++)
	    {
	      unsigned char k = bline[i];
	      (pline + i*4)[0]= palette[k][0];
	      (pline + i*4)[1]= palette[k][1];
	      (pline + i*4)[2]= palette[k][2];
	      (pline + i*4)[3]= palette[k][3];
	    }
	  bline += cols;
	  pline += cols * 4;
	}
    }

  color.r = r;
  color.g = g;
  color.b = b;
  color.a = a;
  return pixmap;
}
