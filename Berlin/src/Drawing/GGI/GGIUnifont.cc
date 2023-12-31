/*$Id: GGIUnifont.cc,v 1.1 2000/01/07 06:06:39 graydon Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

extern "C" {
#include "ggi/ggi.h"
#include "ggi/ggi2d.h"
}

#include "Drawing/GGI/GGIUnifont.hh"
#include "Prague/Sys/MMap.hh"

#include <string>
#include <string.h>
#include <iostream>
#include <stdlib.h>

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>


// This is a default font, just in case -- a character cell bitmapped unicode
// font which is generated "on the fly" from the GNU unifont, which we're
// storing in a packed binary array we mmap() in. this is so that, even if all
// the font manufactureres in the world turn against us, we can still render
// multilingual text, albeit not quite as well as certain (ahem) proprietary
// text systems

static ggi_sint dev(Coord x) {
  return static_cast<ggi_sint>(x);
}

static ggi_color col(Color c1) {
  ggi_color c2;
  // GGI _appears_ to use 16 bit color + alpha throughout. ??
  static int scale = 65535;
  c2.r = static_cast<uint16>(c1.red * scale);
  c2.g = static_cast<uint16>(c1.green * scale);
  c2.b = static_cast<uint16>(c1.blue * scale);
  c2.a = static_cast<uint16>(c1.alpha * scale);
  return c2;
}

static Unistring UNIFY(const char *c) {
  Unistring tmp;
  unsigned int len = strlen(c);
  tmp.length(len);
  for (unsigned long i = 0; i < len; i++) {
      tmp[i] = (Unichar)(c[i]);
  }
  return tmp;
}

GGIUnifont::GGIUnifont(ggi_visual_t v) : vis(v)
{
    myDescriptor.pointsize = 16;
    myDescriptor.name = UNIFY("GNU Unifont");
    char *env = getenv("BERLIN_ROOT");
    if (!env)
      {
	cerr << "Please set environment variable BERLIN_ROOT first" << endl;
	exit(-1);
      }
    string glyphDB = string(env) + "/etc/glyph.dat";
    glyphmap = new MMap(glyphDB, -1, MMap::read, MMap::shared, 0, 0);
}

GGIUnifont::~GGIUnifont() { delete glyphmap ; }

void GGIUnifont::setColor(Color c) 
{
    myColor = c;
}


void GGIUnifont::drawText(const Unistring &u, const Vertex &p) 
{
  unsigned char *glyphs = (unsigned char *)glyphmap->addr();
  // prepare GGI to draw
  ggi_color c = col(myColor);
  ggi2dSetDrawColor(vis, ggiMapColor(vis,&c));

  // !!FIXME!! this doesn't do directional layout yet

  const unsigned int scanlines_in_a_glyph = 16; // always; glyphs have constant height in unifont    
  const unsigned int bits_in_a_byte = 8;
  const unsigned int stride = 33;
  unsigned long len = u.length();

  ggi_sint xb = dev(p.x);
  ggi_sint yb = dev(p.y);
  ggi_sint x = 0;
  ggi_sint y = 0;
  
  for (unsigned long idx = 0; idx < len; idx++) {

    unsigned int base = stride * u[idx];
    bool is_halfwidth = (glyphs[base] == (unsigned char)0xFF) ? 1 : 0;
    unsigned int scanline_width = is_halfwidth ? 1 : 2; // 1 or 2 bytes, that is
    base++;			// advance past the width marker

    for (unsigned int curr_scanline = 0; curr_scanline < scanlines_in_a_glyph; curr_scanline++) {
      for (unsigned int byte_within_line = 0; byte_within_line < scanline_width; byte_within_line++) {	
	for (int bit = 7; bit >= 0; bit--) {
	  unsigned int offset = base + (curr_scanline * scanline_width) + byte_within_line;
	  if (((unsigned char)(glyphs[offset] >> (bit))) & 1) {ggi2dDrawPixel(vis,xb+x,yb-y);}
	  x = (x + 1) % (scanline_width * bits_in_a_byte);
	}
      }
      y = (y + 1) % scanlines_in_a_glyph; 
    }
    xb += scanline_width * bits_in_a_byte;
  }
}


void GGIUnifont::acceptFontVisitor(Text::FontVisitor_ptr v)
{
    v->visitBaseFont(this->_this());
    CORBA::release(v);
}

void GGIUnifont::allocateText(const Unistring &u, Graphic::Requisition &r)
{    
  unsigned char *glyphs = (unsigned char *)glyphmap->addr();
  
  int width = 0;
  int height = 16;
  
  for(unsigned int idx = 0; idx < u.length(); idx++) {
    unsigned int stride = 33;
    unsigned int base = stride * u[idx];
    bool is_halfwidth = (glyphs[base] == (unsigned char)0xFF) ? 1 : 0;
    width += is_halfwidth ? 8 : 16; 
  }
  
  r.x.natural = r.x.minimum = r.x.maximum = width;
  r.x.defined = true;
  r.x.align = 0.;
  r.y.natural = r.y.minimum = r.y.maximum = height;
  r.y.defined = true;
  r.y.align = 0.;
}

CORBA::Boolean  GGIUnifont::canDrawText(const Unistring &u){
  return true;
}

Text::FontDescriptor *GGIUnifont::descriptor(){
  return &myDescriptor;
}

FeatureValueList *GGIUnifont::queryFeature(FeatureType ft) { return new FeatureValueList(); }
void GGIUnifont::setFeature(FeatureType ft, FeatureValue fv) {}
