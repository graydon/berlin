/*$Id: GLUnifont.cc,v 1.14 1999/11/30 21:52:35 tobias Exp $
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

#include "Drawing/openGL/GLUnifont.hh"
#include "Prague/Sys/MMap.hh"

#include <GL/gl.h>
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

GLUnifont::GLUnifont()
{
    myDescriptor.pointsize = 16;
    myDescriptor.name = Unicode::toCORBA(Unicode::String("GNU Unifont"));
    char *env = getenv("BERLIN_ROOT");
    if (!env)
      {
	cerr << "Please set environment variable BERLIN_ROOT first" << endl;
	exit(-1);
      }
    string glyphDB = string(env) + "/etc/glyph.dat";
    glyphmap = new MMap(glyphDB, -1, MMap::read, MMap::shared, 0, 0);
}

GLUnifont::~GLUnifont() { delete glyphmap ; }

void GLUnifont::setColor(Color c) 
{
    myColor = c;
}

void GLUnifont::drawText(const Unistring &u, const Vertex &p) 
{
  unsigned char *glyphs = (unsigned char *)glyphmap->addr();
  // prepare GL to draw
  glColor4d(myColor.red,myColor.green,myColor.blue,myColor.alpha); // load color
  glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
  glPixelStorei(GL_UNPACK_ALIGNMENT,1); // set to byte-aligned unpacking
  glRasterPos2i((int)(p.x),(int)(p.y));  // position pen
  
  for (unsigned long idx = 0; idx < u.length(); idx++) {
    
    unsigned int stride = 33;
    unsigned int base = stride * u[idx];
    bool is_halfwidth = (glyphs[base] == (unsigned char)0xFF) ? 1 : 0;
    unsigned char width = is_halfwidth ? 8 : 16; 
    unsigned char height = 16;
    base++;			// advance past the width marker
    glBitmap(width, height, 0.0, 0.0, (float)width, 0.0, (const GLubyte *)(&(glyphs[base])));
    
  }
}


void GLUnifont::acceptFontVisitor(Text::FontVisitor_ptr v)
{
    v->visitBaseFont(this->_this());
    CORBA::release(v);
}

void GLUnifont::allocateText(const Unistring &u, Graphic::Requisition &r)
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

CORBA::Boolean  GLUnifont::canDrawText(const Unistring &u){
  return true;
}

Text::FontDescriptor *GLUnifont::descriptor(){
  return &myDescriptor;
}

FeatureValueList *GLUnifont::queryFeature(FeatureType ft) {
  return new FeatureValueList();
}

void GLUnifont::setFeature(FeatureType ft, FeatureValue fv) {}
