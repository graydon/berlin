/*$Id: GLDrawingKit.hh,v 1.20 1999/11/10 21:57:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _GLDrawingKit_hh
#define _GLDrawingKit_hh

#include "Warsaw/config.hh"
#include "Warsaw/DrawingKit.hh"
#include "Berlin/CloneableImpl.hh"
#include "Drawing/openGL/GLDrawable.hh"
#include "Drawing/openGL/GLPencil.hh"
#include "Drawing/openGL/GLFont.hh"
#include "Drawing/openGL/GLUnifont.hh"
#include "Drawing/openGL/GLRaster.hh"
#include "Berlin/Thread.hh"
#include "Berlin/ObjectCache.hh"
#include "Warsaw/Image.hh"

#include <string>
#include <vector>
extern "C" {
#include <ggi/ggi.h>
}

class GLDrawingKit : lcimplements(DrawingKit), virtual public CloneableImpl
{
public:
  GLDrawingKit();
  ~GLDrawingKit();
  Drawable_ptr getDrawable();

  void image(Raster_ptr);
  void setFont(const Text::FontDescriptor &, const Style::Spec &) throw (Text::NoSuchFontException);
  Text::Font_ptr currentFont();
  Pencil_ptr getPencil(const Style::Spec &);
      
  ggi_visual_t getVisual() { return drawable->Visual();}
  void clear(Coord, Coord, Coord, Coord);
  void sync() { glFlush();}
  Coord width() { return drawable->width();}
  Coord height() { return drawable->height();}
 private:
  Mutex mutex;
  GLFont *font;
  GLUnifont *gnufont;
  GLDrawable *drawable;
  vector<GLPencil *> pencils;
  ObjectCache<Raster_var, GLRaster> rasters;
};

#endif /* _GLDrawingKit_hh */
