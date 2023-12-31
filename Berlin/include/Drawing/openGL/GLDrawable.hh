/*$Id: GLDrawable.hh,v 1.10 1999/11/10 21:57:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _GLDrawable_hh
#define _GLDrawable_hh

#include <Warsaw/config.hh>
#include <Warsaw/Drawable.hh>
#include <Berlin/RegionImpl.hh>
#include <GL/ggimesa.h>
#include <GL/gl.h>
extern "C" {
#include <ggi/ggi.h>
}
#include <stack>

class GLDrawable : implements(Drawable)
{
  typedef stack<RegionImpl *> cstack_t;
 public:
  GLDrawable();
  ~GLDrawable();
  Coord dpi(Axis);
  void pushTransform(Transform_ptr);
  void popTransform();
  void pushClipping(Region_ptr, Transform_ptr);
  void popClipping();
  void makeCurrent() { GGIMesaMakeCurrent(context);}
  ggi_visual_t Visual() { return visual;}
  Coord toCoord(PixelCoord, Axis);
  PixelCoord toPixels(Coord, Axis);
  Coord width() { return mode.visible.x;}
  Coord height() { return mode.visible.y;}
 protected:
  void start() { GGIMesaMakeCurrent(context);}
  void GLDrawable::reshape( int width, int height );
 private:
  GGIMesaContext context;
  ggi_visual_t visual;
  cstack_t clipping;
  ggi_mode mode;
};

#endif /* _GLDrawable_hh */
