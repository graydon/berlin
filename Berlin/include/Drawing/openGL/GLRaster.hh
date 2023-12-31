/*$Id: GLRaster.hh,v 1.4 1999/11/10 21:57:36 stefan Exp $
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
#ifndef _GLRaster_hh
#define _GLRaster_hh

#include <Warsaw/config.hh>
#include <Warsaw/Raster.hh>
#include <Warsaw/Transform.hh>
#include <vector>
#include <GL/gl.h>
class GLRaster
{
public:
  GLRaster(Raster_var);
  ~GLRaster();
  void draw();
  Raster_var remote;
  PixelCoord width;
  PixelCoord height;
  GLuint texture;
  GLfloat s, t;
private:
  GLuint bind(GLint components, GLenum format, unsigned char *data);
  void unbind();
};

#endif /* _GLRaster_hh */
