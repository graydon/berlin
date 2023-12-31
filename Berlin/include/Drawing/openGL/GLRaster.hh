/*$Id: GLRaster.hh,v 1.7 2000/08/31 18:52:32 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _GLRaster_hh
#define _GLRaster_hh

#include <Warsaw/config.hh>
#include <Warsaw/Raster.hh>
#include <Warsaw/Transform.hh>
#include <vector>
#include <GL/gl.h>

struct GLRaster
{
  GLRaster(Warsaw::Raster_var r) : remote(Warsaw::Raster::_duplicate(r)) {}
  Warsaw::Raster_var remote;
  Warsaw::PixelCoord width;
  Warsaw::PixelCoord height;
  GLuint texture;
};

struct GLTexture : GLRaster
{
  GLTexture(Warsaw::Raster_var);
  ~GLTexture();
private:
  GLuint bind(GLint components, GLenum format, unsigned char *data);
};

struct GLImage : GLRaster
{
  GLImage(Warsaw::Raster_var);
  ~GLImage();
  GLfloat s, t;
private:
  GLuint bind(GLint components, GLenum format, unsigned char *data);
};

#endif 
