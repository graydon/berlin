/*$Id: GLRaster.cc,v 1.3 1999/11/10 21:57:36 stefan Exp $
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

#include "Prague/Sys/Memory.hh"
#include "Drawing/openGL/GLRaster.hh"

/*
 * this file contains modified code from Mesa's GLU (mipmap.c)
 * -stefan
 */

using namespace Prague;


template <class T>
T ceiling(T a, T b) { return a % b == 0 ? a/b : a/b + 1;}
const double epsilong = 0.001;

bool scaleImage(GLenum format,
		size_t widthin, size_t heightin, const unsigned char *datain,
		size_t widthout, size_t heightout, unsigned char *dataout)
{
  GLint components;
  /* Determine number of components per pixel */
  switch (format)
    {
    case GL_COLOR_INDEX:
    case GL_STENCIL_INDEX:
    case GL_DEPTH_COMPONENT:
    case GL_RED:
    case GL_GREEN:
    case GL_BLUE:
    case GL_ALPHA:
    case GL_LUMINANCE:
      components = 1;
      break;
    case GL_LUMINANCE_ALPHA:
      components = 2;
      break;
    case GL_RGB:
      components = 3;
      break;
    case GL_RGBA:
#ifdef GL_EXT_abgr
    case GL_ABGR_EXT:
#endif
      components = 4;
      break;
    default:
      return false;
    }

  /* Get glPixelStore state */
  GLint unpackrowlength, unpackalignment, unpackskiprows, unpackskippixels;
  GLint packrowlength, packalignment, packskiprows, packskippixels;
  GLint rowstride, rowlen;
  glGetIntegerv(GL_UNPACK_ROW_LENGTH, &unpackrowlength);
  glGetIntegerv(GL_UNPACK_ALIGNMENT, &unpackalignment);
  glGetIntegerv(GL_UNPACK_SKIP_ROWS, &unpackskiprows);
  glGetIntegerv(GL_UNPACK_SKIP_PIXELS, &unpackskippixels);
  glGetIntegerv(GL_PACK_ROW_LENGTH, &packrowlength);
  glGetIntegerv(GL_PACK_ALIGNMENT, &packalignment);
  glGetIntegerv(GL_PACK_SKIP_ROWS, &packskiprows);
  glGetIntegerv(GL_PACK_SKIP_PIXELS, &packskippixels);

  /* Allocate storage for intermediate images */
  GLfloat *tempin = new GLfloat [widthin * heightin * components];
  if (!tempin) return false;
  GLfloat *tempout = new GLfloat [widthout * heightout * components];
  if (!tempout)
    {
      delete [] tempin;
      return false;
    }

   /*
    * Unpack the pixel data and convert to floating point
    */
  if (unpackrowlength > 0) rowlen = unpackrowlength;
  else rowlen = widthin;
  if (unpackalignment <= 8) rowstride = components * rowlen;
  else rowstride = unpackalignment/8 * ceiling(components * rowlen * 8, unpackalignment);
  size_t k = 0;
  for (size_t i = 0; i < heightin; i++)
    {
      const unsigned char *ubptr = datain + i * rowstride + unpackskiprows * rowstride + unpackskippixels * components;
      for (size_t j = 0;j < widthin*components; j++) tempin[k++] = static_cast<GLfloat>(*ubptr++);
    }

  /*
   * Scale the image!
   */
  GLfloat sx, sy;
  if (widthout > 1) sx = static_cast<GLfloat>(widthin-1) / (widthout-1);
  else sx = static_cast<GLfloat>(widthin-1);
  if (heightout > 1) sy = static_cast<GLfloat>(heightin-1) / (heightout-1);
  else sy = static_cast<GLfloat>(heightin-1);

   if (sx < 1. && sy < 1.)
     {
       /* magnify both width and height:  use weighted sample of 4 pixels */
       for (size_t i = 0; i < heightout; i++)
	 {
	   size_t i0 = static_cast<size_t>(i * sy);
	   size_t i1 = static_cast<size_t>(i0 + 1);
	   if (i1 >= heightin) i1 = heightin - 1;
/*	 i1 = (i+1) * sy - EPSILON;*/
	   GLfloat alpha = i*sy - i0;
	   for (size_t j = 0; j < widthout; j++)
	     {
	       size_t j0 = static_cast<size_t>(j * sx);
	       size_t j1 = static_cast<size_t>(j0 + 1);
	       if (j1 >= widthin) j1 = widthin-1;
	       /*	    j1 = (j+1) * sx - EPSILON; */
	       GLfloat beta = j*sx - j0;

	       /* compute weighted average of pixels in rect (i0,j0)-(i1,j1) */
	       GLfloat *src00 = tempin + (i0 * widthin + j0) * components;
	       GLfloat *src01 = tempin + (i0 * widthin + j1) * components;
	       GLfloat *src10 = tempin + (i1 * widthin + j0) * components;
	       GLfloat *src11 = tempin + (i1 * widthin + j1) * components;

	       GLfloat *dst = tempout + (i * widthout + j) * components;

	       for (size_t k = 0; k < components; k++)
		 {
		   GLfloat s1 = *src00++ * (1.0-beta) + *src01++ * beta;
		   GLfloat s2 = *src10++ * (1.0-beta) + *src11++ * beta;
		   *dst++ = s1 * (1.0-alpha) + s2 * alpha;
		 }
	     }
	 }
     }
   else
     {
       /* shrink width and/or height:  use an unweighted box filter */
       for (size_t i = 0; i < heightout; i++)
	 {
	   size_t i0 = static_cast<size_t>(i * sy);
	   size_t i1 = static_cast<size_t>(i0 + 1);
	   if (i1 >= heightin) i1 = heightin-1; 
	   /*	 i1 = (i+1) * sy - EPSILON; */
	   for (size_t j = 0; j < widthout; j++)
	     {
	       size_t j0 = static_cast<size_t>(j * sx);
	       size_t j1 = static_cast<size_t>(j0 + 1);
	       if (j1 >= widthin) j1 = widthin-1;
	       /*	    j1 = (j+1) * sx - EPSILON; */
	       GLfloat *dst = tempout + (i * widthout + j) * components;
	       /* compute average of pixels in the rectangle (i0,j0)-(i1,j1) */
	       for (size_t k = 0; k < components; k++)
		 {
		   GLfloat sum = 0.0;
		   for (size_t ii = i0; ii <= i1; ii++)
		       for (size_t jj = j0; jj<= j1; jj++)
			 sum += *(tempin + (ii * widthin + jj) * components + k);
		   sum /= (j1-j0+1) * (i1-i0+1);
		   *dst++ = sum;
		 }
	     }
	 }
     }
   
   /*
    * Return output image
    */

   if (packrowlength > 0) rowlen = packrowlength;
   else rowlen = widthout;
   if (packalignment <= 8) rowstride = components * rowlen;
   else rowstride = packalignment/8 * ceiling(components * rowlen * 8, packalignment);
   
   k = 0;
   for (size_t i = 0; i < heightout; i++)
     {
       unsigned char *ubptr = dataout + i * rowstride + packskiprows * rowstride + packskippixels * components;
       for (size_t j = 0; j < widthout * components; j++) *ubptr++ = static_cast<unsigned char>(tempout[k++]);
     }
   /* free temporary image storage */
   delete [] tempin;
   delete [] tempout;
   return true;
}



/*
 * openGL requires glTexImage2D to take width and height in the form 2^k
 * se we extract the exponent here and the residue
 */
inline void logbase2(unsigned int n, int &v)//, float &r)
{
  unsigned int k;
  for (k = 0; n >>= 1; k++);
  v = 1 << (k + 1);//, r = v - n;
}

/*
 * Given an pixel format and datatype, return the number of bytes to
 * store one pixel.
 */
static GLint bytes_per_pixel(GLenum format)
{
  switch (format)
    {
    case GL_COLOR_INDEX:
    case GL_STENCIL_INDEX:
    case GL_DEPTH_COMPONENT:
    case GL_RED:
    case GL_GREEN:
    case GL_BLUE:
    case GL_ALPHA:
    case GL_LUMINANCE: return 1; break;
    case GL_LUMINANCE_ALPHA: return 2; break;
    case GL_RGB: return 3; break;
    case GL_RGBA:
#ifdef GL_EXT_abgr
    case GL_ABGR_EXT:
#endif
      return 4; break;
    default: return 0;
    }
}

void resizeImage(GLenum format,
		 size_t widthin, size_t heightin, const unsigned char *datain,
		 size_t widthout, size_t heightout, unsigned char *dataout)
{
  GLint bpp = bytes_per_pixel(format);
  Memory::zero(dataout, widthout * heightout * bpp);
  for (size_t h = 0; h != heightin; h++, datain += widthin * bpp, dataout += widthout * bpp)
    Memory::copy(datain, dataout, widthin * bpp);
}


GLuint GLRaster::bind(GLint components, GLenum format, unsigned char *data)
{
  GLuint texture;
  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  GLint w, h;
  unsigned char *image, *newimage;
  GLint neww, newh, level, bpp;
  int error;
  GLint unpackrowlength, unpackalignment, unpackskiprows, unpackskippixels;
  GLint packrowlength, packalignment, packskiprows, packskippixels;

  GLint maxsize;
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, &maxsize);
  logbase2(width, w);
  if (w > maxsize) w = maxsize;
  logbase2(height, h);
  if (h > maxsize) h = maxsize;
  bpp = bytes_per_pixel(format);

  /* Get current glPixelStore values */
  glGetIntegerv(GL_UNPACK_ROW_LENGTH, &unpackrowlength);
  glGetIntegerv(GL_UNPACK_ALIGNMENT, &unpackalignment);
  glGetIntegerv(GL_UNPACK_SKIP_ROWS, &unpackskiprows);
  glGetIntegerv(GL_UNPACK_SKIP_PIXELS, &unpackskippixels);
  glGetIntegerv(GL_PACK_ROW_LENGTH, &packrowlength);
  glGetIntegerv(GL_PACK_ALIGNMENT, &packalignment);
  glGetIntegerv(GL_PACK_SKIP_ROWS, &packskiprows);
  glGetIntegerv(GL_PACK_SKIP_PIXELS, &packskippixels);

  /* set pixel packing */
  glPixelStorei(GL_PACK_ROW_LENGTH, 0);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glPixelStorei(GL_PACK_SKIP_ROWS, 0);
  glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

  bool done = false;

  if (w != width || h != height)
#if 0
    {
      /* must rescale image to get "top" mipmap texture image */
      image = new unsigned char [(w+4) * h * bpp];
      bool error = scaleImage(format, width, height, data, w, h, image);
      if (error) done = true;
    }
#else
    {
      /* just copy the raster into a larger image, adapting the texture coordinates */
      image = new unsigned char [(w+4) * h * bpp];
      resizeImage(format, width, height, data, w, h, image);
      s = static_cast<GLfloat>(width)/w;
      t = static_cast<GLfloat>(height)/h;
    }
#endif
  else image = data;

  level = 0;
  while (!done)
    {
      if (image != data)
	{
	  /* set pixel unpacking */
	  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	  glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
	  glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
	}
      glTexImage2D(GL_TEXTURE_2D, level, components, w, h, 0, format, GL_UNSIGNED_BYTE, image);
      if (w==1 && h==1)  break;

      neww = (w < 2) ? 1 : w / 2;
      newh = (h < 2) ? 1 : h / 2;
      newimage = new unsigned char [(neww + 4) * newh * bpp];
      error = scaleImage(format, w, h, image, neww, newh, newimage);
      if (error) done = true;
      if (image != data) delete [] image;
      image = newimage;
      w = neww;
      h = newh;
      level++;
    }
  if (image != data) delete [] image;

   /* Restore original glPixelStore state */
  glPixelStorei(GL_UNPACK_ROW_LENGTH, unpackrowlength);
  glPixelStorei(GL_UNPACK_ALIGNMENT, unpackalignment);
  glPixelStorei(GL_UNPACK_SKIP_ROWS, unpackskiprows);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS, unpackskippixels);
  glPixelStorei(GL_PACK_ROW_LENGTH, packrowlength);
  glPixelStorei(GL_PACK_ALIGNMENT, packalignment);
  glPixelStorei(GL_PACK_SKIP_ROWS, packskiprows);
  glPixelStorei(GL_PACK_SKIP_PIXELS, packskippixels);
  return texture;
}

void GLRaster::unbind()
{
  glDeleteTextures(1, &texture);  
}

GLRaster::GLRaster(Raster_var r)
  : remote(r),
    s(1.), t(1.)
{
  Raster::Info info = remote->header();
  Raster::ColorSeq_var pixels;
  Raster::Index lower, upper;
  lower.x = lower.y = 0;
  upper.x = info.width, upper.y = info.height;
  remote->storePixels(lower, upper, pixels);
  width = info.width;
  height = info.height;
  vector<unsigned char> data(4*width*height);
  vector<unsigned char>::iterator pixel = data.begin();
  for (int y = height - 1; y >= 0; y--)
    for (int x = 0; x != width; x++)
      {
	Color &color = pixels[y * info.width + x];
	*pixel++ = static_cast<char>(color.red * 256);
	*pixel++ = static_cast<char>(color.green * 256);
	*pixel++ = static_cast<char>(color.blue * 256);
	*pixel++ = static_cast<char>(color.alpha * 256);
      }
  texture = bind(GL_RGBA, GL_RGBA, data.begin());
}

GLRaster::~GLRaster()
{
  unbind();
}

void GLRaster::draw()
{
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture);
  glColor4f(1., 1., 1., 1.);
  glBegin(GL_POLYGON);
  Path path;
  path.p.length(4);
  path.p[0].x = path.p[0].y = path.p[0].z = 0.;
  path.p[1].x = width, path.p[1].y = path.p[1].z = 0.;
  path.p[2].x = width, path.p[2].y = height, path.p[2].z = 0.;
  path.p[3].x = 0, path.p[3].y = height, path.p[3].z = 0.;
  glTexCoord2f(0., 0.); glVertex3f(path.p[3].x, path.p[3].y, path.p[3].z);
  glTexCoord2f(s, 0.);  glVertex3f(path.p[2].x, path.p[2].y, path.p[2].z);
  glTexCoord2f(s, t);   glVertex3f(path.p[1].x, path.p[1].y, path.p[1].z);
  glTexCoord2f(0., t);  glVertex3f(path.p[0].x, path.p[0].y, path.p[0].z);
  glEnd();
  glDisable(GL_TEXTURE_2D);
}
