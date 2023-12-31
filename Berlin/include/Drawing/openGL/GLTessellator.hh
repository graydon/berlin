/*$Id: GLTessellator.hh,v 1.1 1999/12/22 16:53:39 stefan Exp $
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
#ifndef _GLTessellator_hh
#define _GLTessellator_hh

#include <Drawing/FT/Glyph.hh>
#include <Drawing/FT/Outline.hh>
#include <GL/gl.h>

class GLTessellator
{
public:
  class Handler;
  GLTessellator(const FT::Glyph &glyph) : outline(glyph) {}
  ~GLTessellator() {}
  bool tessellate(double, Handler *handler = 0);
  double bearingX() const { return outline.bearingX();}
  double bearingY() const { return outline.bearingY();}
  double advance() const { return outline.advance();}
private:
  FT::Outline outline;
};

class GLTessellator::Handler
{
  friend class GLTessellator;
public:
  Handler(bool v) : verbose(v), tessellator(0) {}
  virtual ~Handler() {}
  virtual void begin(int);
  virtual void vertex(FT::Outline::Point *);
  virtual void end();
  virtual void error(int);
protected:
private:
  static void handleBegin(GLenum);
  static void handleVertex(void *);
  static void handleEnd();
  static void handleError(GLenum);
  bool            verbose;
  GLTessellator  *tessellator;
  static Handler  defaultHandler;
  static Handler *handler;
};

#endif /* _GLTessellator_hh */
