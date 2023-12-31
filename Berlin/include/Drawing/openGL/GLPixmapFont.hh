/*$Id: GLPixmapFont.hh,v 1.2 2000/03/20 22:23:24 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _GLPixmapFont_hh
#define _GLPixmapFont_hh

#include <Drawing/FT/Face.hh>
#include <Drawing/FT/Instance.hh>
#include <Drawing/FT/PixmapFont.hh>
#include <Drawing/FT/PixmapFont.hh>
#include "Drawing/openGL/GLFont.hh"

class GLPixmapFont : public GLFont
//. this font uses pixmaps and thus supports anti-aliasing
{
  typedef vector<FT::Pixmap *> block;
public:
  GLPixmapFont(const FT::Face &, size_t);
  ~GLPixmapFont();
  virtual unsigned long size() { return _size;}
  virtual unsigned long weight() { return _weight;}
  virtual const Unistring &family() { return _family;}
  virtual const Unistring &subfamily() { return _subfamily;}
  virtual const Unistring &fullname() { return _fullname;}
  virtual const Unistring &style() { return _style;}
  virtual void drawText(const Unistring &);
  virtual void allocateText(const Unistring &, Graphic::Requisition &);

//   FT::PixmapFont *font() const { return pfont;}
//   void render(int x, int y, const unsigned short *, const unsigned short *) const;
//   int width(const char *text) { return pfont->width(text);}
//   int height() const { return instance.height();}
//   int descender() const { return instance.descender();}
private:
  FT::Pixmap *loadGlyph(unsigned short) const;
  const FT::Face &face;
  FT::Instance    instance;
  FT::PixmapFont *pfont;
  mutable vector<block>   cache;
  unsigned long _size;
  unsigned long _weight;
  Unistring _family;
  Unistring _subfamily;
  Unistring _fullname;
  Unistring _style;
};

#endif /* _GLPixmapFont_hh */
