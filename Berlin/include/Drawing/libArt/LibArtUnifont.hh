/*$Id: LibArtUnifont.hh,v 1.9 2001/01/26 17:09:49 stefan Exp $
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
#ifndef _LibArtUnifont_hh
#define _LibArtUnifont_hh

#include <map>
#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Berlin/Console.hh>
#include <Warsaw/Graphic.hh>
#include <Prague/Sys/MMap.hh>
#include <Drawing/libArt/LibArtFont.hh>
#include <Warsaw/Unicode.hh>

class LibArtUnifont : public LibArtFont
//. This is a default font, just in case -- a character cell bitmapped unicode
//. font which is generated "on the fly" from the GNU unifont, which we're
//. storing in a packed binary array we mmap() in. this is so that, even if all
//. the font manufactureres in the world turn against us, we can still render
//. multilingual text, albeit not quite as well as certain (ahem) proprietary
//. text systems
{
public:
  LibArtUnifont(Console::Drawable *drawable);
  virtual ~LibArtUnifont();
  virtual CORBA::ULong size();
  virtual void size(CORBA::ULong) {}
  virtual CORBA::ULong weight();
  virtual void weight(CORBA::ULong) {}
  virtual Warsaw::Unistring *family();
  virtual Warsaw::Unistring *subfamily();
  virtual Warsaw::Unistring *fullname();
  virtual Warsaw::Unistring *style();
  virtual Warsaw::DrawingKit::FontMetrics metrics();
  virtual Warsaw::DrawingKit::GlyphMetrics metrics(Warsaw::Unichar &);
  virtual void allocateChar(const Warsaw::Unichar, Warsaw::Graphic::Requisition &);
  virtual void getPixBuf(const Warsaw::Unichar, ArtPixBuf *&);
protected:
  void glyph2pixels(const Warsaw::Unichar, unsigned char *);
  double xres, yres;  

  unsigned char slab[16*16];
  ArtPixBuf *myPixBuf;
//   map<Unichar,ArtPixBuf *> cache;
  Prague::MMap *glyphmap;
};

#endif
