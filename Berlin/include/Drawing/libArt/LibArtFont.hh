/*$Id: LibArtFont.hh,v 1.11 2000/08/31 18:52:32 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _LibArtFont_hh
#define _LibArtFont_hh

#include <Warsaw/config.hh>
#include <Warsaw/Types.hh>
#include <Warsaw/DrawingKit.hh>
#include <string>
#include <vector>
#include <libart_lgpl/art_misc.h>
#include <libart_lgpl/art_pixbuf.h>

class LibArtFont
{
public:
  LibArtFont() {}
  virtual ~LibArtFont() {}
  virtual CORBA::ULong size() = 0;
  virtual void size(CORBA::ULong) = 0;
  virtual CORBA::ULong weight() = 0;
  virtual void weight(CORBA::ULong) = 0;
  virtual Warsaw::Unistring *family() = 0;
  virtual Warsaw::Unistring *subfamily() = 0;
  virtual Warsaw::Unistring *fullname() = 0;
  virtual Warsaw::Unistring *style() = 0;
  virtual Warsaw::DrawingKit::FontMetrics metrics() = 0;
  virtual Warsaw::DrawingKit::GlyphMetrics metrics(Warsaw::Unichar &) = 0;
  // this method returns true if the font will handle the trafo internally,
  // false otherwise
  virtual bool transform(double trafo[4]) { return false; };
  virtual void allocateChar(const Warsaw::Unichar ch, Warsaw::Graphic::Requisition &) = 0;
  virtual void getPixBuf(const Warsaw::Unichar ch, ArtPixBuf *&) = 0;
};

#endif

