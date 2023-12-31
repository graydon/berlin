#ifndef _GLFont_hh
#define _GLFont_hh

/*$Id: GLFont.hh,v 1.2 1999/05/20 04:53:57 gray Exp $
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

/** this is a simple wrapper around stephane rehel's gltt library. Yay stephane! */

#include "Warsaw/config.hh"
#include "Warsaw/Style.hh"
#include "Warsaw/Types.hh"
#include "Warsaw/Text.hh"
#include <string>
#include <GL/gl.h>

class GLTTPixmapFont;
class FTFace;

class GLFont : 
  implementsscoped(Text,BaseFont) 
{
 public:
  GLFont(const Text::FontDescriptor &fd, const Style::Spec &sty) throw (Text::NoSuchFontException);
  ~GLFont();
  void acceptFontVisitor(Text::FontVisitor_ptr v);
  void drawText(const Unistring &u, const Vertex &v);
  void allocateText(const Unistring &u, Graphic::Requisition &r);
  FeatureValueList *queryFeature(FeatureType ft);
  void setFeature(FeatureType ft, FeatureValue fv);
  
  CORBA::Boolean  canDrawText(const Unistring &u);
  void getDescriptor(Text::FontDescriptor &f);  
 protected:
  const Text::FontDescriptor myDescriptor;
  GLfloat myFontColor[4];
  FTFace *face; 
  GLTTPixmapFont *font;
  
};

#endif

