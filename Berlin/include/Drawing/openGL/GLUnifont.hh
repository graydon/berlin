#ifndef _GLUnifont_hh
#define _GLUnifont_hh

/*$Id: GLUnifont.hh,v 1.4 1999/11/30 21:52:35 tobias Exp $
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

#include <GL/gl.h>
#include <vector>
#include "Warsaw/config.hh"
#include "Warsaw/Style.hh"
#include "Warsaw/Types.hh"
#include "Warsaw/Text.hh"
#include "Prague/Sys/MMap.hh"
#include <Warsaw/Unicode.hh>

// This is a default font, just in case -- a character cell bitmapped unicode
// font which is generated "on the fly" from the GNU unifont, which we're
// storing in a packed binary array we mmap() in. this is so that, even if all
// the font manufactureres in the world turn against us, we can still render
// multilingual text, albeit not quite as well as certain (ahem) proprietary
// text systems

class GLUnifont :
    implementsscoped(Text,BaseFont) 
{

 public:
    GLUnifont();
    virtual ~GLUnifont();
    
    // Text::BaseFont implementation
    void acceptFontVisitor(Text::FontVisitor_ptr v);
    CORBA::Boolean canDrawText(const Unistring &u);
    void drawText(const Unistring &u, const Vertex &v);
    void allocateText(const Unistring &u, Graphic::Requisition &r);
    FeatureValueList *queryFeature(FeatureType ft);
    void setFeature(FeatureType ft, FeatureValue fv);
    Text::FontDescriptor *descriptor();  

    void setColor(Color c);

 protected:
    MMap *glyphmap;
    Text::FontDescriptor myDescriptor;
    Color myColor;
};


#endif
