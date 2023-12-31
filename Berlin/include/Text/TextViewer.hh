//
// $Id: TextViewer.hh,v 1.2 1999/08/26 13:55:39 gray Exp $
//
// This source file is a part of the Berlin Project.
// Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
// http://www.berlin-consortium.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License
// as published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//
#ifndef _TextViewer_hh
#define _TextViewer_hh

#include "Warsaw/config.hh"
#include "Warsaw/View.hh"
#include "Berlin/GapBuffer.hh"
#include "Berlin/PolyGraphic.hh"
#include <map>


class TextChunk;
class FontChange;
class Compositor;

declare_corba_ptr_type(DrawingKit)
declare_corba_ptr_type(TextBuffer)
declare_corba_ptr_type(DrawTraversal)

class TextViewer : implements(View), public virtual PolyGraphic {

    public:

    TextViewer(TextBuffer_ptr txt, DrawingKit_ptr dk, Compositor *);
    TextViewer(DrawingKit_ptr dk, Compositor *);
    void draw(DrawTraversal_ptr dt);
    void update(Subject_ptr s, const CORBA::Any &a);
    virtual ~TextViewer();

    protected:

    TextBuffer_var myTextBuffer;
    DrawingKit_var myCanonicalDK;
    Compositor  *myCompositor;

    GapBuffer<TextChunk *, 32> myGlyphs;
    map<long, FontChange *> myFontSettings;

};

#endif
