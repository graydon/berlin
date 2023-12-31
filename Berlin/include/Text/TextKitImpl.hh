//
// $Id: TextKitImpl.hh,v 1.7 1999/09/17 19:15:23 gray Exp $
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
#ifndef _TextKitImpl_hh
#define _TextKitImpl_hh

#include "Warsaw/config.hh"
#include "Text/SeqComp.hh"
#include "Warsaw/TextKit.hh"
#include "Warsaw/Text.hh"
#include "Berlin/CloneableImpl.hh"
#include "Berlin/Thread.hh"
#include <map>

declare_corba_ptr_type(DrawingKit)

/** this implements a simple text kit which manufactures text chunks and font
 * change markers. the idea is that a font change marker delegates most of the
 * responsibility of actually finding and setting a "current font" to the
 * methods of the current drawingKit, and really only serves as a placeholder
 * with which a buffer or label can say "here the text is supposed to become
 * helvetica 12 pt., whatever implementation of what you can track down would
 * be just peachy"  
 */

struct GlyphComp {
    typedef pair<Unistring,Text::Font_ptr> Key;
    typedef Graphic_ptr Val;    
    
    inline bool operator()(const Key &a, const Key &b) {
	Text::FontDescriptor *d1 = a.second->descriptor();
	Text::FontDescriptor *d2 = b.second->descriptor();
	return SeqComp(a.first,b.first) || 
	    SeqComp(d1->name,d2->name) ||
	    SeqComp(d1->style,d2->style) ||
	    (d1->pointsize > d2->pointsize);
    }
};

class TextKitImpl : lcimplements(TextKit), public virtual CloneableImpl {
 public:
    TextKitImpl();
    virtual ~TextKitImpl();
    virtual void bind(ServerContext_ptr);
    Text::FontDescriptorSeq* fonts();
    DrawingKit_ptr dk();
    void dk(DrawingKit_ptr);
    Graphic_ptr  chunk(const Unistring & u, Text::Font_ptr  f);
    Graphic_ptr  fontChange(const Text::FontDescriptor & fd, const Style::Spec &s);
    
 protected:
    DrawingKit_var canonicalDK;
    static map<GlyphComp::Key,GlyphComp::Val,GlyphComp> glyphCache;
    static Mutex staticMutex;
};



#endif
