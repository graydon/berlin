//
// $Id: TextKitImpl.cc,v 1.7 1999/11/30 21:52:35 tobias Exp $
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

#include "Text/TextKitImpl.hh"
#include "Warsaw/DrawingKit.hh"
#include "Text/TextChunk.hh"
#include "Text/FontChange.hh"
#include "Berlin/Plugin.hh"
#include <Warsaw/Unicode.hh>
#include <string>

map<GlyphComp::Key,GlyphComp::Val,GlyphComp> TextKitImpl::glyphCache;
Mutex TextKitImpl::staticMutex;


TextKitImpl::TextKitImpl() {}
TextKitImpl::~TextKitImpl() {}

DrawingKit_ptr TextKitImpl::dk() {
    MutexGuard guard(staticMutex);
    return canonicalDK;
}

void TextKitImpl::bind(ServerContext_ptr sc) {
    canonicalDK = DrawingKit::_narrow(sc->getSingleton(interface(DrawingKit)));
}

// we have 1 default font which we're distributing with berlin, the
// fixed-size GNU unifont.

Text::FontDescriptorSeq* TextKitImpl::fonts() {
    Text::FontDescriptorSeq *fdsq = new Text::FontDescriptorSeq();
    fdsq->length(1);
    (*fdsq)[0].pointsize = 16;
    (*fdsq)[0].name = Unicode::toCORBA(Unicode::String("GNU Unifont"));
    return fdsq;
}

Graphic_ptr TextKitImpl::chunk(const Unistring &u, Text::Font_ptr f) {
    GlyphComp::Key k = GlyphComp::Key(u,f);
    if (glyphCache.find(k) == glyphCache.end() ) {
	Graphic::Requisition r;
	f->allocateText(u,r);
	// cerr << " allocated space tor text: " << r.x.natural << "x"
	//      << r.y.natural << endl;
	TextChunk *t = new TextChunk(Unicode::toPrague(u), r);
	t->_obj_is_ready(_boa());
	glyphCache[k] = t->_this();
    }
    return glyphCache[k];
}

Graphic_ptr TextKitImpl::fontChange(const Text::FontDescriptor &fd, const Style::Spec &s) {
    FontChange *fc = new FontChange(fd,s);
    fc->_obj_is_ready(_boa());
    return fc->_this();
}


EXPORT_PLUGIN(TextKitImpl, interface(TextKit))
