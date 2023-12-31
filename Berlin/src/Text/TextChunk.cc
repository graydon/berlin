//
// $Id: TextChunk.cc,v 1.10 1999/11/30 21:52:35 tobias Exp $
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

#include "Text/TextChunk.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/Region.hh"
#include "Warsaw/Transform.hh"
#include "Berlin/Logger.hh"

TextChunk::TextChunk(const Unicode::String & u, const Requisition &r) : 
    myCanonicalSize(r), myText(u)  {
}

void TextChunk::request(Graphic::Requisition &r) {
    r = myCanonicalSize;
}

void TextChunk::getText(Unicode::String &u) {
  myText = u;
}

unsigned long TextChunk::getLength() {
    return myText.length();
}

void TextChunk::draw(DrawTraversal_ptr dt) {
SectionLog section("TextChunk::draw");
    Region_var allocation = dt->allocation();
    DrawingKit_ptr dk = dt->kit();
    Text::Font_var f = dk->currentFont();
    Vertex l,u;
    allocation->bounds(l,u);
    u.x = l.x;
//     Transform_var transform = dt->transformation();
//     transform->transformVertex(u);
    f->drawText(Unicode::toCORBA(myText), u);
}
