//
// $Id: FontChange.cc,v 1.4 1999/11/06 20:23:08 stefan Exp $
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

#include "Text/FontChange.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Berlin/Logger.hh"

FontChange::FontChange(const Text::FontDescriptor & f, const Style::Spec &sty) :
  myFontDescriptor(f), myStyle(sty) {
}

void FontChange::request(Requisition &r) {
  r.x.defined = false;
  r.y.defined = false;
}


void FontChange::draw(DrawTraversal_ptr dt) {
    SectionLog section("FontChange::Draw");
    DrawingKit_ptr dk = dt->kit();
    try {
	dk->setFont(myFontDescriptor, myStyle);
    } catch (Text::NoSuchFontException &nsfe) {
	//	Logger::log(Logger::text) << "Unable to load font \"" << myFontDescriptor.name << "\"";
    }
}
