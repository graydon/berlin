/*$Id: TextChunk.cc,v 1.22 2001/01/09 21:35:10 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
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

#include <Warsaw/config.hh>
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/DrawTraversal.hh>
#include <Warsaw/Region.hh>
#include <Warsaw/IO.hh>
#include "Text/TextChunk.hh"

using namespace Warsaw;

TextChunk::TextChunk(Unichar u, const Warsaw::Graphic::Requisition &r)
  : _width(r.x.natural), _height(r.y.natural), _xalign(r.x.align), _yalign(r.y.align), _char(u)
{
}

void TextChunk::request(Warsaw::Graphic::Requisition &r)
{
  r.x.defined = true;
  r.x.minimum = r.x.natural = r.x.maximum = _width;
  r.x.align   = _xalign;
  r.y.defined = true;
  r.y.minimum = r.y.natural = r.y.maximum = _height;
  r.y.align   = _yalign;
}

void TextChunk::get_text(Babylon::String &u) 
{ 
  u = Babylon::String(_char);
}

unsigned long TextChunk::get_length() 
{ 
  return 1;
}

void TextChunk::draw(DrawTraversal_ptr traversal)
{
  DrawingKit_var drawing = traversal->drawing();
  drawing->draw_char(_char);  
}
