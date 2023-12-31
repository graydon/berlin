/*$Id: TextChunk.hh,v 1.13 2001/01/09 21:35:08 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _TextChunk_hh
#define _TextChunk_hh

#include <Warsaw/config.hh>
#include <Warsaw/Unicode.hh>
#include <Berlin/GraphicImpl.hh>

//. A text chunk is constructed with a "canonical size", which is essentially its
//. requisition within the font it was constructed for, on the drawable it was
//. intended to be drawn on. This might not actually be the font it winds up
//. drawing with, nor the drawable it winds up using, but that's for its
//. container to compensate for. At this level, all the chunk needs to do is
//. store and request how big it *thinks* it should be, and ignore how large it
//. winds up being at draw-time.
class TextChunk : public virtual GraphicImpl
{
public:
  TextChunk(const Warsaw::Unichar ch, const Warsaw::Graphic::Requisition &);
  virtual void draw(Warsaw::DrawTraversal_ptr);
  virtual void request(Warsaw::Graphic::Requisition &);
  
  void get_text(Babylon::String &); 
  unsigned long get_length();
protected:
  Warsaw::Coord _width, _height;
  Warsaw::Alignment _xalign, _yalign;
  Warsaw::Unichar _char;
};

#endif
