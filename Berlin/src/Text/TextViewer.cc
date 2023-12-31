/*$Id: TextViewer.cc,v 1.23 2001/04/18 06:07:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 1999 Stefan Seefeld <stefan@berlin-consortium.org> 
 * Copyright (C) 2000 Nathaniel Smith <njs@berlin-consortium.org>
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
#include <Warsaw/TextBuffer.hh>
#include "Text/TextViewer.hh"
#include <Prague/Sys/Tracer.hh>
#include <algorithm>

using namespace Prague;
using namespace Warsaw;

TextViewer::TextViewer(TextBuffer_ptr txt, TextKit_ptr tk, DrawingKit_ptr dk, Compositor *c)
  : Composition(dk, c),
    _kit(TextKit::_duplicate(tk)),
    _buffer(TextBuffer::_duplicate(txt))
{
  Trace trace("TextViewer::TextViewer");
}

TextViewer::~TextViewer() {}

void TextViewer::update(const CORBA::Any &a)
{
  Trace trace1("TextViewer::update");
  TextBuffer::Change *ch;  
  if (a >>= ch)
    {
      switch (ch->type)
	{
	case TextBuffer::insert:
	  {
	    Trace trace2("TextViewer::update - insert");
	    Prague::Guard<Mutex> guard(_mutex);
	    Unistring_var us = _buffer->get_chars(ch->pos, (CORBA::ULong)ch->len);
	    CORBA::ULong len = us->length();
	    for (unsigned long i = 0; i < len; i++)
	      {
		Graphic_var child = _kit->glyph(us[i]);
		/*
		 * FIXME: rewrite that with iterators
		 */
		Edge edge;
		edge.peer = Warsaw::Graphic::_duplicate(child);
		edge.localId = unique_child_id();
		edge.peerId = child->add_parent_graphic(Graphic_var(_this()), edge.localId);
		_children.insert(_children.begin() + ch->pos + i, edge);
	      }
	  }
	  break;
	  
	case TextBuffer::remove:
	  {
	    Trace trace2("TextViewer::update - remove");
	    Prague::Guard<Mutex> guard(_mutex);
	    /*
	     * FIXME: rewrite that with iterators
	     */
	    unsigned long start = std::min(ch->pos, static_cast<CORBA::ULong>(_children.size()));
	    unsigned long end = std::min(ch->pos + ch->len, static_cast<CORBA::ULong>(_children.size()));
	    if (ch->len < 0) std::swap(start, end);
	    for (glist_t::iterator i = _children.begin() + start; i != _children.begin() + end; i++)
	      (*i).peer->remove_parent_graphic((*i).peerId);
	    _children.erase(_children.begin() + start, _children.begin() + end);
	  }
	  break;
	case TextBuffer::cursor:
	  {
	    Trace trace2("TextViewer::update - cursor");
	    // we'll do some cursor-ish stuff someday
	  }
	  break;
	}
      //   need_redraw();
      need_resize();
    }
}
    
void TextViewer::activate_composite()
{
  Trace trace("TextViewer::activate_composite");
  Composition::activate_composite();
  Prague::Guard<Mutex> guard(_mutex);
  Unistring_var us = _buffer->value();
  CORBA::ULong len = us->length();
  for (unsigned long i = 0; i < len; i++)
    {
      Graphic_var child = _kit->glyph(us[i]);
      Edge edge;
      edge.peer = Warsaw::Graphic::_duplicate(child);
      edge.localId = unique_child_id();
      edge.peerId = child->add_parent_graphic(Graphic_var(_this()), edge.localId);
      _children.insert(_children.begin() + i, edge);
    }
}

