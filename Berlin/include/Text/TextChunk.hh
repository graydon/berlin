#ifndef _TextChunk_hh
#define _TextChunk_hh
//
// $Id: TextChunk.hh,v 1.4 1999/11/30 21:52:35 tobias Exp $
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

// A text chunk is constructed with a "canonical size", which is essentially its
// requisition within the font it was constructed for, on the drawable it was
// intended to be drawn on. This might not actually be the font it winds up
// drawing with, nor the drawable it winds up using, but that's for its
// container to compensate for. At this level, all the chunk needs to do is
// store and request how big it *thinks* it should be, and ignore how large it
// winds up being at draw-time.

#include "Warsaw/config.hh"
#include "Warsaw/Text.hh"
#include "Berlin/GraphicImpl.hh"
#include <Warsaw/Unicode.hh>

declare_corba_ptr_type(DrawingKit)
declare_corba_ptr_type(Font)

class TextChunk : public virtual GraphicImpl {
    public:
    TextChunk(const Unicode::String & u, const Requisition & r);	    
    virtual void draw(DrawTraversal_ptr dt);
    virtual void request(Graphic::Requisition &);

    void getText(Unicode::String &u); 
    unsigned long getLength();
    
    protected:
    Graphic::Requisition myCanonicalSize;
    Unicode::String myText;
};


#endif

