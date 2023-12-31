#ifndef _TextViewer_hh
#define _TextViewer_hh

//
// $Id: Compositor.hh,v 1.1 1999/06/06 05:01:59 gray Exp $
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

#include "Warsaw/config.hh"
#include "Warsaw/Graphic.hh"

class RegionImpl;

declare_corba_ptr_type(Region)
declare_corba_ptr_type(DrawingKit)

// this is a strategy object for adjusting text layouts to compensate for font
// misses or hinting. It plays a very similar role to a LayoutManager.

class Compositor {
    public:
    typedef RegionImpl **Allocations;
    Compositor(Axis a);
    virtual ~Compositor();
    virtual void compose(long n, Graphic_ptr *chunks, DrawingKit_ptr dk, Region_ptr given, Compositor::Allocations result) = 0;
    protected:
    static void setSpan(RegionImpl *r, Axis a, Coord origin, Coord length, Alignment align);
    Axis axis;
};

//
// does no compensation -- to be used when current dk == canonical dk
//

class IdentityCompositor : public Compositor {
    public:
    IdentityCompositor(Axis a);
    virtual ~IdentityCompositor();
    virtual void compose(long n, Graphic_ptr *chunks, DrawingKit_ptr dk, Region_ptr given, Compositor::Allocations result);    
};

#endif
