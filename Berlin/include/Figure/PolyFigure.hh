/*$Id: PolyFigure.hh,v 1.2 1999/10/13 21:32:31 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _PolyFigure_hh
#define _PolyFigure_hh

#include <Warsaw/config.hh>
#include <Warsaw/Figure.hh>
#include <Berlin/PolyGraphic.hh>
#include <Berlin/ImplVar.hh>

class TransformImpl;

class PolyFigure : implements(Figure), public PolyGraphic
{
public:
    PolyFigure();
    PolyFigure(const PolyFigure &);
    virtual ~PolyFigure();

    virtual void request(Requisition &);
    virtual void extension(const Allocation::Info &, Region_ptr);
    virtual void traverse(Traversal_ptr);
    virtual Transform_ptr transformation();
    virtual void needRedraw();
    virtual void needResize();
    virtual void allocate(Tag, const Allocation::Info &);

    /*
     * shameless hack !!!: eventually these settings are dealt with
     *                     by styles so PolyFigures simply ignore it...
     *                     -stefan
     */
    Mode type() { return 0;}
    void type(Mode) {}
    Color foreground() { return Color();}
    void foreground(const Color &) {}
    Color background() { return Color();}
    void background(const Color &) {}
    virtual void resize() {}

protected:
    void updateBbox();
    Impl_var<TransformImpl> tx;
    Impl_var<RegionImpl> bbox;
};

class UPolyFigure : public PolyFigure
{
public:
  UPolyFigure() {}
  UPolyFigure(const UPolyFigure &);
  virtual void traverse(Traversal_ptr);
};

#endif /* _PolyFigure_hh */
