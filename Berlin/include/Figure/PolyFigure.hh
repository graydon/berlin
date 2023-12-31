/*$Id: PolyFigure.hh,v 1.7 2000/10/31 18:15:28 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _PolyFigure_hh
#define _PolyFigure_hh

#include <Warsaw/config.hh>
#include <Warsaw/Figure.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/PolyGraphic.hh>

class TransformImpl;

class PolyFigure : public virtual POA_Figure::FigureBase,
		   public PolyGraphic
{
public:
    PolyFigure();
    PolyFigure(const PolyFigure &);
    virtual ~PolyFigure();

    virtual void request(Warsaw::Graphic::Requisition &);
    virtual void extension(const Warsaw::Allocation::Info &, Warsaw::Region_ptr);
    virtual void traverse(Warsaw::Traversal_ptr);
    virtual Warsaw::Transform_ptr transformation();
    virtual void need_redraw();
    virtual void need_resize();
    virtual void allocate(Warsaw::Tag, const Warsaw::Allocation::Info &);

    /*
     * shameless hack !!!: eventually these settings are dealt with
     *                     by styles so PolyFigures simply ignore it...
     *                     -stefan
     */
    Figure::Mode type() { return 0;}
    void type(Figure::Mode) {}
    Warsaw::Color foreground() { return Warsaw::Color();}
    void foreground(const Warsaw::Color &) {}
    Warsaw::Color background() { return Warsaw::Color();}
    void background(const Warsaw::Color &) {}
    virtual void resize() {}

protected:
    void update_bbox();
    Impl_var<TransformImpl> _tx;
    Impl_var<RegionImpl>    _bbox;
};

class UPolyFigure : public PolyFigure
{
public:
  UPolyFigure() {}
  UPolyFigure(const UPolyFigure &);
  virtual void traverse(Warsaw::Traversal_ptr);
};

#endif
