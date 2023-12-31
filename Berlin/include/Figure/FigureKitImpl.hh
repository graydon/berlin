/*$Id: FigureKitImpl.hh,v 1.8 1999/10/04 22:57:10 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#ifndef _FigureKitImpl_hh
#define _FigureKitImpl_hh

#include "Warsaw/FigureKit.hh"
#include "Warsaw/config.hh"
#include "Berlin/CloneableImpl.hh"
#include <vector>

class Figure;

class FigureKitImpl : lcimplements(FigureKit), virtual public CloneableImpl {
 public:
    FigureKitImpl();
    virtual ~FigureKitImpl();

    Graphic_ptr root(Graphic_ptr);
    Graphic_ptr fitter(Graphic_ptr);
    Graphic_ptr group();
    Graphic_ptr ugroup();

    Figures::Point_ptr point(Coord, Coord);
    Figures::Line_ptr line(Coord, Coord, Coord, Coord);
    Figures::Rectangle_ptr rectangle(Coord, Coord, Coord, Coord);
    Figures::Circle_ptr circle(Coord, Coord, Coord);
    Figures::Ellipse_ptr ellipse(Coord, Coord, Coord, Coord);
    //   Figures::Path_ptr openBspline(const Figure::Vertices &);
    //   Figures::Path_ptr closedBspline(const Figure::Vertices &);
    Figures::Path_ptr multiline(const Figure::Vertices &);
    Figures::Path_ptr polygon(const Figure::Vertices &);

    Image_ptr   pixmap(Raster_ptr);
    Transformator_ptr projection(Graphic_ptr);
 protected:
    vector<Figure *> figures;
};


#endif
