/*$Id: FigureKitImpl.hh,v 1.16 2000/12/21 21:05:43 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#ifndef _FigureKitImpl_hh
#define _FigureKitImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/FigureKit.hh>
#include <Warsaw/Figure.hh>
#include <Berlin/KitImpl.hh>
#include <vector>

class FigureKitImpl : public virtual POA_Warsaw::FigureKit,
		      public KitImpl
{
 public:
  FigureKitImpl(KitFactory *, const Warsaw::Kit::PropertySeq &);
  virtual ~FigureKitImpl();
  
  Warsaw::Graphic_ptr root(Warsaw::Graphic_ptr);
  Warsaw::Graphic_ptr fitter(Warsaw::Graphic_ptr);
  Warsaw::Graphic_ptr group();
  Warsaw::Graphic_ptr ugroup();
  
  Figure::Point_ptr point(Warsaw::Coord, Warsaw::Coord);
  Figure::Line_ptr line(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  Figure::Rectangle_ptr rectangle(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  Figure::Circle_ptr circle(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  Figure::Ellipse_ptr ellipse(Warsaw::Coord, Warsaw::Coord, Warsaw::Coord, Warsaw::Coord);
  //   Figures::Path_ptr openBspline(const Figure::Vertices &);
  //   Figures::Path_ptr closedBspline(const Figure::Vertices &);
  Figure::Path_ptr multiline(const Warsaw::Path &);
  Figure::Path_ptr polygon(const Warsaw::Path &);
  
  Warsaw::Image_ptr   pixmap(Warsaw::Raster_ptr);
  Warsaw::Graphic_ptr texture(Warsaw::Graphic_ptr, Warsaw::Raster_ptr);
  Warsaw::Graphic_ptr transformer(Warsaw::Graphic_ptr);
};

#endif
