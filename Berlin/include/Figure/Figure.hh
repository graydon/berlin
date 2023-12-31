/*$Id: Figure.hh,v 1.2 1999/05/20 04:53:57 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * http://www.berlin-consortium.org
 *
 * this code is based on code from Fresco.
 * Copyright (c) 1987-91 Stanford University
 * Copyright (c) 1991-94 Silicon Graphics, Inc.
 * Copyright (c) 1993-94 Fujitsu, Ltd.
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

// this file defines some common stuff which all figures have --
// obtaining a pen given the Style::Spec, holding the style, copying
// self to another figureKit.
 
#ifndef _Figure_hh
#define _Figure_hh

#include "Warsaw/config.hh"
#include "Berlin/GraphicImpl.hh"
#include "Warsaw/Style.hh"

class Requisition;
declare_corba_ptr_type(DrawingKit)
declare_corba_ptr_type(FigureKit)
declare_corba_ptr_type(Pencil)

class Figure : virtual public GraphicImpl {

public:
  Figure(const Style::Spec &sty);
  virtual ~Figure();
  virtual Graphic_ptr copyTo(FigureKit_ptr fk) = 0;
  virtual void request(Requisition &r);
  
protected:
  Pencil_ptr getStyledPencil(DrawingKit_ptr dk);
  Style::Spec myStyle;
};


#endif
