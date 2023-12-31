/*$Id: SimpleFigures.hh,v 1.2 1999/06/15 20:54:19 gray Exp $
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

#ifndef _SimpleFigures_hh
#define _SimpleFigures_hh

#include "Warsaw/config.hh"
#include "Warsaw/Types.hh"
#include "Figure/Figure.hh"

class RectFig : public virtual Figure
{
public:
  RectFig(Coord, Coord, const Style::Spec &);    
  virtual ~RectFig();
  virtual void request(Requisition &);
  virtual void draw(DrawTraversal_ptr);
  virtual Graphic_ptr copyTo(FigureKit_ptr fk);
private:
  Coord width, height;
};

class EllipseFig : public virtual Figure  {
public:
  EllipseFig(const Style::Spec &);    
  virtual ~EllipseFig();
  virtual void draw(DrawTraversal_ptr);
  virtual Graphic_ptr copyTo(FigureKit_ptr fk);
};

class PathFig : public virtual Figure  {
public:
  PathFig(const Style::Spec &, const Path &);    
  virtual ~PathFig();
  virtual void draw(DrawTraversal_ptr);
  virtual Graphic_ptr copyTo(FigureKit_ptr fk);
protected:
  Path myPath;
};

class PatchFig : public virtual Figure  {
public:
  PatchFig(const Style::Spec &, const Patch &);    
  virtual ~PatchFig();
  virtual void draw(DrawTraversal_ptr);
  virtual Graphic_ptr copyTo(FigureKit_ptr fk);
protected:
  Patch myPatch;
};

#endif
