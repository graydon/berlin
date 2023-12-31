/*$Id: SimpleFigures.cc,v 1.6 1999/08/26 13:55:41 gray Exp $
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


#include "Figure/SimpleFigures.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/DrawTraversal.hh"
#include "Warsaw/Pencil.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/Vertex.hh"
#include "Warsaw/FigureKit.hh"
#include "Warsaw/Transform.hh"
#include <iostream>

// rectangles

RectFig::RectFig(Coord w, Coord h, const Style::Spec &sty) : Figure(sty), width(w), height(h) {}
RectFig::~RectFig() {}

void RectFig::request(Requisition &requisition)
{
  requisition.x.defined = true;
  requisition.x.natural = requisition.x.maximum = requisition.x.minimum = width;
  requisition.x.align = 0.;
  requisition.y.defined = true;
  requisition.y.natural = requisition.y.maximum = requisition.y.minimum = height;
  requisition.y.align = 0.;
}

void RectFig::draw(DrawTraversal_ptr traversal)
{
  DrawingKit_var dk = traversal->kit();
  Pencil_var pen = getStyledPencil(dk);
  Path path;
  path.p.length(5);
  path.p[0].x = path.p[0].y = path.p[0].z = 0;
  path.p[2].x = width, path.p[2].y = height, path.p[2].z = 0;
  path.p[1].x = path.p[2].x, path.p[1].y = path.p[0].y, path.p[1].z = 0.;
  path.p[3].x = path.p[0].x, path.p[3].y = path.p[2].y, path.p[3].z = 0.;
  path.p[4] = path.p[0];
  Transform_var transform = traversal->transformation();
  for (unsigned int i = 0; i != path.p.length(); i++) transform->transformVertex(path.p[i]);
  pen->drawPath(path);
}

Graphic_ptr RectFig::copyTo(FigureKit_ptr fk) {return fk->rectangle(width, height, myStyle);}


// ellipses

void EllipseFig::draw(DrawTraversal_ptr dt)
{
  DrawingKit_ptr dk = dt->kit();
  Pencil_ptr p = getStyledPencil(dk);
  RegionImpl region(dt->allocation(), dt->transformation());
  p->drawEllipse(region.upper, region.lower);    
}

Graphic_ptr EllipseFig::copyTo(FigureKit_ptr fk) {return fk->ellipse(myStyle);}
EllipseFig::EllipseFig(const Style::Spec &sty) : Figure(sty) {}
EllipseFig::~EllipseFig() {}


// paths

void PathFig::draw(DrawTraversal_ptr dt) {
  DrawingKit_ptr dk = dt->kit();
  Pencil_ptr p = getStyledPencil(dk);
  RegionImpl region(dt->allocation(), dt->transformation());

  Path pathToDraw;
  pathToDraw.p.length(myPath.p.length());
  
  // stretch path to fit region
  Coord dx = region.upper.x - region.lower.x;
  Coord dy = region.upper.y - region.lower.y;
  Coord dz = region.upper.z - region.lower.z;

  for (unsigned long i = 0; i < myPath.p.length(); i++) {
    pathToDraw.p[i].x = myPath.p[i].x * dx;
    pathToDraw.p[i].y = myPath.p[i].y * dy;
    pathToDraw.p[i].z = myPath.p[i].z * dz;
  }
  pathToDraw.m = myPath.m;    
  p->drawPath(pathToDraw);    
}

Graphic_ptr PathFig::copyTo(FigureKit_ptr fk) {return fk->path(myStyle,myPath);}
PathFig::PathFig(const Style::Spec &sty, const Path &p) : Figure(sty), myPath(p) {}
PathFig::~PathFig() {}

// patches

void PatchFig::draw(DrawTraversal_ptr dt) {
  DrawingKit_ptr dk = dt->kit();
  Pencil_ptr p = getStyledPencil(dk);
  RegionImpl region(dt->allocation(), dt->transformation());

  Patch patchToDraw;
  patchToDraw.p.length(myPatch.p.length());
  
  // stretch patch to fit region
  Coord dx = region.upper.x - region.lower.x;
  Coord dy = region.upper.y - region.lower.y;
  Coord dz = region.upper.z - region.lower.z;

  for (unsigned long i = 0; i < myPatch.p.length(); i++) {
    patchToDraw.p[i].x = myPatch.p[i].x * dx;
    patchToDraw.p[i].y = myPatch.p[i].y * dy;
    patchToDraw.p[i].z = myPatch.p[i].z * dz;
  }
  patchToDraw.m = myPatch.m;    

  p->drawPatch(patchToDraw);    
}

Graphic_ptr PatchFig::copyTo(FigureKit_ptr fk) {return fk->patch(myStyle,myPatch);}
PatchFig::PatchFig(const Style::Spec &sty, const Patch &p) : Figure(sty), myPatch(p) {}
PatchFig::~PatchFig() {}
