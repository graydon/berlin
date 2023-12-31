/*$Id: GLPencil.cc,v 1.12 1999/11/10 21:57:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
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

#include "Drawing/openGL/GLPencil.hh"
#include <iostream>

GLPencil::GLPencil(const Style::Spec &sty, GLDrawable *d)
  : myDrawable(d)
{

  // init default values
  //   GLfloat defaultColor[4] = {1.0, 1.0, 1.0, 1.0};
  //   myFillColor = myLineColor = defaultColor;
  myFillMode = Style::solid;
  myThickness = 1.0;

  // !!!FIXME!!! this part is incomplete. at the moment, it only 
  // interprets a couple style parameters and ignores the rest. 
  // please, some GL hackers, set it the way it should be :)
  
  for (unsigned long i = 0; i < sty.length(); i++) {
    Color *tmp;
    switch (sty[i].a) {
    case Style::fillcolor:
      sty[i].val >>= tmp;
      //       cerr << "selected fill color" << tmp->red << tmp->green << tmp->blue << tmp->alpha << endl;
      myFillColor[0] = tmp->red;
      myFillColor[1] = tmp->green;
      myFillColor[2] = tmp->blue;
      myFillColor[3] = tmp->alpha;
      break;
      
    case Style::linecolor:
      sty[i].val >>= tmp;
      myLineColor[0] = tmp->red;
      myLineColor[1] = tmp->green;
      myLineColor[2] = tmp->blue;
      myLineColor[3] = tmp->alpha;
      break;

    case Style::fill:
      sty[i].val >>= myFillMode; break;
    case Style::linethickness:
      sty[i].val >>= myThickness; break;
    case Style::ends:
    case Style::texture:
    case Style::linegradient:
    case Style::fillgradient:
      break;
    }
  }
};


void GLPencil::drawPath(const Path &p)
{
  myDrawable->makeCurrent();
  glLineWidth(myThickness);
  
  if (p.m.length() == 0)
    {
      // we're drawing polys
      if (myFillMode == Style::solid)
	{
	  glClearColor(0.0,0.0,0.0,0.0);
	  glBegin(GL_POLYGON);
	  // filled polys
	  glColor4d(myFillColor[0],myFillColor[1],myFillColor[2],myFillColor[3]);      
	  for (unsigned long i = 0; i < p.p.length(); i++)
	    glVertex3f(p.p[i].x, p.p[i].y, p.p[i].z);
	  glEnd();
	}
      else
	{ // for the time being there's only solid and nofill
	  glClearColor(0.0,0.0,0.0,0.0);
	  glBegin(GL_LINE_STRIP);
	  // line strips (no final connecting line)      
	  glColor4d(myLineColor[0],myLineColor[1],myLineColor[2],myLineColor[3]);      
	  for (unsigned long i = 0; i < p.p.length(); i++)
	    glVertex3f(p.p[i].x, p.p[i].y, p.p[i].z);
	  glEnd();
	}
      // !!FIXME!! fill in evaluator setup / teardown code for curves
    }
}

void GLPencil::drawPatch(const Patch &)
{
  // to be completed
}

void GLPencil::drawRect(const Vertex &lower, const Vertex &upper)
{
  myDrawable->makeCurrent();
  glLineWidth(myThickness);  
  if (myFillMode == Style::solid)
    {
      glClearColor(0.0,0.0,0.0,0.0);
      glColor4d(myFillColor[0],myFillColor[1],myFillColor[2],myFillColor[3]);      
      glRectf(lower.x,lower.y,upper.x,upper.y);
    }
  else
    {
      glClearColor(0.0,0.0,0.0,0.0);
      glColor4d(myLineColor[0],myLineColor[1],myLineColor[2],myLineColor[3]);      
      glBegin(GL_LINE_LOOP);
      glVertex3d(lower.x,lower.y,0);
      glVertex3d(upper.x,lower.y,0);
      glVertex3d(upper.x,upper.y,0);
      glVertex3d(lower.x,upper.y,0);
      glEnd();
    }     
}

void GLPencil::drawEllipse(const Vertex &lower, const Vertex &upper)
{
  // !!!FIXME!!! quadrics code here
}


