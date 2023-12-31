/*$Id: GLTessellator.cc,v 1.1 1999/12/22 16:53:39 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1998-1999 Stephane Rehel
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
#include "Drawing/openGL/GLTessellator.hh"
#include <GL/gl.h>
#include <GL/glu.h>

using namespace FT;

GLTessellator::Handler  GLTessellator::Handler::defaultHandler(false);
GLTessellator::Handler *GLTessellator::Handler::handler;

bool GLTessellator::tessellate(double precision, Handler *handler)
{
  if(!handler) { handler = &Handler::defaultHandler;}
  GLUtriangulatorObj *tobj = gluNewTess();
  if (!tobj || !outline.vectorize(precision))
    {
      gluDeleteTess(tobj);
      return false;
    }
  /*
   * FIXME: add Mutex here
   */
  Handler::handler = handler;
  handler->tessellator = this;

  typedef void (*callback_t)(...);

  gluTessCallback(tobj, GLenum(GLU_BEGIN), reinterpret_cast<callback_t>(&Handler::handleBegin));
  gluTessCallback(tobj, GLenum(GLU_VERTEX), reinterpret_cast<callback_t>(&Handler::handleVertex));
  gluTessCallback(tobj, GLenum(GLU_END), reinterpret_cast<callback_t>(&Handler::handleEnd));
  gluTessCallback(tobj, GLenum(GLU_ERROR), reinterpret_cast<callback_t>(&Handler::handleError));
  bool in_polygon = false;
  bool first_contour = false;
  cout << "Tessellator::polygonize" << endl;
  for(Outline::clist_t::const_iterator i = outline.contours().begin(); i != outline.contours().end(); i++)
    {
      const Outline::Contour &contour = *i;
      cout << "polygonize contour with " << contour.points.size() << " points and area " << contour.area << endl;
      if(contour.exterior())
	{
	  if(in_polygon) gluEndPolygon(tobj);
	  gluBeginPolygon(tobj);
	  in_polygon = true;
	  first_contour = true;
	}
      if(!in_polygon) continue;
      if(first_contour)
	{
	  gluNextContour(tobj, GLenum(GLU_EXTERIOR));
	  first_contour = false;
	}
      else gluNextContour(tobj, GLenum(GLU_INTERIOR));
      for (Outline::Contour::iterator j = contour.points.end() - 1; j >= contour.points.begin(); j--)
	{
	  const Outline::Point &p = *j;
	  GLdouble data[3];
	  data[0]= p.x;
	  data[1]= p.y;
	  data[2]= 0.;
	  gluTessVertex(tobj, data, (void *)&p);
	}
    }
  if(in_polygon) gluEndPolygon(tobj);
  gluDeleteTess(tobj);
  handler->tessellator = 0;
  Handler::handler = 0;
  return true;
}

void GLTessellator::Handler::begin(int type)
{
  glBegin(GLenum(type));
}

void GLTessellator::Handler::vertex(FT::Outline::Point *point)
{
  if(!point) return;
  glVertex2f(point->x, point->y);
}

void GLTessellator::Handler::end()
{
  glEnd();
}

void GLTessellator::Handler::error(int error)
{
  if(!verbose) return;
  const char *str = (const char*)gluErrorString(GLenum(error));
  cerr << "GLU error #" << error << " (" << (str ? str : "") << ")\n";
}

void GLTessellator::Handler::handleBegin(GLenum type)
{
  if (handler) handler->begin(int(type));
}

void GLTessellator::Handler::handleVertex(void *data)
{
  if (handler) handler->vertex(reinterpret_cast<Outline::Point *>(data));
}

void GLTessellator::Handler::handleEnd()
{
  if (handler) handler->end();
}

void GLTessellator::Handler::handleError(GLenum error)
{
  if (handler) handler->error(int(error));
}

