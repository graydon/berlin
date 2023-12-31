/*$Id: GLDrawable.cc,v 1.24 1999/11/11 14:14:06 stefan Exp $
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
#include "Drawing/openGL/GLDrawable.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/ImplVar.hh"
extern "C" {
#include "ggi/ggi.h"
}
#include <iostream>

GLDrawable::GLDrawable()
{
  context = GGIMesaCreateContext();
  if (!context)
    {
      cerr << "GGIMesaCreateContext() failed" << endl;
      exit(4);
    }
  // Configure the mode struct.
  mode.visible.x = mode.visible.y = GGI_AUTO;
  mode.virt.x = mode.virt.y = GGI_AUTO;
  mode.size.x = 768;
  mode.size.y = 1000;
  mode.dpp.x = mode.dpp.y = 1;
  mode.graphtype = GT_AUTO;
  mode.frames = 1;
  // Open the default visual --
  visual = ggiOpen(0);
  if (!visual) { cerr << "ggiOpen(NULL) failed!" << endl; exit(5);} // exit code 5 -- can't acquire a visual
  
  // We've acquired a visual, now let's decide on a mode. See libggi docs
  // on the format of the environment variable GGI_DEFMODE, which we use to
  // get all of our mode preferences.
  
  if(ggiCheckMode(visual, &mode) == 0)
    {
      
      // The mode works! We try to set it....
      if(ggiSetMode(visual, &mode) != 0)
	{
	  cerr << "Cannot set visual, even though GGI says it's ok???\n";
	  exit(6);
	  // Cannot set the mode. Strange... Exit code 6, cannot find
	  // a suitable mode for the visual
	}
      //       cerr << "Successfully set the mode on our visual!\n";
    }
  else
    {
      cerr << "GGI says our mode won't work. Trying the one it suggests...\n";
      // CheckMode said our mode wouldn't work.
      // CheckMode should have modified our mode, so we try again...
      if(ggiCheckMode(visual, &mode) != 0)
	{
	  cerr << "What?? GGI doesn't like its own suggestion. Bailing.\n";
	  // Hmm. internal GGI problem. The mode GGI gave us still won't work.
	  exit( 6 );
	}
      else
	{
	  cerr << "Ahh, GGI likes its own suggestion; trying to set the suggested mode.\n";
	  // ggiCheckMode worked this time, on the mode it gave us last time.
	  // Try to set the mode.
	  if(ggiSetMode(visual, &mode) != 0)
	    {
	      cerr << "Huh?? Still can't set the mode. Bailing.\n";
	      exit( 6 );
	      // What?? after all this, GGI _STILL_ won't set the mode for us?
	      // If we get here GGI is having some serious trouble
	    }
	}
    }
  // If we get here, we've successfully set a mode from GGI_DEFMODE.
  // I know, I'm paranoid, but this implementation will save trouble in the
  // long run. --Aaron

  ggiAddFlags(visual, GGIFLAG_ASYNC);
    
  if (GGIMesaSetVisual(context, visual, GL_TRUE, GL_FALSE))
    {
      cerr << "GGIMesaSetVisual() failed" << endl;
      exit( 7 );
      // exit code 7. Cannot set visual for GGIMesa.
    }
  GGIMesaMakeCurrent(context);
  reshape( mode.visible.x, mode.visible.y );

  // initialize some friendly OpenGL states
//    static GLfloat white[4] = {0.1, 0.1, 0.1, 0.1 };
//    glEnable(GL_LIGHTING);   
//    glLightfv( GL_LIGHT0, GL_AMBIENT, white ); 
//    glEnable(GL_LIGHT0);   
//    glClear( GL_COLOR_BUFFER_BIT );
   glShadeModel(GL_SMOOTH);
    //glDisable(GL_DEPTH_TEST);
//    glDisable(GL_ALPHA_TEST);
    glDisable( GL_LIGHTING );  
  //  glEnable( GL_CULL_FACE ); 
  // glEnable( GL_DEPTH_TEST ); 
   glFrontFace(GL_CW); 
//   glShadeModel(GL_FLAT);
   glEnable(GL_ALPHA_TEST);
   glEnable(GL_SCISSOR_TEST);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   //   glDrawBuffer(GL_BACK);
}


// this is just a utility function for reshaping.
void GLDrawable::reshape( int width, int height )
{
  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION); 
  glLoadIdentity();
  glOrtho(0, width, height, 0, -1000.0, 1000.0); 
  glTranslatef(0.375,0.375,0.);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
}


GLDrawable::~GLDrawable()
{
  while (clipping.size())
    {
      RegionImpl *c = clipping.top();
      c->_dispose();
      clipping.pop();
    }
  GGIMesaDestroyContext(context);
}

Coord GLDrawable::dpi(Axis axis)
{
  return 72.;
}

Coord GLDrawable::toCoord(PixelCoord p, Axis axis) { return p/dpi(axis);}
PixelCoord GLDrawable::toPixels(Coord c, Axis axis) { return static_cast<long>(c*dpi(axis));}

void GLDrawable::pushTransform(Transform_ptr transform)
{
  glPushMatrix();
  if (!CORBA::is_nil(transform))
    {
      Transform::Matrix matrix;
      transform->storeMatrix(matrix);
      GLdouble glmatrix[16] = {matrix[0][0], matrix[1][0], matrix[2][0], matrix[3][0],
			       matrix[0][1], matrix[1][1], matrix[2][1], matrix[3][1],
			       matrix[0][2], matrix[1][2], matrix[2][2], matrix[3][2],
			       matrix[0][3], matrix[1][3], matrix[2][3], matrix[3][3]};
      glMultMatrixd(glmatrix);
    }
}

void GLDrawable::popTransform()
{
  glPopMatrix();
}

void GLDrawable::pushClipping(Region_ptr region, Transform_ptr transformation)
{
  Impl_var<RegionImpl> clip(new RegionImpl(region, Transform_var(Transform::_nil())));
  makeCurrent();
  PixelCoord x, y, w, h;
  x = (long)(clip->lower.x + 0.5);
  y = (long)(height() - clip->upper.y + 0.5);
  w = (long)(clip->upper.x - clip->lower.x + 0.5);
  h = (long)(clip->upper.y - clip->lower.y + 0.5);
  glScissor((GLint)x,(GLint)y,(GLsizei)w,(GLsizei)h);
}

void GLDrawable::popClipping()
{
  glScissor(0, 0, static_cast<GLint>(width()), static_cast<GLint>(height()));
}
