/*$Id: GLDrawingKit.cc,v 1.73 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Warsaw/IO.hh>
#include "Drawing/openGL/GLQuadric.hh"
#include "Drawing/openGL/GLDrawingKit.hh"
#include "Drawing/openGL/GLUnifont.hh"
#include <Berlin/Logger.hh>
#include <Berlin/Color.hh>
#include <Prague/Sys/Profiler.hh>

#include <strstream>
#include <iostream>

using namespace Prague;
using namespace Warsaw;

GLDrawingKit::GLDrawingKit(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p),
    drawable(Console::drawable()),
    tx(0),
    font(new GLUnifont),
    textures(100),
    images(500)
{
    
#if defined(CONSOLE_GGI)
  context = GGIMesaCreateContext();
  if (!context)
    {
      cerr << "GGIMesaCreateContext() failed" << endl;
      exit(4);
    }
  /*
   * if we made it up to here the console is GGI, so let's use inside info
   */
  GGIDrawable &ggi = drawable->impl();
  if (GGIMesaSetVisual(context, ggi.visual(), GL_TRUE, GL_FALSE))
    {
      cerr << "GGIMesaSetVisual() failed" << endl;
      exit(7);
    }
  GGIMesaMakeCurrent(context);
  
#elif defined(CONSOLE_GLUT)
  
  // Do GLUT-specific stuff
  
#else
#  error "GLDrawingKit cannot be used with this console."
#endif
  
  glViewport(0, 0, drawable->width(), drawable->height());
  glMatrixMode(GL_PROJECTION); 
  glLoadIdentity();
  glOrtho(0, drawable->width()/drawable->resolution(xaxis), drawable->height()/drawable->resolution(yaxis), 0, -1000.0, 1000.0); 
  glTranslatef(0.375, 0.375, 0.);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glShadeModel(GL_SMOOTH);
  glDisable(GL_LIGHTING);  
  glFrontFace(GL_CW);
  glEnable(GL_ALPHA_TEST);
  glEnable(GL_SCISSOR_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

}

GLDrawingKit::~GLDrawingKit()
{
  delete font;
#if defined(CONSOLE_GGI)
  GGIMesaDestroyContext(context);
#endif
}

void GLDrawingKit::set_transformation(Transform_ptr t)
{
  static GLdouble identity[16] = {1., 0., 0., 0.,
				  0., 1., 0., 0.,
				  0., 0., 1., 0.,
				  0., 0., 0., 1.};
  if (CORBA::is_nil(t)) glLoadMatrixd(identity);
  else
    {
      tr = Transform::_duplicate(t);
      Transform::Matrix matrix;
      tr->store_matrix(matrix);
      GLdouble glmatrix[16] = {matrix[0][0], matrix[1][0], matrix[2][0], matrix[3][0],
			       matrix[0][1], matrix[1][1], matrix[2][1], matrix[3][1],
			       matrix[0][2], matrix[1][2], matrix[2][2], matrix[3][2],
			       matrix[0][3], matrix[1][3], matrix[2][3], matrix[3][3]};
      glLoadMatrixd(glmatrix);
    }
}

void GLDrawingKit::set_clipping(Region_ptr r)
{
  cl = Region::_duplicate(r);
  if (CORBA::is_nil(cl)) glScissor(0, 0, drawable->width(), drawable->height());
  else
    {
      Vertex lower, upper;
      cl->bounds(lower, upper);
      PixelCoord x = static_cast<PixelCoord>(lower.x*drawable->resolution(xaxis) + 0.5);
      PixelCoord y = static_cast<PixelCoord>((drawable->height()/drawable->resolution(yaxis) - upper.y)*drawable->resolution(yaxis) + 0.5);
      PixelCoord w = static_cast<PixelCoord>((upper.x - lower.x)*drawable->resolution(xaxis) + 0.5);
      PixelCoord h = static_cast<PixelCoord>((upper.y - lower.y)*drawable->resolution(yaxis) + 0.5);
      glScissor(x, y, w, h);
#if 0 // uncomment this if you want to debug the repairing of regions
      glColor4d(1., 0., 0., 1.);
      glRectf(0, 0, 10000., 10000.);
      glFlush();
      Console::drawable()->flush();
      sleep(1);
#endif
    }
}

void GLDrawingKit::set_foreground(const Color &c)
{
  fg = c;
  glColor4d(lt.red * fg.red, lt.green * fg.green, lt.blue * fg.blue, fg.alpha);
}

void GLDrawingKit::set_lighting(const Color &c)
{
  lt = c;
  glColor4d(lt.red * fg.red, lt.green * fg.green, lt.blue * fg.blue, fg.alpha);
}

void GLDrawingKit::set_point_size(Coord s)
{
  ps = s;
  // FIXME !: glPointSize uses pixel units !
  glPointSize(ps);
}

void GLDrawingKit::set_line_width(Coord w)
{
  lw = w;
  // FIXME !: glLineWidth uses pixel units !
  glLineWidth(lw);
}

void GLDrawingKit::set_line_endstyle(Warsaw::DrawingKit::Endstyle style)
{
  es = style;
}

void GLDrawingKit::set_surface_fillstyle(Warsaw::DrawingKit::Fillstyle style)
{
  if (fs == Warsaw::DrawingKit::textured) glDisable(GL_TEXTURE_2D);
  fs = style;
  if (fs == Warsaw::DrawingKit::textured) glEnable(GL_TEXTURE_2D);
}

void GLDrawingKit::set_texture(Raster_ptr t)
{
  tx = CORBA::is_nil(t) ? 0 : textures.lookup(Raster::_duplicate(t));
  if (tx) glBindTexture(GL_TEXTURE_2D, tx->texture);
}

// void GLDrawingKit::clear(Coord l, Coord t, Coord r, Coord b)
// {
//   glColor4d(0., 0., 0., 1.);
//   glRectf(l, t, r, b);
// }

void GLDrawingKit::draw_path(const Path &path)
{
  if (fs == Warsaw::DrawingKit::solid)
    {
//       glBegin(GL_TRIANGLE_FAN);
      glBegin(GL_LINE_LOOP);
      for (unsigned long i = 0; i < path.length() - 1; i++) glVertex3f(path[i].x, path[i].y, path[i].z);
      glEnd();
    }
  else if (fs == Warsaw::DrawingKit::textured)
    {
//       glBegin(GL_TRIANGLE_FAN);
      glBegin(GL_LINE_LOOP);
      for (unsigned long i = 0; i < path.length() - 1; i++)
	{
	  glTexCoord2f(path[i].x * tx->width * 10., path[i].y * tx->height * 10.); 
	  glVertex3f(path[i].x, path[i].y, path[i].z);
	}
      glEnd();
    }
  else
    {
//       glBegin(GL_TRIANGLE_FAN);
      glBegin(GL_LINE_LOOP);
      // line strips (no final connecting line)      
      for (unsigned long i = 0; i < path.length() - 1; i++) glVertex3f(path[i].x, path[i].y, path[i].z);
      glEnd();
    }
}

void GLDrawingKit::draw_rectangle(const Vertex &lower, const Vertex &upper)
{
  if (fs == Warsaw::DrawingKit::solid) glRectf(lower.x, lower.y, upper.x, upper.y);
  else if (fs == Warsaw::DrawingKit::textured)
    {
      double w = (upper.x - lower.x)/(tx->width * 10.);
      double h = (upper.y - lower.y)/(tx->height * 10.);
      glBegin(GL_POLYGON);
      glTexCoord2f(0., 0.); glVertex2d(lower.x, lower.y);
      glTexCoord2f(w, 0.);  glVertex2d(upper.x, lower.y);
      glTexCoord2f(w, h);   glVertex2d(upper.x, upper.y);
      glTexCoord2f(0., h);  glVertex2d(lower.x, upper.y);
      glEnd();
    }
  else
    {
      glBegin(GL_LINE_LOOP);
      glVertex2d(lower.x, lower.y);
      glVertex2d(upper.x, lower.y);
      glVertex2d(upper.x, upper.y);
      glVertex2d(lower.x, upper.y);
      glEnd();
    }
}

void GLDrawingKit::draw_quadric(const Warsaw::DrawingKit::Quadric, Warsaw::Coord, Warsaw::Coord)
{
}

void GLDrawingKit::draw_ellipse(const Vertex &lower, const Vertex &upper)
{
  glPushMatrix();
  glScaled(upper.x - lower.x, upper.y - lower.y, upper.z - lower.z);
  glTranslated(lower.x, lower.y, lower.z);
  GLQuadric quadric(fs, GLQuadric::out);
  quadric.disk(0., 1., 360., 100.);
  glPopMatrix();
}

void GLDrawingKit::draw_image(Raster_ptr raster)
{
  Profiler prf("GLDrawingKit::draw_image");
  GLImage *glimage = images.lookup(Raster::_duplicate(raster));
  GLint tbackup = -1;
  if (fs == Warsaw::DrawingKit::textured) glGetIntegerv(GL_TEXTURE_BINDING_2D, &tbackup);
  else glEnable(GL_TEXTURE_2D);
  GLfloat color_cache[4];
  glGetFloatv(GL_CURRENT_COLOR, color_cache);
  glBindTexture(GL_TEXTURE_2D, glimage->texture);
  glColor4f(lt.red, lt.green, lt.blue, color_cache[3]); // use the current lighting
  glBegin(GL_POLYGON);
  Path path;
  path.length(4);
  Coord width = glimage->width*10.;
  Coord height = glimage->height*10.;
  path[0].x = path[0].y = path[0].z = 0.;
  path[1].x = width, path[1].y = path[1].z = 0.;
  path[2].x = width, path[2].y = height, path[2].z = 0.;
  path[3].x = 0, path[3].y = height, path[3].z = 0.;
  glTexCoord2f(0., 0.);                   glVertex3f(path[3].x, path[3].y, path[3].z);
  glTexCoord2f(glimage->s, 0.);           glVertex3f(path[2].x, path[2].y, path[2].z);
  glTexCoord2f(glimage->s, glimage->t);   glVertex3f(path[1].x, path[1].y, path[1].z);
  glTexCoord2f(0., glimage->t);           glVertex3f(path[0].x, path[0].y, path[0].z);
  glEnd();
  glColor4fv(color_cache);  
  if (fs != Warsaw::DrawingKit::textured) glDisable(GL_TEXTURE_2D);
  else glBindTexture(GL_TEXTURE_2D, tbackup);
}

void GLDrawingKit::set_font_size(CORBA::ULong s) {}
void GLDrawingKit::set_font_weight(CORBA::ULong w) {}
void GLDrawingKit::set_font_family(const Unistring &f) {}
void GLDrawingKit::set_font_subfamily(const Unistring &sf) {}
void GLDrawingKit::set_font_fullname(const Unistring &fn) {}
void GLDrawingKit::set_font_style(const Unistring &s) {}
void GLDrawingKit::set_font_attribute(const NVPair & nvp) {}
void GLDrawingKit::allocate_text(const Unistring &s, Graphic::Requisition &req) {}
void GLDrawingKit::draw_text(const Unistring &us) {}
void GLDrawingKit::allocate_char(Unichar c, Graphic::Requisition &req) { font->allocateChar(c, req);}
void GLDrawingKit::draw_char(Unichar c) { font->drawChar(c);}
void GLDrawingKit::copy_drawable(Drawable_ptr d, PixelCoord x, PixelCoord y, PixelCoord w, PixelCoord h) {}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "GLDrawingKit"};
  return new KitFactoryImpl<GLDrawingKit> ("IDL:Warsaw/DrawingKit:1.0", properties, 1);
}

