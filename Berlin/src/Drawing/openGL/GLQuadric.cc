/*$Id: GLQuadric.cc,v 1.4 2000/08/31 18:52:33 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
 * http://www.berlin-consortium.org
 *
 * this code is adapted from the Mesa 3-D graphics library
 * Copyright (C) 1995-1999 Brian Paul
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
#include "Drawing/openGL/GLQuadric.hh"
#include "GL/gl.h"
#include <cmath>

using namespace Warsaw;

/*
 * Convert degrees to radians:
 */
inline double deg_to_rad(double a) { return a * M_PI / 180.0;}

/*
 * Sin and Cos for degree angles:
 */
inline double sind(double a) { return sin(deg_to_rad(a));}
inline double cosd(double a) { return cos(deg_to_rad(a));}

/*
 * Call glNormal3d after scaling normal to unit length.
 */
static void normal3d(double x, double y, double z)
{
  double mag = sqrt( x*x + y*y + z*z );
  if (mag > 0.00001)
    {
      x /= mag;
      y /= mag;
      z /= mag;
    }
  glNormal3d(x, y, z);
}

void GLQuadric::cylinder(double baseRadius, double topRadius, double height, int slices, int stacks)
{
  double nsign = orient == in ? -1. : 1.;
  double da = 2.0*M_PI / slices;
  double dr = (topRadius - baseRadius) / stacks;
  double dz = height / stacks;
  double nz = (baseRadius - topRadius) / height;  /* Z component of normal vectors */

//   if (style == GLU_POINT)
//     {
//       glBegin(GL_POINTS);
//       for (int i = 0; i < slices; i++)
// 	{
// 	  x = cos(i*da);
// 	  y = sin(i*da);
// 	  normal3d(x*nsign, y*nsign, nz*nsign);
// 	  z = 0.0;
// 	  r = baseRadius;
// 	  for (int j = 0; j <= stacks; j++)
// 	    {
// 	      glVertex3d(x*r, y*r, z);
// 	      z += dz;
// 	      r += dr;
// 	    }
// 	}
//       glEnd();
//     }
  if (style == DrawingKit::outlined)// || DrawStyle == GLU_SILHOUETTE)
    {
//       /* Draw rings */
//       if (style == DrawingKit::outlined)
// 	{
// 	  z = 0.0;
// 	  r = baseRadius;
// 	  for (int j = 0; j <= stacks; j++)
// 	    {
// 	      glBegin(GL_LINE_LOOP);
// 	      for (int i = 0; i < slices; i++)
// 		{
// 		  x = cos(i*da);
// 		  y = sin(i*da);
// 		  normal3d(x*nsign, y*nsign, nz*nsign);
// 		  glVertex3d(x*r, y*r, z);
// 		}
// 	      glEnd();
// 	      z += dz;
// 	      r += dr;
// 	    }
// 	}
//       else
      {
	/* draw one ring at each end */
	if (baseRadius != 0.0)
	  {
	    glBegin(GL_LINE_LOOP);
	    for (int i = 0; i < slices; i++)
	      {
		double x = cos(i*da);
		double y = sin(i*da);
		normal3d(x*nsign, y*nsign, nz*nsign);
		glVertex3d(x*baseRadius, y*baseRadius, 0.0);
	      }
	    glEnd();
	    glBegin(GL_LINE_LOOP);
	    for (int i = 0; i < slices; i++)
	      {
		double x = cos(i*da);
		double y = sin(i*da);
		normal3d(x*nsign, y*nsign, nz*nsign);
		glVertex3d(x*topRadius, y*topRadius, height);
	      }
	    glEnd();
	  }
      }
      /* draw length lines */
      glBegin(GL_LINES);
      for (int i = 0; i < slices; i++)
	{
	  double x = cos(i*da);
	  double y = sin(i*da);
	  normal3d(x*nsign, y*nsign, nz*nsign);
	  glVertex3d(x*baseRadius, y*baseRadius, 0.0);
	  glVertex3d(x*topRadius, y*topRadius, height);
	}
      glEnd();
    }
  else // filled = solid | textured | ...
    {
      double ds = 1.0 / slices;
      double dt = 1.0 / stacks;
      double t = 0.0;
      double z = 0.0;
      double r = baseRadius;
      for (int j = 0; j < stacks; j++)
	{
	  double s = 0.0;
	  glBegin(GL_QUAD_STRIP);
	  for (int i = 0; i <= slices; i++)
	    {
	      double x, y;
	      if (i == slices)
		{
		  x = sin(0);
		  y = cos(0);
		}
	      else
		{
		  x = sin(i * da);
		  y = cos(i * da);
		}
	      if (nsign == 1.0)
		{
		  normal3d(x*nsign, y*nsign, nz*nsign);
		  if (style == DrawingKit::textured) glTexCoord2f(s, t);
		  glVertex3d(x * r, y * r, z);
		  normal3d(x*nsign, y*nsign, nz*nsign);
		  if (style == DrawingKit::textured) glTexCoord2f(s, t + dt);
		  glVertex3d( x * (r + dr), y * (r + dr), z + dz);
		}
	      else
		{
		  normal3d(x*nsign, y*nsign, nz*nsign);
		  if (style == DrawingKit::textured) glTexCoord2f(s, t);
		  glVertex3d(x * r, y * r, z);
		  normal3d(x*nsign, y*nsign, nz*nsign);
		  if (style == DrawingKit::textured) glTexCoord2f(s, t + dt);
		  glVertex3d(x * (r + dr), y * (r + dr), z + dz);
		}
	      s += ds;
	    } /* for slices */
	  glEnd();
	  r += dr;
	  t += dt;
	  z += dz;
	} /* for stacks */
    }
}

void GLQuadric::sphere(double radius, int slices, int stacks)
{
  double nsign = orient == in ? -1. : 1.;
  double drho = M_PI / static_cast<double>(stacks);
  double dtheta = 2.0 * M_PI / static_cast<double>(slices);

  /* texturing: s goes from 0.0/0.25/0.5/0.75/1.0 at +y/+x/-y/-x/+y axis */
  /* t goes from -1.0/+1.0 at z = -radius/+radius (linear along longitudes) */
  /* cannot use triangle fan on texturing (s coord. at top/bottom tip varies) */

  if (style != DrawingKit::outlined)
    {
      if (style != DrawingKit::textured)
	{
	  /* draw +Z end as a triangle fan */
	  glBegin(GL_TRIANGLE_FAN);
	  glNormal3d(0.0, 0.0, 1.0);
	  glVertex3d(0.0, 0.0, nsign * radius);
	  for (int j = 0; j <= slices; j++)
	    {
	      double theta = j == slices ? 0.0 : j * dtheta;
	      double x = -sin(theta) * sin(drho);
	      double y = cos(theta) * sin(drho);
	      double z = nsign * cos(drho);
	      if (norm)  glNormal3d(x*nsign, y*nsign, z*nsign);
	      glVertex3d(x*radius, y*radius, z*radius);
	    }
	  glEnd();
	}
      double ds = 1.0 / slices;
      double dt = 1.0 / stacks;
      double t = 1.0;  /* because loop now runs from 0 */
      int imin = style == DrawingKit::textured ? 0 : 1;
      int imax = style == DrawingKit::textured ? stacks : stacks - 1;
      /* draw intermediate stacks as quad strips */
      for (int i = imin; i < imax; i++)
	{
	  double rho = i * drho;
	  glBegin(GL_QUAD_STRIP);
	  double s = 0.0;
	  for (int j = 0; j <= slices; j++)
	    {
	      double theta = (j==slices) ? 0.0 : j * dtheta;
	      double x = -sin(theta) * sin(rho);
	      double y = cos(theta) * sin(rho);
	      double z = nsign * cos(rho);
	      if (norm)  glNormal3d(x*nsign, y*nsign, z*nsign);
	      if (style == DrawingKit::textured) glTexCoord2f(s, t);
	      glVertex3d(x*radius, y*radius, z*radius);
	      x = -sin(theta) * sin(rho+drho);
	      y = cos(theta) * sin(rho+drho);
	      z = nsign * cos(rho+drho);
	      if (norm)  glNormal3d(x*nsign, y*nsign, z*nsign);
	      if (style == DrawingKit::textured) glTexCoord2f(s, t - dt);
	      s += ds;
	      glVertex3d(x*radius, y*radius, z*radius);
	    }
	  glEnd();
	  t -= dt;
	}

      if (style != DrawingKit::textured)
	{
	  /* draw -Z end as a triangle fan */
	  glBegin(GL_TRIANGLE_FAN);
	  glNormal3d(0.0, 0.0, -1.0);
	  glVertex3d(0.0, 0.0, -radius * nsign);
	  double rho = M_PI - drho;
	  double s = 1.0;
	  double t = dt;
	  for (int j = slices; j >= 0; j--)
	    {
	      double theta = j == slices ? 0.0 : j * dtheta;
	      double x = -sin(theta) * sin(rho);
	      double y = cos(theta) * sin(rho);
	      double z = nsign * cos(rho);
	      if (norm) glNormal3d(x*nsign, y*nsign, z*nsign);
	      if (style == DrawingKit::textured) glTexCoord2f(s, t);
	      s -= ds;
	      glVertex3d(x*radius, y*radius, z*radius);
	    }
	  glEnd();
	}
    }
  else if (style == DrawingKit::outlined)
    {
      /* draw stack lines */
      for (int i = 1; i < stacks; i++)
	{
	  /* stack line at i==stacks-1 was missing here */
	  double rho = i * drho;
	  glBegin(GL_LINE_LOOP);
	  for (int j = 0; j < slices; j++)
	    {
	      double theta = j * dtheta;
	      double x = cos(theta) * sin(rho);
	      double y = sin(theta) * sin(rho);
	      double z = cos(rho);
	      if (norm) glNormal3d(x*nsign, y*nsign, z*nsign);
	      glVertex3d(x*radius, y*radius, z*radius);
	    }
	  glEnd();
	}
      /* draw slice lines */
      for (int j = 0; j < slices; j++)
	{
	  double theta = j * dtheta;
	  glBegin(GL_LINE_STRIP);
	  for (int i = 0; i <= stacks; i++)
	    {
	      double rho = i * drho;
	      double x = cos(theta) * sin(rho);
	      double y = sin(theta) * sin(rho);
	      double z = cos(rho);
	      if (norm) glNormal3d(x*nsign, y*nsign, z*nsign);
	      glVertex3d(x*radius, y*radius, z*radius);
	    }
	  glEnd();
	}
    }
//   else if (DrawStyle == GLU_POINT)
//     {
//       /* top and bottom-most points */
//       glBegin(GL_POINTS);
//       if (normals) glNormal3d(0.0, 0.0, nsign);
//       glVertex3d(0.0, 0.0, radius);
//       if (normals) glNormal3d(0.0, 0.0, -nsign);
//       glVertex3d(0.0, 0.0, -radius);
      
//       /* loop over stacks */
//       for (int i = 1; i < stacks-1; i++)
// 	{
// 	  rho = i * drho;
// 	  for (int j = 0; j < slices; j++)
// 	    {
// 	      theta = j * dtheta;
// 	      x = cos(theta) * sin(rho);
// 	      y = sin(theta) * sin(rho);
// 	      z = cos(rho);
// 	      if (normals) glNormal3d(x*nsign, y*nsign, z*nsign);
// 	      glVertex3d(x*radius, y*radius, z*radius);
// 	    }
// 	}
//       glEnd();
//     }
}

void GLQuadric::disk(double innerRadius, double outerRadius, int slices, int loops)
{
  /* Normal vectors */
  if (norm != none)
    {
      if (orient == out) glNormal3d(0.0, 0.0, +1.0);
      else glNormal3d(0.0, 0.0, -1.0);
    }
  double da = 2.0*M_PI / slices;
  double dr = (outerRadius - innerRadius) / loops;

  switch (style)
    {
    case DrawingKit::solid:
    case DrawingKit::textured:
      {
	/* texture of a disk is a cut out of the texture unit square
	 * x, y in [-outerRadius, +outerRadius]; s, t in [0, 1]
	 * (linear mapping)
	 */
	double dtc = 2.0f * outerRadius;
	double r1 = innerRadius;
	for (int l = 0; l < loops; l++)
	  {
	    double r2 = r1 + dr;
	    if (orient == out)
	      {
		glBegin(GL_QUAD_STRIP);
		for (int s = 0;  s <= slices; s++)
		  {
		    double a = s == slices ? 0.0 : s * da;
		    double sa = sin(a), ca = cos(a);
		    glTexCoord2f(0.5 + sa * r2 / dtc, 0.5 + ca * r2 / dtc);
		    glVertex2f(r2*sa, r2*ca);
		    glTexCoord2f(0.5 + sa * r1 / dtc, 0.5 + ca * r1 / dtc);
		    glVertex2f(r1*sa, r1*ca);
		  }
		glEnd();
	      }
	    else
	      {
		glBegin(GL_QUAD_STRIP);
		for (int s = slices; s >= 0; s--)
		  {
		    double a = s == slices ? 0.0 : s * da;
		    double sa = sin(a), ca = cos(a);
		    if (style == DrawingKit::textured) glTexCoord2f(0.5 - sa * r2 / dtc, 0.5 + ca * r2 / dtc);
		    glVertex2f( r2*sa, r2*ca);
		    if (style == DrawingKit::textured) glTexCoord2f(0.5 - sa * r1 / dtc, 0.5 + ca * r1 / dtc);
		    glVertex2f( r1*sa, r1*ca);
		  }
		glEnd();
	      }
	    r1 = r2;
	  }
	break;
      }
//     case DrawingKit::outlined:
//       {
// 	/* draw loops */
// 	for (int l = 0; l <= loops; l++)
// 	  {
//             double r = innerRadius + l * dr;
// 	    glBegin(GL_LINE_LOOP);
//             for (int s = 0; s < slices; s++)
// 	      {
// 		double a = s * da;
// 		glVertex2f(r*sin(a), r*cos(a));
// 	      }
// 	    glEnd();
// 	  }
// 	/* draw spokes */
// 	for (int s = 0; s < slices; s++)
// 	  {
//             double a = s * da;
// 	    double x = sin(a);
// 	    double y = cos(a);
// 	    glBegin(GL_LINE_STRIP);
//             for (int l = 0; l <= loops; l++)
// 	      {
// 		GLdouble r = innerRadius + l * dr;
// 		glVertex2f(r*x, r*y);
// 	      }
// 	    glEnd();
// 	  }
// 	break;
//       }
//     case GLU_POINT:
//       {
// 	GLint s;
// 	glBegin(GL_POINTS);
// 	for (int s = 0; s < slices; s++)
// 	  {
//             GLdouble a = s * da;
// 	    GLdouble x = sin(a);
// 	    GLdouble y = cos(a);
//             GLint l;
//             for (int l = 0; l <= loops; l++)
// 	      {
// 		GLdouble r = innerRadius * l * dr;
// 		glVertex2f(r*x, r*y);
// 	      }
// 	  }
// 	glEnd();
// 	break;
//       }
    case DrawingKit::outlined:
      {
	if (innerRadius != 0.0)
	  {
	    glBegin(GL_LINE_LOOP);
	    for (double a = 0.0; a < 2.0*M_PI; a += da)
	      {
		double x = innerRadius * sin(a);
		double y = innerRadius * cos(a);
		glVertex2f(x, y);
	      }
	    glEnd();
	  }
	glBegin(GL_LINE_LOOP);
	for (double a = 0; a < 2.0*M_PI; a += da)
	  {
	    GLdouble x = outerRadius * sin(a);
	    GLdouble y = outerRadius * cos(a);
	    glVertex2f(x, y);
	  }
	glEnd();
	break;
      }
    default:
      abort();
    }
}

void GLQuadric::partialDisk(double innerRadius, double outerRadius, int slices, int loops,
			    double startAngle, double sweepAngle)
{
  if (norm != none)
    {
      if (orient == out) glNormal3d( 0.0, 0.0, +1.0 );
      else glNormal3d( 0.0, 0.0, -1.0 );
    }
//   if (style==GLU_POINT)
//     {
//       GLint loop, slice;
//       GLdouble radius, delta_radius;
//       GLdouble angle, delta_angle;
//       delta_radius = (outerRadius - innerRadius) / (loops-1);
//       delta_angle = DEG_TO_RAD((sweepAngle) / (slices-1));
//       glBegin( GL_POINTS );
//       radius = innerRadius;
//       for (int loop = 0; loop < loops; loop++)
// 	{
// 	  angle = DEG_TO_RAD(startAngle);
// 	  for (int slice = 0; slice < slices; slice++)
// 	    {
// 	      glVertex2d(radius * sin(angle), radius * cos(angle));
// 	      angle += delta_angle;
// 	    }
// 	  radius += delta_radius;
// 	}
//       glEnd();
//     }
//   if (style == GLU_LINE)
//     {
//       GLint loop, slice;
//       GLdouble radius, delta_radius;
//       GLdouble angle, delta_angle;
//       delta_radius = (outerRadius - innerRadius) / loops;
//       delta_angle = DEG_TO_RAD(sweepAngle / slices);
//       /* draw rings */
//       radius = innerRadius;
//       for (int loop = 0; loop < loops; loop++)
// 	{
// 	  angle = DEG_TO_RAD(startAngle);
// 	  glBegin(GL_LINE_STRIP);
// 	 for (int slice=0; slice < slices; slice++)
// 	   {
// 	     glVertex2d(radius * sin(angle), radius * cos(angle));
// 	     angle += delta_angle;
// 	   }
// 	 glEnd();
// 	 radius += delta_radius;
// 	}
//       /* draw spokes */
//       angle = DEG_TO_RAD(startAngle);
//       for (int slice = 0; slice < slices; slice++)
// 	{
// 	  radius = innerRadius;
// 	  glBegin(GL_LINE_STRIP);
// 	  for (int loop = 0; loop < loops; loop++)
// 	    {
// 	      glVertex2d(radius * sin(angle), radius * cos(angle));
// 	      radius += delta_radius;
// 	    }
// 	  glEnd();
// 	  angle += delta_angle;
// 	}
//     }
  if (style == DrawingKit::outlined)
    {
      double da = deg_to_rad(sweepAngle / slices);
      /* draw outer ring */
      glBegin(GL_LINE_STRIP);
      double angle = deg_to_rad(startAngle);
      for (int slice = 0; slice <= slices; slice++)
	{
	  glVertex2d(outerRadius * sin(angle), outerRadius * cos(angle));
	  angle += da;
	}
      glEnd();
      /* draw inner ring */
      if (innerRadius > 0.0)
	{
	  glBegin(GL_LINE_STRIP);
	  angle = deg_to_rad(startAngle);
	  for (int slice = 0; slice < slices; slice++)
	    {
	      glVertex2d(innerRadius * sin(angle), innerRadius * cos(angle));
	      angle += da;
	    }
	  glEnd();
	}
      /* draw spokes */
      if (sweepAngle < 360.0)
	{
	  double start = deg_to_rad(startAngle);
	  double stop  = deg_to_rad(startAngle + sweepAngle);
	  glBegin(GL_LINES);
	  glVertex2d(innerRadius*sin(start), innerRadius*cos(start));
	  glVertex2d(outerRadius*sin(start), outerRadius*cos(start));
	  glVertex2d(innerRadius*sin(stop), innerRadius*cos(stop));
	  glVertex2d(outerRadius*sin(stop), outerRadius*cos(stop));
	  glEnd();
	}
    }
  else// if (style == GLU_FILL)
    {
//       GLint loop, slice;
//       GLdouble radius, delta_radius;
//       GLdouble angle, delta_angle;
      double radius = innerRadius;
      double dr = (outerRadius - innerRadius) / loops;
      double da = deg_to_rad(sweepAngle / slices);
      for (int loop = 0; loop < loops; loop++)
	{
	  glBegin(GL_QUAD_STRIP);
	  double angle = deg_to_rad(startAngle);
	  for (int slice = 0; slice < slices; slice++)
	    {
	      if (orient == out)
		{
		  glVertex2d((radius + dr)*sin(angle), (radius + dr)*cos(angle));
		  glVertex2d(radius * sin(angle), radius * cos(angle));
		}
	      else 
		{
		  glVertex2d(radius * sin(angle), radius * cos(angle));
		  glVertex2d((radius + dr)*sin(angle), (radius + dr)*cos(angle));
		}
	      angle += da;
	    }
	  glEnd();
	  radius += dr;
	}
    }
}
