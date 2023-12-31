/*$Id: TransformImpl.hh,v 1.5 1999/10/13 21:32:31 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this code is based on Fresco.
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
#ifndef _TransformImpl_h
#define _TransformImpl_h

#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>

class TransformImpl : implements(Transform)
{
public:
  TransformImpl();
  TransformImpl(Transform::Matrix m);
  virtual ~TransformImpl();

  virtual void copy(Transform_ptr);
  virtual void loadIdentity();
  virtual void loadMatrix(const Matrix);
  virtual void storeMatrix(Matrix);
  virtual CORBA::Boolean equal(Transform_ptr);
  virtual CORBA::Boolean Identity();
  virtual CORBA::Boolean Translation();
  virtual CORBA::Boolean detIsZero();
  virtual void scale(const Vertex &);
  virtual void rotate(double, Axis);
  virtual void translate(const Vertex &);
  virtual void premultiply(Transform_ptr);
  virtual void postmultiply(Transform_ptr);
  virtual void invert();
  virtual void transformVertex(Vertex &);
  virtual void inverseTransformVertex(Vertex &);

  Transform::Matrix &matrix() { return mat;}
protected:
  Transform::Matrix mat;
  bool valid;
  bool identity;
  bool translate_only;
  bool xy;

  void init();
  void modified() { valid = false;}
  void recompute();
  Coord det();
};

#endif
