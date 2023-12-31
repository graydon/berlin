/*$Id: TransformImpl.hh,v 1.12 2001/02/06 22:02:21 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#ifndef _TransformImpl_h
#define _TransformImpl_h

#include <Warsaw/config.hh>
#include <Warsaw/Transform.hh>
#include <Berlin/ServantBase.hh>
#include <Berlin/Provider.hh>

class TransformImpl : public virtual POA_Warsaw::Transform,
		      public virtual ServantBase
{
  friend class Provider<TransformImpl>;
public:
  TransformImpl();
  TransformImpl(const TransformImpl &);
  TransformImpl(Warsaw::Transform_ptr t) : _this_valid (false) { copy(t);}
  TransformImpl(Warsaw::Transform::Matrix m);
  virtual ~TransformImpl();
  TransformImpl &operator = (const TransformImpl &);
  virtual void copy(Warsaw::Transform_ptr);
  virtual void load_identity();
  virtual void load_matrix(const Warsaw::Transform::Matrix);
  virtual void store_matrix(Warsaw::Transform::Matrix);
  virtual CORBA::Boolean equal(Warsaw::Transform_ptr);
  virtual CORBA::Boolean identity();
  virtual CORBA::Boolean translation();
  virtual CORBA::Boolean det_is_zero();
  virtual void scale(const Warsaw::Vertex &);
  virtual void rotate(CORBA::Double, Warsaw::Axis);
  virtual void translate(const Warsaw::Vertex &);
  virtual void premultiply(Warsaw::Transform_ptr);
  virtual void postmultiply(Warsaw::Transform_ptr);
  virtual void invert();
  virtual void transform_vertex(Warsaw::Vertex &);
  virtual void inverse_transform_vertex(Warsaw::Vertex &);

  Warsaw::Transform::Matrix &matrix() { return _matrix;}
  const Warsaw::Transform::Matrix &matrix() const { return _matrix;}

  Warsaw::Transform_ptr _this()
  {
    if (!_this_valid)
      {
	__this = POA_Warsaw::Transform::_this ();
	_this_valid = true;
      }

    return Warsaw::Transform::_duplicate (__this);
  }

private:
  void init();
  void modified() { _dirty = true;}
  void recompute();
  Warsaw::Coord det();
  Warsaw::Transform::Matrix _matrix;
  bool _dirty       : 1;
  bool _identity    : 1;
  bool _translation : 1;
  bool _xy          : 1;
  bool _this_valid  : 1;
  bool _active      : 1;

  Warsaw::Transform_var __this;
};

#endif
