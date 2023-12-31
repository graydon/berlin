/*$Id: TransformImpl.cc,v 1.19 2001/02/06 22:02:21 stefan Exp $
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
#include <Prague/Sys/Tracer.hh>
#include <Warsaw/config.hh>
#include <Warsaw/IO.hh>
#include "Berlin/TransformImpl.hh"
#include "Berlin/Math.hh"
#include "Berlin/Logger.hh"
#include <cassert>

static const double radians_per_degree = Math::pi / 180;
static const double tolerance = 1e-4;

using namespace Prague;
using namespace Warsaw;

/*
 * transformation matrices are of the form:
 *
 *        [ r00 r01 r02 tx]
 *    M = [ r10 r11 r12 ty]
 *        [ r20 r21 r22 tz]
 *        [ 0   0   0   1 ]
 */
template <size_t N>
bool LUfactor(Coord matrix[N][N], size_t pivot[N])
{
  for (size_t j = 0; j < N; j++)
    {
      size_t jp = j;
      Coord t = Math::abs(matrix[j][j]);
      for (size_t i = j + 1; i < N; i++)
	if (Math::abs(matrix[i][j]) > t)
	  {
	    jp = i;
	    t = Math::abs(matrix[i][j]);
	  }
      pivot[j] = jp;
      if (Math::equal(matrix[jp][j], 0., 1e-10)) return false;
      if (jp != j) for (size_t k = 0; k < N; k++) swap(matrix[j][k], matrix[jp][k]);
      if (j < N - 1)
        {
	  Coord scale =  1. / matrix[j][j];
	  for (size_t k = j + 1; k < N; k++)
	    matrix[k][j] *= scale;
	  for (size_t ii = j + 1; ii < N; ii++)
	    for (size_t jj = j + 1; jj < N; jj++)
	      matrix[ii][jj] -= matrix[ii][j] * matrix[j][jj];
        }
    }
  return true;
}

template <size_t N>
void LUsolve(const Coord matrix[N][N], const size_t pivot[N], Coord v[N])
{
  short ii = -1;
  Coord sum = 0.0;

  for (size_t i = 0; i < N; i++) 
    {
      sum = v[pivot[i]];
      v[pivot[i]] = v[i];
      if (ii != -1)
	for (size_t j = ii; j <= i - 1; j++) 
	  sum -= matrix[i][j] * v[j];
      else if (sum) ii = i;
      v[i] = sum;
    }
  for (short i = static_cast<short>(N) - 1; i >= 0; i--) 
    {
      sum = v[i];
      for (size_t j = i + 1; j < N; j++) 
	sum -= matrix[i][j] * v[j];
      v[i] = sum / matrix[i][i];
    }
}

TransformImpl::TransformImpl()
  : _dirty(false),
    _identity(true),
    _translation(true),
    _xy(true),
    _this_valid(false),
    _active(true)
{
  init();
}

TransformImpl::TransformImpl(const TransformImpl &transform)
  : _dirty(transform._dirty),
    _identity(transform._identity),
    _translation(transform._translation),
    _xy(transform._xy),
    _this_valid(false),
    _active(true)
{
  load_matrix(transform._matrix);
}

TransformImpl::TransformImpl(Warsaw::Transform::Matrix matrix)
  : _dirty(true),
    _identity(false),
    _translation(false),
    _xy(false),
    _this_valid(false),
    _active(true)
{
  load_matrix(matrix);
}

TransformImpl::~TransformImpl() {}

TransformImpl &TransformImpl::operator = (const TransformImpl &transform)
{
  Trace trace("TransformImpl::operator =");
  load_matrix(transform._matrix);
}

void TransformImpl::init()
{
  _matrix[0][0] = _matrix[1][1] = _matrix[2][2] = _matrix[3][3] = 1.;
  _matrix[0][1] = _matrix[0][2] = _matrix[0][3] = 0.;
  _matrix[1][0] = _matrix[1][2] = _matrix[1][3] = 0.;
  _matrix[2][0] = _matrix[2][1] = _matrix[2][3] = 0.;
  _matrix[3][0] = _matrix[3][1] = _matrix[3][2] = 0.;
  _identity     = true;
  _translation  = true;
  _xy           = true;
  _dirty        = false;
}

void TransformImpl::recompute()
{
  _translation = (Math::equal(_matrix[0][0], 1., tolerance) &&
		  Math::equal(_matrix[1][1], 1., tolerance) &&
		  Math::equal(_matrix[2][2], 1., tolerance) &&
		  Math::equal(_matrix[0][1], 0., tolerance) &&
		  Math::equal(_matrix[1][0], 0., tolerance) &&
		  Math::equal(_matrix[0][2], 0., tolerance) &&
		  Math::equal(_matrix[2][0], 0., tolerance) &&
		  Math::equal(_matrix[1][2], 0., tolerance) &&
		  Math::equal(_matrix[2][1], 0., tolerance));
  _xy          = ((_translation ||
		   (Math::equal(_matrix[2][2], 1., tolerance) &&
		    Math::equal(_matrix[0][2], 0., tolerance) &&
		    Math::equal(_matrix[2][0], 0., tolerance) &&
		    Math::equal(_matrix[1][2], 0., tolerance) &&
		    Math::equal(_matrix[2][1], 0., tolerance))) &&
		  Math::equal(_matrix[2][3], 0., tolerance));
  _identity    = (_translation &&
		  Math::equal(_matrix[0][3], 0., tolerance) &&
		  Math::equal(_matrix[1][3], 0., tolerance) &&
		  Math::equal(_matrix[2][3], 0., tolerance));
  
  _dirty       = false;
}

Coord TransformImpl::det()
{
  double pos = 0., neg = 0., t;
  t =  _matrix[0][0] * _matrix[1][1] * _matrix[2][2];
  if (t >= 0.) pos += t; else neg += t;
  t =  _matrix[0][1] * _matrix[1][2] * _matrix[2][0];
  if (t >= 0.) pos += t; else neg += t;
  t =  _matrix[0][2] * _matrix[1][0] * _matrix[2][1];
  if (t >= 0.) pos += t; else neg += t;
  t = -_matrix[0][2] * _matrix[1][1] * _matrix[2][0];
  if (t >= 0.) pos += t; else neg += t;
  t = -_matrix[0][1] * _matrix[1][0] * _matrix[2][2];
  if (t >= 0.) pos += t; else neg += t;
  t = -_matrix[0][0] * _matrix[1][2] * _matrix[2][1];
  if (t >= 0.) pos += t; else neg += t;
  return pos + neg;
}

void TransformImpl::copy(Transform_ptr transform)
{
  Trace trace("TransformImpl::copy");
  if (CORBA::is_nil(transform)) init();
  else
    {
      Warsaw::Transform::Matrix matrix;
      transform->store_matrix(matrix);
      load_matrix(matrix);
    }
}

void TransformImpl::load_matrix(const Warsaw::Transform::Matrix matrix)
{
  Trace trace("TransformImpl::load_matrix");
  assert(_active);
  for (short i = 0; i != 3; i++)
    for (short j = 0; j != 4; j++)
      _matrix[i][j] = matrix[i][j];
  _matrix[3][0] = _matrix[3][1] = _matrix[3][2] = 0., _matrix[3][3] = 1.;
  modified();
}

void TransformImpl::load_identity() { init();}

void TransformImpl::store_matrix(Warsaw::Transform::Matrix matrix)
{
  Trace trace("TransformImpl::store_matrix");
  assert(_active);
  for (short i = 0; i != 3; i++)
    for (short j = 0; j != 4; j++)
      matrix[i][j] = _matrix[i][j];
  matrix[3][0] = matrix[3][1] = matrix[3][2] = 0., matrix[3][3] = 1.;
}

CORBA::Boolean TransformImpl::equal(Transform_ptr transform)
{
  Trace trace("TransformImpl::equal");
  if (_dirty) recompute();
  if (_identity) return CORBA::is_nil(transform) || transform->identity();
  if (CORBA::is_nil(transform) || transform->identity()) return false;
  Warsaw::Transform::Matrix matrix;
  transform->store_matrix(matrix);
  return
    Math::equal(_matrix[0][0], matrix[0][0], tolerance) &&
    Math::equal(_matrix[0][1], matrix[0][1], tolerance) &&
    Math::equal(_matrix[0][2], matrix[0][2], tolerance) &&
    Math::equal(_matrix[0][3], matrix[0][3], tolerance) &&
    Math::equal(_matrix[1][0], matrix[1][0], tolerance) &&
    Math::equal(_matrix[1][1], matrix[1][1], tolerance) &&
    Math::equal(_matrix[1][2], matrix[1][2], tolerance) &&
    Math::equal(_matrix[1][3], matrix[1][3], tolerance) &&
    Math::equal(_matrix[2][0], matrix[2][0], tolerance) &&
    Math::equal(_matrix[2][1], matrix[2][1], tolerance) &&
    Math::equal(_matrix[2][2], matrix[2][2], tolerance) &&
    Math::equal(_matrix[2][3], matrix[2][3], tolerance);
}

CORBA::Boolean TransformImpl::identity()
{
  if (_dirty) recompute();
  return _identity;
}

CORBA::Boolean TransformImpl::translation()
{
  if (_dirty) recompute();
  return _translation;
}

CORBA::Boolean TransformImpl::det_is_zero()
{
  Coord d = det();
  return d < tolerance && d > -tolerance;
}

void TransformImpl::scale(const Vertex &v)
{
  assert(_active);
  _matrix[0][0] *= v.x;
  _matrix[0][1] *= v.x;
  _matrix[0][2] *= v.x;
  
  _matrix[1][0] *= v.y;
  _matrix[1][1] *= v.y;
  _matrix[1][2] *= v.y;

  _matrix[2][0] *= v.z;
  _matrix[2][1] *= v.z;
  _matrix[2][2] *= v.z;
  modified();
}

void TransformImpl::rotate(double angle, Axis a)
{
  assert(_active);
  Coord r_angle = angle * radians_per_degree;
  Coord c = cos(r_angle);
  Coord s = sin(r_angle);
  Warsaw::Transform::Matrix matrix;
  short i = 0, j = 1;
  if (a == xaxis) i = 2;
  else if (a == yaxis) j = 2;

  matrix[i][0] = _matrix[i][0], matrix[i][1] = _matrix[i][1], matrix[i][2] = _matrix[i][2], matrix[i][3] = _matrix[i][3];
  matrix[j][0] = _matrix[j][0], matrix[j][1] = _matrix[j][1], matrix[j][2] = _matrix[j][2], matrix[j][3] = _matrix[j][3];

  _matrix[i][0] = c * matrix[i][0] - s * matrix[j][0];
  _matrix[i][1] = c * matrix[i][1] - s * matrix[j][1];
  _matrix[i][2] = c * matrix[i][2] - s * matrix[j][2];
  _matrix[i][3] = c * matrix[i][3] - s * matrix[j][3];

  _matrix[j][0] = s * matrix[i][0] + c * matrix[j][0];
  _matrix[j][1] = s * matrix[i][1] + c * matrix[j][1];
  _matrix[j][2] = s * matrix[i][2] + c * matrix[j][2];
  _matrix[j][3] = s * matrix[i][3] + c * matrix[j][3];

  modified();
}

void TransformImpl::translate(const Vertex &v)
{
  assert(_active);
  _matrix[0][3] += v.x;
  _matrix[1][3] += v.y;
  _matrix[2][3] += v.z;
  modified();
}

void TransformImpl::premultiply(Transform_ptr transform)
{
  Trace trace("TransformImpl::premultiply");
  assert(_active);
  if (!CORBA::is_nil(transform) && !transform->identity())
    {
      Warsaw::Transform::Matrix matrix;
      transform->store_matrix(matrix);
      if (identity()) load_matrix(matrix);
      else
	{
	  for (unsigned short i = 0; i != 3; i++)
	    {
	      Coord mi0 = _matrix[i][0], mi1 = _matrix[i][1], mi2 = _matrix[i][2], mi3 = _matrix[i][3];
	      _matrix[i][0] = mi0 * matrix[0][0] + mi1 * matrix[1][0] + mi2 * matrix[2][0] + mi3 * matrix[3][0];
	      _matrix[i][1] = mi0 * matrix[0][1] + mi1 * matrix[1][1] + mi2 * matrix[2][1] + mi3 * matrix[3][1];
	      _matrix[i][2] = mi0 * matrix[0][2] + mi1 * matrix[1][2] + mi2 * matrix[2][2] + mi3 * matrix[3][2];
	      _matrix[i][3] = mi0 * matrix[0][3] + mi1 * matrix[1][3] + mi2 * matrix[2][3] + mi3 * matrix[3][3];
	    }
	  modified();
	}
    }
}

void TransformImpl::postmultiply(Transform_ptr transform)
{
  Trace trace("TransformImpl::postmultiply");
  assert(_active);
  if (!CORBA::is_nil(transform) && !transform->identity())
    {
      Warsaw::Transform::Matrix matrix;
      transform->store_matrix(matrix);
      if (identity()) load_matrix(matrix);
      else
	{
	  for (unsigned short i = 0; i != 4; i++)
	    {
	      Coord m0i = _matrix[0][i], m1i = _matrix[1][i], m2i = _matrix[2][i];
	      _matrix[0][i] = matrix[0][0] * m0i + matrix[0][1] * m1i + matrix[0][2] * m2i;
	      _matrix[1][i] = matrix[1][0] * m0i + matrix[1][1] * m1i + matrix[2][1] * m2i;
	      _matrix[2][i] = matrix[2][0] * m0i + matrix[2][1] * m1i + matrix[2][2] * m2i;
	    }
	  modified();
	}
    }
}

void TransformImpl::invert()
{
  Trace trace("TransformImpl::invert");
  assert(_active);
  if (_dirty) recompute();
  if (_translation)
    {
      _matrix[0][3] = -_matrix[0][3];
      _matrix[1][3] = -_matrix[1][3];
      _matrix[2][3] = -_matrix[2][3];
      modified();
    }
  else
    {
      Coord d = det();
      if (Math::equal(d, 0., tolerance)) return;
      Warsaw::Transform::Matrix matrix;

      matrix[0][0] = _matrix[0][0], matrix[0][1] = _matrix[0][1], matrix[0][2] = _matrix[0][2], matrix[0][3] = _matrix[0][3];
      matrix[1][0] = _matrix[1][0], matrix[1][1] = _matrix[1][1], matrix[1][2] = _matrix[1][2], matrix[1][3] = _matrix[1][3];
      matrix[2][0] = _matrix[2][0], matrix[2][1] = _matrix[2][1], matrix[2][2] = _matrix[2][2], matrix[2][3] = _matrix[2][3];
      matrix[3][0] = 0., matrix[3][1] = 0., matrix[3][2] = 0., matrix[3][3] = 1.;


      _matrix[0][0] =  (matrix[1][1] * matrix[2][2] - matrix[1][2] * matrix[2][1]) / d;
      _matrix[0][1] = -(matrix[0][1] * matrix[2][2] - matrix[0][2] * matrix[2][1]) / d;
      _matrix[0][2] =  (matrix[0][1] * matrix[1][2] - matrix[0][2] * matrix[1][1]) / d;
      _matrix[1][0] = -(matrix[1][0] * matrix[2][2] - matrix[1][2] * matrix[2][0]) / d;
      _matrix[1][1] =  (matrix[0][0] * matrix[2][2] - matrix[0][2] * matrix[2][0]) / d;
      _matrix[1][2] = -(matrix[0][0] * matrix[1][2] - matrix[0][2] * matrix[1][0]) / d;
      _matrix[2][0] =  (matrix[1][0] * matrix[2][1] - matrix[1][1] * matrix[2][0]) / d;
      _matrix[2][1] = -(matrix[0][0] * matrix[2][1] - matrix[0][1] * matrix[2][0]) / d;
      _matrix[2][2] =  (matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0]) / d;

      _matrix[0][3] = - (matrix[0][3] * _matrix[0][0] + matrix[1][3] * _matrix[0][1] + matrix[2][3] * _matrix[0][2]);
      _matrix[1][3] = - (matrix[0][3] * _matrix[1][0] + matrix[1][3] * _matrix[1][1] + matrix[2][3] * _matrix[1][2]);
      _matrix[2][3] = - (matrix[0][3] * _matrix[2][0] + matrix[1][3] * _matrix[2][1] + matrix[2][3] * _matrix[2][2]);

      modified();
    }
}

void TransformImpl::transform_vertex(Vertex &v)
{
  Coord tx = v.x;
  Coord ty = v.y;
  v.x = _matrix[0][0] * tx + _matrix[0][1] * ty + _matrix[0][2] * v.z + _matrix[0][3];
  v.y = _matrix[1][0] * tx + _matrix[1][1] * ty + _matrix[1][2] * v.z + _matrix[1][3];
  v.z = _matrix[2][0] * tx + _matrix[2][1] * ty + _matrix[2][2] * v.z + _matrix[2][3];
}

void TransformImpl::inverse_transform_vertex(Vertex &v)
{
#if 0
  size_t pivot[4];
  Coord vertex[4];
  vertex[0] = v.x;//(v.x - _matrix[0][3]);
  vertex[1] = v.y;//(v.y - _matrix[1][3]);
  vertex[2] = v.z;//(v.z - _matrix[2][3]);
  vertex[3] = 0.;
  Coord lu[4][4];
  for (short i = 0; i != 4; i++)
    for (short j = 0; j != 4; j++)
      lu[i][j] = _matrix[i][j];
  if (LUfactor<4>(lu, pivot))
    {
      LUsolve<4>(lu, pivot, vertex);
      v.x = vertex[0];
      v.y = vertex[1];
      v.z = vertex[2];
    }
#else
  Coord d = det();
  if (Math::equal(d, 0., tolerance)) return;
  Vertex tmp = v;
  tmp.x -= _matrix[0][3];
  tmp.y -= _matrix[1][3];
  tmp.z -= _matrix[2][3];
  v.x = ((_matrix[1][1] * _matrix[2][2] - _matrix[1][2] * _matrix[2][1]) * tmp.x -
	 (_matrix[0][1] * _matrix[2][2] - _matrix[0][2] * _matrix[2][1]) * tmp.y +
	 (_matrix[0][1] * _matrix[1][2] - _matrix[0][2] * _matrix[1][1]) * tmp.z) / d;
  v.y = (-(_matrix[1][0] * _matrix[2][2] - _matrix[1][2] * _matrix[2][0]) * tmp.x +
	 (_matrix[0][0] * _matrix[2][2] - _matrix[0][2] * _matrix[2][0]) * tmp.y -
	 (_matrix[0][0] * _matrix[1][2] - _matrix[0][2] * _matrix[1][0])) / d;
  v.z = ((_matrix[1][0] * _matrix[2][1] - _matrix[1][1] * _matrix[2][0]) -
	 (_matrix[0][0] * _matrix[2][1] - _matrix[0][1] * _matrix[2][0]) +
	 (_matrix[0][0] * _matrix[1][1] - _matrix[0][1] * _matrix[1][0])) / d;
#endif
}

