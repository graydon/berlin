/*$Id: GapBuffer.hh,v 1.8 2001/04/18 06:07:25 stefan Exp $
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
#ifndef _GapBuffer_hh
#define _GapBuffer_hh

#include <vector>

/*
 * the design of a gap buffer should follow the typical behavior
 * when editing text. Editing text has two parts: modifying it
 * and simply browsing it. The text contains a 'cursor', which indicates
 * the actual position. When in editing mode, we have to take care to
 * use smart memory management. For this purpose we introduce a little
 * memory gap into the string where new characters can be inserted,
 * so not on every insert() all the following characters have to be moved
 *
 * a gap buffer is layed out like this:
 *   
 *   |***********************..........**********|
 *   ^                      ^         ^          ^
 *  begin()                gbegin()  gend()     end()
 *
 * the following (in)equalities should always hold:
 * 
 * gend() > gbegin()
 * gend() - gbegin() <= gmaxsize()
 * end() >= gend()
 * 
 */
template <class T, short gapsize>
class GapBuffer : private std::vector<T>
{
  typedef std::vector<T> rep_type;
  typedef typename std::vector<T>::value_type value_type;
  iterator gbegin() { return begin() + _gapbegin;}
  iterator gend() { return begin() + _gapend;}
  iterator cursor() { return begin() + _cursor;}
  void newgap()
    {
      rep_type::insert(gbegin(), gapsize, value_type(0));
      _gapend += gapsize;
    }
  void movegap(int d)
    {
      if (d > 0)
	{
	  if (gend() + d > end()) rep_type::insert(end(), size_type(gend() + d - end()), value_type(0));
	  copy(gend(), gend() + d, gbegin());
	}
      else
	copy(rep_type::reverse_iterator(gbegin()), rep_type::reverse_iterator(gbegin() + d), rep_type::reverse_iterator(gend()));
      _gapbegin += d, _gapend += d;
    }
  size_type gap() { return _gapend - _gapbegin;}
  void editing() { size_type d = _cursor - _gapbegin; if (d != 0) movegap(d);}
  void compact() { size_type d = end() - gend(); if (d > 0) movegap(d);}
public:
  GapBuffer() : _cursor(0), _gapbegin(0), _gapend(0) {}
  size_type size() { compact(); return gbegin() - begin();}
  void forward()
    {
      if (_cursor == _gapbegin && gend() != end()) _cursor += gap();
      else if (cursor() < end()) _cursor++;
    }
  void backward()
    {
      if (_cursor == _gapend) _cursor -= gap();
      else if (cursor() > begin()) _cursor--;
    }
  void shift(size_type d)
    {
      size_type tmp = _cursor + d;
      if ((_cursor > _gapend && tmp > _gapend) || (_cursor <= _gapbegin && tmp <= _gapbegin)) _cursor = tmp;
      else if (d < 0) _cursor += d - gap();
      else _cursor += d + gap();
    }
  size_type position() { return _cursor > _gapend ? _cursor - gap() : _cursor;}
  void position(size_type p) { shift(p - _cursor);}
  void insert(value_type u)
    {
      editing();
      if (!gap()) newgap();
      *cursor() = u;
      _cursor++, _gapbegin++;
    }
  void insert(value_type *u, size_type n)
    {
      editing();
      rep_type::insert(cursor(), u, u + n);
      _cursor += n, _gapbegin += n, _gapend += n;
    }
  void remove_backward(size_type n)
    {
      if (_cursor <= _gapbegin)
	{
	  if (_cursor < n) n = _cursor;
	  erase(cursor() - n, cursor());
	  _cursor -= n, _gapbegin -= n, _gapend -= n;
	}
      else if (_cursor - _gapend > n)
	{
	  erase(cursor() - n, cursor());
	  _cursor -= n;
	}
      else
	{
	  size_type d = _cursor - _gapend;
	  erase(gbegin() - (n - d), gbegin());
	  erase(cursor() - d, cursor());
	  _gapbegin -= n - d, _gapend -= n - d;
	  _cursor -= n;
	}
    }
  void remove_forward(size_type n)
    {
      if (_cursor >= _gapend)
	{
	  if (size_type(end() - cursor()) < n) n = end() - cursor();
	  erase(cursor(), cursor() + n);
	}
      else if (_gapbegin - _cursor > n)
	{
	  erase(cursor(), cursor() + n);
	  _gapbegin -= n, _gapend -= n;
	}
      else
	{
	  size_type d = _gapbegin - _cursor;
	  erase(gend(), gend() + (n - d));
	  erase(cursor(), cursor() + d);
	  _gapbegin -= d, _gapend -= d;
	}
    }
  const value_type *get() { compact(); return &*begin();}
  void clear_buffer()
  {
    position(0);
    remove_forward(size());
  }
private:
  size_type _cursor;
  size_type _gapbegin;
  size_type _gapend;
};

#endif
