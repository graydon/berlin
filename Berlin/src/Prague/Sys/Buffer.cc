/*+P
 * This file is part of OffiX,
 * a C++ API for the X Window System and Unix
 * Copyright (C) 1995-98  Stefan Seefeld
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
 -P*/
static char *rcsid = "$Id: Buffer.cc,v 1.1 1999/04/22 14:45:00 gray Exp $";
#include "OffiX/Sys/Buffer.h"
#include "OffiX/Sys/Memory.h"
#include <string.h>
#include <math.h>
#include <iostream.h>

/* @Method{Buffer::Buffer(char n)}
 *
 * @Description{construct a Buffer containing the single character @var{n}}
 */
Buffer::Buffer(char n)
{
  size = 16;
  length = 1;
  buffer = new char [size];
  buffer[0] = n;
  buffer[length] = '\0';
};

/* @Method{Buffer::Buffer(const char *string)}
 *
 * @Description{construct a Buffer from the '\0' terminated string @var{buf}}
 */
Buffer::Buffer(const char *buf)
{
  int tmp = 0; 
  length = buf ? strlen(buf) : 0;
  if (length) frexp(length, &tmp);
  size = (tmp < 4) ? 16 : 1 << tmp;
  buffer = new char [size];
  if (length) strcpy(buffer, buf);
  buffer[length] = '\0';
};

/* @Method{Buffer::Buffer(const Buffer &B)}
 *
 * @Description{construct a new Buffer from @var{B}}
 */
Buffer::Buffer(const Buffer &B)
{
  size = B.size;
  length = B.length;
  buffer = new char [size];
  strcpy(buffer, B.buffer);
};

/* @Method{Buffer::~Buffer()}
 *
 * @Description{}
 */
Buffer::~Buffer()
{
  delete buffer;
};

/* @Method{Buffer &Buffer::operator =(const char *string)}
 *
 * @Description{assigns the Buffer to the '\0' terminated @var{string}}
 */
Buffer & Buffer::operator = (const char *c)
{
  delete buffer;
  int tmp; 
  length = strlen(c);
  if (frexp(length, &tmp) == 0.5) tmp--;
  size = (tmp < 4) ? 16 : 1 << tmp;
  buffer = new char [size];
  strcpy(buffer, c);
  return *this;
};

/* @Method{void Buffer::append(char c)}
 *
 * @Description{append the character @var{c} to the Buffer}
 */
void Buffer::append(char c)
{
  if (length == size) grow();
  buffer[length++] = c;
  buffer[length] = '\0';
};

/* @Method{void Buffer::append(const char *string)}
 *
 * @Description{append the '\0' terminated @var{string} to the Buffer}
 */
void Buffer::append(const char *c)
{
  while (length + strlen(c) >= size) grow();
  strcat(buffer, c);
  length = strlen(buffer);
};

/* @Method{void Buffer::append(const char *string, unsigned long n)}
 *
 * @Description{append @var{n} characters from @var{string} to the Buffer}
 */
void Buffer::append(const char *string, unsigned long n)
{
  while (length + n >= size) grow();
  strncat(buffer, string, n);
  length = strlen(buffer);
};

/* @Method{void Buffer::insert(unsigned long i, char c)}
 *
 * @Description{insert @var{c} at position @var{i}}
 */
void Buffer::insert(unsigned long i, char c)
{
  if (i > length) append(c);
  else
    {
      if (length == size) grow();
      Memory::move(buffer+i, buffer+i+1, length+1-i);
      buffer[i] = c;
      length++;
    }
};

/* @Method{void Buffer::insert(unsigned long i, const char *c)}
 *
 * @Description{insert '\0' terminated @var{string} at position @var{i}}
 */
void Buffer::insert(unsigned long i, const char *c)
{
  if (!c) return;
  if (i > length) append(c);
  else
    {
      while (length + strlen(c) >= size) grow();
      Memory::move(buffer+i, buffer+i+strlen(c), length+1-i);
      Memory::copy(c, buffer+i, strlen(c));
      length += strlen(c);  
    };
}

/* @Method{void Buffer::insert(unsigned long i, const char *string, unsigned long n)}
 *
 * @Description{insert @var{n} characters from @var{string} into the Buffer at position @var{i}}
 */
void Buffer::insert(unsigned long i, const char *c, unsigned long n)
{
  if (i > length) append(c, n);
  else
    {
      while (length + n >= size) grow();
      Memory::move(buffer+i, buffer+i+n, length+1-i);
      Memory::copy(c, buffer+i, n);
      length += n;
    }  
};

/* @Method{void Buffer::remove(unsigned long i, unsigned long n = 1)}
 *
 * @Description{remove @var{n} characters starting at position @var{i}}
 */
void Buffer::remove(unsigned long i, unsigned long n)
{
  if (i > length) return;
  if (i+n >= length) n = length - i;
  Memory::move(buffer+i+n, buffer+i, length+1-i-n);
};

/* @Method{void Buffer::clear()}
 *
 * @Description{clear the Buffer}
 */
void Buffer::clear()
{
  delete buffer;
  size = 16;
  length = 0;
  buffer = new char[size];
  buffer[length] = '\0';
};

/* @Method{void Buffer::grow()}
 *
 * @Description{doubles the total size and copies the data}
 */
void Buffer::grow()
{
  char *tmp = new char [2*size];
  Memory::copy(buffer, tmp, length + 1);
  delete buffer;
  buffer = tmp;
  size <<= 1;
};
