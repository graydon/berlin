/*$Id: LocatorImpl.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Jez Higgins <jez@jezuk.demon.co.uk>
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
#ifndef _LocatorImpl_hh
#define _LocatorImpl_hh

#include <Prague/SAX/Locator.hh>
#include <string>

namespace SAX
{

class LocatorImpl : public Locator
{
public:
  LocatorImpl() : line(0), column(0) {}
  LocatorImpl(const Locator &l) { copy(l);}
  LocatorImpl(const string &p, const string &s, int l, int c)
    : publicID(p), systemID(s), line(l), column(c) {}
  virtual ~LocatorImpl() {}
  LocatorImpl &operator=(const Locator &l) { copy(l); return *this;}
  bool operator==(const Locator &rhs) const;

  const string &getPublicId() const { return publicID;}
  const string &getSystemId() const { return systemID;}
  int getLineNumber() const { return line;}
  int getColumnNumber() const { return column;}

  void setPublicId(const string &id) { publicID = id;}
  void setSystemId(const string &id) { systemID = id;}
  void setLineNumber(int l) { line = l;}
  void setColumnNumber(int c) { column = c;}

private:
  void copy(const Locator &);
  string publicID;
  string systemID;
  int line;
  int column;
};

inline bool LocatorImpl::operator==(const Locator &rhs) const
{
  return (publicID == rhs.getPublicId() &&
	  systemID == rhs.getSystemId() &&
	  line == rhs.getLineNumber() &&
	  column == rhs.getColumnNumber());
}

inline void LocatorImpl::copy(const Locator &l)
{
  publicID = l.getPublicId();
  systemID = l.getSystemId();
  line = l.getLineNumber();
  column = l.getColumnNumber();  
}

};

#endif

