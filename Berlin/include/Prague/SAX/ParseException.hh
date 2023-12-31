/*$Id: ParseException.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#ifndef _ParseException_hh
#define _ParseException_hh

#include <Prague/SAX/Exception.hh>
#include <Prague/SAX/LocatorImpl.hh>
#include <exception>
#include <string>

namespace SAX
{

class ParseException : public Exception, public LocatorImpl
{
 public:
  ParseException(const string &msg, const Locator &l) : Exception(msg), LocatorImpl(l) {}
  ParseException(const string &msg, const Locator &l, exception &e) : Exception(msg, e), LocatorImpl(l) {}
  ParseException(const string &msg, const string &p, const string &s, int l, int c)
    : Exception(msg), LocatorImpl(p, s, l, c) {}
  ParseException(const string &msg, const string &p, const string &s, int l, int c, exception &e)
    : Exception(msg, e), LocatorImpl(p, s, l, c) {}
  virtual ~ParseException() {}

  const string& getPublicId() const { return locator.getPublicId();}
  const string& getSystemId() const { return locator.getSystemId();}
  int getLineNumber() const { return locator.getLineNumber();}
  int getColumnNumber() const { return locator.getColumnNumber();}
private:
  LocatorImpl locator;
};

};
#endif 

