/*$Id: InputSource.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#ifndef _InputSource_hh
#define _InputSource_hh

#include <iosfwd>
#include <string>

namespace SAX
{

class InputSource
{
 public:
  InputSource() : is(0) {}
  InputSource(const string &id) : systemID(id), is(0) {}
  InputSource(istream &iss) : is(&iss) {}
  virtual ~InputSource() {}
  void setPublicId(const string &id) { publicID = id;}
  const string &getPublicId() const { return publicID;}
  void setSystemId(const string &id) { systemID = id;}
  const string &getSystemId() const { return systemID;}

  void setStream(istream &iss) { is = &iss;}
  istream *getStream() const { return is;}

  void setEncoding(const string &enc) { encoding = enc;}
  const string &getEncoding() const { return encoding;}
 private:
  string   publicID;
  string   systemID;
  istream *is;
  string   encoding;
};

};

#endif

