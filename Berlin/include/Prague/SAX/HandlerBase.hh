/*$Id: HandlerBase.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#ifndef _HandlerBase_hh
#define _HandlerBase_hh

#include <iosfwd>

#include <Prague/SAX/EntityResolver.hh>
#include <Prague/SAX/DTDHandler.hh>
#include <Prague/SAX/DocumentHandler.hh>
#include <Prague/SAX/ErrorHandler.hh>
#include <Prague/SAX/ParseException.hh>

namespace SAX
{

class HandlerBase : public EntityResolver, public DTDHandler, public DocumentHandler, public ErrorHandler
{
public:
  virtual ~HandlerBase() {}
  virtual istream *resolveEntity(const string &, const string &) { return 0;}
  virtual void notationDecl(const string &, const string &, const string &) {}
  virtual void unparsedEntityDecl(const string &, const string &, const string &, const string &) {}
  virtual void setDocumentLocator(const Locator &locator) {}
  virtual void startDocument() {}
  virtual void endDocument() {}
  virtual void startElement(const string &, const AttributeList &) {}
  virtual void endElement(const string &) {}
  virtual void characters(const string &) {}
  virtual void ignorableWhitespace (const string &) {}
  virtual void processingInstruction(const string &, const string &) {}
  virtual void warning(ParseException &) {}
  virtual void error(ParseException &) {}
  virtual void fatalError(ParseException &e) { throw e;}
};

};

#endif

