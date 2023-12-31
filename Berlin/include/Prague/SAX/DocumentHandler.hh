/*$Id: DocumentHandler.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#ifndef _DocumentHandler_hh
#define _DocumentHandler_hh

namespace SAX 
{
class Locator;
class Exception;
class AttributeList;

class DocumentHandler
{
public:
  virtual ~DocumentHandler() {};

  virtual void setDocumentLocator(const Locator &) = 0;

  virtual void startDocument() = 0;
  virtual void endDocument() = 0;

  virtual void startElement(const string &name, const AttributeList &atts)  = 0;
  virtual void endElement(const string &name)  = 0;
  virtual void characters(const string &) = 0;

  virtual void ignorableWhitespace(const string &ch) = 0;
  virtual void processingInstruction(const string &target, const string &data) = 0;
};

};

#endif

