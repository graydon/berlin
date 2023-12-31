/*$Id: ParserImpl.hh,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#ifndef _ParserImpl_hh
#define _ParserImpl_hh

#include <Prague/SAX/Parser.hh>

namespace SAX 
{

class ParserImpl : public Parser
{
public:
  ParserImpl();
  ~ParserImpl();
  virtual void setEntityResolver(EntityResolver *);
  virtual void setDTDHandler(DTDHandler *);
  virtual void setDocumentHandler(DocumentHandler *);
  virtual void setErrorHandler(ErrorHandler *);
  virtual void parse(InputSource &);
private:
  static void startElementHandler(void *, const char *, const char **);
  static void endElementHandler(void *, const char *);
  static void characterDataHandler(void *, const char *, int);
  void *parser;
  EntityResolver  *eresolver;
  DTDHandler      *dtdhandler;
  DocumentHandler *dochandler;
  ErrorHandler    *ehandler;
};

};

#endif

