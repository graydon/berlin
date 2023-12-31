/*$Id: ParserImpl.cc,v 1.2 2000/09/23 21:18:36 stefan Exp $
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
#include "Prague/SAX/ParserImpl.hh"
#include "Prague/SAX/AttributeListImpl.hh"
#include "Prague/SAX/InputSource.hh"
#include "Prague/SAX/DocumentHandler.hh"
#include "Prague/SAX/ParseException.hh"
#include "expat/xmlparse/xmlparse.h"
#include <fstream>

using namespace SAX;

ParserImpl::ParserImpl()
  : parser(XML_ParserCreate(0)),
    eresolver(0),
    dtdhandler(0),
    dochandler(0),
    ehandler(0)
{
  XML_SetUserData(parser, this);
}

ParserImpl::~ParserImpl()
{
  XML_ParserFree(parser);
}

void ParserImpl::setEntityResolver(EntityResolver *r)
{
  cout << "sorry, ParserImpl::setEntityResolver not implemented" << endl;
//   eresolver = e;
//   if (e);
//   else;
}

void ParserImpl::setDTDHandler(DTDHandler *h)
{
  cout << "sorry, ParserImpl::setDTDHandler not implemented" << endl;
//   dtdhandler = d;
}

void ParserImpl::setDocumentHandler(DocumentHandler *h)
{
  dochandler = h;
  if (dochandler)
    {
      XML_SetElementHandler(parser, startElementHandler, endElementHandler);
      XML_SetCharacterDataHandler(parser, characterDataHandler);
    }
  else
    {
      XML_SetElementHandler(parser, 0, 0);
      XML_SetCharacterDataHandler(parser, 0);
    }
}

void ParserImpl::setErrorHandler(ErrorHandler *h)
{
  cout << "sorry, ParserImpl::setErrorHandler not implemented" << endl;
//   ehandler = h;
}

void ParserImpl::parse(InputSource &source)
{
  ifstream ifs(source.getSystemId().c_str());
  if(!ifs)
    {
      cerr << "can't read input file source.getSystemId()" << endl;
      return;
    }
  if (dochandler) dochandler->startDocument();
  while(!ifs.eof())
    {
      char data [4096];
      ifs.read(data, 4096);
      if (!XML_Parse(parser, data, ifs.gcount(), false))
	throw ParseException(XML_ErrorString(XML_GetErrorCode(parser)),
			     source.getPublicId(),
			     source.getSystemId(),
			     XML_GetCurrentLineNumber(parser),
			     XML_GetCurrentColumnNumber(parser));
    }
  if (!XML_Parse(parser, 0, 0, true))
    throw ParseException(XML_ErrorString(XML_GetErrorCode(parser)),
			 source.getPublicId(),
			 source.getSystemId(),
			 XML_GetCurrentLineNumber(parser),
			 XML_GetCurrentColumnNumber(parser));
  if (dochandler) dochandler->endDocument();
}

void ParserImpl::startElementHandler(void *X, const XML_Char *name, const XML_Char **atts)
{
  ParserImpl *parser = reinterpret_cast<ParserImpl *>(X);
  AttributeListImpl attributes;
  if (atts) for (const XML_Char **i = atts; *i != 0; i+= 2) attributes.addAttribute(*i, "", *(i + 1));
  parser->dochandler->startElement(name, attributes);
};

void ParserImpl::endElementHandler(void *X, const XML_Char *name)
{
  ParserImpl *parser = reinterpret_cast<ParserImpl *>(X);
  parser->dochandler->endElement(name);
};

void ParserImpl::characterDataHandler(void *X, const XML_Char *d, int len)
{
  ParserImpl *parser = reinterpret_cast<ParserImpl *>(X);
  string data(d, len);
  parser->dochandler->characters(data);
};

