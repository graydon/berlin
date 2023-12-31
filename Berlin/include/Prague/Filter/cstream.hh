/* $Id: cstream.hh,v 1.1 1999/04/22 14:44:58 gray Exp $ */
#ifndef _cstream_h
#define _cstream_h

#include <iostream.h>
#include <OffiX/Filter/cbuf.h>

/*
 *	Class name : cstream
 *
 *	Description : a specialized istream for input containing comments...
 */
class cstream : public istream
{
public:
  cstream(istream &is, char comment = '#') : istream(new cbuf(is.rdbuf(), comment)) {}
};

#endif /* _cstream_h */
