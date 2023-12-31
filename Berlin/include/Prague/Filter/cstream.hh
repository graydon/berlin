/* $Id: cstream.hh,v 1.2 2000/08/31 18:52:32 stefan Exp $ */
#ifndef _cstream_hh
#define _cstream_hh

#include <iostream>
#include <Prague/Filter/cbuf.hh>

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

#endif
