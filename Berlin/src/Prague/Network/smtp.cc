/*$Id: smtp.cc,v 1.2 1999/07/23 19:01:38 gray Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
 * http://www.berlin-consortium.org
 *
 * this file is based on code from the socket++ library
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
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

#include <Prague/Network/smtp.hh>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

using namespace Prague;

void smtp::smtpbuf::get_response ()
     // get all the response that one can get and send all of them to o
{
  // if o is 0, then we trash data.
  while (underflow () != EOF)
    {
      int n = in_avail ();
      if (n < 5) continue;
      // data is of this form: 221 repsonse <CRLF> or 221-response <CRLF>
      char* q = gptr ();
      char* p = q;
      // zap upto <CRLF>
      for (int i = 2; i <= n; i++, p++)
	if (*p == '\r' && *(p+1) == '\n')
	  {
	    if (o) o->write (q, i);
	    gbump (i);
	    break;
	  }
    if (q [3] != '-') break;
    }
}

void smtp::smtpbuf::send_cmd (const char* cmd, const char* s, const char* p)
{
  xsputn (cmd, ::strlen (cmd));
  if (s) xsputn (s, ::strlen (s));
  if (p) xsputn (p, ::strlen (p));
  xsputn ("\r\n", 2);
  sync ();
  if (o) get_response ();
}

void smtp::smtpbuf::helo ()
{
  if (o) get_response ();
  send_cmd ("HELO ", localhost ());
}

void smtp::smtpbuf::mail (const char* reverse_path)
{
  if (reverse_path) send_cmd ("MAIL FROM:<", reverse_path, ">");
  else send_cmd ("MAIL FROM:<>");
}

void smtp::smtpbuf::rcpt (const char* forward_path)
{
  if (forward_path) send_cmd ("RCPT TO:<", forward_path, ">");
}

void smtp::smtpbuf::help (const char* s)
{
  send_cmd ("HELP ", s);
}

void smtp::smtpbuf::send_buf (const char* buf, int len)
{
  if (buf == 0 || len <= 0) return;
  // send line by line
  const unsigned char* p = (const unsigned char*) buf;
  if (*p == '.') sputc ((unsigned int) '.');
  for (int i = 0; i < len; i++, p++)
    {
      if (*p == '\n')
	{
	  sputc ((unsigned int) '\r');
	  sputc (*p);
	  if (*(p+1) == '.') sputc ((unsigned int) '.');
	}
      else sputc (*p);
    }
}

void smtp::smtpbuf::data (const char* buf, int len)
{
  data ();
  send_buf (buf, len);
  xsputn ("\r\n.\r\n", 5);
  sync ();
  if (o) get_response ();
}

void smtp::smtpbuf::data (const char* filename)
{
  data ();
  int  fd = 0;
  char buf [1024];
  int  rcnt;
  if (filename == 0 || (fd = ::open (filename, O_RDONLY )) == -1) fd = 0;

  while ((rcnt = ::read (fd, buf, 1024)) > 0) send_buf (buf, rcnt);
  xsputn ("\r\n.\r\n", 5);
  sync ();
  if (o) get_response ();
}

int smtp::get_response (char* buf, int len)
     // same as get line except what it returns
     // return 1 if output continues after this line
     // return 0 if output has terminated
{
  if (len < 8)
    {
      this->getline (buf, len);
      return 0;
    }
  buf [3] = 0;
  this->getline (buf, len);
  return buf [3] == '-';
}
  
ostream& operator << (ostream& o, smtp& s)
{
  char buf [1024];
  int  cont = 1;
  while (cont)
    {
      cont = s.get_response (buf, 1024);
      o << buf << endl;
    }
  return o;
}


void smtp::smtpbuf::serve_clients (int portno)
{
}

