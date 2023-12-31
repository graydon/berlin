/*$Id: smtp.cc,v 1.5 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1992-1996 Gnanasekaran Swaminathan <gs4t@virginia.edu>
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

#include <Prague/Network/smtp.hh>
#include <fcntl.h>
#include <unistd.h>
#include <cstdlib>
#include <cstring>

using namespace Prague;

void smtp::smtpbuf::get_response()
     // get all the response that one can get and send all of them to o
{
  // if o is 0, then we trash data.
  while (underflow() != EOF)
    {
      int n = in_avail();
      if (n < 5) continue;
      // data is of this form: 221 repsonse <CRLF> or 221-response <CRLF>
      char* q = gptr ();
      char* p = q;
      // zap upto <CRLF>
      for (int i = 2; i <= n; i++, p++)
	if (*p == '\r' && *(p+1) == '\n')
	  {
	    if(_os) _os->write(q, i);
	    gbump(i);
	    break;
	  }
    if (q[3] != '-') break;
    }
}

void smtp::smtpbuf::send_cmd(const std::string &cmd, const std::string &s, const std::string &p)
{
  xsputn(cmd.c_str(), cmd.length());
  if (s.length()) xsputn(s.c_str(), s.length());
  if (p.length()) xsputn(p.c_str(), p.length());
  xsputn("\r\n", 2);
  sync();
  if (_os) get_response();
}

void smtp::smtpbuf::helo()
{
  if (_os) get_response();
  send_cmd("HELO ", localaddr().hostname());
}

void smtp::smtpbuf::mail(const std::string &from)
{
  if (from.length()) send_cmd("MAIL FROM:<", from, ">");
  else send_cmd("MAIL FROM:<>");
}

void smtp::smtpbuf::rcpt(const std::string &to)
{
  if (to.length()) send_cmd("RCPT TO:<", to, ">");
}

void smtp::smtpbuf::help(const std::string &s)
{
  send_cmd("HELP ", s);
}

void smtp::smtpbuf::send_buf(const char *buf, int len)
{
  if (buf == 0 || len <= 0) return;
  // send line by line
  const unsigned char *p = (const unsigned char*) buf;
  if (*p == '.') sputc((unsigned int) '.');
  for (int i = 0; i < len; i++, p++)
    {
      if (*p == '\n')
	{
	  sputc((unsigned int) '\r');
	  sputc(*p);
	  if(*(p+1) == '.') sputc((unsigned int) '.');
	}
      else sputc(*p);
    }
}

void smtp::smtpbuf::data(const char *buf, int len)
{
  data ();
  send_buf(buf, len);
  xsputn("\r\n.\r\n", 5);
  sync();
  if (_os) get_response();
}

void smtp::smtpbuf::data(const std::string &filename)
{
  data ();
  int  fd = 0;
  char buf [1024];
  int  rcnt;
  if (filename.empty() || (fd = ::open(filename.c_str(), O_RDONLY )) == -1) fd = 0;

  while ((rcnt = ::read(fd, buf, 1024)) > 0) send_buf(buf, rcnt);
  xsputn("\r\n.\r\n", 5);
  sync();
  if (_os) get_response();
}

int smtp::get_response(char *buf, int len)
     // same as get line except what it returns
     // return 1 if output continues after this line
     // return 0 if output has terminated
{
  if (len < 8)
    {
      getline(buf, len);
      return 0;
    }
  buf[3] = 0;
  getline(buf, len);
  return buf[3] == '-';
}
  
std::ostream &operator << (std::ostream &os, smtp &s)
{
  char buf[1024];
  int  cont = 1;
  while (cont)
    {
      cont = s.get_response(buf, 1024);
      os << buf << std::endl;
    }
  return os;
}

void smtp::smtpbuf::serve_clients(int portno)
{
}

