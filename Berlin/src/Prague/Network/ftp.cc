/*$Id: ftp.cc,v 1.4 1999/08/30 14:42:06 gray Exp $
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

#include <Prague/Network/ftp.hh>
#include <fstream.h>
#include <unistd.h>
#include <cstdlib>
#include <cstdio>
#include <cerrno>
#include <arpa/inet.h>

#if defined (__osf__) && defined (__DECCXX)
   extern "C" {
#    include <netdb.h>
   }
#else 
#  include <netdb.h>
#endif

using namespace Prague;

char reptype [][8] = {
  "A N",
  "A T",
  "A C",
  "E N",
  "E T",
  "E C",
  "I",
  "L "
};

char filestru [][8] = {
  "F",
  "R",
  "P"
};

char transmode [][8] = {
  "S",
  "B",
  "C"
};

// ftpdata waits on a port at the local machine.
// When a connection is made, it receives a file from remote
// host if the ostream o is set, or it sends a file to the remote
// host if the istream i is set.
ftp::replycodea ftp::ftpbuf::ftpdata (int portno, istream* i, ostream* o, const char* cmd, const char* arg)
{
  cout << "ftpdata" << endl;
  sockinetbuf sb (sockbuf::sock_stream, 0);
  sb.bind_until_success (portno);
  useraddr (sb.localaddr ());
  
  sb.listen (1);

  cout << "I'm here" << endl;
  if (send_cmd (cmd, arg) >= ftp::rca_error) return ftp::rca_error;
  cout << "...and still here" << endl;
  
  if (o)
    {
      sockbuf c = sb.accept ();
      cout << "...and still here" << endl;
      // read data from c and put it in o
      char buf [1024];
      int  rdsz;
      while ((rdsz = c.sys_read (buf, 1024)) != EOF) o->write (buf, rdsz);
      cout << "done" << endl;
    }
  else if (i)
    {
      sockbuf c = sb.accept (); 
      // read data from i and send it to c
      char buf [1024];
      int  rdsz;
      streambuf *rb = i->rdbuf ();
      while ((rdsz = rb->xsgetn (buf, 1024)) > 0)
	{
	  int wrsz = c.sys_write (buf, rdsz);
	  if (rdsz != wrsz) cerr << "write error\n";
	}
    }
  // Note: socketbuf object c must have been destructed by the time you reach
  //       here.
  return get_response ();
} 

ftp::replycodea ftp::ftpbuf::get_response ()
     // get all the response that one can get and send all of them to o
{
  // if o is 0, then we trash data.
  bool  firstline = true;
  while (underflow () != EOF)
    {
      int n = in_avail ();
      if (n < 5) continue;
      // data is of this form: 221 repsonse <CRLF> or 221-response <CRLF>
      char* q = gptr ();
      char* p = q;
      // zap upto <CRLF>
      int i = 0;
      for (i = 2; i <= n; i++, p++)
	if (*p == '\r' && *(p+1) == '\n') break;
      if (o) o->write (q, i);
      gbump (i);
      if (firstline)
	{
	  strncpy (replycode, q, 3);
	  replycode [3] = ' ';
	  if (q [3] == ' ') break;
	  firstline = false;
	}
      else if (strncmp (q, replycode, 4) == 0) break;
    }
  return (replycodea) replycode [0];
}

ftp::replycodea ftp::ftpbuf::send_cmd (const char* cmd, const char* arg)
{
  xsputn (cmd, ::strlen (cmd));
  if (arg)
    {
      xsputn (" ", 1);
      xsputn (arg, ::strlen (arg));
    }
  xsputn ("\r\n", 2);
  sync ();
  return get_response ();
}

ftp::ftp (ostream* out)
  : ios (0)
{
  ios::init (new ftpbuf (out));
}

ftp::ftpbuf::ftpbuf (ostream* out)
  : protocol::protocolbuf (protocol::tcp),
    o (out)
{
  replycode [4] = 0;
}

void ftp::ftpbuf::serve_clients (int portno)
// right now no server ftp class can be used as a server
{}

ftp::replycodea ftp::ftpbuf::cd (const char* dir)
{
  return send_cmd ("CWD", dir);
}

ftp::replycodea ftp::ftpbuf::useraddr (sockinetaddr sa)
{
  if (sa.sin_addr.s_addr == 0)
    {
      // local host
      char hostname [64];
      if (::gethostname (hostname, 63) == -1);// throw sockerr (EADDRNOTAVAIL);
      hostent* hp = gethostbyname (hostname);
      //     if (hp == 0) throw sockerr (EADDRNOTAVAIL);
      memcpy (&sa.sin_addr, hp->h_addr, hp->h_length);
    }

  struct in_addr ina = sa.sin_addr;
  int    portno      = ntohs(sa.sin_port);
  char*  ina_p       = inet_ntoa (ina);
  char   addr [80];
  
  char* p = 0;
  strcpy (addr, ina_p);
  while ((p = strchr (addr, '.'))) *p = ',';
  
  int hi_portno = portno >> 8;
  int lo_portno = portno & 0xff;
  
  sprintf (addr + strlen (addr), ",%d,%d", hi_portno, lo_portno);
  
  return send_cmd ("PORT", addr);
}

ftp::replycodea ftp::ftpbuf::useraddr (const char* hostname, int portno)
{
  return useraddr (sockinetaddr (hostname, portno));
}

ftp::replycodea ftp::ftpbuf::server_port (int portno)
{
  int hi_portno = portno >> 8;
  int lo_portno = portno & 0xff;
  char port [80];
  sprintf (port, "%d,%d", hi_portno, lo_portno);
  return send_cmd ("PASV", port);
}
  
ftp::replycodea ftp::ftpbuf::rep_type (ftp::reptype rt)
{
  return send_cmd ("TYPE", ::reptype [int(rt)]);
}

ftp::replycodea ftp::ftpbuf::file_stru (ftp::filestru fs)
{
  return send_cmd ("STRU", ::filestru [int(fs)]);
}

ftp::replycodea ftp::ftpbuf::trans_mode (ftp::transmode tm)
{
  return send_cmd ("STRU", ::transmode [int(tm)]);
}

ftp::replycodea ftp::ftpbuf::getfile (const char* rpath, const char* lpath)
{
  if (lpath == 0) lpath = rpath;
  if (rpath == 0) list();
  ofstream f (lpath);
  return ftpdata (10000, 0, &f, "RETR", rpath);
}

ftp::replycodea ftp::ftpbuf::list (const char* rpath, int justnames)
{
  if (justnames) return ftpdata (10000, 0, o, "NLST", rpath);
  else return ftpdata (10000, 0, o, "LIST", rpath);
}

ftp::replycodea ftp::ftpbuf::putfile (const char* lpath, const char* rpath)
{
  if (rpath == 0) rpath = lpath;
  if (lpath == 0) return ftp::rca_error;
  ifstream f(lpath);
  return ftpdata (10000, &f, 0, "STOR", rpath);
}

ftp::replycodea ftp::ftpbuf::putfile (const char* lpath)
{
  if (lpath == 0) return ftp::rca_error;
  ifstream f(lpath);
  return ftpdata (10000, &f, 0, "STOU", lpath);
}
  
ftp::replycodea ftp::ftpbuf::append  (const char* lpath, const char* rpath)
{
  if (lpath == 0) return ftp::rca_error;
  if (rpath == 0) rpath = lpath;
  ifstream f(lpath);
  return ftpdata (10000, &f, 0, "APPE", 0);
}

ftp::replycodea ftp::ftpbuf::allocate (int numbytes)
{
  char b[32];
  sprintf (b, "%d", numbytes);
  return send_cmd ("ALLO", b);
}

ftp::replycodea ftp::ftpbuf::restart (int marker)
{
  char b[32];
  sprintf (b, "%d", marker);
  return send_cmd ("REST", b);
}

ftp::replycodea ftp::ftpbuf::rename (const char* rpath, const char* newrpath)
{
  if (rpath == 0 || newrpath == 0) return ftp::rca_error;
  if (send_cmd ("RNFR", rpath) >= ftp::rca_error) return rca_error;
  return send_cmd ("RNTO", newrpath);
}

ftp::replycodea ftp::ftpbuf::rmfile (const char* rpath)
{
  return send_cmd ("DELE", rpath);
}

ftp::replycodea ftp::ftpbuf::rmdir  (const char* rpath)
{
  return send_cmd ("RMD", rpath);
}

ftp::replycodea ftp::ftpbuf::mkdir  (const char* rpath)
{
  return send_cmd ("MKD", rpath);
}

