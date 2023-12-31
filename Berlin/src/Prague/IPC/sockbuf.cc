/*$Id: sockbuf.cc,v 1.3 1999/07/23 21:06:11 gray Exp $
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
#include "Prague/IPC/sockbuf.hh"
#include <iostream>
#include <netdb.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <cstdio>
#include <cerrno>
#include <sys/ioctl.h>

using namespace Prague;

const char *sockerr::errstr () const
{
  return strerror(err);
}

bool sockerr::io () const
// recoverable io error.
{
  switch (err) {
  case EWOULDBLOCK:
  case EINPROGRESS:
  case EALREADY:
    return true;
  }
  return false;
}

bool sockerr::arg () const
// recoverable argument error.
{
  switch (err) {
  case ENOTSOCK:
  case EDESTADDRREQ:
  case EMSGSIZE:
  case EPROTOTYPE:
  case ENOPROTOOPT:
  case EPROTONOSUPPORT:
  case ESOCKTNOSUPPORT:
  case EOPNOTSUPP:
  case EPFNOSUPPORT:
  case EAFNOSUPPORT:
  case EADDRINUSE:
  case EADDRNOTAVAIL:
    return true;
  }
  return false;
}

bool sockerr::op () const
// operational error encountered 
{
  switch (err) {
  case ENETDOWN:
  case ENETUNREACH:
  case ENETRESET:
  case ECONNABORTED:
  case ECONNRESET:
  case ENOBUFS:
  case EISCONN:
  case ENOTCONN:
  case ESHUTDOWN:
  case ETOOMANYREFS:
  case ETIMEDOUT:
  case ECONNREFUSED:
  case ELOOP:
  case ENAMETOOLONG:
  case EHOSTDOWN:
  case EHOSTUNREACH:
  case ENOTEMPTY:
    //  case EPROCLIM:
  case EUSERS:
  case EDQUOT:
    return true;
  }
  return false;
}

bool sockerr::conn () const
// return true if err is EISCONN, ENOTCONN, ECONNRESET, ECONNREFUSED,
// ETIMEDOUT, or EPIPE
{
  switch (err) {
  case EISCONN:
  case ENOTCONN:
  case ECONNRESET:
  case ECONNREFUSED:
  case ETIMEDOUT:
  case EPIPE:
    return true;
  }
  return false;
}

bool sockerr::addr () const
// return true if err is EADDRINUSE or EADDRNOTAVAIL
{
  switch (err) {
  case EADDRINUSE:
  case EADDRNOTAVAIL:
    return true;
  }
  return false;
}

bool sockerr::benign () const
// return true if err is EINTR, EWOULDBLOCK, or EAGAIN
{
  switch (err) {
  case EINTR:
  case EWOULDBLOCK:
    //  case EAGAIN:
    return true;
  }
  return false;
}

sockunixaddr::sockunixaddr (const char *path)
{
  sun_family = sockunixbuf::af_unix;
  ::strcpy (sun_path, path);
}

sockunixaddr::sockunixaddr (const sockunixaddr &suna)
{
  sun_family = sockunixbuf::af_unix;
  ::strcpy (sun_path, suna.sun_path);
}

sockinetaddr::sockinetaddr () 
{
  sin_family	  = sockinetbuf::af_inet;
  sin_addr.s_addr = htonl(INADDR_ANY);
  sin_port	  = 0;
}

sockinetaddr::sockinetaddr(unsigned long addr, int port_no)
// addr and port_no are in host byte order
{
  sin_family      = sockinetbuf::af_inet;
  sin_addr.s_addr = htonl(addr);
  sin_port	  = htons(port_no);
}

sockinetaddr::sockinetaddr(unsigned long addr, const char *sn, const char *pn)
// addr is in host byte order
{
  sin_family      = sockinetbuf::af_inet;
  sin_addr.s_addr = htonl (addr); // Added by cgay@cs.uoregon.edu May 29, 1993
  setport(sn, pn);
}

sockinetaddr::sockinetaddr (const char *host_name, int port_no)
// port_no is in host byte order
{
  setaddr(host_name);
  sin_port = htons(port_no);
}

sockinetaddr::sockinetaddr(const char *hn, const char *sn, const char *pn)
{
  setaddr(hn);
  setport(sn, pn);
}

sockinetaddr::sockinetaddr (const sockinetaddr &sina)
{
  sin_family      = sockinetbuf::af_inet;
  sin_addr.s_addr = sina.sin_addr.s_addr;
  sin_port	  = sina.sin_port;
}   

void sockinetaddr::setport(const char *sn, const char *pn)
{
  servent *sp = getservbyname(sn, pn);
  if (sp == 0) throw sockerr (EADDRNOTAVAIL);
  sin_port = sp->s_port;
}

int sockinetaddr::getport () const
{
  return ntohs (sin_port);
}

void sockinetaddr::setaddr(const char *host)
{
#if 0
  if ((sin_addr.s_addr = inet_addr(host)) == -1)
    {
      hostent *hp = gethostbyname(host);
      memcpy(&sin_addr, hp->h_addr, hp->h_length);
      sin_family = hp->h_addrtype;
    }
  else sin_family = sockinetbuf::af_inet;
#else
  in_addr ia;
  if (inet_aton(host, &ia) == 0)
    {
      hostent *hp = gethostbyname(host);
      memcpy(&sin_addr, hp->h_addr, hp->h_length);
      sin_family = hp->h_addrtype;
    }
  else
    {
      sin_addr.s_addr = inet_lnaof(ia);
      sin_family = sockinetbuf::af_inet;
    }
#endif
}

const char* sockinetaddr::gethostname () const
{
  if (sin_addr.s_addr == htonl(INADDR_ANY))
    {
      static char hostname[64];
      if (::gethostname(hostname, 63) == -1) return "";
      return hostname;		
    }
  hostent *hp = gethostbyaddr((const char *) &sin_addr, sizeof(sin_addr), family());
  if (hp == 0) return "";
  if (hp->h_name) return hp->h_name;
  return "";
}

sockbuf::sockbuf (int domain, sockbuf::type st, int proto)
  : ipcbuf(ios::in|ios::out)
{
  data->fd = ::socket (domain, st, proto);
  if (data->fd == -1) throw sockerr (errno);
}

int sockbuf::getopt (int op, void *buf, socklen_t len, int level) const
{
  if (::getsockopt (data->fd, level, op, (char *)buf, &len) == -1) throw sockerr (errno);
  return len;
}

void sockbuf::setopt (int op, void *buf, socklen_t len, int level) const
{
  if (::setsockopt (data->fd, level, op, (char *) buf, len) == -1) throw sockerr (errno);
}

void sockbuf::bind (const sockaddr &sa)
{
  if (::bind (data->fd, sa.addr(), sa.size()) == -1) throw sockerr (errno);
}

void sockbuf::connect (const sockaddr &sa)
{
  if (::connect(data->fd, sa.addr(), sa.size()) == -1) throw sockerr (errno);
}

void sockbuf::listen (int num)
{
  if (::listen (data->fd, num) == -1) throw sockerr (errno);
}

int sockbuf::accept (const sockaddr &sa)
{
  socklen_t len = sa.size();
  int soc = -1;
  if ((soc = ::accept (data->fd, sa.addr(), &len)) == -1) throw sockerr (errno);
  return soc;
}

int sockbuf::accept()
{
  int soc = -1;
  if ((soc = ::accept (data->fd, 0, 0)) == -1) throw sockerr (errno);
  return soc;
}

sockbuf::socklinger sockbuf::linger () const
{
  socklinger old (0, 0);
  getopt (so_linger, &old, sizeof (old));
  return old;
}

sockbuf::socklinger sockbuf::linger (sockbuf::socklinger opt) const
{
  socklinger old (0, 0);
  getopt (so_linger, &old, sizeof (old));
  setopt (so_linger, &opt, sizeof (opt));
  return old;
}

bool sockbuf::atmark() const
  // return true, if the read pointer for socket points to an
  // out of band data
{
  int arg;
  if (::ioctl (data->fd, SIOCATMARK, &arg) == -1) throw sockerr(errno);
  return arg;
}

int sockbuf::pgrp () const
// return the process group id that would receive SIGIO and SIGURG
// signals
{
  int arg;
  if (::ioctl (data->fd, SIOCGPGRP, &arg) == -1) throw sockerr (errno);
  return arg;
}

int sockbuf::pgrp (int new_pgrp) const
// set the process group id that would receive SIGIO and SIGURG signals.
// return the old pgrp
{
  int old = pgrp();
  if (::ioctl (data->fd, SIOCSPGRP, &new_pgrp) == -1) throw sockerr (errno);
  return old;
}

sockbuf::type sockbuf::gettype () const
{
  int ty=0;
  getopt (so_type, &ty, sizeof (ty));
  return sockbuf::type(ty);
}

int sockbuf::clearerror () const
{
  int err=0;
  getopt (so_error, &err, sizeof (err));
  return err;
}

bool sockbuf::debug () const
{
  int old = 0;
  getopt (so_debug, &old, sizeof (old));
  return old;
}

bool sockbuf::debug (bool set) const
{
  int old=0;
  int opt = set;
  getopt (so_debug, &old, sizeof (old));
  try
    {
      setopt (so_debug, &opt, sizeof (opt));
    }
  catch (sockerr e) { cerr << errno << ' ' << e.errstr() << endl;}
  return old;
}

bool sockbuf::reuseaddr () const
{
  int old = 0;
  getopt (so_reuseaddr, &old, sizeof (old));
  return old;
}

bool sockbuf::reuseaddr (bool set) const
{
  int old=0;
  int opt = set;
  getopt (so_reuseaddr, &old, sizeof (old));
  setopt (so_reuseaddr, &opt, sizeof (opt));
  return old;
}

bool sockbuf::keepalive () const
{
  int old = 0;
  getopt (so_keepalive, &old, sizeof (old));
  return old;
}

bool sockbuf::keepalive (bool set) const
{
  int old=0;
  int opt = set;
  getopt (so_keepalive, &old, sizeof (old));
  setopt (so_keepalive, &opt, sizeof (opt));
  return old;
}

int sockbuf::read (void *buf, int len)
{
  if (data->rtmo != -1 && !readready())
    throw sockerr(ETIMEDOUT);
  
  if (data->oobbit && atmark())
    throw sockoob();

  int rval = 0;
  if ((rval = ::read (data->fd, (char*) buf, len)) == -1)
    throw sockerr(errno);
  return rval;
}

int sockbuf::recv (void *buf, int len, int msgf)
{
  if (data->rtmo != -1 && !readready())
    throw sockerr(ETIMEDOUT);
  
  if (data->oobbit && atmark())
    throw sockoob();

  int rval = 0;
  if ((rval = ::recv(data->fd, (char*) buf, len, msgf)) == -1)
    throw sockerr (errno);
  return rval;
}

int sockbuf::recvfrom (sockaddr &sa, void *buf, int len, int msgf)
{
  if (data->rtmo != -1 && !readready())
    throw sockerr(ETIMEDOUT);
  
  if (data->oobbit && atmark())
    throw sockoob();

  int rval = 0;
  socklen_t sa_len = sa.size ();
  
  if ((rval = ::recvfrom(data->fd, (char*) buf, len, msgf, sa.addr(), &sa_len)) == -1)
    throw sockerr (errno);
  return rval;
}

int sockbuf::write(const void *buf, int len)
// upon error, write throws the number of bytes writen so far instead
// of sockerr.
{
  if (data->stmo != -1 && !writeready())
    throw sockerr(ETIMEDOUT);
  int wlen = 0;
  while(len > 0)
    {
      int wval = ::write(data->fd, (char*) buf, len);
      if (wval == -1) throw wlen;
      len -= wval;
      wlen += wval;
    }
  return wlen; // == len if every thing is all right
}

int sockbuf::send(const void *buf, int len, int msgf)
// upon error, write throws the number of bytes writen so far instead
// of sockerr.
{
  if (data->fd != -1 && !writeready())
    throw sockerr (ETIMEDOUT);
  
  int wlen = 0;
  while(len > 0)
    {
      int wval = ::send (data->fd, (char*) buf, len, msgf);
      if (wval == -1) throw wlen;
      len -= wval;
      wlen += wval;
    }
  return wlen;
}

int sockbuf::sendto(sockaddr &sa, const void *buf, int len, int msgf)
// upon error, write throws the number of bytes writen so far instead
// of sockerr.
{
  if (data->fd != -1 && !writeready())
    throw sockerr (ETIMEDOUT);
  int wlen = 0;
  while(len > 0)
    {
      int wval = ::sendto(data->fd, (char*) buf, len, msgf, sa.addr (), sa.size());
      if (wval == -1) throw wlen;
      len -= wval;
      wlen += wval;
    }
  return wlen;
}

sockunixbuf &sockunixbuf::operator = (const sockunixbuf &su)
{
  sockbuf::operator = (su);
  return *this;
}

sockinetbuf &sockinetbuf::operator = (const sockinetbuf &si)
{
  sockbuf::operator = (si);
  return *this;
}

sockinetaddr sockinetbuf::localaddr() const
{
  sockinetaddr sin;
  socklen_t len = sin.size();
  if (::getsockname(data->fd, sin.addr(), &len) == -1) throw sockerr (errno);
  return sin;
}

int sockinetbuf::localport() const
{
  sockinetaddr sin = localaddr();
  if (sin.family() != af_inet) return -1;
  return sin.getport();
}

const char *sockinetbuf::localhost() const
{
  sockinetaddr sin = localaddr();
  if (sin.family() != af_inet) return "";
  return sin.gethostname();
}

sockinetaddr sockinetbuf::peeraddr() const
{
  sockinetaddr sin;
  socklen_t len = sin.size();
  if (::getpeername(data->fd, sin.addr(), &len) == -1) throw sockerr (errno);
  return sin;
}

int sockinetbuf::peerport() const
{
  sockinetaddr sin = peeraddr();
  if (sin.family() != af_inet) return -1;
  return sin.getport();
}

const char *sockinetbuf::peerhost() const
{
  sockinetaddr sin = peeraddr();
  if (sin.family() != af_inet) return "";
  return sin.gethostname();
}

void sockinetbuf::bind_until_success (int portno)
// a. bind to (INADDR_ANY, portno)
// b. if success return
// c. if failure and errno is EADDRINUSE, portno++ and go to step a.
{
  for (;;)
    {
      try
	{
	  bind (portno++);
	}
      catch (sockerr e)
	{
	  if (e.number() != EADDRINUSE) throw;
	  continue;
	}
      break;
    }
}

void sockinetbuf::bind (unsigned long addr, const char *service, const char *protocol)
{
  bind(sockinetaddr(addr, service, protocol));
}

void sockinetbuf::bind (const char *host, const char *service, const char *protocol)
{
  bind(sockinetaddr(host, service, protocol));
}

void sockinetbuf::connect (unsigned long addr, const char *service, const char *protocol)
{
  connect(sockinetaddr(addr, service, protocol));
}

void sockinetbuf::connect (const char *host, const char *service, const char *protocol)
{
  connect(sockinetaddr(host, service, protocol));
}

int sockinetbuf::accept ()
{
  return sockbuf::accept ();
}

int sockinetbuf::accept (const sockaddr &sa)
{
  return sockbuf::accept(sa);
}

int sockinetbuf::accept (unsigned long addr, int port)
{
  return accept(sockinetaddr(addr, port));
}

int sockinetbuf::accept (const char *host, int port)
{
  return accept(sockinetaddr(host, port));
}

bool sockinetbuf::tcpnodelay () const
{
  struct protoent *proto = getprotobyname ("tcp");
  if (proto == 0) throw sockerr (ENOPROTOOPT);
  int old = 0;
  getopt (TCP_NODELAY, &old, sizeof (old), proto->p_proto);
  return old;
}

bool sockinetbuf::tcpnodelay (bool set) const
{
  struct protoent *proto = getprotobyname ("tcp");
  if (proto == 0) throw sockerr (ENOPROTOOPT);
  int old = 0;
  int opt = set;
  getopt (TCP_NODELAY, &old, sizeof (old), proto->p_proto);
  setopt (TCP_NODELAY, &opt, sizeof (opt), proto->p_proto);
  return old;
}


