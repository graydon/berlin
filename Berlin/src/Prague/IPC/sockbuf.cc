/*$Id: sockbuf.cc,v 1.11 2001/03/28 06:09:47 stefan Exp $
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
#include "Prague/IPC/sockbuf.hh"
#include <iostream>
#include <netdb.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <cstdio>
#include <cstring>
#include <cerrno>
#include <sys/ioctl.h>
/*
 * Need BSD Socket ioctls
 * SIOCATMARK, SIOCGPGRP, SIOCSPGRP
 * this probably should be tested for during config
 */
#ifdef __sun__
#include <sys/sockio.h>
#endif

using namespace Prague;

const char *sockerr::errstr () const
{
  return strerror(err);
}

bool sockerr::io() const
// recoverable io error.
{
  switch (err)
    {
    case EWOULDBLOCK:
    case EINPROGRESS:
    case EALREADY:
      return true;
    }
  return false;
}

bool sockerr::arg() const
// recoverable argument error.
{
  switch (err)
    {
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

bool sockerr::op() const
// operational error encountered 
{
  switch (err)
    {
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

bool sockerr::conn() const
// return true if err is EISCONN, ENOTCONN, ECONNRESET, ECONNREFUSED,
// ETIMEDOUT, or EPIPE
{
  switch (err)
    {
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

bool sockerr::addr() const
// return true if err is EADDRINUSE or EADDRNOTAVAIL
{
  switch (err)
    {
    case EADDRINUSE:
    case EADDRNOTAVAIL:
      return true;
    }
  return false;
}

bool sockerr::benign() const
// return true if err is EINTR, EWOULDBLOCK, or EAGAIN
{
  switch (err)
    {
    case EINTR:
    case EWOULDBLOCK:
      //  case EAGAIN:
      return true;
    }
  return false;
}

sockunixaddr::sockunixaddr(const std::string &path)
{
  sun_family = sockunixbuf::af_unix;
  ::strcpy(sun_path, path.c_str());
}

sockunixaddr::sockunixaddr(const sockunixaddr &suna)
{
  sun_family = sockunixbuf::af_unix;
  ::strcpy (sun_path, suna.sun_path);
}

sockinetaddr::sockinetaddr() 
{
  sin_family	  = sockinetbuf::af_inet4;
  sin_addr.s_addr = htonl(INADDR_ANY);
  sin_port	  = 0;
}

sockinetaddr::sockinetaddr(unsigned long addr, int port_no)
// addr and port_no are in host byte order
{
  sin_family      = sockbuf::af_inet4;
  sin_addr.s_addr = htonl(addr);
  sin_port	  = htons(port_no);
}

sockinetaddr::sockinetaddr(unsigned long addr, const std::string &sn, const std::string &pn)
// addr is in host byte order
{
  sin_family      = sockbuf::af_inet4;
  sin_addr.s_addr = htonl(addr);
  port(sn, pn);
}

sockinetaddr::sockinetaddr(const std::string &host_name, int port_no)
// port_no is in host byte order
{
  addr(host_name);
  sin_port = htons(port_no);
}

sockinetaddr::sockinetaddr(const std::string &hn, const std::string &sn, const std::string &pn)
{
  addr(hn);
  port(sn, pn);
}

sockinetaddr::sockinetaddr(const sockinetaddr &sina)
{
  sin_family      = sockbuf::af_inet4;
  sin_addr.s_addr = sina.sin_addr.s_addr;
  sin_port	  = sina.sin_port;
}   

void sockinetaddr::port(const std::string &sn, const std::string &pn)
{
  servent *sp = getservbyname(sn.c_str(), pn.c_str());
  if (sp == 0) throw sockerr (EADDRNOTAVAIL);
  sin_port = sp->s_port;
}

int sockinetaddr::port() const
{
  return ntohs(sin_port);
}

void sockinetaddr::addr(const std::string &host)
{
  in_addr ia;
  if (inet_aton(host.c_str(), &ia) == 0)
    {
      hostent *hp = gethostbyname(host.c_str());
      if (!hp)
	{
	  std::cerr << "sockinetaddr::addr :" << std::endl;
	  std::cerr << "error in host lookup" << std::endl;
	  std::cerr << "error processing for this problem has not yet been implemented, sorry" << std::endl;
	  exit(-1);
	}
      memcpy(&sin_addr, hp->h_addr, hp->h_length);
      sin_family = hp->h_addrtype;
    }
  else
    {
      sin_addr.s_addr = inet_lnaof(ia);
      sin_family = sockbuf::af_inet4;
    }
}

std::string sockinetaddr::hostname() const
{
  if (sin_addr.s_addr == htonl(INADDR_ANY))
    {
      char name[64];
      if (gethostname(name, 63) == -1) return "";
      return name;		
    }
  hostent *hp = gethostbyaddr((const char *) &sin_addr, sizeof(sin_addr), family());
  if (hp == 0) return "";
  if (hp->h_name) return hp->h_name;
  return "";
}

sockbuf::sockbuf(int domain, sockbuf::type st, int proto)
  : ipcbuf(std::ios::in|std::ios::out)
{
  int socket = ::socket(domain, st, proto);
  if (socket == -1) throw sockerr (errno);
  else fd(socket);
}

int sockbuf::getopt(int op, void *buf, socklen_t len, int level) const
{
  if (::getsockopt(fd(), level, op, (char *)buf, &len) == -1) throw sockerr(errno);
  return len;
}

void sockbuf::setopt(int op, void *buf, socklen_t len, int level) const
{
  if (::setsockopt(fd(), level, op, (char *) buf, len) == -1) throw sockerr(errno);
}

void sockbuf::listen(int num)
{
  if (::listen(fd(), num) == -1) throw sockerr(errno);
}

sockbuf::socklinger sockbuf::linger() const
{
  socklinger old(0, 0);
  getopt(so_linger, &old, sizeof(old));
  return old;
}

sockbuf::socklinger sockbuf::linger(sockbuf::socklinger opt) const
{
  socklinger old(0, 0);
  getopt(so_linger, &old, sizeof(old));
  setopt(so_linger, &opt, sizeof(opt));
  return old;
}

bool sockbuf::atmark() const
{
  int arg;
  if (::ioctl(fd(), SIOCATMARK, &arg) == -1) throw sockerr(errno);
  return arg;
}

int sockbuf::pgrp() const
{
  int arg;
  if (::ioctl(fd(), SIOCGPGRP, &arg) == -1) throw sockerr(errno);
  return arg;
}

int sockbuf::pgrp(int new_pgrp) const
{
  int old = pgrp();
  if (::ioctl(fd(), SIOCSPGRP, &new_pgrp) == -1) throw sockerr(errno);
  return old;
}

sockbuf::type sockbuf::gettype() const
{
  int ty=0;
  getopt(so_type, &ty, sizeof (ty));
  return sockbuf::type(ty);
}

int sockbuf::clearerror() const
{
  int err=0;
  getopt (so_error, &err, sizeof (err));
  return err;
}

bool sockbuf::debug() const
{
  int old = 0;
  getopt (so_debug, &old, sizeof (old));
  return old;
}

bool sockbuf::debug(bool set) const
{
  int old=0;
  int opt = set;
  getopt(so_debug, &old, sizeof (old));
  try
    {
      setopt (so_debug, &opt, sizeof (opt));
    }
  catch (sockerr e) { std::cerr << errno << ' ' << e.errstr() << std::endl;}
  return old;
}

bool sockbuf::reuseaddr() const
{
  int old = 0;
  getopt(so_reuseaddr, &old, sizeof (old));
  return old;
}

bool sockbuf::reuseaddr(bool set) const
{
  int old=0;
  int opt = set;
  getopt (so_reuseaddr, &old, sizeof (old));
  setopt (so_reuseaddr, &opt, sizeof (opt));
  return old;
}

bool sockbuf::keepalive() const
{
  int old = 0;
  getopt (so_keepalive, &old, sizeof (old));
  return old;
}

bool sockbuf::keepalive(bool set) const
{
  int old=0;
  int opt = set;
  getopt (so_keepalive, &old, sizeof (old));
  setopt (so_keepalive, &opt, sizeof (opt));
  return old;
}

int sockbuf::read(void *buf, int len)
{
//   if (data->rtmo != -1 && !readready())
//     throw sockerr(ETIMEDOUT);
  
  if (oob() && atmark())
    throw sockoob();

  int rval = 0;
  if ((rval = ::read (fd(), (char *)buf, len)) == -1)
    throw sockerr(errno);
  return rval;
}

int sockbuf::recv(void *buf, int len, int msgf)
{
//   if (data->rtmo != -1 && !readready())
//     throw sockerr(ETIMEDOUT);
  
  if (oob() && atmark())
    throw sockoob();

  int rval = 0;
  if ((rval = ::recv(fd(), (char*) buf, len, msgf)) == -1)
    throw sockerr (errno);
  return rval;
}

int sockbuf::recvfrom(sockaddr &sa, void *buf, int len, int msgf)
{
//   if (data->rtmo != -1 && !readready())
//     throw sockerr(ETIMEDOUT);
  
  if (oob() && atmark())
    throw sockoob();

  int rval = 0;
  socklen_t sa_length = sa.size ();
  
  if ((rval = ::recvfrom(fd(), (char*) buf, len, msgf, sa.addr(), &sa_length)) == -1)
    throw sockerr (errno);
  return rval;
}

int sockbuf::write(const void *buf, int len)
// upon error, write throws the number of bytes writen so far instead
// of sockerr.
{
//   if (data->stmo != -1 && !writeready())
//     throw sockerr(ETIMEDOUT);
  int wlen = 0;
  while(len > 0)
    {
      int wval = ::write(fd(), (char*) buf, len);
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
//   if (fd() != -1 && !writeready())
//     throw sockerr (ETIMEDOUT);
  
  int wlen = 0;
  while(len > 0)
    {
      int wval = ::send (fd(), (char*) buf, len, msgf);
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
//   if (fd() != -1 && !writeready())
//     throw sockerr (ETIMEDOUT);
  int wlen = 0;
  while(len > 0)
    {
      int wval = ::sendto(fd(), (char *)buf, len, msgf, sa.addr (), sa.size());
      if (wval == -1) throw wlen;
      len -= wval;
      wlen += wval;
    }
  return wlen;
}

sockunixaddr sockunixbuf::addr() const
{
  sockunixaddr sa;
  socklen_t len = sa.size();
  if (::getsockname(fd(), sa.addr(), &len) == -1) throw sockerr (errno);
  return sa;
}

void sockunixbuf::bind(const sockunixaddr &sa)
{
  if (::bind (fd(), sa.addr(), sa.size()) == -1) throw sockerr(errno);
}

sockunixbuf *sockunixbuf::accept(sockunixaddr &sa)
{
  socklen_t len = sa.size();
  int soc = -1;
  if ((soc = ::accept(fd(), sa.addr(), &len)) == -1) throw sockerr(errno);
  return new sockunixbuf(soc);
}

sockunixbuf *sockunixbuf::accept()
{
  int soc = -1;
  if ((soc = ::accept(fd(), 0, 0)) == -1) throw sockerr(errno);
  return new sockunixbuf(soc);
}

void sockunixbuf::connect(const sockunixaddr &sa)
{
  if (::connect(fd(), sa.addr(), sa.size()) == -1) throw sockerr(errno);
}

sockinetaddr sockinetbuf::localaddr() const
{
  sockinetaddr sa;
  socklen_t len = sa.size();
  if (::getsockname(fd(), sa.addr(), &len) == -1) throw sockerr(errno);
  return sa;
}

sockinetaddr sockinetbuf::peeraddr() const
{
  sockinetaddr sa;
  socklen_t len = sa.size();
  if (::getpeername(fd(), sa.addr(), &len) == -1) throw sockerr(errno);
  return sa;
}

void sockinetbuf::bind(const sockinetaddr &sa)
{
  if (::bind (fd(), sa.addr(), sa.size()) == -1) throw sockerr(errno);
}

void sockinetbuf::bind_until_success(int portno)
// a. bind to (INADDR_ANY, portno)
// b. if success return
// c. if failure and errno is EADDRINUSE, portno++ and go to step a.
{
  for (;;)
    {
      try
	{
	  bind(sockinetaddr((unsigned long) INADDR_ANY, portno++));
	}
      catch (const sockerr &e)
	{
	  if (e.number() != EADDRINUSE) throw;
	  continue;
	}
      break;
    }
}

void sockinetbuf::connect(const sockinetaddr &sa)
{
  if (::connect(fd(), sa.addr(), sa.size()) == -1) throw sockerr(errno);
}

sockinetbuf *sockinetbuf::accept(sockinetaddr &sa)
{
  socklen_t len = sa.size();
  int soc = -1;
  if ((soc = ::accept(fd(), sa.addr(), &len)) == -1) throw sockerr(errno);
  return new sockinetbuf(soc);
}

sockinetbuf *sockinetbuf::accept()
{
  int soc = -1;
  if ((soc = ::accept(fd(), 0, 0)) == -1) throw sockerr(errno);
  return new sockinetbuf(soc);
}

bool sockinetbuf::tcpnodelay() const
{
  struct protoent *proto = getprotobyname("tcp");
  if (proto == 0) throw sockerr(ENOPROTOOPT);
  int old = 0;
  getopt(TCP_NODELAY, &old, sizeof(old), proto->p_proto);
  return old;
}

bool sockinetbuf::tcpnodelay(bool set) const
{
  struct protoent *proto = getprotobyname("tcp");
  if (proto == 0) throw sockerr(ENOPROTOOPT);
  int old = 0;
  int opt = set;
  getopt(TCP_NODELAY, &old, sizeof(old), proto->p_proto);
  setopt(TCP_NODELAY, &opt, sizeof(opt), proto->p_proto);
  return old;
}
