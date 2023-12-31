/*$Id: sockbuf.hh,v 1.12 2001/03/28 06:09:47 stefan Exp $
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
#ifndef _Prague_sockbuf_hh
#define _Prague_sockbuf_hh
#include <Prague/config.hh>
#include <Prague/IPC/ipcbuf.hh>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <string>

namespace Prague
{

#if defined(__linux__) || defined(__FreeBSD__)  // XXX: ugly!!
#  define MSG_MAXIOVLEN	 16
#endif // __linux__ or __FreeBSD__

//. socket exception classes
class sockerr
{
public:
  sockerr(int e) : err(e) {}
  const char* what() const { return "sockerr";}
  int number() const { return err; }
  const char *errstr() const;
  bool error(int eno) const { return eno == err; }
  //. non-blocking and interrupt io recoverable error.
  bool io() const;
  //. incorrect argument supplied. recoverable error.
  bool arg() const;
  //. operational error. recovery difficult.
  bool op() const;
  //. connection error
  bool conn() const;
  //. address error
  bool addr() const;
  //. recoverable read/write error like EINTR etc.
  bool benign() const;
private:
  int  err;
};

class sockoob
{
public:
  const char *what() const { return "sockoob";}
};

//. abstract base for socket addresses}
class sockaddr
{
public:
  virtual	   ~sockaddr() {}
  operator const ::sockaddr *() const { return addr();}
  operator ::sockaddr       *() { return addr();}
  virtual int	             size() const = 0;
  virtual int	             family() const = 0;
  virtual const ::sockaddr  *addr() const = 0;
  virtual ::sockaddr        *addr() = 0;
};

//. socket unix address representation
class sockunixaddr : public sockaddr_un, public sockaddr
{
public:
  sockunixaddr() {}
  sockunixaddr(const std::string &);
  sockunixaddr(const sockunixaddr &);
  ~sockunixaddr() {}
  int               size() const { return sizeof (sockaddr_un);}
  int               family() const { return sun_family;}
  const ::sockaddr *addr() const { return reinterpret_cast<const ::sockaddr *>(this);}
  ::sockaddr       *addr() { return reinterpret_cast< ::sockaddr *>(this);}
  const char       *path() { return sun_path;}
};

//. socket internet address representation
class sockinetaddr : public sockaddr_in, public sockaddr
{
public:
  sockinetaddr();
  sockinetaddr(unsigned long, int port_no=0);
  sockinetaddr(const std::string &, int port_no=0);
  sockinetaddr(unsigned long, const std::string &, const std::string &pn = "tcp");
  sockinetaddr(const std::string &, const std::string &, const std::string &pn = "tcp");
  sockinetaddr(const sockinetaddr &);
  ~sockinetaddr() {}
  int               size() const { return sizeof (sockaddr_in);}
  int               family() const { return sin_family;}
  const ::sockaddr *addr() const { return reinterpret_cast<const ::sockaddr *>(this);}
  ::sockaddr       *addr() { return reinterpret_cast< ::sockaddr *>(this);}
  int               port() const;
  std::string       hostname() const;
private:
  void              port(const std::string &, const std::string &pn = "tcp");
  void              addr(const std::string &);
};

struct msghdr;

//. an ipcbuf based on a socket
class sockbuf : public ipcbuf
{
public:
  enum domain
  {
    af_unix	        = AF_UNIX,
    af_inet4	        = AF_INET,
#ifdef AF_INET6
    af_inet6	        = AF_INET6,
#endif
    af_ipx	        = AF_IPX,
#ifdef AF_NETLINK
    af_netlink          = AF_NETLINK,
#endif
    af_x25              = AF_X25,
#ifdef AF_AX25
    af_ax25             = AF_AX25,
#endif
#ifdef AF_ATMPVC
    af_atmpvc           = AF_ATMPVC,
#endif
#ifdef AF_PACKET
    af_packet           = AF_PACKET,
#endif
    af_appletalk        = AF_APPLETALK
  };
  enum type
  {
    sock_stream	        = SOCK_STREAM,
    sock_dgram	        = SOCK_DGRAM,
    sock_raw	        = SOCK_RAW,
    sock_rdm	        = SOCK_RDM,
    sock_seqpacket      = SOCK_SEQPACKET
  };
  enum option
  {
    so_debug	        = SO_DEBUG,
    so_reuseaddr	= SO_REUSEADDR,
    so_keepalive	= SO_KEEPALIVE,
    so_dontroute	= SO_DONTROUTE,
    so_broadcast	= SO_BROADCAST,
    so_linger	        = SO_LINGER,
    so_oobinline	= SO_OOBINLINE,
    so_sndbuf		= SO_SNDBUF,
    so_rcvbuf		= SO_RCVBUF,
    so_error		= SO_ERROR,
    so_type		= SO_TYPE
  };	
  enum level
  {
    sol_socket          = SOL_SOCKET
  };
  enum msgflag
  {
    msg_oob		= MSG_OOB,
    msg_peek	        = MSG_PEEK,
    msg_dontroute	= MSG_DONTROUTE,
	
    msg_maxiovlen	= MSG_MAXIOVLEN
  };
  enum shuthow
  {
    shut_read,
    shut_write,
    shut_readwrite
  };
  enum { somaxconn	= SOMAXCONN };
  struct socklinger
  {
    int	l_onoff;	// option on/off
    int	l_linger;	// linger time
    socklinger (int a, int b): l_onoff (a), l_linger (b) {}
  };

  sockbuf(int s) : ipcbuf(std::ios::in|std::ios::out) { fd(s);}
  sockbuf(int, type, int);
  virtual           ~sockbuf() {}
  //. listen for connection requests. Allow up to num requests to be accumulated in the queue
  void               listen(int num = somaxconn);
  //. accept a connection request. Return a new sockbuf for the newly established connection.
  virtual sockbuf   *accept() = 0;
  int                read(void *, int);
  int		     recv(void *, int, int msgf = 0);
  int		     recvfrom(sockaddr &, void *, int, int msgf = 0);
#ifndef __linux__
  int		     recvmsg(msghdr *, int msgf = 0);
  int		     sendmsg(msghdr *, int msgf = 0);
#endif
  int		     write(const void *, int);
  int		     send(const void *, int, int msgf = 0);
  int		     sendto(sockaddr &, const void *, int, int msgf = 0);
  int		     sendtimeout(int wp = -1);
  int		     recvtimeout(int wp = -1);
  void		     shutdown(shuthow);
  int		     getopt(int, void *, socklen_t, int level = sol_socket) const;
  void		     setopt(int, void *, socklen_t, int level = sol_socket) const;
  type		     gettype () const;
  int		     clearerror () const;
  bool		     debug() const;
  bool		     debug(bool) const;
  bool		     reuseaddr() const;
  bool		     reuseaddr(bool) const;
  bool		     keepalive() const;
  bool		     keepalive(bool) const;
  bool		     dontroute() const;
  bool		     dontroute(bool) const;
  bool		     broadcast() const;
  bool		     broadcast(bool) const;
  bool		     oobinline() const;
  bool		     oobinline(bool) const;
  int		     sendbufsz() const;
  int		     sendbufsz(int)   const;
  int		     recvbufsz() const;
  int		     recvbufsz(int)   const;
  socklinger         linger() const;
  socklinger         linger(socklinger) const;
  socklinger         linger(int onoff, int tm) const { return linger(socklinger(onoff, tm));}
  //. return true, if the read pointer for socket points to an out of band data
  bool               atmark() const;
  //. return the process group id that would receive SIGIO and SIGURG signals
  int                pgrp() const;
  //. set the process group id that would receive SIGIO and SIGURG signals. return the old pgrp
  int                pgrp(int) const;
  void               closeonexec(bool set = true) const;
  long               nread() const;
  long               howmanyc() const;
  void               nbio(bool set = true) const;
protected:
};

//. a sockbuf for the unix domain
class sockunixbuf : public sockbuf
{
public:
  typedef sockunixaddr address_type;
  sockunixbuf(int s) : sockbuf(s) {}
  sockunixbuf(sockbuf::type ty, int proto = 0) : sockbuf(af_unix, ty, proto) {}
  ~sockunixbuf() {}
  sockunixaddr addr() const;
  void bind(const sockunixaddr &);
  virtual sockunixbuf *accept();
  virtual sockunixbuf *accept(sockunixaddr &);
  void connect(const sockunixaddr &);
};

//. a sockbuf for the internet domain
class sockinetbuf : public sockbuf
{
public:
  typedef sockinetaddr address_type;
  sockinetbuf (int s) : sockbuf(s) {}
  sockinetbuf (sockbuf::type ty, int proto = 0) : sockbuf(af_inet4, ty, proto) {}
  ~sockinetbuf () {}
  sockinetaddr localaddr() const;
  sockinetaddr peeraddr() const;
  void         bind_until_success (int);
  virtual void bind(const sockinetaddr &);
  virtual void connect(const sockinetaddr &);
  virtual sockinetbuf *accept();
  virtual sockinetbuf *accept(sockinetaddr &);
  bool         tcpnodelay() const;
  bool         tcpnodelay(bool set) const;
};

};

#endif
