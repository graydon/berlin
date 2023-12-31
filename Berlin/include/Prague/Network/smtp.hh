/*$Id: smtp.hh,v 1.5 2001/03/25 08:25:16 stefan Exp $
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

#ifndef _Prague_smtp_hh
#define _Prague_smtp_hh

#include <Prague/Network/protocol.hh>

namespace Prague
{

class smtp: public protocol
{
public:
  class smtpbuf : public protocol::protocolbuf
  {
  public:
    smtpbuf(std::ostream *os = 0) : protocol::protocolbuf (protocol::tcp), _os(os) {}
    void                send_buf(const char *buf, int buflen);

    void                helo();
    void                quit() { send_cmd("QUIT");}
    void                turn() { send_cmd("TURN");}
    void                rset() { send_cmd("RSET");}
    void                noop() { send_cmd("NOOP");}
    void                vrfy(const std::string &s) { send_cmd("VRFY ", s);}
    void                expn(const std::string &s) { send_cmd("EXPN ", s);}

    void                data() { send_cmd("DATA");}
    void                data(const char *buf, int buflen);
    void                data(const std::string &filename); // filename = 0 => stdin

    void                mail(const std::string &from);
    void                rcpt(const std::string &to);
    void                help(const std::string &s = "");

    virtual void        serve_clients(int portno = -1);
    virtual const char *rfc_name() const { return "smtp";}
    virtual const char *rfc_doc() const { return "rfc821";}
  private:
    std::ostream       *_os; // send all the responses to os
    void                send_cmd(const std::string &cmd, const std::string &s = "", const std::string &p = "");
    void                get_response();
    //     smtpbuf(smtpbuf &);
    //     smtpbuf& operator = (smtpbuf &);
  };
public:
  smtp(std::ostream *out): protocol(new smtpbuf(out)) {}
  ~smtp() { delete protocol::rdbuf(); init(0);}
  int      get_response (char *buf, int len);
  smtpbuf *rdbuf()       { return static_cast<smtpbuf *>(protocol::rdbuf());}
  smtpbuf *operator ->() { return rdbuf();}
};

};

extern std::ostream &operator << (std::ostream &, Prague::smtp &);

#endif
