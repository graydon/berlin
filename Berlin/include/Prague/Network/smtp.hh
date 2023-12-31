/*$Id: smtp.hh,v 1.2 1999/07/23 19:01:37 gray Exp $
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

#ifndef _smtp_hh
#define _smtp_hh

#include <Prague/Network/protocol.hh>

namespace Prague
{

class smtp: public protocol
{
public:
  class smtpbuf : public protocol::protocolbuf
  {
    ostream*            o; // send all the responses to o
    void                send_cmd (const char* cmd, const char* s = 0, const char* p = 0);
    void                get_response ();
    smtpbuf (smtpbuf&);
    smtpbuf& operator = (smtpbuf&);
  public:
    smtpbuf (ostream* out = 0) : protocol::protocolbuf (protocol::tcp), o (out) {}
    void                send_buf (const char* buf, int buflen);

    void                helo ();
    void                quit () { send_cmd ("QUIT"); }
    void                turn () { send_cmd ("TURN"); }
    void                rset () { send_cmd ("RSET"); }
    void                noop () { send_cmd ("NOOP"); }
    void                vrfy (const char* s) { send_cmd ("VRFY ", s); }
    void                expn (const char* s) { send_cmd ("EXPN ", s); }

    void                data () { send_cmd ("DATA"); }
    void                data (const char* buf, int buflen);
    void                data (const char* filename); // filename = 0 => stdin

    void                mail (const char* reverse_path);
    void                rcpt (const char* forward_path);
    void                help (const char* s = 0);

    virtual void        serve_clients (int portno = -1);
    virtual const char* rfc_name () const { return "smtp"; }
    virtual const char* rfc_doc  () const { return "rfc821"; }
  };
    
protected:
  smtp(): ios (0) {}

public:
  smtp (ostream* out): ios (0) { ios::init (new smtpbuf (out)); }
  ~smtp () { delete ios::rdbuf (); ios::init (0); }

  int      get_response (char* buf, int len);

  smtpbuf* rdbuf ()       { return static_cast<smtpbuf *> (protocol::rdbuf ()); }
  smtpbuf* operator -> () { return rdbuf (); }
};

};

extern ostream& operator << (ostream &o, const Prague::smtp &s);

#endif /* _smtp_hh */
