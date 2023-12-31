/*$Id: ftp.hh,v 1.3 1999/09/02 17:46:56 gray Exp $
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

#ifndef _ftp_hh
#define _ftp_hh

#include <Prague/Network/protocol.hh>
#include <sys/param.h>

namespace Prague
{

class ftp : public protocol
{
public:
  enum reptype
  {
    rt_ascii_nonprint,
    rt_ascii_telnet,
    rt_ascii_asa,
    rt_ebcdic_nonprint,
    rt_ebcdic_telnet,
    rt_ebcdic_asa,
    rt_image,
    rt_local
  };
  enum filestru
  {
    fs_file,
    fs_record,
    fs_page
  };
  enum transmode
  {
    tm_stream,
    tm_block,
    tm_comp
  };
  enum replycodea
  {
    rca_posprelim    = '1',
    rca_poscomp      = '2',
    rca_posinter     = '3',
    rca_error        = '4',
    rca_negtranscomp = '4',
    rca_negpermcomp  = '5'
  };

  enum replycodeb
  {
    rcb_syntax       = '0',
    rcb_info         = '1',
    rcb_conn         = '2',
    rcb_auth         = '3',
    rcb_unspec       = '4',
    rcb_filesys      = '5'
  };

  class ftpbuf : public protocol::protocolbuf
  {
  public:
    ftpbuf (ostream* out = 0);

    ftp::replycodea get_response ();
    const char*     reply_code () const { return replycode; }

    ftp::replycodea help () { return send_cmd ("HELP"); }
    ftp::replycodea noop () { return send_cmd ("NOOP"); }
    ftp::replycodea quit () { return send_cmd ("QUIT"); }
    ftp::replycodea abort () { return send_cmd ("ABOR"); }
    ftp::replycodea user (const char* name) {return send_cmd ("USER", name);}
    ftp::replycodea passwd (const char* pw) {return send_cmd ("PASS", pw); }
    ftp::replycodea acct (const char* ac) {return send_cmd ("ACCT", ac);}
    ftp::replycodea cd   (const char* dir);
    ftp::replycodea useraddr (sockinetaddr sa);
    ftp::replycodea useraddr (const char* host, int portno);
    ftp::replycodea server_port (int portno);
    ftp::replycodea rep_type (ftp::reptype rt);
    ftp::replycodea file_stru (ftp::filestru fs);
    ftp::replycodea trans_mode  (ftp::transmode tm);

    // service commands
    ftp::replycodea getfile (const char* rpath, const char* lpath);
    ftp::replycodea list (const char* lpath=0, int justnames = 0);
    ftp::replycodea putfile (const char* lpath, const char* rpath);
    ftp::replycodea putfile (const char* lpath);
    ftp::replycodea append  (const char* lpath, const char* rpath);
    ftp::replycodea allocate (int numbytes);
    ftp::replycodea restart (int marker);
    ftp::replycodea rename (const char* rpath, const char* newrpath);
    ftp::replycodea rmfile (const char* rpath);
    ftp::replycodea rmdir  (const char* rpath);
    ftp::replycodea mkdir  (const char* rpath);
    ftp::replycodea pwd    () { return send_cmd ("PWD"); }
    ftp::replycodea system () { return send_cmd ("SYST"); }
    ftp::replycodea status () { return send_cmd ("STAT"); }

    virtual void        serve_clients (int portno = -1);
    virtual const char* rfc_name () const { return "ftp"; }
    virtual const char* rfc_doc  () const { return "rfc959"; }
  private:
    // the following are used when this is used as a server
    char*           usr;
    char*           password;
    char*           account;
    char            cwd [MAXPATHLEN];
    char            parentdir [MAXPATHLEN];
    ftp::filestru   fs;
    ftp::transmode  tm;
    sockinetaddr    udata; // user will listen at this addr for data conn.
    int             serverportno;
    char            replycode [5];

    ostream*           o;

    ftp::replycodea send_cmd (const char* cmd, const char* arg=0);
    ftp::replycodea ftpdata (int portno, istream* i, ostream* out,
			     const char* cmd, const char* arg=0);

    ftpbuf (ftpbuf&);
    ftpbuf& operator = (ftpbuf&);
  };

public:
  ftp (ostream* out);
  ~ftp () { delete ios::rdbuf (); ios::init (0); }


  ftpbuf* rdbuf ()       { return static_cast<ftpbuf *> (protocol::rdbuf ()); }
  ftpbuf* operator -> () { return rdbuf (); }
protected:
  ftp (): ios (0) {}
};  

};
  
#endif /* _ftp_hh */
