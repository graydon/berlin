/*$Id: url.cc,v 1.4 2001/03/25 08:25:16 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
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
#include "Prague/Sys/regex.hh"
#include "Prague/Network/url.hh"
#include <iostream>

using namespace Prague;

void url::parse(const std::string &tt)
{
  std::string t(tt);
  /*
   * search for a fragment identifier.
   */
  int i = t.find('#');
  if (i >= 0)
    {
      f = t.substr(i + 1);
      t.erase(i, t.length() - i);
    }
  /*
   * search for a scheme
   */
  regex reg("^[a-zA-Z0-9\\+\\.\\-]+:");
  i = reg.search(t);
  if (i >= 0)
    {
      int j = t.find(':');
      s = t.substr(0, j);
      t.erase(0, j + 1);
    }
  if (t[0] == '/' && t[1] == '/')
    {
      /*
       * common internet scheme syntax
       */
      t.erase(0, 2);
      i = t.find('/');
      std::string nl;
      nl = t.substr(0, i);
      t.erase(0, i);
      if ((i = nl.find('@')) >= 0)
	{
	  std::string login = nl.substr(0, i);
	  nl.erase(0, i + 1);
	  if ((i = login.find(':')) >= 0)
	    {
	      u = login.substr(0, i);
	      pw = login.substr(login.length() - i);
	    }
	  else u = login;
	}
      if ((i = nl.find(':')) >= 0)
	{
	  std::string portstr = nl.substr(i + 1);
	  nl.erase(i, nl.length());
	  po = atoi(portstr.c_str());
	}
      h = nl;
    }
  /*
   * search for a query.
   */
  if ((i = t.find('?')) >= 0)
    {
      q = t.substr(t.length() - i - 1);
      t.erase(i, t.length());
    }
  /*
   * search for parameters.
   */
  if ((i = t.find(';')) >= 0)
    {
      pa = t.substr(t.length() - i - 1);
      t.erase(i, t.length());
    }
  /*
   * the rest is the path.
   */
  p = t;
  if (s.length()) std::cout << "scheme\t:" << s << std::endl;
  if (u.length()) std::cout << "user\t:" << u << std::endl;
  if (pw.length()) std::cout << "password\t:" << pw << std::endl;
  if (h.length()) std::cout << "hostname\t:" << h << std::endl;
  if (po >= 0) std::cout << "port\t:" << po << std::endl;
  if (p.length()) std::cout << "path\t:" << p << std::endl;
  if (f.length()) std::cout << "fraction\t:" << f << std::endl;
  if (q.length()) std::cout << "query\t:" << q << std::endl;
  if (pa.length()) std::cout << "parameters\t:" << pa << std::endl;
}

url::url(const std::string &t)
 : po(-1)
{
  parse(t);
}

url::url(const url &absurl, const std::string &relurl)
 : po(-1)
{
  parse(relurl);
//   if (method()) return;
//   method = absurl->method();
//   if (!hostname())
//     {
//       _user     = absurl->user();
//       _password = absurl->password();
//       _hostname = absurl->hostname();
//       _port     = absurl->port();
//       if (!path() || _path[0] != '/')
// 	{
// 	  if (!path())
// 	    {
// 	      _path = absurl->path();
// 	      if (!parameters())
// 		{
// 		  _parameters = absurl->parameters();
// 		  if (!query()) _query = absUrl->query();
// 		}
// 	    }
// 	  else
// 	    {
// 				// Step 6...
// 	      std::string newPath;
// 	      int idx1 = 0;
// 	      newPath = absurl->path();
// 	      if ((idx1 = newPath.findRev('/')) >= 0) newPath.truncate(idx1 + 1);
// 	      else newPath.truncate( 0 );
// 	      newPath += path();
// 	      idx1 = 0;
// 	      if (newPath[0] == '/') idx1++;
// 	      while (idx1 < int(newPath.length()))
// 		{
// 		  if (newPath.mid(idx1, 2) == "./")
// 		    {
// 		      newPath.erase(idx1, 2);
// 		      //printf( "Url::Url() -- (1)path = '%s'\n", (const char*)newPath );
// 		    }
// 		  else
// 		    {
// 		      idx1 = newPath.find('/', idx1 + 1);
// 		      if (idx1 < 0) idx1 = newPath.length();
// 		      else idx1++;
// 		    }
// 		}
// 	      if (newPath.substr(2) == "/.")
// 		{
// 		  newPath.truncate(newPath.length() - 1);
// 		  //printf( "Url::Url() -- (2)path = '%s'\n", (const char*)newPath );
// 		}		
// 	      bool found = true;
// 	      while (found)
// 		{
// 		  found = false;
// 		  idx1 = 0;
// 		  if (newPath.left(1) == '/') idx1++;
// 		  while (!found && (idx1 < int(newPath.length())))
// 		    {
// 		      int idx2 = newPath.find('/', idx1 + 1);
// 		      if (idx2 > idx1 && newPath.mid(idx1, 3) != "../" && newPath.mid(idx2, 4) == "/../")
// 			{
// 			  found = true;
// 			  newPath.erase(idx1, idx2 - idx1 + 4);
// 			  //printf( "Url::Url() -- (3)path = '%s'\n", (const char*)newPath );
// 			}
// 		      else
// 			{
// 			  if (idx2 < 0) idx1 = newPath.length();
// 			  else idx1 = idx2 + 1;
// 			}
// 		    }
// 		}
// 	      if (newPath.substr(3) == "/..")
// 		{
// 		  idx1 = newPath.findRev('/', newPath.length() - 4) + 1;
// 		  if (newPath.mid(idx1, 3) != "../")
// 		    {
// 		      newPath.truncate(idx1);
// 		      //printf( "Url::Url() -- (4)path = '%s'\n", (const char*)newPath );
// 		    }
// 		}
// 	      path = newPath;
// 	    }
// 	}
//     }
}

/*
 * @Method{}
 *
 * @Description{Return the fully qualified URL as a std::string}
 */
// url::operator const std::string &() const
// {
//   std::string tmp;
//   if (method())
//     {
//       tmp += method();
//       tmp += ":";
//     }
//   if (hostname())
//     {
//       tmp += "//";
//       if (user())
// 	{
// 	  tmp += user();
// 	  if (password())
// 	    {
// 	      tmp += ":";
// 	      tmp += password();
// 	    }
// 	  tmp += "@";
// 	}
//       tmp += hostname();
//       if (port() >= 0)
// 	{
// 	  tmp += ":";
// 	  tmp += std::string().setNum(port());
// 	}
//       if (path() && path()[0] != '/')
// 	{
// 	  //printf( "Url::url() -- path() = '%s'\n", path() );
// 	  tmp += "/";
// 	}
//     }
//   tmp += path();
//   if (parameters())
//     {
//       tmp += ";";
//       tmp += parameters();
//     }
//   if (query())
//     {
//       tmp += "?";
//       tmp += query();
//     }
//   if (fragment())
//     {
//       tmp += "#";
//       tmp += fragment();
//     }
//   return tmp;
// }

void url::encode(std::string &s)
{
  unsigned int  i;
  unsigned char c;
  std::string escaped;
  for (i = 0; i < s.length(); i++)
    {
      c = s[i];
      if (c < 32       ||
	  c > 126      ||
	  isspace(c)   ||
	  strchr("<>\"#%{}|\\^~[]'`;/?:@=&", c))
	{
	  // Escape the character.
// 	  escaped.sprintf("%%%02X", c);
// 	  s.replace( i, 1, escaped);
	  i += 2;
	}
    }
}

void url::decode(std::string &s)
{
  unsigned int  i;
  unsigned char c;
  std::string escaped;
  for (i = 0; i < s.length(); i++)
    {
      c = s[i];
      if (c < 32       ||
	  c > 126      ||
	  isspace(c)   ||
	  strchr("<>\"#%{}|\\^~[]'`;/?:@=&", c))
	{
	  // Escape the character.
// 	  escaped.sprintf("%%%02X", c);
// 	  s.replace( i, 1, escaped);
	  i += 2;
	}
    }
}
