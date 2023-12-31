/*$Id: User.hh,v 1.5 2001/03/21 06:28:22 stefan Exp $
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
#ifndef _User_hh
#define _User_hh

#include <string>
#include <pwd.h>

namespace Prague
{

class User
{
public:
  User(int id = -1);
  User(const std::string &);
  ~User() {}
  uid_t uid() const { return pwd->pw_uid;}
  gid_t gid() const { return pwd->pw_gid;}
  const char *name() const { return pwd->pw_name;}
  const char *realName() const { return pwd->pw_gecos;}
  const char *home() const { return pwd->pw_dir;}
protected:
  passwd *pwd;
private:
};

};

#endif /* _User_hh */
