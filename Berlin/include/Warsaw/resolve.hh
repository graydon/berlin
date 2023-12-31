/*$Id: resolve.hh,v 1.8 2001/04/18 06:07:26 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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
#ifndef _resolve_hh
#define _resolve_hh

#include <Warsaw/Server.hh>
#include <Warsaw/exception.hh>
#include <assert.h>

template <class T>
typename T::_ptr_type resolve_init(CORBA::ORB_ptr orb, const char *name)
{
  CORBA::Object_var object;
  try
    {
      object = orb->resolve_initial_references(name);
    }
  catch (const CORBA::ORB::InvalidName &e)
    {
      std::cerr << "Invalid Name : " << name << std::endl;
      throw;
    }
  catch (const CORBA::Exception &e)
    {
      std::cerr << "Cannot get initial reference for " << name << ": " << e << std::endl;
      throw 0;
    }
  assert(!CORBA::is_nil(object));

  typename T::_var_type reference;
  try
    {
      reference = T::_narrow(object);
    }
  catch (const CORBA::Exception &e)
    {
      std::cerr << "Cannot downcast reference for " << name << ": " << e << std::endl;
      throw 0;
    }
  if (CORBA::is_nil(reference))
    {
      std::cerr << "Incorrect type of reference for " << name << std::endl;
      throw 0;
    }
  return reference._retn();
}

template <class T>
typename T::_ptr_type resolve_name(CosNaming::NamingContext_ptr context, const CosNaming::Name &name)
{
  CORBA::Object_var object;
  try
    {
      object = context->resolve(name);
    }
  catch (const CosNaming::NamingContext::NotFound &e)
    {
      throw;
    }
  catch (const CORBA::Exception &e)
    {
      std::cerr << "Cannot resolve binding: " << e << std::endl;
      throw 0;
    }
  if (CORBA::is_nil(object))
    {
      std::cerr << "Nil binding in Naming service" << std::endl;
      throw 0;
    }

  typename T::_var_type reference;
  try
    {
      reference = T::_narrow(object);
    }
  catch (const CORBA::Exception &e)
    {
      std::cerr << "Cannot narrow reference: " << e << std::endl;
      throw 0;
    }
  if (CORBA::is_nil(reference))
    {
      std::cerr << "Reference has incorrect type" << std::endl;
      throw 0;
    }
  return reference._retn();
}

template <class T>
typename T::_ptr_type resolve_name(CosNaming::NamingContext_ptr context, const char * name)
{
  CosNaming::Name cosname;
  cosname.length(1);
  cosname[0].id   = name; 
  cosname[0].kind = "Object"; // string_dup ?? 
  return resolve_name<T>(context, cosname);
}

inline void bind_name(CORBA::ORB_ptr orb, CORBA::Object_ptr object, const char *name)
{
  CosNaming::NamingContext_var context = resolve_init<CosNaming::NamingContext>(orb, "NameService");
  CosNaming::Name cosname;
  cosname.length(1);
  cosname[0].id   = name; 
  cosname[0].kind = "Object"; // string_dup ?? 
  context->rebind(cosname, object);
}

template <class T>
typename T::_ptr_type resolve_kit(Warsaw::ServerContext_ptr context, const char *name, const Warsaw::Kit::PropertySeq &props)
{
  CORBA::Object_ptr object;
  try
    {
      object = context->resolve(name, props);
    }
  catch(const CORBA::Exception &e)
    {
      std::cerr << "Cannot resolve reference for " << name << ": " << e << std::endl;
      return T::_nil();
    }
  typename T::_var_type reference;
  try
    {
      reference = T::_narrow(object);
    }
  catch (const CORBA::Exception &e)
    {
      std::cerr << "Cannot narrow reference: " << e << std::endl;
      throw 0;
    }
  if (CORBA::is_nil(reference))
    {
      std::cerr << "Reference has incorrect type" << std::endl;
      throw 0;
    }
  return reference._retn();
}

template <class T>
typename T::_ptr_type resolve_kit(Warsaw::ServerContext_ptr context, const char *name)
{
  Warsaw::Kit::PropertySeq empty;
  empty.length(0);
  return resolve_kit<T>(context, name, empty);
}

#endif
