/*$Id: CommandKitImpl.cc,v 1.18 2001/04/18 06:07:27 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Graydon Hoare <graydon@pobox.com> 
 * Copyright (C) 1999, 2000 Stefan Seefeld <stefan@berlin-consortium.org> 
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

#include <Warsaw/config.hh>
#include <Warsaw/Controller.hh>
#include <Berlin/ImplVar.hh>
#include <Berlin/CommandImpl.hh>
#include "Command/CommandKitImpl.hh"
#include "Command/TelltaleImpl.hh"
#include "Command/SelectionImpl.hh"
#include "Command/BoundedValueImpl.hh"
#include "Command/BoundedRangeImpl.hh"
#include "Command/TextBufferImpl.hh"
#include "Command/StreamBufferImpl.hh"
#include <string>
#include <vector>

using namespace Warsaw;

class DebugCommand : public CommandImpl
{
public:
  DebugCommand(Command_ptr c, std::ostream &os, const char *t) : _command(Warsaw::Command::_duplicate(c)), _os(os), _text(t) {}
  virtual void execute(const CORBA::Any &any)
  {
    _os << _text << " : entering execute" << std::endl; 
    _command->execute(any);
    _os << _text << " : leaving execute" << std::endl;
  }
 private:
  Command_var   _command;
  std::ostream &_os;
  std::string   _text;
};

class LogCommand : public CommandImpl
{
public:
  LogCommand(std::ostream &os, const char *text) : _os(os), _text(text) {}
  virtual void execute(const CORBA::Any &) { _os << _text << std::endl;}
 private:
  std::ostream &_os;
  std::string   _text;
};

class MacroCommandImpl : public virtual POA_Warsaw::MacroCommand,
			 public CommandImpl
{
  typedef std::vector<Warsaw::Command_var> clist_t;
public:
  virtual ~MacroCommandImpl()
  {
    for (clist_t::iterator i = _commands.begin(); i != _commands.end(); ++i)
      (*i)->destroy();
  }
  virtual void append(Warsaw::Command_ptr c)
  {
    _commands.push_back(Warsaw::Command::_duplicate(c));
  }
  virtual void prepend(Warsaw::Command_ptr c)
  {
    _commands.insert(_commands.begin(), Warsaw::Command::_duplicate(c));
  }
  virtual void execute(const CORBA::Any &any)
    {
      for (clist_t::iterator i = _commands.begin(); i != _commands.end(); ++i)
	(*i)->execute(any);
    }
 private:
  clist_t _commands;
};

CommandKitImpl::CommandKitImpl(KitFactory *f, const Warsaw::Kit::PropertySeq &p)
  : KitImpl(f, p) {}
CommandKitImpl::~CommandKitImpl() {}
Command_ptr CommandKitImpl::debugger(Warsaw::Command_ptr c, const char *text)
{
  DebugCommand *command = new DebugCommand(c, std::cout, text);
  activate(command);
  return command->_this();
}

Command_ptr CommandKitImpl::log(const char *text)
{
  LogCommand *command = new LogCommand(std::cout, text);
  activate(command);
  return command->_this();
}

MacroCommand_ptr CommandKitImpl::composite()
{
  MacroCommandImpl *command = new MacroCommandImpl();
  activate(command);
  return command->_this();
}

TelltaleConstraint_ptr CommandKitImpl::exclusive(Telltale::Mask m)
{
  ExclusiveChoice *constraint = new ExclusiveChoice(m);
  activate(constraint);
  return constraint->_this();
}

TelltaleConstraint_ptr CommandKitImpl::selection_required()
{
  SelectionRequired *constraint = new SelectionRequired(Controller::toggled);
  activate(constraint);
  return constraint->_this();
}

Telltale_ptr CommandKitImpl::normal_telltale()
{
  TelltaleImpl *telltale = new TelltaleImpl(TelltaleConstraint::_nil());
  activate(telltale);
  return telltale->_this();
}

Telltale_ptr CommandKitImpl::constrained_telltale(TelltaleConstraint_ptr constraint)
{
    TelltaleImpl *telltale = new TelltaleImpl(constraint);
    activate(telltale);
    constraint->add(Telltale_var(telltale->_this()));
    return telltale->_this();
}

Selection_ptr CommandKitImpl::group(Selection::Policy policy)
{
  TelltaleConstraintImpl *constraint = 0;
  switch (policy)
    {
    case Selection::exclusive:
      constraint = new ExclusiveChoice(Controller::toggled);
      break;
    case Selection::required:
      constraint = new SelectionRequired(Controller::toggled);
      break;
    case Selection::exclusive|Selection::required:
      constraint = new ExclusiveRequired(Controller::toggled);
      break;
    default: break;
    }
  SelectionImpl *selection = 0;
  if (constraint)
    {
      activate(constraint);
      selection = new SelectionImpl(policy, TelltaleConstraint_var(constraint->_this()));
    }
  else selection = new SelectionImpl(policy, TelltaleConstraint::_nil());
  activate(selection);
  return selection->_this();
}

BoundedValue_ptr CommandKitImpl::bvalue(Coord l, Coord u, Coord v, Coord s, Coord p)
{
  BoundedValueImpl *bounded = new BoundedValueImpl(l, u, v, s, p);
  activate(bounded);
  return bounded->_this();
}

BoundedRange_ptr CommandKitImpl::brange(Coord l, Coord u, Coord lv, Coord uv, Coord s, Coord p)
{
  BoundedRangeImpl *bounded = new BoundedRangeImpl(l, u, lv, uv, s, p);
  activate(bounded);
  return bounded->_this();
}

TextBuffer_ptr CommandKitImpl::text()
{
  TextBufferImpl *buffer = new TextBufferImpl();
  activate(buffer);
  return buffer->_this();  
}

StreamBuffer_ptr CommandKitImpl::stream(CORBA::Long b)
{
  StreamBufferImpl *buffer = new StreamBufferImpl(b);
  activate(buffer);
  return buffer->_this();  
}

extern "C" KitFactory *load()
{
  static std::string properties[] = {"implementation", "CommandKitImpl"};
  return new KitFactoryImpl<CommandKitImpl>("IDL:Warsaw/CommandKit:1.0", properties, 1);
}
