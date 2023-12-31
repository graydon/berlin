/*$Id: WindowImpl.hh,v 1.8 1999/11/06 20:23:08 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Stefan Seefeld <seefelds@magellan.umontreal.ca> 
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
#ifndef _WindowImpl_hh
#define _WindowImpl_hh

#include <Warsaw/config.hh>
#include <Warsaw/Window.hh>
#include <Warsaw/Command.hh>
#include <Warsaw/Desktop.hh>
#include <Berlin/ControllerImpl.hh>
#include <Prague/Sys/Thread.hh>
#include <vector>

class UnmappedStageHandle;

class WindowImpl : implements(Window), public ControllerImpl
{
  class Manipulator : implements(Command)
  {
  public:
    virtual ~Manipulator() {}
    void bind(StageHandle_ptr h) { handle = StageHandle::_duplicate(h);}
    virtual void execute(const CORBA::Any &) = 0;
  protected:
    StageHandle_var handle;
  };
  class Mapper : implements(Command)
  {
  public:
    Mapper(WindowImpl *w, bool f) : window(w), flag(f) {}
    virtual void execute(const CORBA::Any &);
  private:
    WindowImpl *window;
    bool flag;
  };
  typedef vector<Manipulator *> mtable_t;
 public:
  WindowImpl();
  virtual ~WindowImpl();
  void insert(Desktop_ptr, bool);
  CORBA::Boolean mapped() { Prague::MutexGuard guard(mutex); return !unmapped;}
  Command_ptr move();
  Command_ptr resize();
  Command_ptr moveResize(Alignment, Alignment, CORBA::Short);
  Command_ptr relayer();
  Command_ptr map(CORBA::Boolean);
//   virtual void pick(PickTraversal_ptr);
  void map();
  void unmap();
 private:
  StageHandle_var handle;
  UnmappedStageHandle *unmapped;
  mtable_t manipulators;
  Mapper *mapper, *unmapper;
  Prague::Mutex mutex;
};

class UnmappedStageHandle : implements(StageHandle)
{
 public:
  UnmappedStageHandle(Stage_ptr par, Graphic_ptr cc, const Vertex &pp, const Vertex &ss, Stage::Index ll)
    : stage(Stage::_duplicate(par)), c(Graphic::_duplicate(cc)), p(pp), s(ss), l(ll) {}
  UnmappedStageHandle(StageHandle_ptr handle)
    : stage(handle->parent()),
    c(handle->child()),
    p(handle->position()),
    s(handle->size()),
    l(handle->layer())
    {}
  virtual Stage_ptr parent() { return Stage::_duplicate(stage);}
  virtual Graphic_ptr child() { return Graphic::_duplicate(c);}
  virtual Vertex position() { return p;}
  virtual void position(const Vertex &pp) { p = pp;}
  virtual Vertex size() { return s;}
  virtual void size(const Vertex &ss) { s = s;}
  virtual Stage::Index layer() { return l;}
  virtual void layer(Stage::Index ll) { l = ll;}
 private:
  Stage_var stage;
  Graphic_var c;
  Vertex p;
  Vertex s;
  Stage::Index l;
};

#endif /* _WindowImpl_hh */
