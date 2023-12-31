/*$Id: Application.cc,v 1.6 1999/11/30 21:52:35 tobias Exp $
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

#include "Application.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/Image.hh"
#include "Warsaw/Window.hh"

using namespace Prague;

void Application::Mapper::execute(const CORBA::Any &)
{
  for (Application::list_t::iterator i = examples.begin(); i != examples.end(); i++)
    if ((*i).toggle->test(Telltale::chosen))
      {
	CORBA::Any any;
	(*i).map->execute(any);
	return;
      }
}

class ExitCommand : implements(Command) 
{
 public:
  void execute(const CORBA::Any &) {
    exit(0);
  }
};

Application::Application(ServerContextManager_ptr manager)
  : client(new ClientContextImpl),
    server(manager->newServerContext(ClientContext_var(client->_this()))),
    tk(obtain(server, TextKit)),
    dk(obtain(server, DesktopKit)),
    lk(obtain(server, LayoutKit)),
    wk(obtain(server, WidgetKit)),
    fk(obtain(server, FigureKit)),
    ck(obtain(server, CommandKit)),
    ik(obtain(server, ImageKit)),
    f(DrawingKit_var(tk->dk())->currentFont()),
    vbox(lk->vbox()),
    exclusive(wk->exclusive()),
    mapper(new Mapper(examples))
{
  background.red = background.green = background.blue = 0.6; background.alpha = 1.;
  Raster_var raster = ik->create("logo.png");
  Image_var  image = fk->pixmap(raster);
  Graphic_var hbox = lk->hbox();
//   hbox->append(Graphic_var(lk->hfil()));
  hbox->append(image);
  hbox->append(Graphic_var(lk->hfil()));
  vbox->append(hbox);

  Graphic_var glyph1 =
    tk->chunk(Unicode::toCORBA(Unicode::String("close")), f);
  done = lk->margin(glyph1, 2.);
}

void Application::append(Graphic_ptr demo, const Unicode::String &name)
{
  Controller_var toggle = wk->toggle(Graphic_var(lk->fixed(Graphic_var(Graphic::_nil()), 5., 5.)), background);
  exclusive->add(toggle);
  Graphic_var hbox = lk->hbox();
  Button_var button = wk->pushButton(done, background, Command_var(Command::_nil()));
  hbox->append(Graphic_var(lk->hfil()));
  hbox->append(button);
  hbox->append(Graphic_var(lk->hfil()));
  Graphic_var vb = lk->vbox();
  vb->append(demo);
  vb->append(hbox);
  Window_var window = dk->transient(vb);
  Command_var unmap = window->map(false);
  button->action(unmap);
  examples.push_back(item(toggle, Command_var(window->map(true))));
  hbox = lk->hbox();
  Graphic_var space = lk->hspace(20.);
  Graphic_var label = tk->chunk(Unicode::toCORBA(name), f);
  hbox->append(Graphic_var(lk->align(toggle, 0., 0.)));
  hbox->append(space);
  hbox->append(label);
  vbox->append(hbox);
}

void Application::run()
{
  vbox->append(Graphic_var(lk->vspace(20)));
  Graphic_var hbox = lk->hbox();
  hbox->append(Graphic_var(lk->hglue(20., 0., 1000.)));
  Graphic_var glyph1 = tk->chunk(Unicode::toCORBA(Unicode::String("run")), f);
  Graphic_var label1 = lk->margin(glyph1, 2.);
  hbox->append(Button_var(wk->pushButton(label1, background, Command_var(mapper->_this()))));
  hbox->append(Graphic_var(lk->vspace(20)));
  Graphic_var glyph2 = tk->chunk(Unicode::toCORBA(Unicode::String("quit")), f);
  Graphic_var label2 = lk->margin(glyph2, 2.);
  ExitCommand *cmd = new ExitCommand();
  cmd->_obj_is_ready(CORBA::BOA::getBOA());
  hbox->append(Button_var(wk->pushButton(label2, background, Command_var(cmd->_this()))));
  hbox->append(Graphic_var(lk->hglue(20., 0., 1000.)));
  vbox->append(hbox);
  Graphic_var margin = lk->margin(vbox, 20.);
  Window_var window = dk->shell(Graphic_var(wk->outset(margin, background, true)));
  while (true) Thread::delay(Prague::Time(1000));
}
