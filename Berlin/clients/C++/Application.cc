/*$Id: Application.cc,v 1.19 2001/01/09 21:35:08 tobias Exp $
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

#include <Warsaw/config.hh>
#include <Warsaw/resolve.hh>
#include "Application.hh"
#include <Warsaw/DrawingKit.hh>
#include <Warsaw/Image.hh>
#include <Warsaw/Window.hh>

using namespace Prague;
using namespace Warsaw;

class Application::Mapper : public Application::CommandImpl
{
public:
  Mapper(Application::list_t &d, Warsaw::Selection_ptr s) : demos(d), selection(Warsaw::Selection::_duplicate(s)) {}
  virtual void execute(const CORBA::Any &);
private:
  Application::list_t &demos;
  Warsaw::Selection_var selection;
};

void Application::CommandImpl::destroy()
{
  PortableServer::POA_var poa = _default_POA();
  PortableServer::ObjectId *oid = poa->servant_to_id(this);
  poa->deactivate_object(*oid);
  delete oid;
}

void Application::Mapper::execute(const CORBA::Any &)
{
  Selection::Items *items = selection->toggled();
  if (!items->length()) return;
  Tag t = (*items)[items->length() - 1];
  delete items;
  CORBA::Any any;
  for (Application::list_t::iterator i = demos.begin(); i != demos.end(); i++)
    if (t == (*i).id) (*i).mapper->execute(any);
}

class ExitCommand : public Application::CommandImpl
{
 public:
  void execute(const CORBA::Any &) {}// exit(0);}
};

Application::Application(ServerContext_ptr sc)
  : server(ServerContext::_duplicate(sc)),
    tk(resolve_kit<TextKit>(server, "IDL:Warsaw/TextKit:1.0")),
    dk(resolve_kit<DesktopKit>(server, "IDL:Warsaw/DesktopKit:1.0")),
    lk(resolve_kit<LayoutKit>(server, "IDL:Warsaw/LayoutKit:1.0")),
    ttk(resolve_kit<ToolKit>(server, "IDL:Warsaw/ToolKit:1.0")),
    wk(resolve_kit<WidgetKit>(server, "IDL:Warsaw/WidgetKit:1.0")),
    fk(resolve_kit<FigureKit>(server, "IDL:Warsaw/FigureKit:1.0")),
    ck(resolve_kit<CommandKit>(server, "IDL:Warsaw/CommandKit:1.0")),
    ik(resolve_kit<ImageKit>(server, "IDL:Warsaw/ImageKit:1.0")),
    gk(resolve_kit<GadgetKit>(server, "IDL:Warsaw/GadgetKit:1.0")),
    vbox(lk->vbox()),
    choice(wk->toggle_choice()),
    mapper(new Mapper(demos, Selection_var(choice->state())))
{
  background.red = background.green = background.blue = 0.6; background.alpha = 1.;
  Raster_var raster = ik->create("berlin-48.png");
  Image_var  image = fk->pixmap(raster);
  Graphic_var hbox = lk->hbox();
  hbox->append_graphic(image);
  hbox->append_graphic(Graphic_var(lk->hfill()));
  vbox->append_graphic(hbox);

  Graphic_var glyph = tk->chunk(Unicode::to_CORBA(Babylon::String("close")));
  done = lk->margin(Graphic_var(ttk->rgb(glyph, 0., 0., 0.)), 20.);
  glyph = tk->chunk(Unicode::to_CORBA(Babylon::String("settings")));
  settings = lk->margin(Graphic_var(ttk->rgb(glyph, 0., 0., 0.)), 20.);
}

void Application::append(Controller_ptr demo, const Babylon::String &name)
{
  Item item = make_item(name);

  Graphic_var hbox = lk->hbox();
  hbox->append_graphic(Graphic_var(lk->hfill()));
  Trigger_var button1 = wk->button(done, Command_var(Command::_nil()));
  hbox->append_graphic(button1);
  hbox->append_graphic(Graphic_var(lk->hspace(200.)));
  Trigger_var button2 = wk->button(settings, item.settings);
  hbox->append_graphic(button2);
  hbox->append_graphic(Graphic_var(lk->hfill()));
  Graphic_var vb = lk->vbox();
  vb->append_graphic(demo);
  vb->append_graphic(hbox);

  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::outset);
  Graphic_var decorator = ttk->frame(vb, 20., spec, true);
  decorator = gk->alpha(decorator, item.alpha);
  decorator = gk->lighting(decorator, item.red, item.green, item.blue);
  decorator = gk->rotator(decorator, item.zrotation, zaxis);
  decorator = gk->rotator(decorator, item.yrotation, yaxis);
  decorator = gk->zoomer(decorator, item.zoom);

  Controller_var group = ttk->group(Graphic_var(lk->align(decorator, 0., 0.)));
  group->append_controller(demo);
  group->append_controller(button1);
  group->append_controller(button2);
  Window_var window = dk->transient(group);
  button1->action(Command_var(dk->map(window, false)));
  item.mapper = dk->map(window, true);
  demos.push_back(item);
}

void Application::run()
{
  vbox->append_graphic(Graphic_var(lk->vspace(200.)));
  ToolKit::FrameSpec spec;
  spec.brightness(0.5); spec._d(ToolKit::concav);
  vbox->append_graphic(Graphic_var(ttk->frame(choice, 40., spec, false)));
  vbox->append_graphic(Graphic_var(lk->vspace(200.)));
  Graphic_var glyph1 = tk->chunk(Unicode::to_CORBA(Babylon::String("run")));
  Graphic_var label1 = lk->margin(glyph1, 20.);
  Trigger_var run = wk->button(Graphic_var(ttk->rgb(label1, 0., 0., 0.)), Command_var(mapper->_this()));
  Graphic_var glyph2 = tk->chunk(Unicode::to_CORBA(Babylon::String("quit")));
  Graphic_var label2 = lk->margin(glyph2, 20.);
  ExitCommand *cmd = new ExitCommand();
  Trigger_var quit = wk->button(Graphic_var(ttk->rgb(label2, 0., 0., 0.)), Command_var(cmd->_this()));

  vbox->append_graphic(Graphic_var(lk->vspace(200.)));

  Graphic_var hbox = lk->hbox();
  hbox->append_graphic(Graphic_var(lk->hglue(200., 0., 10000.)));
  hbox->append_graphic(run);
  hbox->append_graphic(Graphic_var(lk->hspace(200.)));
  hbox->append_graphic(quit);
  hbox->append_graphic(Graphic_var(lk->hglue(200., 0., 10000.)));
  vbox->append_graphic(hbox);
  Graphic_var margin = lk->margin(vbox, 200.);
  
  spec.brightness(1.0); spec._d(ToolKit::outset);
  Controller_var group = ttk->group(Graphic_var(ttk->frame(margin, 10., spec, true)));
  group->append_controller(choice);
  group->append_controller(run);
  group->append_controller(quit);
  Window_var window = dk->shell(group);
  while (true) Thread::delay(Prague::Time(1000));
}

Application::Item Application::make_item(const Babylon::String &name)
{
  Item item;

  /*
   * insert an item into the choice
   */
  Graphic_var label = tk->chunk(Unicode::to_CORBA(name));
  item.id = choice->append_item(Graphic_var(ttk->rgb(label, 0., 0., 0.)));
  /*
   * create the control elements
   */
  item.alpha = ck->bvalue(0., 1., 1., 0.1, 0.1);
  item.red = ck->bvalue(0., 1., 1., 0.1, 0.1);
  item.blue = ck->bvalue(0., 1., 1., 0.1, 0.1);
  item.green = ck->bvalue(0., 1., 1., 0.1, 0.1);
  item.zrotation = ck->bvalue(0., 360., 0., 5., 5.);
  item.yrotation = ck->bvalue(0., 360., 0., 5., 5.);
  item.zoom = ck->bvalue(-1., 1., 0., 0.1, 0.1);
  
  /*
   * create the settings window
   */
  Layout::Grid::Index index;
  index.col = 2, index.row = 7;
  Layout::Grid_var grid = lk->fixed_grid(index);
  index.row = 0;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("alpha")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.alpha, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  index.row = 1;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("red")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.red, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  index.row = 2;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("green")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.green, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  index.row = 3;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("blue")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.blue, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  index.row = 4;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("z rotation")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.zrotation, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  index.row = 5;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("y rotation")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.yrotation, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  index.row = 6;
  index.col = 0;
  grid->replace(Graphic_var(lk->valign(Graphic_var(ttk->rgb(Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("zoom")))),
							    0., 0., 0.)), 0.5)), index);
  index.col = 1;
  grid->replace(Graphic_var(lk->valign(Graphic_var(lk->margin_flexible(Graphic_var(wk->slider(item.zoom, xaxis)),
								       100., 100., 100.)), 0.5)), index);

  Graphic_var hbox = lk->hbox();
  hbox->append_graphic(Graphic_var(lk->hfill()));
  Graphic_var glyph = tk->chunk(Unicode::to_CORBA(Babylon::String("done")));
  Graphic_var dlabel = lk->margin(glyph, 20.);
  Trigger_var done = wk->button(Graphic_var(ttk->rgb(dlabel, 0., 0., 0.)), Command::_nil());
  hbox->append_graphic(done);
  hbox->append_graphic(Graphic_var(lk->hfill()));

  Graphic_var vbox = lk->vbox();
  vbox->append_graphic(Graphic_var(lk->vspace(200.)));
  vbox->append_graphic(grid);
  vbox->append_graphic(Graphic_var(lk->vspace(200.)));
  vbox->append_graphic(hbox);
  ToolKit::FrameSpec outset;
  outset.brightness(0.5); outset._d(ToolKit::outset);
  Controller_var root = ttk->group(Graphic_var(ttk->frame(Graphic_var(lk->margin(vbox, 100.)), 20., outset, true)));
  Window_var window = dk->transient(root);
  item.settings = dk->map(window, true);
  done->action(Command_var(dk->map(window, false)));
  return item;
}
