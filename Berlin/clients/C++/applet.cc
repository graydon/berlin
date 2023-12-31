/*$Id: pinyin_demo.cc,v 1.8 2001/01/10 17:13:34 tobias Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999,2000 Tobias Hunger <Tobias@berlin-consortium.org>
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

// stuff from stdc++
#include <iostream>
#include <memory>

// everything needed for clients
#include <Warsaw/config.hh>
#include <omniORB3/CORBA.h>
#include <omniORB3/poa.h>
#include <Warsaw/resolve.hh>
#include <Warsaw/exception.hh>
#include <Warsaw/ClientContextImpl.hh>
#include <Warsaw/Server.hh>
#include <Warsaw/Graphic.hh>
#include <Warsaw/Window.hh>
#include <Warsaw/Trigger.hh>
#include <Warsaw/Unicode.hh>
#include <Warsaw/Controller.hh>

#include <Berlin/CommandImpl.hh>
#include <Berlin/ControllerImpl.hh>

class Berlin_Server {
public:
    Berlin_Server(int argc, char** argv) {
	// CORBA initialization
	orb = CORBA::ORB_init(argc, argv, "omniORB3");
	name = resolve_init<CosNaming::NamingContext>(orb, "NameService");
	poa = resolve_init<PortableServer::POA>(orb, "RootPOA");
	poa_manager = poa->the_POAManager();
	poa_manager->activate();
	
	// Berlin initialization
	client = new ClientContextImpl;
	server = resolve_name<Warsaw::Server>(name, "IDL:Warsaw/Server:1.0");
	server_context =
	    server->create_server_context(Warsaw::ClientContext_var(client->_this()));
    }
 
    template<class T>
    typename T::_ptr_type get_kit(const char *name,
				  const Warsaw::Kit::PropertySeq &props = 0) {
	return resolve_kit<T>(server_context, name, props);
    }

    CORBA::ORB_ptr get_ORB() { return orb; }
    CosNaming::NamingContext_ptr get_naming_context() { return name; }
    PortableServer::POA_ptr get_POA() { return poa; }
    PortableServer::POAManager_ptr get_POAManager() { return poa_manager; }

    ClientContextImpl * get_client_context() { return client; }
    Warsaw::Server_ptr get_server() { return server; }
    Warsaw::ServerContext_ptr get_server_context() { return server_context;}

    Warsaw::ServerContext_var operator() () { return server_context; }

    ~Berlin_Server() { delete client; };
private:
    Berlin_Server() {};
    Berlin_Server(const Berlin_Server &) {};

    // CORBA
    CORBA::ORB_var orb;
    CosNaming::NamingContext_var name;
    PortableServer::POA_var poa;
    PortableServer::POAManager_var poa_manager;

    // BERLIN
    ClientContextImpl * client;
    Warsaw::Server_var server;
    Warsaw::ServerContext_var server_context;
};

// need to include the Kits I'll use
#include <Warsaw/Focus.hh>
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/TextKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/DesktopKit.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/WidgetKit.hh>

class InputCntr : public ControllerImpl {
public:
    InputCntr() : ControllerImpl(false) {}
    virtual CORBA::Boolean receive_focus(Warsaw::Focus_ptr f) { cerr << "receive focus from device " << f->device() << endl; return ControllerImpl::receive_focus(f);}
    void key_press(const Warsaw::Input::Event & e) {
	cerr << "InputCntr::key_press" << endl;
	const Warsaw::Input::Toggle &toggle = e[0].attr.selection();
	Babylon::Char uc(static_cast<Babylon::UCS4>(toggle.number));
	cerr << "Got character \"" << uc << "\"" << endl;
    }
};

class ExitCommand : public CommandImpl {
public :
    void execute(const CORBA::Any &) {
	cerr << "Cleaning up..." << endl;
	PortableServer::POA_var poa = _default_POA();
	poa->destroy(1, 0);
	cerr << "EXITING!" << endl;
	exit(0);
    }

    void destroy() {
	PortableServer::POA_var poa = _default_POA();
	PortableServer::ObjectId *oid = poa->servant_to_id(this);
	poa->deactivate_object(*oid);
	delete oid;
    }
};

int main(int argc, char ** argv) {
    try {
	// Do CORBA-magic and connect to server:
	Berlin_Server server(argc, argv);

	// Get Kits:
	Warsaw::LayoutKit_var lk =
	    server.get_kit<Warsaw::LayoutKit>("IDL:Warsaw/LayoutKit:1.0");
	Warsaw::TextKit_var tk =
	    server.get_kit<Warsaw::TextKit>("IDL:Warsaw/TextKit:1.0");
	Warsaw::ToolKit_var ttk =
	    server.get_kit<Warsaw::ToolKit>("IDL:Warsaw/ToolKit:1.0");
	Warsaw::DesktopKit_var dk =
	    server.get_kit<Warsaw::DesktopKit>("IDL:Warsaw/DesktopKit:1.0");
	Warsaw::CommandKit_var ck =
	    server.get_kit<Warsaw::CommandKit>("IDL:Warsaw/CommandKit:1.0");
	Warsaw::WidgetKit_var wk =
	    server.get_kit<Warsaw::WidgetKit>("IDL:Warsaw/WidgetKit:1.0");

	// Create Graphics:

	// Build a button from scratch:
	cerr << "Create Graphics:" << endl;
	Warsaw::Graphic_var glyphs =
	    tk->chunk(Unicode::to_CORBA(Babylon::String("Exit")));
	cerr << "   * Text created" << endl;
	Warsaw::Graphic_var color_glyphs = 
	    lk->margin(Warsaw::Graphic_var(ttk->rgb(glyphs, 1.0, 1.0, 1.0)), 20.0);
	cerr << "   * Color changed" << endl;
	Warsaw::Graphic_var hbox = lk->hbox();
	cerr << "   * hbox created" << endl;
	hbox->append_graphic(lk->hfill());
	hbox->append_graphic(color_glyphs);
	hbox->append_graphic(lk->hfill());
	cerr << "   * hbox filled" << endl;
	ExitCommand * command = new ExitCommand();
	cerr << "   * Command created" << endl;
	Warsaw::Trigger_var button =
	    wk->button(hbox, Warsaw::Command_var(command->_this()));
	cerr << "   * Button created" << endl;
	Warsaw::Graphic_var color_button = ttk->rgb(button, 0.0, 0.0, 0.5);
	cerr << "   * Button color changed" << endl;
	InputCntr ic;
	Warsaw::Controller_var wrapped_button = ic._this();
 	wrapped_button->body(hbox);
	cerr << "   * Button wrapped in InputCtrl" << endl;
	Warsaw::Window_var window = dk->shell(wrapped_button);
	cerr << "   * Window created" << endl;
// 	window->append_controller(wrapped_button);
	cerr << "   * Controller (wrapped_button) added to Window." << endl;

	// Wait...
	while(true) Prague::Thread::delay(Prague::Time(1000));

    } catch (const CORBA::Exception & e) {
	cerr << "Uncaught CORBA exception! " << e << endl;
	return 1;
    } catch (const std::exception & e) {
	cerr << "Uncaught exception: " << e.what() << endl; 
	return 2;
    }
    return 0;
} // main
