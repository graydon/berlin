/*$Id: pinyin_demo.cc,v 1.7 2001/04/26 01:30:03 stefan Exp $
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
#include <string>

// everything needed for clients
#include "client.hh"

// need to include the Kits I'll use
#include <Warsaw/LayoutKit.hh>
#include <Warsaw/TextKit.hh>
#include <Warsaw/ToolKit.hh>
#include <Warsaw/DesktopKit.hh>
#include <Warsaw/CommandKit.hh>
#include <Warsaw/WidgetKit.hh>
#include <Warsaw/Unicode.hh>

#include <Berlin/ObserverImpl.hh>

// pinyin input class
#include "TextConverter.hh"

#include <Warsaw/TextBuffer.hh>
#include <Babylon/Babylon.hh>
#include <Prague/Sys/Path.hh>
#include <Prague/Sys/GetOpt.hh>
#include <Berlin/RCManager.hh>

class InputObserver : public ObserverImpl {
public:
    InputObserver(Warsaw::TextBuffer_var i,
		  Warsaw::TextBuffer_var s,
		  Warsaw::TextBuffer_var o) : input(i),
					      select(s),
					      output(o) {
	// get the path of the Memorymap we need from the RCManager
	Prague::Path path = RCManager::get_path("pinyindbpath");
	string pinyinDB = path.lookup_file("pinyin.db");
	converter = new TextConverter(pinyinDB);
    }
	

    // This function gets called as soon as the buffer I observe is updated.
    virtual void update(const CORBA::Any &any) {
	Warsaw::TextBuffer::Change * change;

	if (any >>= change) {
	    // 'any' contained a TextBuffer::Change. This is what I exspected.
	    
	    // bs will contain a Babylon::String which is equivalent
	    // to the contents of 'input'.
	    // Unicode::to_internal() converts from the UCS2 encoded Unistring
	    // passed by CORBA to the UCS4 encoded Babylon::String
	    Babylon::String bs(Unicode::to_internal(Warsaw::Unistring_var(input->value())));

	    Babylon::Char last = bs[bs.length() - 1];
	    if (!bs.empty() && last >= 'A' && last <= 'Z') {
		// uppercase letter selects one chinese char out of those
		// shown in 'select'
		
		bs.erase(bs.length() - 1);
		Babylon::String select_from(converter->convert(bs));
		
		if ((last.value() - 'A') < select_from.length()) {
		    // recode letter into Unistring and add to 'output'
		    // I use insert_string() since the chinese chars might have
		    // a codepoint above 0xFFFF which insert_char() is not
		    // able to handle.
		    output->insert_string(Unicode::to_CORBA(Babylon::String(select_from[last.value() - 'A'])));
		    
		    input->clear(); // this clears select too:
		                    // it results in a call to this function
		                    // with an empty 'input'
		} else
		    // delete the uppercase char since it did not
		    // denote a character in 'select':
		    input->remove_backward(1);
	    } else {
		// sharpen 'select':
		select->clear();
		
		Babylon::String select_from(converter->convert(bs));
		
		// Only show the first 26 characters:
		if (select_from.length() > 26)
		    select_from.erase(26, select_from.length());
		
		select->insert_string(Unicode::to_CORBA(select_from));
	    }
	} else
	    // Our 'any' contains no TextBuffer::Change! Since
	    // the TextBuffer only emits that I encountered
	    // a really weird error...
	    exit(10);
    }
    
private:
    Warsaw::TextBuffer_var input;
    Warsaw::TextBuffer_var select;
    Warsaw::TextBuffer_var output;
    
    TextConverter * converter;
}; // class InputObserver

int main(int argc, char ** argv) {
    // Parse args:
    Prague::GetOpt getopt(argv[0], "a simple pinyin input application.");
    getopt.add('r',
	       "resource",
	       Prague::GetOpt::mandatory,
	       "the resource file to load");
    size_t argo = getopt.parse(argc, argv);
    argc -= argo;
    argv += argo;
    std::string value;
    getopt.get("resource", &value);
    if (!value.empty()) RCManager::read(Prague::Path::expand_user(value));
    else {
	getopt.usage();
	exit(1);
    }

    // do the real work:
    try {
	// Do CORBA-magic and connect to server:
	Berlin_Server server(argc, argv);

	// Get Kits:

	// This is what really happens:
	Warsaw::LayoutKit_var lk =
	    server.get_kit<Warsaw::LayoutKit>("IDL:Warsaw/LayoutKit:1.0");

	// Here is some Macro-Magic for the same:
	REGISTER_KIT(server, tk, TextKit, 1.0);
	REGISTER_KIT(server, tlk, ToolKit, 1.0);
	REGISTER_KIT(server, dk, DesktopKit, 1.0);
	REGISTER_KIT(server, ck, CommandKit, 1.0);
	REGISTER_KIT(server, wk, WidgetKit, 1.0);

	// Create the GUI:

	// Create Textlabels:

	// A rather complex command:
	// - Create a Babylon::String containing "PinYin Input Applet"
	// - Change that string into something CORBA understands
	// - Create a text chunk containing said string
	// - Make it black using the ToolKit's rgb decorator.
	Warsaw::Graphic_var title = tlk->rgb(Warsaw::Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("PinYin Input Applet")))), 0.0, 0.0, 0.0);

	// create additional labels:
	Warsaw::Graphic_var chinese_label =
	    tlk->rgb(Warsaw::Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("Possible Matches:")))), 0.0, 0.0, 0.0);
	Warsaw::Graphic_var input_label =
	    tlk->rgb(Warsaw::Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("Input:")))), 0.0, 0.0, 0.0);
	Warsaw::Graphic_var output_label =
	    tlk->rgb(Warsaw::Graphic_var(tk->chunk(Unicode::to_CORBA(Babylon::String("Chinese Text:")))), 0.0, 0.0, 0.0);

	// We'll need three buffers to hold our text:
	Warsaw::TextBuffer_var input_buf = ck->text();
	Warsaw::TextBuffer_var chinese_buf = ck->text();
	Warsaw::TextBuffer_var output_buf = ck->text();

	// These buffers need associated views, else they are invisible:
	Warsaw::Graphic_var input_view = tk->simple_viewer(input_buf);
	Warsaw::Graphic_var chinese_view = tk->simple_viewer(chinese_buf);
	Warsaw::Graphic_var output_view = tk->simple_viewer(output_buf);

	// Construct a frame. It will be drawn around each buffer later:
	Warsaw::ToolKit::FrameSpec frame;
	frame.brightness(0.5); frame._d(Warsaw::ToolKit::inset);

	// Add text in front of the views:
	// We use a hbox which will layout it's children one after another.
	Warsaw::Graphic_var input_line = lk->hbox();

	// The first thing to add is the label
	input_line->append_graphic(input_label);
	// Now we add glue: a strechable, invisible graphic
	input_line->append_graphic(Warsaw::Graphic_var(lk->hglue(100.0, lk->fill(), 0.0)));
	// Then the viewer created earlier. We add some decorations while we are
	// at it anyway. Looks awfully complex, but is rather easy:-)
	input_line->append_graphic(tlk->frame(Warsaw::Graphic_var(lk->margin(Warsaw::Graphic_var(lk->hfixed(Warsaw::Graphic_var(tlk->rgb(input_view, 0.0, 0.0, 0.0)), 4000)), 50.0)), 20.0, frame, true));
	// This line does a lot of things:
	// - It adds a decorator to 'input_view' which makes it display it's
	//   contents in black (0.0, 0.0, 0.0). White would be (1.0, 1.0, 1.0).
	//   To do this it asks the ToolKit for an rgb-Graphic which wraps the
	//   simple_viewer.
	// - Another decorator fixes the horizontal size. This graphic is
	//   requested from the LayoutKit.
	// - Another decorator from the LayoutKit is applied: it draws a
	//   margin around what we have so far.
	// - Finally the frame we constructed earlier is used by the
	//   frame-graphic (obtained from the ToolKit) to wrap everything
	//   in a nive beleveled frame.

	// The same again for the other viewers:
	Warsaw::Graphic_var output_line = lk->hbox();
	output_line->append_graphic(output_label);
	output_line->append_graphic(Warsaw::Graphic_var(lk->hglue(100.0, lk->fill(), 0.0)));
	output_line->append_graphic(tlk->frame(Warsaw::Graphic_var(lk->margin(Warsaw::Graphic_var(lk->hfixed(Warsaw::Graphic_var(tlk->rgb(output_view, 0.0, 0.0, 0.0)), 4000)), 50.0)), 20.0, frame, true));
	Warsaw::Graphic_var chinese_line = lk->hbox();

	chinese_line->append_graphic(chinese_label);
	chinese_line->append_graphic(Warsaw::Graphic_var(lk->hglue(100.0, lk->fill(), 0.0)));
	chinese_line->append_graphic(tlk->frame(Warsaw::Graphic_var(lk->margin(Warsaw::Graphic_var(lk->hfixed(Warsaw::Graphic_var(tlk->rgb(chinese_view, 0.0, 0.0, 0.0)), 4000)), 50.0)), 20.0, frame, true));

	// This was the hard part!
	// Now we only need to arrange our graphics one above the other:

	// I ask the LayoutKit for a vbox
	Warsaw::Graphic_var vbox = lk->vbox();

	// Add the title created earlier ...
	vbox->append_graphic(Warsaw::Graphic_var(lk->margin_lrbt_flexible(title, 0., 1e10, 0., 0., 1e10, 0., 50., 0., 0., 50., 0., 0.)));

	// ... and then the lines.
	vbox->append_graphic(chinese_line);	
	vbox->append_graphic(input_line);	
	vbox->append_graphic(output_line);

	// 'vbox' contains the complete graphics of our little application now!
	// It will even display whatever is in the buffers as '*_view' each
	// observes one of the buffers.

	// Now we only need to get something into those buffers:

	// Construct my observer and make it observe 'input_buf'
	InputObserver observer(input_buf, chinese_buf, output_buf);
	input_buf->attach(Warsaw::Observer_var(observer._this()));
	// The 'observer' will now watch 'input_buf' for changes
	// and alter the other buffers as necessary.

	// Now we only need to request a window that will contain our UI:
	// While we are at it we will wrap it into a text_input Controller.
	// This controller accepts keyboard-events and updates a TextBuffer.
	// It does not draw anything! That's done by the observers of
	// 'input_buf' (which are 'observer' and 'input_view' in this applet).
	Warsaw::ToolKit::FrameSpec spec;
	spec.brightness(0.5); spec._d(Warsaw::ToolKit::outset);
	Warsaw::Graphic_var body = tlk->frame(vbox, 20., spec, true);
	Warsaw::Window_var window =
	    dk->shell(tlk->text_input(body, input_buf));

	// Don't quit but idle around a bit so the server has the chance
	// to do its work:-)
	while(true) Prague::Thread::delay(Prague::Time(1000));

	// Did you notice that everything besides 'observer' was
	// actually created in the server? We asked it for the Kits
	// (which get loaded into the server) and those in turn created
	// the graphics we used.
    }

    // All that's left to do is error handling:
    catch (const CORBA::Exception & e) {
	cerr << "Uncaught CORBA exception! " << e << endl;
	return 1;
    }
    catch (const std::exception & e) {
	cerr << "Uncaught exception: " << e.what() << endl; 
	return 2;
    }

    return 0;
} // main
