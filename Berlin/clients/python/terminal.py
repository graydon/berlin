#!/usr/bin/env python

import sys, time
import string

# Import Warsaw generated stuff
import Warsaw

# Import berlin client lib
import berlin
from berlin import Label, Callback, nullAny

class DemoApp (berlin.App):
    def run(self):
	terminal   = kits.widget.terminal()
	scrollable = kits.widget.scrollable(kits.tool.rgb(terminal, 0., 0., 0.))
	scrollable.append_controller(terminal)
	outset     = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.outset, 0.5)
	size_fixer = kits.layout.fixed_size(scrollable, 4000., 3000.)
	frame      = kits.tool.frame(size_fixer, 20., outset, 1)
	group      = kits.tool.group(frame)
	group.append_controller(scrollable)
	shell      = kits.desktop.shell(group)
    

def main():
    # Create singletons
    global app, kits, connection
    connection = berlin.get_connection()
    kits = berlin.get_kits()
    app = DemoApp()

    app.run()

    # Wait for the quit event
    app.quit.wait()

    # Tell the orb to shutdown (and wait for pending calls to complete)
    connection.orb.shutdown(1)

if __name__ == '__main__':
    main()

# vim: ts=8:sts=4:sw=4
