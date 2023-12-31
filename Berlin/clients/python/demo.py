#!/usr/bin/env python

import sys, time
import string

# Import berlin client lib
import berlin, Warsaw
from berlin import Label, Callback, nullAny

class DemoApp (berlin.App):
    def run(self):
	# Create a pixmap
	pixmap = kits.figure.pixmap(kits.image.create("berlin-128.png"))
	margin = kits.layout.margin_flexible(pixmap, 100., 500., 100.)
	spec = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.outset, 0.5)
        demo = kits.tool.frame(margin, 10., spec, 1)
	group = kits.tool.group(kits.tool.debugger(demo, "debug"))
	shell = kits.desktop.shell(group)
    

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
