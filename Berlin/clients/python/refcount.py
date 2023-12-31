#!/usr/bin/env python

import sys, time
import string

# Import berlin client lib
import berlin
from berlin import Label, Callback, nullAny

class RefCountApp (berlin.App):
    def run(self):
	# Create a pixmap
	raster = kits.image.create("../etc/PNG/berlin-48.png")
	pixmap = kits.figure.pixmap(raster)
	wrapper1 = kits.layout.margin(pixmap, 50)
	pixmap.decrement()
	wrapper2 = kits.layout.margin(wrapper1, 50)
	wrapper1.decrement()
	wrapper3 = kits.layout.margin(wrapper2, 50)
	wrapper2.decrement()
	group  = kits.tool.group(wrapper3)
	wrapper3.decrement()
	shell  = kits.desktop.shell(group)
	group.decrement()
	time.sleep(2)
	wrapper3.decrement()

def main():
    # Create singletons
    global app, kits, connection
    connection = berlin.get_connection()
    kits = berlin.get_kits()
    app = RefCountApp()

    app.run()

    # Wait for the quit event
    app.quit.wait()

    # Tell the orb to shutdown (and wait for pending calls to complete)
    connection.orb.shutdown(1)

if __name__ == '__main__':
    main()

# vim: ts=8:sts=4:sw=4# Import the CORBA module
