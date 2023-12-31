#!/usr/bin/env python

import sys, time
import string

# Import Warsaw generated interface
import Warsaw
import Figure

# Import berlin client lib
import berlin
from berlin import Label, Callback, nullAny

class DemoApp (berlin.App):
    def run(self):
	vbox   = kits.layout.vbox();
	adjustable1 = kits.command.bvalue(0., 90., 0., 10., 10.)
	adjustable2 = kits.command.bvalue(0., 90., 0., 10., 10.)
	vbox.append_graphic(kits.layout.align(kits.widget.slider(adjustable1, Warsaw.xaxis), 0.0, 0.0))
	vbox.append_graphic(kits.layout.align(kits.widget.slider(adjustable2, Warsaw.xaxis), 0.0, 0.0))
	hbox = kits.layout.vbox()
	rectangle = kits.figure.rectangle(0., 0., 50., 50.)
	rectangle._set_type(Figure.fill)
	rectangle._set_foreground(Warsaw.Color(1., 1., 0., 0.9))
	rectangle._set_background(Warsaw.Color(1., 0.5, 0.5, 0.9))
	rectangle = kits.tool.rgb(rectangle, 1., 0., 0.)
	hbox.append_graphic(kits.widget.button(rectangle, Warsaw.Command._nil))
	# hbox.append(layout.align(gadget.rotator(gadget.rotator(layout.align(rectangle, 0.5, 0.5), adjustable1, Warsaw.yaxis), adjustable2, Warsaw.yaxis), 0., 0.))
	rect_align = kits.layout.align(rectangle, 1.0, 1.0)
	rect_align = kits.tool.debugger(rect_align, 'rect_debug')
	z_rotator = kits.gadget.rotator(rect_align, adjustable1, Warsaw.zaxis)
	z_rotator = kits.tool.debugger(z_rotator, 'zrotator_debug')
	y_rotator = kits.gadget.rotator(z_rotator, adjustable2, Warsaw.yaxis)
	y_rotator = kits.tool.debugger(y_rotator, 'yrotator_debug')
	hbox.append_graphic(kits.layout.align(y_rotator, 0., 0.))
	hbox.append_graphic(kits.widget.button(rectangle, Warsaw.Command._nil))
	vbox.append_graphic(hbox)
	outset = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.outset, 0.5)
	shell = kits.desktop.shell(kits.tool.group(kits.tool.frame(vbox, 20., outset, 1)))
       

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
