#!/usr/bin/env python

import sys, time
import string

# Import the Warsaw stubs
import Warsaw

# Import berlin client lib
import berlin
from berlin import Label, Callback, nullAny

class DemoApp (berlin.App):
    def mk_frame(self, color):
	spec = Warsaw.ToolKit.FrameSpec(
	    Warsaw.ToolKit.colored, apply(Warsaw.Color, color))
	return kits.tool.frame(self.pixmap, 50, spec, 1)
    def run(self):
	# Create a pixmap
	raster = kits.image.create("berlin-48.png")
	self.pixmap = kits.figure.pixmap(raster)

	if 1: # The elegant way to do it
	    colors = (
		(1.0, 0.5, 0.5, 1.0), # pink
		(0.5, 1.0, 0.5, 1.0), # yellow
		(0.5, 0.5, 1.0, 1.0), # blue
		(1.0, 1.0, 0.5, 1.0), # green
		(1.0, 0.5, 1.0, 1.0)  # red
	    )
	    box = kits.layout.hbox()
	    frames = map(self.mk_frame, colors)
	    # Add the frames to the box. Note the use of binding a method to
	    # an object instance
	    map(box.append_graphic, frames)
	    # We can only shell a controller, so wrap the hbox in a group
	    group = kits.tool.group(box)
	    shell = kits.desktop.shell(group)
	    # Get an iterator that points at the first graphic (frame)
	    # Note the iterator is a reference to a server-side object
	    iterator = box.first_child_graphic()
	    # Remove the frames one by one
	    for frame in frames:
		iterator.remove()
		time.sleep(1)
	    # Add the frames back, one by one
	    for frame in frames:
		iterator.insert(frame)
		time.sleep(1)
	    # Free the iterator
	    iterator.destroy()

	else: # The simple way to do it :)
	    pixmap = self.pixmap
	    spec   = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.colored, Warsaw.Color(1., 0.5, 0.5, 1.))
	    frame1 = tool.frame(pixmap, 50, spec, 1)
	    spec   = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.colored, Warsaw.Color(0.5, 1., 0.5, 1.))
	    frame2 = tool.frame(pixmap, 50, spec, 1)
	    spec   = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.colored, Warsaw.Color(0.5, 0.5, 1., 1.))
	    frame3 = tool.frame(pixmap, 50, spec, 1)
	    spec   = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.colored, Warsaw.Color(1., 1., 0.5, 1.))
	    frame4 = tool.frame(pixmap, 50, spec, 1)
	    spec   = Warsaw.ToolKit.FrameSpec(Warsaw.ToolKit.colored, Warsaw.Color(1., 0.5, 1., 1.))
	    frame5 = tool.frame(pixmap, 50, spec, 1)
	    box    = layout.hbox()
	    box.append_graphic(frame1)
	    box.append_graphic(frame2)
	    box.append_graphic(frame3)
	    box.append_graphic(frame4)
	    box.append_graphic(frame5)
	    group  = tool.group(box)
	    shell  = desktop.shell(group)
	    iterator = box.first_child_graphic()
	    for i in [1,2,3,4,5]:
		iterator.remove()
		time.sleep(1)
	    iterator.insert(frame1)
	    time.sleep(1)
	    iterator.insert(frame2)
	    time.sleep(1)
	    iterator.insert(frame3)
	    time.sleep(1)
	    iterator.insert(frame4)
	    time.sleep(1)
	    iterator.insert(frame5)
	    iterator.destroy()
    

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
