#!/usr/bin/env python

import sys
import string

# Import the CORBA module
from omniORB import CORBA

# Import the Warsaw stubs
import Warsaw
import POA_Warsaw
import CosNaming

# Define an implementation of the ClientContext interface
class ClientContextImpl (POA_Warsaw.ClientContext):
	def userName(self): return "joe"
	def ping(self): return

def init():
	"Call this function to initialise the application code"

	global orb, poa, poaManager
	# Initialise the ORB
	orb = CORBA.ORB_init(sys.argv, CORBA.ORB_ID)
	# Find the POA
	poa = orb.resolve_initial_references("RootPOA")
	# Activate the POA
	poaManager = poa._get_the_POAManager()
	poaManager.activate()

	global client
	# Create an ClientContext
	ci = ClientContextImpl()
	# Create an object reference, and implicitly activate the object
	client = ci._this()


	global context, server
	# get a server context
	context = orb.resolve_initial_references("NameService")
	name = [CosNaming.NameComponent("IDL:Server:1.0", "Object")]
	server = context.resolve(name)
	context  = server.newServerContext(client)

	global layout, tool, widget, desktop, image, figure, text, command
	# get some kits
	properties = []
	object  = context.resolve("IDL:LayoutKit:1.0", properties)
	layout  = object._narrow(Warsaw.LayoutKit)
	object  = context.resolve("IDL:ToolKit:1.0", properties)
	tool	= object._narrow(Warsaw.ToolKit)
	object  = context.resolve("IDL:WidgetKit:1.0", properties)
	widget  = object._narrow(Warsaw.WidgetKit)
	object  = context.resolve("IDL:DesktopKit:1.0", properties)
	desktop = object._narrow(Warsaw.DesktopKit)
	object  = context.resolve("IDL:ImageKit:1.0", properties)
	image   = object._narrow(Warsaw.ImageKit)
	object  = context.resolve("IDL:FigureKit:1.0", properties)
	figure  = object._narrow(Warsaw.FigureKit)
	object  = context.resolve("IDL:TextKit:1.0", properties)
	text	= object._narrow(Warsaw.TextKit)
	object  = context.resolve("IDL:CommandKit:1.0", properties)
	command = object._narrow(Warsaw.CommandKit)

def pixmap(filename):
	"""Get a pixmap on the server. Note path is relative to where you started the
	*SERVER* from, not the client"""
	raster = image.create(filename)
	return figure.pixmap(raster)

def loop():
	"Infinite loop kinda thing"
	while 1:
		line = sys.stdin.readline()
		if len(line) == 1: break

def toUni(string): 
	"Convert a python string to a Warsaw Unistring"
	return map(lambda x:ord(x), string)

def fromUni(unistring): 
	"Convert a Warsaw Unistring to a python string"
	return string.join(map(lambda x:chr(x), unistring), '') 


def main():
	"Testing function"
	init()
	pic = pixmap("etc/PNG/berlin-48.png")
	grid   = layout.fixedGrid(Warsaw.Grid.Index(2,1))
	grid.replace(layout.valign(pic, 1.0), Warsaw.Grid.Index(0, 0))
	grid.replace(layout.valign(pic, 0.5), Warsaw.Grid.Index(1, 0))
	group  = tool.group(tool.debugger(grid, "grid"))
	shell  = desktop.shell(group)

if __name__ == '__main__':
	main()
