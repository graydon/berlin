# Python Berlin client library
# Copyright (c) 2000 by Stephen Davies
# This file is licensed for use under the GPL
#
# Tabstop is 8, SoftTabStop is 4, ShiftWidth is 4 (as per last line)
#
"""
Import from this module to connect to the Berlin server and access
the kits therein. Use the get_kits() and get_connection() functions to access
instances of the Kits and Connection objects.
"""

import sys, string, threading

# Import omniORBpy stuff
from omniORB import CORBA
import CosNaming

# Import the Warsaw stubs
import Warsaw, Unidraw
import Warsaw__POA

class ClientContextImpl (Warsaw__POA.ClientContext):
    "A default implementation of the ClientContext interface"
    def userName(self): return US("joe")
    def ping(self): return

def US(s):
    "Convert the given python string to a Unistring"
    return map(lambda x:ord(x), s)

# The default kits available. More can be added using kits.add_kit()
# TODO: include version and property information in the tuple
_defaultKitNames = {
    'tool'    : ('Warsaw',  'ToolKit'),
    'image'   : ('Warsaw',  'ImageKit'),
    'figure'  : ('Warsaw',  'FigureKit'),
    'command' : ('Warsaw',  'CommandKit'),
    'gadget'  : ('Warsaw',  'GadgetKit'),
    'desktop' : ('Warsaw',  'DesktopKit'),
    'layout'  : ('Warsaw',  'LayoutKit'),
    'text'    : ('Warsaw',  'TextKit'),
    'widget'  : ('Warsaw',  'WidgetKit'),
    'unidraw' : ('Unidraw', 'UnidrawKit'),
}

# An empty Any object for sending to Commands
nullAny = CORBA.Any(CORBA.TC_null, None)
    
# You should use these functions instead of directly creating instances
def get_kits():
    "Return a Kits singleton"
    if not kits: return Kits()
    return kits
def get_connection(clientContext = None):
    """Return a Connection singleton. If it doesn't already exist, one is
    created using the ClientContext parameter"""
    if not connection: return Connection(clientContext)
    return connection
# The global vars
kits = None
connection = None

class Kits:
    """This class resolves kits upon request, and caches the resolved kit
    objects. It loads _defaultKitNames as the default set of kits, but more
    can be added using the add_kit method."""
    def __init__(self):
	"""Initialise the Kits singleton, and insert it into the global
	(berlin module) namespace"""
	global kits
	kits = self
	self._kitNames = {}
	self._kitNames.update(_defaultKitNames)
	self._kitObjects = {}

    def add_kit(name, module, kit):
	"""Adds the given kit to the list of accessable kits"""
	self._kitNames[name] = (module, kit)

    def __getattr__(self, kit):
	"""Returns a cached kit. If the kit is not already cached, then it is
	looked up using the list of kits that you can update with add_kit()"""
	# Check cache
	if self._kitObjects.has_key(kit):
	    return self._kitObjects[kit]
	# Try and resolve
	if self._kitNames.has_key(kit):
	    # Find and resolve the kit
	    module, name = self._kitNames[kit]
	    idl = "IDL:%s/%s:1.0"%self._kitNames[kit]
	    obj = connection.context.resolve(idl, [])
	    cls = globals()[module].__dict__[name]
	    obj = obj._narrow(cls)
	    if obj is not None:
		self._kitObjects[kit] = obj
		return obj
	# Raise an exception to signal error
	raise NameError, "Kit '%s' not found."%kit

class Connection:
    """Connection wrapper that does all the CORBA stuff"""
    def __init__(self, clientContext=None):
	"""Initialises the singleton, inserts it into the global (berlin
	module) namespace, and finds the Berlin server. You may pass your own
	ClientContext object (not CORBA var!), else a ClientContextImpl will
	be used."""
	global connection
	connection = self

	# Initialise the ORB
	self.orb = CORBA.ORB_init(sys.argv, CORBA.ORB_ID)

	# Find the POA
	self.poa = self.orb.resolve_initial_references("RootPOA")
	# Activate the POA
	self.poaManager = self.poa._get_the_POAManager()
	self.poaManager.activate()

	# Create an ClientContext
	if clientContext is None: clientContext = ClientContextImpl()
	# Create an object reference, and implicitly activate the object
	self.clientContext = clientContext._this()

	# get a server context
	object = self.orb.resolve_initial_references("NameService")
	context = object._narrow(CosNaming.NamingContext)
	name = [CosNaming.NameComponent("IDL:Warsaw/Server:1.0", "Object")]
	object = context.resolve(name)
	self.server = object._narrow(Warsaw.Server)
	#print "Got Server var =",self.server
	#print "Client Context var =",self.clientContext
	self.context  = self.server.create_server_context(self.clientContext)

class KBThread (threading.Thread):
    """A thread that waits for the user to hit enter, and then signals an
    application quit. This assumes there is a global var called 'app' with an
    attribute called 'quit' that is a threading.Event object"""
    def run(self):
	"Run until enter"
	print "Hit ENTER to quit."
	while 1:
	    line = sys.stdin.readline()
	    if len(line) == 1: break
	self.do_quit()
    def do_quit(self):
	"Override if you want to quit some other way"
	app.quit.set()


class App:
    "Base class for applications with some default useful functionality"
    def __init__(self):
	"""Creates global kits and connection objects, the app.quit Event
	object, and a KBThread to set it."""
	global app
	app = self
	self.quit = threading.Event() # set quit to.. quit :)
	self.kbthread = KBThread()
	self.kbthread.setDaemon(1) # dont wait for kb thread :)
	self.kbthread.start()


def Label(string):
    "Creates a black label from the given string"
    return kits.tool.rgb(kits.text.chunk(US(string)),0.,0.,0.)

class Callback(Warsaw__POA.Command):
    """Small wrapper command which calls a python function with a single
    argument which is the Any."""
    def __init__(self, callback):
	self.callback = callback
    def execute(self, any):
	print "Callback to",self.callback,"with",any
	self.callback(any)


# vim: ts=8:sts=4:sw=4
