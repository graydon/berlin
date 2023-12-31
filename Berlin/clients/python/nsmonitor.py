#!/usr/bin/env python

import sys
import string

# Import the CORBA module
from omniORB import CORBA
import CosNaming

def main():
    # Initialise the ORB
    orb = CORBA.ORB_init(sys.argv, CORBA.ORB_ID)
    # get the server context
    try:
        object = orb.resolve_initial_references("NameService");
        context = object._narrow(CosNaming.NamingContext)
        if context is None:
            print "Failed to narrow the root naming context"
            sys.exit(1)
    except CORBA.SystemException, exception:
        print "Failed to contact the name server"
        sys.exit(1)

    list, iterator = context.list(32);
    for binding in list:
        for component in binding.binding_name:
            print "(" + component.id + ", " + component.kind + ")",  
        print binding.binding_type
    # now iterate over the remaining items...

if __name__ == '__main__':
    main()
