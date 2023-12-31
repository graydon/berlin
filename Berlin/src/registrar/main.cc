#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <unistd.h>
#include <syslog.h>
#include <iostream.h>
#include <sys/types.h>

#include "support/registrar/Registrar.hh"
#include "support/registrar/RegistrarDefs.hh"
#include "support/registrar/Registrar_impl.hh"

Registrar_impl *registry;

/* Taken from omniORB example */

static CORBA::Boolean bindObjectToName(CORBA::ORB_ptr orb,
                                       CORBA::Object_ptr obj) {
    CosNaming::NamingContext_var rootContext;
  
    try {
        // Obtain a reference to the root context of the Name service:
        CORBA::Object_var initServ;
        initServ = orb->resolve_initial_references("NameService");

        // Narrow the object returned by resolve_initial_references()
        // to a CosNaming::NamingContext object:
        rootContext = CosNaming::NamingContext::_narrow(initServ);
        if (CORBA::is_nil(rootContext)) {
            syslog(LOG_ERR, "Failed to narrow naming context.");
            return 0;
        }
    } catch(CORBA::ORB::InvalidName& ex) {
        syslog(LOG_ERR, "Service required is invalid [does not exist].");
        return 0;
    }

    try {
        // Bind a context called "Berlin" to the root context:
 
        CosNaming::Name contextName;
        contextName.length(1);
        contextName[0].id   = (const char*) "Berlin";  // string copied
        contextName[0].kind = (const char*) "System";   // string copied    
        // Note on kind: The kind field is used to indicate the type
        // of the object. This is to avoid conventions such as that used
        // by files (name.type -- e.g. test.ps = postscript etc.)

        /* XXX - Really need to see if there is any official 'kind' for
                 databases */

        CosNaming::NamingContext_var berlinContext;
        try {
            // Bind the context to root, and assign berlinContext to it:
            berlinContext = rootContext->bind_new_context(contextName);
        } catch(CosNaming::NamingContext::AlreadyBound& ex) {
            // If the context already exists, this exception will be raised.
            // In this case, just resolve the name and assign berlinContext
            // to the object returned:
            CORBA::Object_var tmpobj;
            tmpobj = rootContext->resolve(contextName);
            berlinContext = CosNaming::NamingContext::_narrow(tmpobj);
            if (CORBA::is_nil(berlinContext)) {
                syslog(LOG_ERR, "Failed to narrow naming context.");
                return 0;
            }
        } 
        // Bind the object (obj) to berlinContext, naming it Registrar:
        CosNaming::Name objectName;
        objectName.length(1);
        objectName[0].id   = (const char*) "Registrar";   // string copied
        objectName[0].kind = (const char*) "Database"; // string copied

        // Bind obj with name Registrar to the berlinContext:
        try {
            berlinContext->bind(objectName, obj);
        } catch(CosNaming::NamingContext::AlreadyBound& ex) {
            berlinContext->rebind(objectName, obj);
        }
        // Note: Using rebind() will overwrite any Object previously bound 
        //       to /Berlin/Registrar with obj.
        //       Alternatively, bind() can be used, which will raise a
        //       CosNaming::NamingContext::AlreadyBound exception if the name
        //       supplied is already bound to an object.

        // Amendment: When using OrbixNames, it is necessary to first try bind
        // and then rebind, as rebind on it's own will throw a
        // NotFoundexception if the Name has not already been bound.
        // [This is incorrect behaviour - it should just bind].
    } catch (CORBA::COMM_FAILURE& ex) {
        syslog(LOG_ERR, "Caught system exception COMM_FAILURE, unable to "
                        "contact the naming service.");
        return 0;
    } catch (omniORB::fatalException& ex) {
        throw;
    } catch (...) {
        syslog(LOG_ERR, "Caught a system exception while using the naming "
                        "service.");
        return 0;
    }
    return 1;
}

void SendRegistrarSignal(int signal) {
    FILE *fd;
    pid_t npid = -1;

    fd = fopen(REGISTRAR_PID, "r");
    if (fd != NULL) {
        fscanf(fd, "%u", &npid);
        fclose(fd);
    } else
        throw Registrar::InternalError("Unable to open pid file");

    if (npid != -1)
        kill(npid, signal);
}

void SetupSignalHandler(int signal, void (*func), int flags) {
    struct sigaction sa;

    sa.sa_handler = func;
    sa.sa_flags = flags;
    sigemptyset(&sa.sa_mask);
    if (sigaction(signal, &sa, NULL) < 0) {
        syslog(LOG_ERR, "Unable to add signal handler");
    }
}

void gotsighup(int signal) {
    syslog(LOG_ERR, "Received SIGHUP - Reconfiguring", signal);
}

void gotsigterm(int signal) {
    syslog(LOG_ERR, "Received SIGTERM - Shutting down", signal);
    unlink(REGISTRAR_PID);
    if (CORBA::is_nil(registry)) {
        exit(0);
    } else {
        registry->Shutdown(0);
    }
}

void gotsigtrap(int signal) {
    syslog(LOG_ERR, "Received SIGTRAP - Toggling debugging", signal);
}

void Usage(char *prog) {
    cout << "Usage: " << prog << " [-n] [-k signal]" << endl;
    cout << "        -k signal reload|stop|debug" << endl;
    cout << "                  Send signal to a running copy and exit" << endl;
    cout << "        -n        Don't fork() when starting" << endl;
    exit(1);
}

int main(int argc, char *argv[]) {
    int opt;
    int quit = 0, nofork = 0;

    while ((opt = getopt(argc, argv, "k:nh?")) != -1) {
        switch (opt) {
            case 'n':
                      /* don't fork */
                      nofork = 1;
                      break;
            case 'k':
                      /* Instead of having some ctl program do this
                         I prefer building it straight in. */

                      if (!strncmp(optarg, "reload", strlen(optarg)))
                          SendRegistrarSignal(SIGHUP);
                      else if (!strncmp(optarg, "stop", strlen(optarg)))
                          SendRegistrarSignal(SIGTERM);
                      else if (!strncmp(optarg, "debug", strlen(optarg)))
                          SendRegistrarSignal(SIGTRAP);
                      else
                          Usage(argv[0]);

                      exit(0);
                      break;
            default:
                      Usage(argv[0]);
                      break;
        }
    }

    if (nofork == 0) {
        pid_t npid;

        npid = fork();
        if (npid == -1)
            cerr << "fork :" << strerror(errno) << endl;
        if (npid != 0) {
            FILE *fd;

            unlink(REGISTRAR_PID);
            fd = fopen(REGISTRAR_PID, "w");
            if (fd != NULL) {
                fprintf(fd, "%u\n", npid);
                if (fflush(fd)) {
                    fclose(fd);
                    unlink(REGISTRAR_PID);
                    cerr << "fflush :" << strerror(errno) << endl;
                }
                fclose(fd);
            } else
                cerr << "fopen :" << strerror(errno) << endl;
            exit(0);
        }
    }

    CORBA::ORB_ptr orb = CORBA::ORB_init(argc, argv, "omniORB2");
    CORBA::BOA_ptr boa = orb->BOA_init(argc, argv, "omniORB2_BOA");

    registry = new Registrar_impl();
    registry->_obj_is_ready(boa);

    Registrar_var myobjRef = registry->_this();

    if (!bindObjectToName(orb, myobjRef)) {
        syslog(LOG_ERR, "Unable to register Nameservice");
        SendRegistrarSignal(SIGTERM);
    } 

#if 0
    CORBA::String_var p = orb->object_to_string(myobjRef);
    cerr << (char*)p << endl;
#endif

    registry->Start(argc, argv);

    SetupSignalHandler(SIGTERM, gotsigterm, SA_RESTART);
    SetupSignalHandler(SIGTRAP, gotsigtrap, SA_RESTART);
    SetupSignalHandler(SIGHUP, gotsighup, SA_RESTART);

    // Start the server, blocking call
    boa->impl_is_ready(0);

    return 0;
}
