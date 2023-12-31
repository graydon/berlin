#include <iostream.h>

#include "support/registrar/Registrar.hh"
#include "support/registrar/RegistrarDefs.hh"

static CORBA::Object_ptr getObjectReference(CORBA::ORB_ptr orb) {
    CosNaming::NamingContext_var rootContext;
  
    try {
        // Obtain a reference to the root context of the Name service:
        CORBA::Object_var initServ;
        initServ = orb->resolve_initial_references("NameService");

        // Narrow the object returned by resolve_initial_references()
        // to a CosNaming::NamingContext object:
        rootContext = CosNaming::NamingContext::_narrow(initServ);
        if (CORBA::is_nil(rootContext)) {
            cerr << "Failed to narrow naming context." << endl;
            return CORBA::Object::_nil();
        }
    } catch(CORBA::ORB::InvalidName& ex) {
        cerr << "Service required is invalid [does not exist]." << endl;
        return CORBA::Object::_nil();
    }


    // Create a name object, containing the name test/context:
    CosNaming::Name name;
    name.length(2);

    name[0].id   = (const char*) "Berlin";       // string copied
    name[0].kind = (const char*) "System"; // string copied
    name[1].id   = (const char*) "Registrar";
    name[1].kind = (const char*) "Database";
    // Note on kind: The kind field is used to indicate the type
    // of the object. This is to avoid conventions such as that used
    // by files (name.type -- e.g. test.ps = postscript etc.)

  
    CORBA::Object_ptr obj;
    try {
        // Resolve the name to an object reference, and assign the reference 
        // returned to a CORBA::Object:
        obj = rootContext->resolve(name);
    } catch(CosNaming::NamingContext::NotFound& ex) {
        // This exception is thrown if any of the components of the
        // path [contexts or the object] aren't found:
        cerr << "Context not found." << endl;
        return CORBA::Object::_nil();
    } catch (CORBA::COMM_FAILURE& ex) {
        cerr << "Caught system exception COMM_FAILURE, unable to contact the "
             << "naming service." << endl;
        return CORBA::Object::_nil();
    } catch(omniORB::fatalException& ex) {
        throw;
    } catch (...) {
        cerr << "Caught a system exception while using the naming service."<< endl;
        return CORBA::Object::_nil();
    }
    return obj;
}

int main(int argc, char *argv[]) {
    CORBA::ORB_ptr orb = CORBA::ORB_init(argc, argv, "omniORB2");
    CORBA::BOA_ptr boa = orb->BOA_init(argc, argv,"omniORB2_BOA");

    try {
        CORBA::Object_var obj = getObjectReference(orb);
        Registrar_var registry = Registrar::_narrow(obj);


        if (CORBA::is_nil(registry)) {
            cerr << "cannot invoke on a nil object reference.\n" << endl;
            return 1;
        }

        CORBA::String_var versionnumber = registry->getVersion();
        cout << "Version returned " << versionnumber << endl;

        Registrar::keyaccess perms;
        perms.mode = 0;
        perms.uid = 0;
        perms.gid = 0;
        cout << "Attempting to add key /tmp/Test3" << endl;
        registry->createKey("/tmp/Test3", perms);

        cout << "Attempting to set /tmp/Test3's perms" << endl;
        perms.mode = 0;
        perms.uid = 1;
        perms.gid = 1;
        registry->setSecurityInfo("/tmp/Test3", perms);

        Registrar::keyaccess rperms;
        cout << "Attempting to get /tmp/Test3's perms" << endl;
        rperms = registry->getSecurityInfo("/tmp/Test3");
        if (rperms.mode == perms.mode && rperms.uid == perms.uid &&
            rperms.gid == perms.gid) {
            cout << "Permissions Match!" << endl;
        } else {
            cout << "Permissions do not match, bug!" << endl;

            cout << "Set permissions: " << perms.mode << " "
                 << perms.uid << " " << perms.gid << endl;

            cout << "Got permissions: " << rperms.mode << " "
                 << rperms.uid << " " << rperms.gid << endl;
        }
        cout << "Attempting to delete key /tmp/Test3" << endl;
        registry->deleteKey("/tmp/Test3");

    } catch(CORBA::COMM_FAILURE& ex) {
        cerr << "Caught system exception COMM_FAILURE, unable to contact the "
             << "object." << endl;
    } catch(omniORB::fatalException& ex) {
        cerr << "Caught omniORB2 fatalException. This indicates a bug is caught "
             << "within omniORB2.\nPlease send a bug report.\n"
             << "The exception was thrown in file: " << ex.file() << "\n"
             << "                            line: " << ex.line() << "\n"
             << "The error message is: " << ex.errmsg() << endl;
    } catch(Registrar::InternalError& ex) {
        cerr << "Caught Internal Error " << ex.Error << endl;
    } catch(...) {
        cerr << "Caught a system exception." << endl;
    }

    return 0;
}
