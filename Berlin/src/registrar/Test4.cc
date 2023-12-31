#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>

#include "support/registrar/Registrar.hh"
#include "support/registrar/RegistrarDefs.hh"

Registrar_var registry;
int printhead = 0;

int dumpDatabaseTree(char *key) {
    Registrar::keys *enumkeys;
    char *subkey;

    enumkeys = registry->enumKeys(key);
    
    for (int i = 0; i < enumkeys->length(); i++) {
        char *subkey = (char *)malloc(strlen(key)+strlen((*enumkeys)[i])+2);

        sprintf(subkey, "%s%s", key, (char*)(*enumkeys)[i]);
        Registrar::keypair *kp = registry->getKeyPair(subkey);

        cout << "<key name=\"" << subkey << "\" type=\"" << kp->type << "\""
             << " uid=\"" << kp->perms.uid << "\" gid=\"" 
             << kp->perms.gid << "\"" << " mode=\"" << kp->perms.mode 
             << "\">" << endl;

        if (kp->type == REG_TYPE_STRING) {
            char *value;
            kp->value >>= value;
            cout << "    <value>" << value << "</value>" << endl;
        } else if (kp->type == REG_TYPE_BOOLEAN) {
            short int value;
            kp->value >>= value;
            if (value == 1)
                cout << "    <value>TRUE</value>" << endl;
            else if (value == 0)
                cout << "    <value>FALSE</value>" << endl;
        } else if (kp->type == REG_TYPE_INTEGER) {
            long int value;
            kp->value >>= value;
            cout << "    <value>" << value << "</value>"
                 << endl;
        } else if (kp->type == REG_TYPE_BINARY) {
            char * value;
            kp->value >>= value;
            // XXX - must encode this first
            cout << "    <value>" << value << "</value>"
                 << endl;
        } else if (kp->type == REG_TYPE_NULL) {
            cout << "    <value></value>" << endl;
        }
        cout << "</key>" << endl;
        strcat(subkey, "/");
        dumpDatabaseTree(subkey);
        free(subkey);
    }
    return enumkeys->length();
}

int dumpDatabaseTreeXML(char *key) {
    cout << "<?xml version=\"1.0\" standalone=\"no\" encoding=\"UTF-8\"?>"
         << endl;

    dumpDatabaseTree(key);
}

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
        registry = Registrar::_narrow(obj);


        if (CORBA::is_nil(registry)) {
            cerr << "cannot invoke on a nil object reference.\n" << endl;
            return 1;
        }

        CORBA::String_var versionnumber = registry->getVersion();
        cout << "Version returned " << versionnumber << endl;


        // Just in case they weren't removed earlier
        try {
            registry->deleteKey("/tmp/Test4/Test4/Test4/Test4");
            registry->deleteKey("/tmp/Test4/Test4/Test4");
            registry->deleteKey("/tmp/Test4/Test4");
            registry->deleteKey("/tmp/Test4");

            registry->deleteKey("/user/Test4/Test4/Test4/Test4");
            registry->deleteKey("/user/Test4/Test4/Test4");
            registry->deleteKey("/user/Test4/Test4");
            registry->deleteKey("/user/Test4");

            registry->deleteKey("/host/Test4/Test4/Test4/Test4");
            registry->deleteKey("/host/Test4/Test4/Test4");
            registry->deleteKey("/host/Test4/Test4");
            registry->deleteKey("/host/Test4");

            registry->deleteKey("/system/Test4/Test4/Test4/Test4");
            registry->deleteKey("/system/Test4/Test4/Test4");
            registry->deleteKey("/system/Test4/Test4");
            registry->deleteKey("/system/Test4");
        } catch (Registrar::NotFound &ex) {}

        Registrar::keyaccess perms;
        perms.mode = 0;
        perms.uid = 0;
        perms.gid = 0;

        const char *data = "This is a test of the Registrar";
        registry->createKey("/tmp/Test4", perms);
        registry->setStringValue("/tmp/Test4", data);
        registry->createKey("/tmp/Test4/Test4", perms);
        registry->setStringValue("/tmp/Test4/Test4", data);
        registry->createKey("/tmp/Test4/Test4/Test4", perms);
        registry->setStringValue("/tmp/Test4/Test4/Test4", data);
        registry->createKey("/tmp/Test4/Test4/Test4/Test4", perms);
        registry->setStringValue("/tmp/Test4/Test4/Test4/Test4", data);

        registry->createKey("/user/Test4", perms);
        registry->setStringValue("/user/Test4", data);
        registry->createKey("/user/Test4/Test4", perms);
        registry->setStringValue("/user/Test4/Test4", data);
        registry->createKey("/user/Test4/Test4/Test4", perms);
        registry->setStringValue("/user/Test4/Test4/Test4", data);
        registry->createKey("/user/Test4/Test4/Test4/Test4", perms);
        registry->setStringValue("/user/Test4/Test4/Test4/Test4", data);

        registry->createKey("/host/Test4", perms);
        registry->setStringValue("/host/Test4", data);
        registry->createKey("/host/Test4/Test4", perms);
        registry->setStringValue("/host/Test4/Test4", data);
        registry->createKey("/host/Test4/Test4/Test4", perms);
        registry->setStringValue("/host/Test4/Test4/Test4", data);
        registry->createKey("/host/Test4/Test4/Test4/Test4", perms);
        registry->setStringValue("/host/Test4/Test4/Test4/Test4", data);

        registry->createKey("/system/Test4", perms);
        registry->setStringValue("/system/Test4", data);
        registry->createKey("/system/Test4/Test4", perms);
        registry->setStringValue("/system/Test4/Test4", data);
        registry->createKey("/system/Test4/Test4/Test4", perms);
        registry->setStringValue("/system/Test4/Test4/Test4", data);
        registry->createKey("/system/Test4/Test4/Test4/Test4", perms);
        registry->setStringValue("/system/Test4/Test4/Test4/Test4", data);

        // Dump a current view of our database
        dumpDatabaseTreeXML("/");

        registry->deleteKey("/tmp/Test4/Test4/Test4/Test4");
        registry->deleteKey("/tmp/Test4/Test4/Test4");
        registry->deleteKey("/tmp/Test4/Test4");
        registry->deleteKey("/tmp/Test4");

        registry->deleteKey("/user/Test4/Test4/Test4/Test4");
        registry->deleteKey("/user/Test4/Test4/Test4");
        registry->deleteKey("/user/Test4/Test4");
        registry->deleteKey("/user/Test4");

        registry->deleteKey("/host/Test4/Test4/Test4/Test4");
        registry->deleteKey("/host/Test4/Test4/Test4");
        registry->deleteKey("/host/Test4/Test4");
        registry->deleteKey("/host/Test4");

        registry->deleteKey("/system/Test4/Test4/Test4/Test4");
        registry->deleteKey("/system/Test4/Test4/Test4");
        registry->deleteKey("/system/Test4/Test4");
        registry->deleteKey("/system/Test4");
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
