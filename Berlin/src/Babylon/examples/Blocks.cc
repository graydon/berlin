#include <Babylon/Babylon.hh>
#include <iostream.h>
#include <iomanip.h>

using namespace Babylon;

void output_char(ostream & out, Char a) {
    cout.setf(ios::uppercase);
    if (a <= 0xFFFF)
	out << hex << setfill('0') << setw(4) << a.value();
    else if (a <= 0xFFFFF)
	out << hex << setw(5) << a.value();
    else if (a <= 0xFFFFFF)
	out << hex << setw(6) << a.value();
    else if (a <= 0xFFFFFFF)
	out << hex << setw(7) << a.value();
    else
	out << hex << setw(8) << a.value();
}

int main (int argc, char **argv) {
    Char A(UC_NULL);
    UCS4 start;
    UCS4 end;
    string name;

    try {
	cout << "# Start Code; End Code; Block Name" << endl;

	while (A < UCS4(UC_MAX_DEFINED)) {
	    start = Dictionary::instance()->first_letter_of_block(A.value());
	    end   = Dictionary::instance()->last_letter_of_block(A.value());
	    name  = A.blockname();
	    
	    output_char(cout, start); cout << "; ";
	    output_char(cout, end); cout << "; " << name << endl;
	    
	    A = Dictionary::instance()->start_of_next_block(A.value());
	} // while
    } // try
    catch (const Block_Error &Except) {
	cerr << "ERROR: Block_Error -> EXITING (" 
	     << Except.what() << ")" << endl;
	exit(1);
    } // catch
    catch (const Undefined_Property &Except) {
	cerr << "ERROR: Undefined_Property -> EXITING ("
	     << Except.what() << ")" << endl;
	exit(2);
    }
    catch (const std::exception & Except) {
	cerr << "ERROR: Something unexspected happened! -> EXITING ("
	     << Except.what() << ")" << endl; 
	exit(3);
    }
}
