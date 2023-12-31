#include <Babylon/Babylon.hh>
#include <iostream.h>
#include <iomanip.h>

using namespace Babylon;

void output_char(ostream & out, Char a) {
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

Char skip_to(Char a) {
    if (a.value() >= 0x3400 && a.value() < 0x4DB5)
	return Char(0x4DB5 - 1);
    if (a.value() >= 0x4E00 && a.value() < 0x9FA5)
	return Char(0x9FA5 - 1);
    if (a.value() >= 0xAC00 && a.value() < 0xD7A3)
	return Char(0xD7A3 - 1);
    if (a.value() >= 0xD800 && a.value() < 0xDB7F)
	return Char(0xDB7F - 1);
    if (a.value() >= 0xDB80 && a.value() < 0xDBFF)
	return Char(0xDBFF - 1);
    if (a.value() >= 0xDC00 && a.value() < 0xDFFF)
	return Char(0xDFFF - 1);
    if (a.value() >= 0xE000 && a.value() < 0xF8FF)
	return Char(0xF8FF - 1);
    if (a.value() >= 0xF0000 && a.value() < 0xFFFFD)
	return Char(0xFFFFD - 1);
//    if (a.value() >= 0x100000 && a.value() < 0x10FFFD)
//	return Char(0x10FFFD - 1);

    return a;
}

int main (int argc, char **argv) {
    Char A(UC_NULL);
    A.is_defined();

    cout.setf(ios::uppercase);

    try {
	for (A = UCS4(0x100000); A < UCS4(0x10FFFF); A++) {
	    if (A.is_defined()) {
		// code value
		output_char(cout, A); cout << ";";
		// character name

		// Gen_Cat
		switch(A.category()) {
		case CAT_Lu: cout << "Lu;"; break;
		case CAT_Ll: cout << "Ll;"; break;
		case CAT_Lt: cout << "Lt;"; break;
		case CAT_Mn: cout << "Mn;"; break;
		case CAT_Mc: cout << "Mc;"; break;
		case CAT_Me: cout << "Me;"; break;
		case CAT_Nd: cout << "Nd;"; break;
		case CAT_Nl: cout << "Nl;"; break;
		case CAT_No: cout << "No;"; break;
		case CAT_Zs: cout << "Zs;"; break;
		case CAT_Zl: cout << "Zl;"; break;
		case CAT_Zp: cout << "Zp;"; break;
		case CAT_Cc: cout << "Cc;"; break;
		case CAT_Cf: cout << "Cf;"; break;
		case CAT_Cs: cout << "Cs;"; break;
		case CAT_Co: cout << "Co;"; break;
		case CAT_Cn: cout << "Cn;"; break;
		case CAT_Lm: cout << "Lm;"; break;
		case CAT_Lo: cout << "Lo;"; break;
		case CAT_Pc: cout << "Pc;"; break;
		case CAT_Pd: cout << "Pd;"; break;
		case CAT_Ps: cout << "Ps;"; break;
		case CAT_Pe: cout << "Pe;"; break;
		case CAT_Pi: cout << "Pi;"; break;
		case CAT_Pf: cout << "Pf;"; break;
		case CAT_Po: cout << "Po;"; break;
		case CAT_Sm: cout << "Sm;"; break;
		case CAT_Sc: cout << "Sc;"; break;
		case CAT_Sk: cout << "Sk;"; break;
		case CAT_So: cout << "So;"; break;
		default:     cout << "UNDEFINED;";
		}
		// comp_class
		cout << dec << A.comb_class() << ";";
		// Bidir_Props
		switch(A.direction()) {
		case BIDIR_L:   cout << "L;"; break;
		case BIDIR_LRE: cout << "LRE;"; break;
		case BIDIR_LRO: cout << "LRO;"; break;
		case BIDIR_R:   cout << "R;"; break;
		case BIDIR_AL:  cout << "AL;"; break;
		case BIDIR_RLE: cout << "RLE;"; break;
		case BIDIR_RLO: cout << "RLO;"; break;
		case BIDIR_PDF: cout << "PDF;"; break;
		case BIDIR_EN:  cout << "EN;"; break;
		case BIDIR_ES:  cout << "ES;"; break;
		case BIDIR_ET:  cout << "ET;"; break;
		case BIDIR_AN:  cout << "AN;"; break;
		case BIDIR_CS:  cout << "CS;"; break;
		case BIDIR_NSM: cout << "NSM;"; break;
		case BIDIR_BN:  cout << "BN;"; break;
		case BIDIR_B:   cout << "B;"; break;
		case BIDIR_S:   cout << "S;"; break;
		case BIDIR_WS:  cout << "WS;"; break;
		case BIDIR_ON:  cout << "ON;"; break;
		default:        cout << "UNDEFINED;";
		}
		// Char_Decomp_Type
		if (A >= 0xAC00 && A <= 0xD7A3) {
		    // The values in this area are
		    // determined algorithmically
		    // and not included in the file
		    cout << ";";
		} else {
		    bool needSpace = 1;
		    switch(A.decomp_type()) {
		    case DECOMP_CANONICAL: cout << ""; needSpace = 0; break;
		    case DECOMP_FONT:      cout << "<font>"; break;
		    case DECOMP_NOBREAK:   cout << "<noBreak>"; break;
		    case DECOMP_INITIAL:   cout << "<initial>"; break;
		    case DECOMP_MEDIAL:    cout << "<medial>"; break;
		    case DECOMP_FINAL:     cout << "<final>"; break;
		    case DECOMP_ISOLATED:  cout << "<isolated>"; break;
		    case DECOMP_CIRCLE:    cout << "<circle>"; break;
		    case DECOMP_SUPER:     cout << "<super>"; break;
		    case DECOMP_SUB:       cout << "<sub>"; break;
		    case DECOMP_VERTICAL:  cout << "<vertical>"; break;
		    case DECOMP_WIDE:      cout << "<wide>"; break;
		    case DECOMP_NARROW:    cout << "<narrow>"; break;
		    case DECOMP_SMALL:     cout << "<small>"; break;
		    case DECOMP_SQUARE:    cout << "<square>"; break;
		    case DECOMP_FRACTION:  cout << "<fraction>"; break;
		    case DECOMP_COMPAT:    cout << "<compat>"; break;
		    default:               cout << "UNDEFINED";
		    }
		    // Decomp String
		    {
			Babylon::String us;
			us = A.decompose();
			if (us[0] == A)
			cout << ";" << flush;
			else {
			    if (needSpace) cout << " " << flush;
			    for (unsigned long i = 0; i < us.length(); i++) {
				output_char(cout, us[i]);
				
				if ( i == ( us.length() - 1 ) )
				    cout << ";";
				else
				    cout << " ";
			    }
			}
		    }
		}
		// Dec_Digit_Value
		try {
		    cout << A.dec_digit_value();
		} catch (Undefined_Property & p) {}
		cout << ";";
		// Digit_Value
		try {
		    cout << A.digit_value();
		} catch (Undefined_Property & p) {}
		cout << ";";
		// Numeric_Value
		try {
		    cout << A.numeric_value();
		} catch (Undefined_Property & p) {}
		cout << ";";
		// Is_Mirrored
		if (A.must_mirror()) cout << "Y;";
		else cout << "N;";
		// Comment and Unicode 1.0 name

		// Uppercase
		if (A.uppercase() != A)
		    output_char(cout, A.uppercase());
		cout << ";";
		// Lowercase
		if (A.lowercase() != A)
		    output_char(cout, A.lowercase());
		cout << ";";
		// Titlecase
		if (A.titlecase() != A)
		    output_char(cout, A.titlecase());

		cout << endl;
	    }
	    A = skip_to(A);
	} // for
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
