#include <Prague/Unicode/Unicode.hh>
#include <iostream.h>
#include <iomanip.h>

using namespace Unicode;

int main (int argc, char **argv) {
  int chars = 0;
  Unicode::Char A;
  bool runOverZero = 1;

  cout.setf(ios::uppercase);
  
  for (A = NULL_UNICODE; A > _Char(0x0000) || runOverZero; A++) {
    runOverZero = 0;
    // skip over identical blocks...
    if (A > 0x3400 && A < 0x4DB5) { // CJK Unified Ideographs Extension A
      chars++; continue;
    }
    if (A > 0x4E00 && A < 0x9FA5) { // CJK Unified Ideographs
      chars++; continue;
    }
    if (A > 0xAC00 && A < 0xD7A3) { // Hangul Syllables
      chars++; continue;
    }
    if (A > 0xD800 && A < 0xDB7F) { // Non-Privat High Surrogates
      chars++; continue;
    }
    if (A > 0xDB80 && A < 0xDBFF) { // Private High Surrogates
      chars++; continue;
    }
    if (A > 0xDC00 && A < 0xDFFF) { // Low Surrogates
      chars++; continue;
    }
    if (A > 0xE000 && A < 0xF8FF) { // Private Use Area
      chars++; continue;
    }

    try {
      A.category(); // breaks the loop for undefined chars
      chars++;
      // code value
      cout << hex << setfill('0') << setw(4) << A.myUnicode()<< ";" << flush;
      // Gen_Cat
      switch(A.category()) {
      case CAT_Lu: cout << "Lu;" << flush; break;
      case CAT_Ll: cout << "Ll;" << flush; break;
      case CAT_Lt: cout << "Lt;" << flush; break;
      case CAT_Mn: cout << "Mn;" << flush; break;
      case CAT_Mc: cout << "Mc;" << flush; break;
      case CAT_Me: cout << "Me;" << flush; break;
      case CAT_Nd: cout << "Nd;" << flush; break;
      case CAT_Nl: cout << "Nl;" << flush; break;
      case CAT_No: cout << "No;" << flush; break;
      case CAT_Zs: cout << "Zs;" << flush; break;
      case CAT_Zl: cout << "Zl;" << flush; break;
      case CAT_Zp: cout << "Zl;" << flush; break;
      case CAT_Cc: cout << "Cc;" << flush; break;
      case CAT_Cf: cout << "Cf;" << flush; break;
      case CAT_Cs: cout << "Cs;" << flush; break;
      case CAT_Co: cout << "Co;" << flush; break;
      case CAT_Cn: cout << "Cn;" << flush; break;
      case CAT_Lm: cout << "Lm;" << flush; break;
      case CAT_Lo: cout << "Lo;" << flush; break;
      case CAT_Pc: cout << "Pc;" << flush; break;
      case CAT_Pd: cout << "Pd;" << flush; break;
      case CAT_Ps: cout << "Ps;" << flush; break;
      case CAT_Pe: cout << "Pe;" << flush; break;
      case CAT_Pi: cout << "Pi;" << flush; break;
      case CAT_Pf: cout << "Pf;" << flush; break;
      case CAT_Po: cout << "Po;" << flush; break;
      case CAT_Sm: cout << "Sm;" << flush; break;
      case CAT_Sc: cout << "Sc;" << flush; break;
      case CAT_Sk: cout << "Sk;" << flush; break;
      case CAT_So: cout << "So;" << flush; break;
      default:     cout << "UNDEFINED;" << flush;
      }
      // comp_class
      cout << dec << A.combClass() << ";" << flush;
      // Bidir_Props
      switch(A.direction()) {
      case BIDIR_L:   cout << "L;" << flush; break;
      case BIDIR_LRE: cout << "LRE;" << flush; break;
      case BIDIR_LRO: cout << "LRO;" << flush; break;
      case BIDIR_R:   cout << "R;" << flush; break;
      case BIDIR_AL:  cout << "AL;" << flush; break;
      case BIDIR_RLE: cout << "RLE;" << flush; break;
      case BIDIR_RLO: cout << "RLO;" << flush; break;
      case BIDIR_PDF: cout << "PDF;" << flush; break;
      case BIDIR_EN:  cout << "EN;" << flush; break;
      case BIDIR_ES:  cout << "ES;" << flush; break;
      case BIDIR_ET:  cout << "ET;" << flush; break;
      case BIDIR_AN:  cout << "AN;" << flush; break;
      case BIDIR_CS:  cout << "CS;" << flush; break;
      case BIDIR_NSM: cout << "NSM;" << flush; break;
      case BIDIR_BN:  cout << "BN;" << flush; break;
      case BIDIR_B:   cout << "B;" << flush; break;
      case BIDIR_S:   cout << "S;" << flush; break;
      case BIDIR_WS:  cout << "WS;" << flush; break;
      case BIDIR_ON:  cout << "ON;" << flush; break;
      default:        cout << "UNDEFINED;" << flush;
      }
      // Char_Decomp_Type
      bool needSpace = 1;
      switch(A.decompType()) {
      case DECOMP_NO_DECOMP: cout << "" << flush; needSpace = 0; break;
      case DECOMP_FONT:      cout << "<font>" << flush; break;
      case DECOMP_NOBREAK:   cout << "<noBreak>" << flush; break;
      case DECOMP_INITIAL:   cout << "<initial>" << flush; break;
      case DECOMP_MEDIAL:    cout << "<medial>" << flush; break;
      case DECOMP_FINAL:     cout << "<final>" << flush; break;
      case DECOMP_ISOLATED:  cout << "<isolated>" << flush; break;
      case DECOMP_CIRCLE:    cout << "<circle>" << flush; break;
      case DECOMP_SUPER:     cout << "<super>" << flush; break;
      case DECOMP_SUB:       cout << "<sub>" << flush; break;
      case DECOMP_VERTICAL:  cout << "<vertical>" << flush; break;
      case DECOMP_WIDE:      cout << "<wide>" << flush; break;
      case DECOMP_NARROW:    cout << "<narrow>" << flush; break;
      case DECOMP_SMALL:     cout << "<small>" << flush; break;
      case DECOMP_SQUARE:    cout << "<square>" << flush; break;
      case DECOMP_FRACTION:  cout << "<fraction>" << flush; break;
      case DECOMP_COMPAT:    cout << "<compat>" << flush; break;
      default:               cout << "UNDEFINED" << flush;
      }
      // Decomp String
      Unicode::String us;
      us = A.decompString();
      if (us[0] == A)
	cout << ";" << flush;
      else {
	if (needSpace) cout << " " << flush;
	for (unsigned long i = 0; i < us.length(); i++) {
	  cout << hex << setfill('0') << setw(4) << us[i].myUnicode() << flush;
	  if ( i == ( us.length()-1 ) )
	    cout << ";" << flush;
	  else
	    cout << " " << flush;
	}
      }
      // Dec_Digit_Value
      try {
	cout << A.decDigitValue() << ";" << flush;
      }
      catch(Unicode::UndefinedProperty &Except) {
	cout << ";" << flush;
      }
      // Digit_Value
      try {
	cout << A.digitValue() << ";" << flush;
      }
      catch(Unicode::UndefinedProperty &Except) {
	cout << ";" << flush;
      }    
      // Numeric_Value
      try {
	cout << A.numericValue() << ";" << flush;
      }
      catch(Unicode::UndefinedProperty &Except) {
	cout << ";" << flush;
      }
      // Is_Mirrored
      if (A.mustMirror()) cout << "Y;" << flush;
      else cout << "N;" << flush;
      // Uppercase
      if ((A.uppercase()).myUnicode() != A.myUnicode())
	cout << hex << setfill('0') << setw(4)
	     << (A.uppercase()).myUnicode() << flush;
      cout << ";" << flush;
      // Lowercase
      if ((A.lowercase()).myUnicode() != A.myUnicode())
	cout << hex << setfill('0') << setw(4)
	     << (A.lowercase()).myUnicode() << flush;
      cout << ";" << flush;
      // Titlecase
      if ((A.titlecase()).myUnicode() != A.myUnicode())
	cout << hex << setfill('0') << setw(4)
	     << (A.titlecase()).myUnicode() << flush;
      cout << ";" << flush;
      // EA_Width
      switch(A.EAWidth()) {
      case EA_WIDTH_W:  cout << "EA_WIDTH_W;" << flush; break;
      case EA_WIDTH_F:  cout << "EA_WIDTH_F;" << flush; break;
      case EA_WIDTH_Na: cout << "EA_WIDTH_Na;" << flush; break;
      case EA_WIDTH_H:  cout << "EA_WIDTH_H;" << flush; break;
      case EA_WIDTH_A:  cout << "EA_WIDTH_A;" << flush; break;
      case EA_WIDTH_N:  cout << "EA_WIDTH_N;" << flush; break;
      default:          cout << "UNDEFINED;" << flush;
      }
      // Line_Break
      switch(A.linebreak()) {
      case LB_BK: cout << "LB_BK" << flush; break;
      case LB_CR: cout << "LB_CR" << flush; break;
      case LB_LF: cout << "LB_LF" << flush; break;
      case LB_CM: cout << "LB_CM" << flush; break;
      case LB_SG: cout << "LB_SG" << flush; break;
      case LB_GL: cout << "LB_GL" << flush; break;
      case LB_CB: cout << "LB_CB" << flush; break;
      case LB_SP: cout << "LB_SP" << flush; break;
      case LB_ZW: cout << "LB_ZW" << flush; break;
      case LB_XX: cout << "LB_XX" << flush; break;
      case LB_OP: cout << "LB_OP" << flush; break;
      case LB_CL: cout << "LB_CL" << flush; break;
      case LB_QU: cout << "LB_QU" << flush; break;
      case LB_NS: cout << "LB_NS" << flush; break;
      case LB_EX: cout << "LB_EX" << flush; break;
      case LB_SY: cout << "LB_SY" << flush; break;
      case LB_IS: cout << "LB_IS" << flush; break;
      case LB_PR: cout << "LB_PR" << flush; break;
      case LB_PO: cout << "LB_PO" << flush; break;
      case LB_NU: cout << "LB_NU" << flush; break;
      case LB_AL: cout << "LB_AL" << flush; break;
      case LB_ID: cout << "LB_ID" << flush; break;
      case LB_IN: cout << "LB_IN" << flush; break;
      case LB_HY: cout << "LB_HY" << flush; break;
      case LB_BB: cout << "LB_BB" << flush; break;
      case LB_BA: cout << "LB_BA" << flush; break;
      case LB_SA: cout << "LB_SA" << flush; break;
      case LB_AI: cout << "LB_AI" << flush; break;
      case LB_B2: cout << "LB_B2" << flush; break;
      default:    cout << "UNDEFINED" << flush;
      }
      cout << endl << flush;
    }
    catch (Unicode::FileError &Except) {
      cout << "ERROR: "
	   << Except.ErrorFilename << ": "
	   << Except.ErrorMessage << "\n" << flush;
    }
    catch (Unicode::UndefinedProperty &Except) {}
    catch (Unicode::BlockError &Except) {}
  }

  cerr << "Number of defined and supported characters: " << chars << endl;
}
