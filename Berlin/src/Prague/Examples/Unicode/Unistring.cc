#include <Prague/Unicode/Unicode.hh>
#include <iostream.h>
#include <iomanip.h>
#include <Warsaw/Types.hh>

using namespace Unicode;

int main (int argc, char **argv) {
  Unicode::String prague("Test!");
  Unistring corba; 

  Unicode::_Char _uc=0x0000;

  Unicode::Char uc1(_uc);
  Unicode::Char uc2;

  uc2=_uc;

  cout << "Char done" << endl << flush;

  Unicode::String s1(uc2);
  cout << "s1 done" << endl << flush;
  Unicode::String s2(_uc);
  cout << "s2 done" << endl << flush;

  cout << "Sizeof CORBA : " << sizeof(corba[0])  << endl;
  cout << "Sizeof Prague: " << sizeof(prague[0]) << endl << endl;

  Unistring *new_corba =
    new Unistring(prague.length(),
		  prague.length(),
		  const_cast<Unichar *>
		  (reinterpret_cast<const Unichar *>(prague.data())),
		  true);

  for (unsigned int i = 0; i < new_corba->length(); i++)
    cout << char( (*new_corba)[i] );
      
  cout << endl;
}
