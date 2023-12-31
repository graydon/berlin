/*
 * Plugin for Unicode Block "Combining Diacritical Marks"
 * (0300-036F)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Combining_Diacritical_Marks : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Combining_Diacritical_Marks() {
      my_first_letter=0x0300;
      my_last_letter =0x036F;
      // my_version="3.0"
    }
      
    ~Combining_Diacritical_Marks() {
    }

    _Char firstLetter() {
      return my_first_letter;
    }

    _Char lastLetter() {
      return my_last_letter;
    }

    // Query Functions:
    _Char unicodeValue(const _Char _uc) const {
      return _uc;
    }

    _Char uppercase(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 
      
      switch(_uc) {
      case 0x0345:
	return Unicode::_Char(0x0399);
      default:
	return _uc;
      }
    }

    _Char lowercase(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return _uc;
    }

    _Char titlecase(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      switch(_uc) {
      case 0x0345:
	return Unicode::_Char(0x0399);
      default:
	return _uc;
      }
    }

    float numericValue(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     

    int decDigitValue(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }

    int digitValue(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }

    string blockname(const _Char _uc) const {
      return "Combning Diacritical Marks";
    }

    Gen_Cat category(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
	
      return Unicode::CAT_Mn;
    }

    Can_Comb_Class combClass(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Can_Comb_Class(Combining_Diacritical_Marks::
				     comb_class[_uc - my_first_letter]);
    }

    Bidir_Props bidirProps(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::BIDIR_NSM;
    }

    Char_Decomp decompType(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::DECOMP_NO_DECOMP;
    }

    String decompString(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      switch(_uc) {
      case 0x0340:
	return Unicode::String(0x0300);
      case 0x0341:
	return Unicode::String(0x0301);
      case 0x0343:
	return Unicode::String(0x0313);
      case 0x0344: {
	Unicode::String us;
	us.resize(2);
	us[0] = 0x0308;
	us[1] = 0x0301;
	return us;
      }
      default:
	return Unicode::String(_uc);
      }
    }

    bool mustMirror(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return Unicode::EA_WIDTH_A;
    }

    Line_Break linebreak(const _Char _uc) const {
      if ( (_uc > 0x034E && _uc < 0x0360) || _uc > 0x0362)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::LB_CM;
    }
    
  private:
    // Functions
    Combining_Diacritical_Marks(const Combining_Diacritical_Marks &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const unsigned char comb_class[99];
  }; // class Combining_Diacritical_Marks

  /*
  const unsigned char Combining_Diacritical_Marks::comb_class[] = {
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
    16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
    32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
    48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
    64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
    80,81,82,83,
    84,85,86,87,88,89,90,91,92,93,94,95,
    96,97,98
  };
  */
  const unsigned char Combining_Diacritical_Marks::comb_class[] = {
    230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 230, 
    230, 230, 230, 230, 230, 232, 220, 220, 220, 220, 232, 216, 220, 220, 220, 220, 
    220, 202, 202, 220, 220, 220, 220, 202, 202, 220, 220, 220, 220, 220, 220, 220, 
    220, 220, 220, 220,   1,   1,   1,   1,   1, 220, 220, 220, 220, 230, 230, 230,
    230, 230, 230, 230, 230, 240, 230, 220, 220, 220, 230, 230, 230, 220, 220,

    // spaceholder: these characters are not defined
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,
  
    234, 234, 233
  };

}; // namespace Unicode

dload(Unicode::Combining_Diacritical_Marks);
  
