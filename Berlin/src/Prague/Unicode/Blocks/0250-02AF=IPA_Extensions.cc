/*
 * Plugin for Unicode Block "IPA Extensions"
 * (0250-02AF)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class IPA_Extensions : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    IPA_Extensions() {
      my_first_letter=0x0250;
      my_last_letter =0x02AF;
      // my_version="3.0"
    }
      
    ~IPA_Extensions() {
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
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return IPA_Extensions::upper[_uc - my_first_letter];
    }

    _Char lowercase(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return IPA_Extensions::lower[_uc - my_first_letter];
    }

    _Char titlecase(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      return IPA_Extensions::title[_uc - my_first_letter];
    }

    float numericValue(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     

    int decDigitValue(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }

    int digitValue(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }

    string blockname(const _Char _uc) const {
      return "IPA Extensions";
    }

    Gen_Cat category(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
	
      return Unicode::CAT_Ll;
    }

    Can_Comb_Class combClass(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Can_Comb_Class(0);
    }

    Bidir_Props bidirProps(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::BIDIR_L;
    }

    Char_Decomp decompType(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::DECOMP_NO_DECOMP;
    }

    String decompString(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::String(_uc);
    }

    bool mustMirror(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 
      
      switch(_uc) {
      case 0x0251:
      case 0x0261:
	return Unicode::EA_WIDTH_A;
      default:
	return Unicode::EA_WIDTH_N;
      }
    }

    Line_Break linebreak(const _Char _uc) const {
      if (_uc > 0x02AD)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      switch(_uc) {
      case 0x0251:
      case 0x0261:
	return Unicode::LB_AI;
      default:
	return Unicode::LB_AL;
      }
    }
    
  private:
    // Functions
    IPA_Extensions(const IPA_Extensions &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const Unicode::_Char upper[94];
    static const Unicode::_Char lower[94];
    static const Unicode::_Char title[94];
  }; // class IPA_Extensions

  const Unicode::_Char IPA_Extensions::upper[] = {
    0x0250, 0x0251, 0x0252, 0x0181, 0x0186, 0x0255, 0x0189, 0x018A, 
    0x0258, 0x018F, 0x025A, 0x0190, 0x025C, 0x025D, 0x025E, 0x025F, 
    0x0193, 0x0261, 0x0262, 0x0194, 0x0264, 0x0265, 0x0266, 0x0267, 
    0x0197, 0x0196, 0x026A, 0x026B, 0x026C, 0x026D, 0x026E, 0x019C, 
    0x0270, 0x0271, 0x019D, 0x0273, 0x0274, 0x019F, 0x0276, 0x0277, 
    0x0278, 0x0279, 0x027A, 0x027B, 0x027C, 0x027D, 0x027E, 0x027F, 
    0x01A6, 0x0281, 0x0282, 0x01A9, 0x0284, 0x0285, 0x0286, 0x0287, 
    0x01AE, 0x0289, 0x01B1, 0x01B2, 0x028C, 0x028D, 0x028E, 0x028F, 
    0x0290, 0x0291, 0x01B7, 0x0293, 0x0294, 0x0295, 0x0296, 0x0297, 
    0x0298, 0x0299, 0x029A, 0x029B, 0x029C, 0x029D, 0x029E, 0x029F, 
    0x02A0, 0x02A1, 0x02A2, 0x02A3, 0x02A4, 0x02A5, 0x02A6, 0x02A7, 
    0x02A8, 0x02A9, 0x02AA, 0x02AB, 0x02AC, 0x02AD
  };

   const Unicode::_Char IPA_Extensions::lower[] = {
     0x0250, 0x0251, 0x0252, 0x0253, 0x0254, 0x0255, 0x0256, 0x0257, 
     0x0258, 0x0259, 0x025A, 0x025B, 0x025C, 0x025D, 0x025E, 0x025F, 
     0x0260, 0x0261, 0x0262, 0x0263, 0x0264, 0x0265, 0x0266, 0x0267, 
     0x0268, 0x0269, 0x026A, 0x026B, 0x026C, 0x026D, 0x026E, 0x026F, 
     0x0270, 0x0271, 0x0272, 0x0273, 0x0274, 0x0275, 0x0276, 0x0277, 
     0x0278, 0x0279, 0x027A, 0x027B, 0x027C, 0x027D, 0x027E, 0x027F, 
     0x0280, 0x0281, 0x0282, 0x0283, 0x0284, 0x0285, 0x0286, 0x0287, 
     0x0288, 0x0289, 0x028A, 0x028B, 0x028C, 0x028D, 0x028E, 0x028F, 
     0x0290, 0x0291, 0x0292, 0x0293, 0x0294, 0x0295, 0x0296, 0x0297, 
     0x0298, 0x0299, 0x029A, 0x029B, 0x029C, 0x029D, 0x029E, 0x029F, 
     0x02A0, 0x02A1, 0x02A2, 0x02A3, 0x02A4, 0x02A5, 0x02A6, 0x02A7, 
     0x02A8, 0x02A9, 0x02AA, 0x02AB, 0x02AC, 0x02AD
  };

  const Unicode::_Char IPA_Extensions::title[] = {
    0x0250, 0x0251, 0x0252, 0x0181, 0x0186, 0x0255, 0x0189, 0x018A, 
    0x0258, 0x018F, 0x025A, 0x0190, 0x025C, 0x025D, 0x025E, 0x025F, 
    0x0193, 0x0261, 0x0262, 0x0194, 0x0264, 0x0265, 0x0266, 0x0267, 
    0x0197, 0x0196, 0x026A, 0x026B, 0x026C, 0x026D, 0x026E, 0x019C, 
    0x0270, 0x0271, 0x019D, 0x0273, 0x0274, 0x019F, 0x0276, 0x0277, 
    0x0278, 0x0279, 0x027A, 0x027B, 0x027C, 0x027D, 0x027E, 0x027F, 
    0x01A6, 0x0281, 0x0282, 0x01A9, 0x0284, 0x0285, 0x0286, 0x0287, 
    0x01AE, 0x0289, 0x01B1, 0x01B2, 0x028C, 0x028D, 0x028E, 0x028F, 
    0x0290, 0x0291, 0x01B7, 0x0293, 0x0294, 0x0295, 0x0296, 0x0297, 
    0x0298, 0x0299, 0x029A, 0x029B, 0x029C, 0x029D, 0x029E, 0x029F, 
    0x02A0, 0x02A1, 0x02A2, 0x02A3, 0x02A4, 0x02A5, 0x02A6, 0x02A7, 
    0x02A8, 0x02A9, 0x02AA, 0x02AB, 0x02AC, 0x02AD
  };

}; // namespace Unicode

dload(Unicode::IPA_Extensions);
  
