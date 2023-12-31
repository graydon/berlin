/*
 * Plugin for Unicode Block "CJK Unified Ideographs"
 * (4E00-9FA5)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class CJK_Unified_Ideographs : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    CJK_Unified_Ideographs() {
      my_first_letter=0x4E00;
      my_last_letter =0x9FFF;
      // my_version="3.0"
    }
      
    ~CJK_Unified_Ideographs() {
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
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return _uc;
    }
    
    _Char lowercase(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return _uc;
    }
    
    _Char titlecase(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return _uc;
    }
    
    float numericValue(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     
    
    int decDigitValue(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }
    
    int digitValue(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }
    
    string blockname(const _Char _uc) const {
      return "CJK Unified Ideographs";
    }
    
    Gen_Cat category(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::CAT_Lo;
    }
    
    Can_Comb_Class combClass(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::Can_Comb_Class(0);
    }
    
    Bidir_Props bidirProps(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::BIDIR_L;
    }
    
    Char_Decomp decompType(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::DECOMP_NO_DECOMP;
    }

  
    String decompString(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::String(_uc);
    }

    bool mustMirror(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::EA_WIDTH_W;
    }

    Line_Break linebreak(const _Char _uc) const {
      if (_uc > 0x9FA5)
	throw (Unicode::UndefinedProperty(_uc, Unicode::PROP_CHARACTER));

      return Unicode::LB_ID;
    }
    
  private:
    // Functions
    CJK_Unified_Ideographs(const CJK_Unified_Ideographs &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;
  }; // class CJK_Unified_Ideographs

}; // namespace Unicode

dload(Unicode::CJK_Unified_Ideographs);
  
