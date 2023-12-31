/*
 * Plugin for Unicode Block "CJK Unified Ideographs Extension A"
 * (3400-4DB5)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class CJK_Unified_Ideographs_Extension_A : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    CJK_Unified_Ideographs_Extension_A() {
      my_first_letter=0x3400;
      my_last_letter =0x4DB5;
      // my_version="3.0"
    }
      
    ~CJK_Unified_Ideographs_Extension_A() {
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
      return _uc;
    }
    
    _Char lowercase(const _Char _uc) const {
      return _uc;
    }
    
    _Char titlecase(const _Char _uc) const {
      return _uc;
    }
    
    float numericValue(const _Char _uc) const {
      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     
    
    int decDigitValue(const _Char _uc) const {
      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }
    
    int digitValue(const _Char _uc) const {
      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }
    
    string blockname(const _Char _uc) const {
      return "CJK Unified Ideographs Extension A";
    }
    
    Gen_Cat category(const _Char _uc) const {
      return Unicode::CAT_Lo;
    }
    
    Can_Comb_Class combClass(const _Char _uc) const {
      return Unicode::Can_Comb_Class(0);
    }
    
    // Allways BIDIR_L with "Latin Extended-B"
    Bidir_Props bidirProps(const _Char _uc) const {
      return Unicode::BIDIR_L;
    }
    
    Char_Decomp decompType(const _Char _uc) const {
      return Unicode::DECOMP_NO_DECOMP;
    }

  
    String decompString(const _Char _uc) const {
      Unicode::String us(_uc);
      return us;
    }

    bool mustMirror(const _Char _uc) const {
      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      return Unicode::EA_WIDTH_W;
    }

    Line_Break linebreak(const _Char _uc) const {
      return Unicode::LB_ID;
    }
    
  private:
    // Functions
    CJK_Unified_Ideographs_Extension_A (const
					CJK_Unified_Ideographs_Extension_A &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;
  }; // class CJK_Unified_Ideographs_Extension_A

}; // namespace Unicode

dload(Unicode::CJK_Unified_Ideographs_Extension_A);
  
