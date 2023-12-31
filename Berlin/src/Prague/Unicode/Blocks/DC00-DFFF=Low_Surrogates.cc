/*
 * Plugin for Unicode Block "Low Surrogates"
 * (DC00-DFFF)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Low_Surrogates : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Low_Surrogates() {
      my_first_letter=0xDC00;
      my_last_letter =0xDFFF;
      // my_version="3.0"
    }
      
    ~Low_Surrogates() {
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
      return "Low Surrogates";
    }
    
    Gen_Cat category(const _Char _uc) const {
      return Unicode::CAT_Cs;
    }
    
    Can_Comb_Class combClass(const _Char _uc) const {
      return Unicode::Can_Comb_Class(0);
    }
    
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
      return Unicode::EA_WIDTH_N;
    }

    Line_Break linebreak(const _Char _uc) const {
      return Unicode::LB_SG;
    }
    
  private:
    // Functions
    Low_Surrogates(const Low_Surrogates &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;
  }; // Low_Surrogates

}; // namespace Unicode

dload(Unicode::Low_Surrogates);
  
