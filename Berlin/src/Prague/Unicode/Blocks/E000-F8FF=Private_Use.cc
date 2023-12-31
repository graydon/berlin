/*
 * Plugin for Unicode Block "Private Use"
 * (DC00-DFFF)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Private_Use : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Private_Use() {
      my_first_letter=0xE000;
      my_last_letter =0xF8FF;
      // my_version="3.0"
    }
      
    ~Private_Use() {
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
      return "Private Use";
    }
    
    Gen_Cat category(const _Char _uc) const {
      return Unicode::CAT_Co;
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
      return Unicode::EA_WIDTH_A;
    }

    Line_Break linebreak(const _Char _uc) const {
      return Unicode::LB_AI;
    }
    
  private:
    // Functions
    Private_Use(const Private_Use &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;
  }; // class Private_Use

}; // namespace Unicode

dload(Unicode::Private_Use);
  
