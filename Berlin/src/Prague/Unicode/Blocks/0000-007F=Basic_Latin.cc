/*
 * Plugin for Unicode Block "Basic Latin"
 * (0000-007F)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Basic_Latin : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Basic_Latin() {
      my_first_letter=0x0000;
      my_last_letter=0x007F;
      // my_version="3.0"
    }
      
    ~Basic_Latin() {
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

    // No need to store this information. I can
    // calculate it on the fly.
    _Char uppercase(const _Char _uc) const {
      if (_uc < 0x0061 || _uc > 0x007A )
	return _uc;
      else
	return _uc - 0x0020;
    }

    // No need to store this information. I can
    // calculate it on the fly.
    _Char lowercase(const _Char _uc) const {
      if (_uc < 0x0041 || _uc > 0x005A )
	return _uc;
      else
	return _uc + 0x0020;      
    }

    // No need to store this information. I can
    // calculate it on the fly.
    _Char titlecase(const _Char _uc) const {
      if (_uc < 0x0061 || _uc > 0x007A )
	return _uc;
      else
	return _uc - 0x0020;     
    }

    // Digits are only between 0x0030 and 0x0039
    float numericValue(const _Char _uc) const {
      if (_uc < 0x0030 || _uc > 0x0039)
	throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
      else
	return float(_uc - 0x0030);
    }     

    // Digits are only between 0x0030 and 0x0039
    int decDigitValue(const _Char _uc) const {
      if (_uc < 0x0030 || _uc > 0x0039)
	throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
      else
	return int(_uc - 0x0030);    
    }

    // Digits are only between 0x0030 and 0x0039
    int digitValue(const _Char _uc) const {
      if (_uc < 0x0030 || _uc > 0x0039)
	throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
      else
	return int(_uc - 0x0030);
    }

    // Allways "Basic Latin" with Basic_Latin
    string blockname(const _Char _uc) const {
      return "Basic Latin";
    }

    Gen_Cat category(const _Char _uc) const {
      return Unicode::Gen_Cat(Basic_Latin::cat[_uc]);
    }

    // Allways CC_SPACING with Basic_Latin
    Can_Comb_Class combClass(const _Char _uc) const {
      return Unicode::CC_SPACING;
    }

    Bidir_Props bidirProps(const _Char _uc) const {
      return Unicode::Bidir_Props(Basic_Latin::bidir[_uc]);
    }

    // Allways DECOMP_NO_DECOMP with Basic_Latin
    Char_Decomp decompType(const _Char _uc) const {
      return DECOMP_NO_DECOMP;
    }

    // Always just the original character!
    String decompString(const _Char _uc) const {
      Unicode::String _us;

      _us.resize(1);
      _us[0]=_uc;


      return _us;
    }

    bool mustMirror(const _Char _uc) const {
      if (_uc == 0x0028 || _uc == 0x0029 || _uc == 0x003C || _uc == 0x003E ||
	  _uc == 0x005B || _uc == 0x005D || _uc == 0x007B || _uc == 0x007D )
	return 1;

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if (_uc >= 0x0020 && _uc <= 0x007E)
	return Unicode::EA_WIDTH_Na;
      else
	return Unicode::EA_WIDTH_N;
    }

    Line_Break linebreak(const _Char _uc) const {
      return Unicode::Line_Break(Basic_Latin::lb[_uc]);
    }
    
  private:
    // Functions
    Basic_Latin(const Basic_Latin &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const unsigned char cat[128];
    static const unsigned char bidir[128];
    static const unsigned char lb[128];
  }; // class Basic_Latin

  const unsigned char Basic_Latin::cat[]={
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Zs, CAT_Po, CAT_Po, CAT_Po, CAT_Sc, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Ps, CAT_Pe, CAT_Po, CAT_Sm, CAT_Po, CAT_Pd, CAT_Po, CAT_Po, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Po, CAT_Po, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Po, 
    CAT_Po, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ps, CAT_Po, CAT_Pe, CAT_Sk, CAT_Pc, 
    CAT_Sk, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ps, CAT_Sm, CAT_Pe, CAT_Sm, CAT_Cc
  };
  
  const unsigned char Basic_Latin::bidir[] = {
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_S,  BIDIR_B,  BIDIR_S,  BIDIR_WS, BIDIR_B,  BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_B,  BIDIR_B,  BIDIR_B,  BIDIR_S, 
    BIDIR_WS, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_CS, BIDIR_ET, BIDIR_CS, BIDIR_ES, 
    BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, 
    BIDIR_EN, BIDIR_EN, BIDIR_CS, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L, 
    BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L, 
    BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L, 
    BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L, 
    BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L, 
    BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_L, 
    BIDIR_L,  BIDIR_L,  BIDIR_L,  BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_BN
  };

  const unsigned char Basic_Latin::lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_BA, LB_LF, LB_CM, LB_BK, LB_CR, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_SP, LB_EX, LB_QU, LB_AL, LB_PR, LB_PO, LB_AL, LB_QU, 
    LB_OP, LB_CL, LB_AL, LB_PR, LB_IS, LB_HY, LB_IS, LB_SY, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_IS, LB_IS, LB_AL, LB_AL, LB_AL, LB_EX, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_OP, LB_PR, LB_CL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_OP, LB_BA, LB_CL, LB_AL, LB_CM
  };

}; // namespace Unicode

dload(Unicode::Basic_Latin);
  
