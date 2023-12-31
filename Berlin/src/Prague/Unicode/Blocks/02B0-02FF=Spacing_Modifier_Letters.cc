/*
 * Plugin for Unicode Block "Spacing Modifier Letters"
 * (02B0-02FF)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Spacing_Modifier_Letters : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Spacing_Modifier_Letters() {
      my_first_letter=0x02B0;
      my_last_letter =0x02FF;
      // my_version="3.0"
    }
      
    ~Spacing_Modifier_Letters() {
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
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return _uc;
    }

    _Char lowercase(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return _uc;
    }

    _Char titlecase(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      return _uc;
    }

    float numericValue(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     

    int decDigitValue(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }

    int digitValue(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }

    string blockname(const _Char _uc) const {
      return "Spacing Modifier Letters";
    }

    Gen_Cat category(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);
	
      return Unicode::Gen_Cat(Spacing_Modifier_Letters::cat[_uc - my_first_letter]);
    }

    Can_Comb_Class combClass(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Can_Comb_Class(0);
    }

    Bidir_Props bidirProps(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Bidir_Props(Spacing_Modifier_Letters::
				  bidir[_uc - my_first_letter]);
    }

    Char_Decomp decompType(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Char_Decomp(Spacing_Modifier_Letters::
				  decomp_type[_uc - my_first_letter]);
    }

    String decompString(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      Unicode::String us;
      us.resize(2);
      us[0]=Spacing_Modifier_Letters::decompStr[_uc - my_first_letter][0];
      us[1]=Spacing_Modifier_Letters::decompStr[_uc - my_first_letter][1];  
      if (us[1] == 0x0000)
	us.resize(1);
      return us;
    }

    bool mustMirror(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return Unicode::EA_Width(Spacing_Modifier_Letters::ea[_uc - my_first_letter]);
    }

    Line_Break linebreak(const _Char _uc) const {
      if (_uc > 0x02EE)
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Line_Break(Spacing_Modifier_Letters::lb[_uc - my_first_letter]);
    }
    
  private:
    // Functions
    Spacing_Modifier_Letters(const Spacing_Modifier_Letters &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const unsigned char cat[63];
    static const unsigned char bidir[63];
    static const unsigned char ea[63];
    static const unsigned char lb[63];
    static const unsigned char decomp_type[63];
    static const Unicode::_Char decompStr[63][2];
  }; // class Spacing_Modifier_Letters

  const unsigned char Spacing_Modifier_Letters::cat[]= {
    CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, 
    CAT_Lm, CAT_Sk, CAT_Sk, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, 
    CAT_Lm, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Lm, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Lm, CAT_Sk, CAT_Sk, CAT_Sk, 
    CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Sk, CAT_Lm 
  };

  const unsigned char Spacing_Modifier_Letters::bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_L
  };
  
  const unsigned char Spacing_Modifier_Letters::ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N,
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

  const unsigned char Spacing_Modifier_Letters::lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_BB, LB_AI, LB_AI, LB_AI, LB_BB, LB_AI, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const unsigned char Spacing_Modifier_Letters::decomp_type[] = {
    DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER,
    DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER,
    DECOMP_SUPER, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, DECOMP_SUPER, 
    DECOMP_SUPER, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP
  };

  const Unicode::_Char Spacing_Modifier_Letters::decompStr[][2] = {
    { 0x0068, 0x0000 }, { 0x0266, 0x0000 }, { 0x006A, 0x0000 }, { 0x0072, 0x0000 }, 
    { 0x0279, 0x0000 }, { 0x027B, 0x0000 }, { 0x0281, 0x0000 }, { 0x0077, 0x0000 }, 
    {  0x0079, 0x0000 }, { 0x02B9, 0x0000 }, { 0x02BA, 0x0000 }, { 0x02BB, 0x0000 }, 
    { 0x02BC, 0x0000 }, { 0x02BD, 0x0000 }, { 0x02BE, 0x0000 }, { 0x02BF, 0x0000 }, 
    { 0x02C0, 0x0000 }, { 0x02C1, 0x0000 }, { 0x02C2, 0x0000 }, { 0x02C3, 0x0000 }, 
    { 0x02C4, 0x0000 }, { 0x02C5, 0x0000 }, { 0x02C6, 0x0000 }, { 0x02C7, 0x0000 }, 
    { 0x02C8, 0x0000 }, { 0x02C9, 0x0000 }, { 0x02CA, 0x0000 }, { 0x02CB, 0x0000 }, 
    { 0x02CC, 0x0000 }, { 0x02CD, 0x0000 }, { 0x02CE, 0x0000 }, { 0x02CF, 0x0000 }, 
    { 0x02D0, 0x0000 }, { 0x02D1, 0x0000 }, { 0x02D2, 0x0000 }, { 0x02D3, 0x0000 }, 
    { 0x02D4, 0x0000 }, { 0x02D5, 0x0000 }, { 0x02D6, 0x0000 }, { 0x02D7, 0x0000 }, 
    { 0x0020, 0x0306 }, { 0x0020, 0x0307 }, { 0x0020, 0x030A }, { 0x0020, 0x0328 }, 
    { 0x0020, 0x0303 }, { 0x0020, 0x030B }, { 0x02DE, 0x0000 }, { 0x02DF, 0x0000 }, 
    { 0x0263, 0x0000 }, { 0x006C, 0x0000 }, { 0x0073, 0x0000 }, { 0x0078, 0x0000 }, 
    { 0x0295, 0x0000 }, { 0x02E5, 0x0000 }, { 0x02E6, 0x0000 }, { 0x02E7, 0x0000 }, 
    { 0x02E8, 0x0000 }, { 0x02E9, 0x0000 }, { 0x02EA, 0x0000 }, { 0x02EB, 0x0000 }, 
    { 0x02EC, 0x0000 }, { 0x02ED, 0x0000 }, { 0x02EE, 0x0000 }
  };

}; // namespace Unicode

dload(Unicode::Spacing_Modifier_Letters);
  
