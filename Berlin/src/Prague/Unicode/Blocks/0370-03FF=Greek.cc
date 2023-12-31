/*
 * Plugin for Unicode Block "Greek"
 * (0370-03FF)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Greek : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Greek() {
      my_first_letter=0x0370;
      my_last_letter =0x03FF;
      // my_version="3.0"
    }
      
    ~Greek() {
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
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return Greek::upper[_uc - my_first_letter - 4];
    }

    _Char lowercase(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Greek::lower[_uc - my_first_letter - 4];
    }

    _Char titlecase(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      return Greek::title[_uc - my_first_letter - 4];
    }

    float numericValue(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     

    int decDigitValue(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }

    int digitValue(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }

    string blockname(const _Char _uc) const {
      return "Greek";
    }

    Gen_Cat category(const _Char _uc) const {
      if ( !( isdefined[(_uc - my_first_letter)] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);
	
      return Unicode::Gen_Cat(Greek::cat[_uc - my_first_letter - 4]);
    }

    Can_Comb_Class combClass(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Can_Comb_Class(0);
    }

    Bidir_Props bidirProps(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      switch(_uc) {
      case 0x0374:
      case 0x0375:
      case 0x037E:
      case 0x0384:
      case 0x0385:
      case 0x0387:
	return Unicode::BIDIR_ON;
      default:
	return Unicode::BIDIR_L;
      }
    }

    Char_Decomp decompType(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Char_Decomp(Greek::
				  decomp_type[_uc - my_first_letter - 4]);
    }

    String decompString(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      Unicode::String us;
      us.resize(2);
      us[0]=Greek::decompStr[_uc - my_first_letter - 4][0];
      us[1]=Greek::decompStr[_uc - my_first_letter - 4][1];  
      if (us[1] == 0x0000)
	us.resize(1);
      return us;
    }

    bool mustMirror(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return Unicode::EA_Width(Greek::ea[_uc - my_first_letter - 4]);
    }

    Line_Break linebreak(const _Char _uc) const {
      if ( !( isdefined[_uc - my_first_letter] ) )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Line_Break(Greek::lb[_uc - my_first_letter - 4]);
    }
    
  private:
    // Functions
    Greek(const Greek &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;
    
    static const bool isdefined[144];
    static const _Char title[132];
    static const _Char lower[132];
    static const _Char upper[132];
    static const unsigned char cat[132];
    static const unsigned char ea[132];
    static const unsigned char lb[132];
    static const unsigned char decomp_type[132];
    static const Unicode::_Char decompStr[132][2];
  }; // class Greek

  const _Char Greek::title[] = {
    0x0374, 0x0375,

    0x0000, 0x0000, 0x0000, 0x0000, // spaceholder

    0x037A, 
    
    0x0000, 0x0000, 0x0000, // spaceholder
 
    0x037E, 

    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, // spaceholder

    0x0384, 0x0385, 0x0386, 0x0387, 0x0388, 0x0389, 0x038A, 

    0x0000, // spaceholder
    
    0x038C,
    
    0x0000, // spaceholder

    0x038E, 0x038F, 0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 
    0x0396, 0x0397, 0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 
    0x039E, 0x039F, 0x03A0, 0x03A1, 
    
    0x0000, // spaceholder

    0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9, 0x03AA, 
    0x03AB, 0x0386, 0x0388, 0x0389, 0x038A, 0x03B0, 0x0391, 0x0392, 
    0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 0x0398, 0x0399, 0x039A, 
    0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 0x03A0, 0x03A1, 0x03A3, 
    0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9, 0x03AA, 
    0x03AB, 0x038C, 0x038E, 0x038F, 

    0x0000, // spaceholder

    0x0392, 0x0398, 0x03D2, 0x03D3, 0x03D4, 0x03A6, 0x03A0, 0x03D7, 

    0x0000, 0x0000, // spaceholder

    0x03DA, 0x03DA, 0x03DC, 0x03DC, 0x03DE, 0x03DE, 0x03E0, 0x03E0, 
    0x03E2, 0x03E2, 0x03E4, 0x03E4, 0x03E6, 0x03E6, 0x03E8, 0x03E8, 
    0x03EA, 0x03EA, 0x03EC, 0x03EC, 0x03EE, 0x03EE, 0x039A, 0x03A1, 
    0x03A3, 0x03F3
  };

  const _Char Greek::upper[] = {
    0x0374, 0x0375, 

    0x0000, 0x0000, 0x0000, 0x0000, // spaceholder

    0x037A, 

    0x0000, 0x0000, 0x0000,  // spaceholder

    0x037E, 

    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, // spaceholder

    0x0384, 0x0385, 0x0386, 0x0387, 0x0388, 0x0389, 0x038A, 

    0x0000, // spaceholder

    0x038C, 

    0x0000, // spaceholder

    0x038E, 0x038F, 0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 
    0x0396, 0x0397, 0x0398, 0x0399, 0x039A, 0x039B, 0x039C, 0x039D, 
    0x039E, 0x039F, 0x03A0, 0x03A1, 

    0x0000, // spaceholder
 
    0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9, 0x03AA, 
    0x03AB, 0x0386, 0x0388, 0x0389, 0x038A, 0x03B0, 0x0391, 0x0392, 
    0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 0x0398, 0x0399, 0x039A, 
    0x039B, 0x039C, 0x039D, 0x039E, 0x039F, 0x03A0, 0x03A1, 0x03A3, 
    0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7, 0x03A8, 0x03A9, 0x03AA, 
    0x03AB, 0x038C, 0x038E, 0x038F, 

    0x0000, // spaceholder
    
    0x0392, 0x0398, 0x03D2, 0x03D3, 0x03D4, 0x03A6, 0x03A0, 0x03D7, 

    0x0000, 0x0000, // spaceholder

    0x03DA, 0x03DA, 0x03DC, 0x03DC, 0x03DE, 0x03DE, 0x03E0, 0x03E0, 
    0x03E2, 0x03E2, 0x03E4, 0x03E4, 0x03E6, 0x03E6, 0x03E8, 0x03E8, 
    0x03EA, 0x03EA, 0x03EC, 0x03EC, 0x03EE, 0x03EE, 0x039A, 0x03A1, 
    0x03A3, 0x03F3
  };

  const _Char Greek::lower[] = {
    0x0374, 0x0375, 
    
    0x0000, 0x0000, 0x0000, 0x0000, // spaceholder
 
    0x037A, 
    
    0x0000, 0x0000, 0x0000, // spaceholder

    0x037E, 
    
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, // spaceholder

    0x0384, 0x0385, 0x03AC, 0x0387, 0x03AD, 0x03AE, 0x03AF, 
    
    0x0000, // spaceholder

    0x03CC, 
    
    0x0000, // spaceholder

    0x03CD, 0x03CE, 0x0390, 0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 
    0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 
    0x03BE, 0x03BF, 0x03C0, 0x03C1, 

    0x0000, // spaceholder

    0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7, 0x03C8, 0x03C9, 0x03CA, 
    0x03CB, 0x03AC, 0x03AD, 0x03AE, 0x03AF, 0x03B0, 0x03B1, 0x03B2, 
    0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA, 
    0x03BB, 0x03BC, 0x03BD, 0x03BE, 0x03BF, 0x03C0, 0x03C1, 0x03C2, 
    0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7, 0x03C8, 0x03C9, 0x03CA, 
    0x03CB, 0x03CC, 0x03CD, 0x03CE, 

    0x0000, // spaceholder

    0x03D0, 0x03D1, 0x03D2, 0x03D3, 0x03D4, 0x03D5, 0x03D6, 0x03D7, 

    0x0000, 0x0000, // spaceholder

    0x03DB, 0x03DB, 0x03DD, 0x03DD, 0x03DF, 0x03DF, 0x03E1, 0x03E1, 
    0x03E3, 0x03E3, 0x03E5, 0x03E5, 0x03E7, 0x03E7, 0x03E9, 0x03E9, 
    0x03EB, 0x03EB, 0x03ED, 0x03ED, 0x03EF, 0x03EF, 0x03F0, 0x03F1, 
    0x03F2, 0x03F3
  };

  const bool Greek::isdefined[] = {
    0, 0, 0, 0, 1, 1, 0, 0,   0, 0, 1, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 1, 1, 1, 1,   1, 1, 1, 0, 1, 0, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 0,
    1, 1, 1, 1, 1, 1, 1, 1,   0, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Greek::cat[] = {
    CAT_Sk, CAT_Sk, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, // spaceholder 
    CAT_Lm, 
    CAT_Lu, CAT_Lu, CAT_Lu, // spaceholder
    CAT_Po, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, // spaceholder
    CAT_Sk, CAT_Sk, CAT_Lu, CAT_Po, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, // spaceholder
    CAT_Lu, 
    CAT_Lu, // spaceholder
    CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, 
    CAT_Lu, // spaceholder
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, // spaceholder
    CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lu, CAT_Lu, // spaceholder
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll
  };

  const unsigned char Greek::ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, 

    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, // spaceholder

    EA_WIDTH_N,

    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, // spaceholder

    EA_WIDTH_N, 

    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, // spaceholder

    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N,
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 

    EA_WIDTH_N, // spaceholder

    EA_WIDTH_N, 

    EA_WIDTH_N, // spaceholder

    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 

    EA_WIDTH_N, // spaceholder
 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 

    EA_WIDTH_N, // spaceholder
 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 

    EA_WIDTH_N, EA_WIDTH_N, // spaceholder

    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N
  };

  const unsigned char Greek::lb[] = {
    LB_AL, LB_AL, 
    
    LB_AL, LB_AL, LB_AL, LB_AL, // spaceholder

    LB_AL, 

    LB_AL, LB_AL, LB_AL, // spaceholder

    LB_AL,

    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, // spaceholder

    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 

    LB_AL, // spaceholder

    LB_AL,
    
    LB_AL, // spaceholder

    LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, 

    LB_AL, // spaceholder

    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AI, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, 

    LB_AL, // spaceholder 

    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 

    LB_AL, LB_AL, // spaceholder
    
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL
  };

  const unsigned char Greek::decomp_type[] = {
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,

    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, // spaceholder

    DECOMP_COMPAT,

    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, // spaceholder

    DECOMP_NO_DECOMP,

    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, // spaceholder

    DECOMP_COMPAT, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,

    DECOMP_NO_DECOMP, // spaceholder

    DECOMP_NO_DECOMP,
    
    DECOMP_NO_DECOMP, // spaceholder
    
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,

    DECOMP_NO_DECOMP, // spaceholder

    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,

    DECOMP_NO_DECOMP, // spaceholder

    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_NO_DECOMP,

    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, // spaceholder

    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP,
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_COMPAT, DECOMP_COMPAT,
    DECOMP_COMPAT, DECOMP_NO_DECOMP
  };

  const Unicode::_Char Greek::decompStr[][2] = {
    { 0x02B9, 0x0000 }, { 0x0375, 0x0000 },

    { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, // spaceholder
 
    { 0x0020, 0x0345 },
    
    { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, // spaceholder

    { 0x003B, 0x0000 }, 
    
    { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, 
    { 0x0000, 0x0000 }, // spaceholder
    
    { 0x0020, 0x0301 }, { 0x00A8, 0x0301 }, { 0x0391, 0x0301 }, { 0x00B7, 0x0000 }, 
    { 0x0395, 0x0301 }, { 0x0397, 0x0301 }, { 0x0399, 0x0301 }, 

    { 0x0000, 0x0000 }, // spaceholder

    { 0x039F, 0x0301 }, 

    { 0x0000, 0x0000 }, // spaceholder
    
    { 0x03A5, 0x0301 }, { 0x03A9, 0x0301 }, { 0x03CA, 0x0301 }, { 0x0391, 0x0000 }, 
    { 0x0392, 0x0000 }, { 0x0393, 0x0000 }, { 0x0394, 0x0000 }, { 0x0395, 0x0000 }, 
    { 0x0396, 0x0000 }, { 0x0397, 0x0000 }, { 0x0398, 0x0000 }, { 0x0399, 0x0000 }, 
    { 0x039A, 0x0000 }, { 0x039B, 0x0000 }, { 0x039C, 0x0000 }, { 0x039D, 0x0000 }, 
    { 0x039E, 0x0000 }, { 0x039F, 0x0000 }, { 0x03A0, 0x0000 }, { 0x03A1, 0x0000 }, 

    { 0x0000, 0x0000 }, // spaceholder
    
    { 0x03A3, 0x0000 }, { 0x03A4, 0x0000 }, { 0x03A5, 0x0000 }, { 0x03A6, 0x0000 }, 
    { 0x03A7, 0x0000 }, { 0x03A8, 0x0000 }, { 0x03A9, 0x0000 }, { 0x0399, 0x0308 }, 
    { 0x03A5, 0x0308 }, { 0x03B1, 0x0301 }, { 0x03B5, 0x0301 }, { 0x03B7, 0x0301 }, 
    { 0x03B9, 0x0301 }, { 0x03CB, 0x0301 }, { 0x03B1, 0x0000 }, { 0x03B2, 0x0000 }, 
    { 0x03B3, 0x0000 }, { 0x03B4, 0x0000 }, { 0x03B5, 0x0000 }, { 0x03B6, 0x0000 }, 
    { 0x03B7, 0x0000 }, { 0x03B8, 0x0000 }, { 0x03B9, 0x0000 }, { 0x03BA, 0x0000 }, 
    { 0x03BB, 0x0000 }, { 0x03BC, 0x0000 }, { 0x03BD, 0x0000 }, { 0x03BE, 0x0000 }, 
    { 0x03BF, 0x0000 }, { 0x03C0, 0x0000 }, { 0x03C1, 0x0000 }, { 0x03C2, 0x0000 }, 
    { 0x03C3, 0x0000 }, { 0x03C4, 0x0000 }, { 0x03C5, 0x0000 }, { 0x03C6, 0x0000 }, 
    { 0x03C7, 0x0000 }, { 0x03C8, 0x0000 }, { 0x03C9, 0x0000 }, { 0x03B9, 0x0308 }, 
    { 0x03C5, 0x0308 }, { 0x03BF, 0x0301 }, { 0x03C5, 0x0301 }, { 0x03C9, 0x0301 }, 

    { 0x0000, 0x0000 }, // spaceholder

    { 0x03B2, 0x0000 }, { 0x03B8, 0x0000 }, { 0x03A5, 0x0000 }, { 0x03D2, 0x0301 }, 
    { 0x03D2, 0x0308 }, { 0x03C6, 0x0000 }, { 0x03C0, 0x0000 }, { 0x03D7, 0x0000 }, 

    { 0x0000, 0x0000 }, { 0x0000, 0x0000 }, // spaceholder

    { 0x03DA, 0x0000 }, { 0x03DB, 0x0000 }, { 0x03DC, 0x0000 }, { 0x03DD, 0x0000 }, 
    { 0x03DE, 0x0000 }, { 0x03DF, 0x0000 }, { 0x03E0, 0x0000 }, { 0x03E1, 0x0000 }, 
    { 0x03E2, 0x0000 }, { 0x03E3, 0x0000 }, { 0x03E4, 0x0000 }, { 0x03E5, 0x0000 }, 
    { 0x03E6, 0x0000 }, { 0x03E7, 0x0000 }, { 0x03E8, 0x0000 }, { 0x03E9, 0x0000 }, 
    { 0x03EA, 0x0000 }, { 0x03EB, 0x0000 }, { 0x03EC, 0x0000 }, { 0x03ED, 0x0000 }, 
    { 0x03EE, 0x0000 }, { 0x03EF, 0x0000 }, { 0x03BA, 0x0000 }, { 0x03C1, 0x0000 }, 
    { 0x03C2, 0x0000 }, { 0x03F3, 0x0000 }, 
  };

}; // namespace Unicode

dload(Unicode::Greek);
  
