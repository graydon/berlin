/*
 * Plugin for Unicode Block "Latin-1 Supplement"
 * (0080-00FF)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Latin1_Supplement : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Latin1_Supplement() {
      my_first_letter=0x0080;
      my_last_letter =0x00FF;
      // my_version="3.0"
    }
      
    ~Latin1_Supplement() {
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
      return Latin1_Supplement::upper[_uc - my_first_letter];
    }

    _Char lowercase(const _Char _uc) const {
      return Latin1_Supplement::lower[_uc - my_first_letter];
    }

    _Char titlecase(const _Char _uc) const {
      return Latin1_Supplement::title[_uc - my_first_letter];
    }

    float numericValue(const _Char _uc) const {
      switch(_uc) {
      case 0x00B2:
	return 2.0;
      case 0x00B3:
	return 3.0;
      case 0x00B9:
	return 1.0;
      case 0x00BC:
	return 0.25;
      case 0x00BD:
	return 0.5;
      case 0x00BE:
	return 0.75;
      default:
	throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
      }
    }     

    int decDigitValue(const _Char _uc) const {
      switch (_uc) {
      case 0x00B2:
	return 2;
      case 0x00B3:
	return 3;
      case 0x00B9:
	return 1;
      default:
	throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
      }
    }

    int digitValue(const _Char _uc) const {
      switch (_uc) {
      case 0x00B2:
	return 2;
      case 0x00B3:
	return 3;
      case 0x00B9:
	return 1;
      default:
	throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
      }
    }

    // Allways "Latin-1 Supplement"
    string blockname(const _Char _uc) const {
      return "Latin-1 Supplement";
    }

    Gen_Cat category(const _Char _uc) const {
      return Unicode::Gen_Cat(Latin1_Supplement::cat[_uc - my_first_letter]);
    }

    Can_Comb_Class combClass(const _Char _uc) const {
      return Unicode::Can_Comb_Class(0);
    }

    Bidir_Props bidirProps(const _Char _uc) const {
      return Unicode::Bidir_Props(Latin1_Supplement::
				  bidir[_uc - my_first_letter]);
    }

    Char_Decomp decompType(const _Char _uc) const {
      return Unicode::Char_Decomp(Latin1_Supplement::
				  decomp[_uc - my_first_letter]);
    }

    String decompString(const _Char _uc) const {
      Unicode::String us;
      us.resize(2);
      us[0]=Latin1_Supplement::decompStr[_uc - my_first_letter][0];
      us[1]=Latin1_Supplement::decompStr[_uc - my_first_letter][1];  
      if (_uc == 0x00BC || _uc == 0x00BE) {
	us.resize(3);
	us[2]=0x0034;
      } else if (_uc == 0x00BD) {
	us.resize(3);
	us[2]=0x0032;
      } else if (us[1] == 0x0000)
	us.resize(1);
      return us;
    }

    bool mustMirror(const _Char _uc) const {
      if (_uc == 0x00AB || _uc == 0x00BB)
	return 1;
      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      return Unicode::EA_Width(Latin1_Supplement::ea[_uc - my_first_letter]);
    }

    Line_Break linebreak(const _Char _uc) const {
      return Unicode::Line_Break(Latin1_Supplement::lb[_uc - my_first_letter]);
    }
    
  private:
    // Functions
    Latin1_Supplement(const Latin1_Supplement &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const Unicode::_Char upper[128];
    static const Unicode::_Char lower[128];
    static const Unicode::_Char title[128];
    static const unsigned char cat[128];
    static const unsigned char bidir[128];
    static const unsigned char ea[128];
    static const unsigned char lb[128];
    static const unsigned char decomp[128];
    static const Unicode::_Char decompStr[128][2];
  }; // class Latin1_Supplement

  const Unicode::_Char Latin1_Supplement::upper[] = {
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, 
    0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
    0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, 
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, 
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, 
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x039C, 0x00B6, 0x00B7, 
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00F7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x0178
  };

   const Unicode::_Char Latin1_Supplement::lower[] = {
     0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
     0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, 
     0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
     0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, 
     0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, 
     0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, 
     0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7, 
     0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, 
     0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7, 
     0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF, 
     0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00D7, 
     0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00DF, 
     0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7, 
     0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF, 
     0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7, 
     0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF
  };

  const Unicode::_Char Latin1_Supplement::title[] = {
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 
    0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F, 
    0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097, 
    0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F, 
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, 
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, 
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x039C, 0x00B6, 0x00B7, 
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF, 
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, 
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, 
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00F7, 
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x0178
  };


  const unsigned char Latin1_Supplement::cat[]= {
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc,
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc,
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc,
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc,
    CAT_Zs, CAT_Po, CAT_Sc, CAT_Sc, CAT_Sc, CAT_Sc, CAT_So, CAT_So,
    CAT_Sk, CAT_So, CAT_Ll, CAT_Pi, CAT_Sm, CAT_Pd, CAT_So, CAT_Sk,
    CAT_So, CAT_Sm, CAT_No, CAT_No, CAT_Sk, CAT_Ll, CAT_So, CAT_Po,
    CAT_Sk, CAT_No, CAT_Ll, CAT_Pf, CAT_No, CAT_No, CAT_No, CAT_Po,
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu,
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Sm,
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll,
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll,
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll,
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Sm,
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll
  };
  
  const unsigned char Latin1_Supplement::bidir[] = {
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_B, BIDIR_BN, BIDIR_BN,
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN,
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN,
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN,
    BIDIR_CS, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON,
    BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON,
    BIDIR_ET, BIDIR_ET, BIDIR_EN, BIDIR_EN, BIDIR_ON, BIDIR_L, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_EN, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON,
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L,
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L 
  };

  const unsigned char Latin1_Supplement::ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N,
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_A, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_Na, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_Na, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N
  };

  const unsigned char Latin1_Supplement::lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_GL, LB_AI, LB_PO, LB_PR, LB_PR, LB_PR, LB_AL, LB_AI, 
    LB_AI, LB_AL, LB_AI, LB_QU, LB_AL, LB_BA, LB_AL, LB_AL, 
    LB_PO, LB_PR, LB_AI, LB_AI, LB_BA, LB_AL, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_QU, LB_AI, LB_AI, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL 
  };

  const unsigned char Latin1_Supplement::decomp[] = {
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NOBREAK, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_COMPAT, DECOMP_NO_DECOMP, DECOMP_SUPER, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_COMPAT, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_SUPER, DECOMP_SUPER, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_COMPAT, DECOMP_SUPER, DECOMP_SUPER, DECOMP_NO_DECOMP, 
    DECOMP_FRACTION, DECOMP_FRACTION, DECOMP_FRACTION, DECOMP_NO_DECOMP, 
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
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, 
    DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP, DECOMP_NO_DECOMP
  };

  const Unicode::_Char Latin1_Supplement::decompStr[][2] = {
    { 0x0080, 0x0000 }, { 0x0081, 0x0000 }, { 0x0082, 0x0000 }, { 0x0083, 0x0000 }, 
    { 0x0084, 0x0000 }, { 0x0085, 0x0000 }, { 0x0086, 0x0000 }, { 0x0087, 0x0000 }, 
    { 0x0088, 0x0000 }, { 0x0089, 0x0000 }, { 0x008A, 0x0000 }, { 0x008B, 0x0000 }, 
    { 0x008C, 0x0000 }, { 0x008D, 0x0000 }, { 0x008E, 0x0000 }, { 0x008F, 0x0000 }, 
    { 0x0090, 0x0000 }, { 0x0091, 0x0000 }, { 0x0092, 0x0000 }, { 0x0093, 0x0000 }, 
    { 0x0094, 0x0000 }, { 0x0095, 0x0000 }, { 0x0096, 0x0000 }, { 0x0097, 0x0000 }, 
    { 0x0098, 0x0000 }, { 0x0099, 0x0000 }, { 0x009A, 0x0000 }, { 0x009B, 0x0000 }, 
    { 0x009C, 0x0000 }, { 0x009D, 0x0000 }, { 0x009E, 0x0000 }, { 0x009F, 0x0000 }, 
    { 0x0020, 0x0000 }, { 0x00A1, 0x0000 }, { 0x00A2, 0x0000 }, { 0x00A3, 0x0000 }, 
    { 0x00A4, 0x0000 }, { 0x00A5, 0x0000 }, { 0x00A6, 0x0000 }, { 0x00A7, 0x0000 }, 
    { 0x0020, 0x0308 }, { 0x00A9, 0x0000 }, { 0x0061, 0x0000 }, { 0x00AB, 0x0000 }, 
    { 0x00AC, 0x0000 }, { 0x00AD, 0x0000 }, { 0x00AE, 0x0000 }, { 0x0020, 0x0304 }, 
    { 0x00B0, 0x0000 }, { 0x00B1, 0x0000 }, { 0x0032, 0x0000 }, { 0x0033, 0x0000 }, 
    { 0x0020, 0x0301 }, { 0x03BC, 0x0000 }, { 0x00B6, 0x0000 }, { 0x00B7, 0x0000 }, 
    { 0x0020, 0x0327 }, { 0x0031, 0x0000 }, { 0x006F, 0x0000 }, { 0x00BB, 0x0000 }, 
    { 0x0031, 0x2044 }, // Spaceholder!
    { 0x0031, 0x2044 }, // Spaceholder!
    { 0x0033, 0x2044 }, // Spaceholder!
    { 0x00BF, 0x0000 }, { 0x0041, 0x0300 }, { 0x0041, 0x0301 }, { 0x0041, 0x0302 }, 
    { 0x0041, 0x0303 }, { 0x0041, 0x0308 }, { 0x0041, 0x030A }, { 0x00C6, 0x0000 }, 
    { 0x0043, 0x0327 }, { 0x0045, 0x0300 }, { 0x0045, 0x0301 }, { 0x0045, 0x0302 }, 
    { 0x0045, 0x0308 }, { 0x0049, 0x0300 }, { 0x0049, 0x0301 }, { 0x0049, 0x0302 }, 
    { 0x0049, 0x0308 }, { 0x00D0, 0x0000 }, { 0x004E, 0x0303 }, { 0x004F, 0x0300 }, 
    { 0x004F, 0x0301 }, { 0x004F, 0x0302 }, { 0x004F, 0x0303 }, { 0x004F, 0x0308 }, 
    { 0x00D7, 0x0000 }, { 0x00D8, 0x0000 }, { 0x0055, 0x0300 }, { 0x0055, 0x0301 }, 
    { 0x0055, 0x0302 }, { 0x0055, 0x0308 }, { 0x0059, 0x0301 }, { 0x00DE, 0x0000 }, 
    { 0x00DF, 0x0000 }, { 0x0061, 0x0300 }, { 0x0061, 0x0301 }, { 0x0061, 0x0302 }, 
    { 0x0061, 0x0303 }, { 0x0061, 0x0308 }, { 0x0061, 0x030A }, { 0x00E6, 0x0000 }, 
    { 0x0063, 0x0327 }, { 0x0065, 0x0300 }, { 0x0065, 0x0301 }, { 0x0065, 0x0302 }, 
    { 0x0065, 0x0308 }, { 0x0069, 0x0300 }, { 0x0069, 0x0301 }, { 0x0069, 0x0302 }, 
    { 0x0069, 0x0308 }, { 0x00F0, 0x0000 }, { 0x006E, 0x0303 }, { 0x006F, 0x0300 }, 
    { 0x006F, 0x0301 }, { 0x006F, 0x0302 }, { 0x006F, 0x0303 }, { 0x006F, 0x0308 }, 
    { 0x00F7, 0x0000 }, { 0x00F8, 0x0000 }, { 0x0075, 0x0300 }, { 0x0075, 0x0301 }, 
    { 0x0075, 0x0302 }, { 0x0075, 0x0308 }, { 0x0079, 0x0301 }, { 0x00FE, 0x0000 }, 
    { 0x0079, 0x0308 }
  };

}; // namespace Unicode

dload(Unicode::Latin1_Supplement);
  
