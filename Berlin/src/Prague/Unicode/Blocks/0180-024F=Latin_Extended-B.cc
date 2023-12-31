/*
 * Plugin for Unicode Block "Latin Extended-B"
 * (0180-024F)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Latin_Extended_B : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Latin_Extended_B() {
      my_first_letter=0x0180;
      my_last_letter =0x024F;
      // my_version="3.0"
    }
      
    ~Latin_Extended_B() {
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
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return Latin_Extended_B::upper[_uc - my_first_letter];
    }

    _Char lowercase(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Latin_Extended_B::lower[_uc - my_first_letter];
    }

    _Char titlecase(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      return Latin_Extended_B::title[_uc - my_first_letter];
    }

    float numericValue(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_NUMERIC_VALUE);
    }     

    int decDigitValue(const _Char _uc) const {
      if (_uc == 0x220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      throw UndefinedProperty(_uc, PROP_DEC_DIGIT_VALUE);
    }

    int digitValue(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      throw UndefinedProperty(_uc, PROP_DIGIT_VALUE);
    }

    string blockname(const _Char _uc) const {
      return "Latin Extended-B";
    }

    Gen_Cat category(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);
	
      return Unicode::Gen_Cat(Latin_Extended_B::cat[_uc - my_first_letter]);
    }

    // Allways 0 with "Latin Extended-B"
    Can_Comb_Class combClass(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Can_Comb_Class(0);
    }

    // Allways BIDIR_L with "Latin Extended-B"
    Bidir_Props bidirProps(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::BIDIR_L;
    }

    Char_Decomp decompType(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);
      
      if (_uc >= 0x01C4 && _uc <=0x01CC)
	return Unicode::DECOMP_COMPAT;
      if (_uc >= 0x01F1 && _uc <= 0x01F3 )
	return Unicode::DECOMP_COMPAT;

      return Unicode::DECOMP_NO_DECOMP;
    }

    String decompString(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      Unicode::String us;
      us.resize(2);
      us[0]=Latin_Extended_B::decompStr[_uc - my_first_letter][0];
      us[1]=Latin_Extended_B::decompStr[_uc - my_first_letter][1];  
      if (us[1] == 0x0000)
	us.resize(1);
      return us;
    }

    // Allways False with Latin Extended-B
    bool mustMirror(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER); 

      return Unicode::EA_Width(Latin_Extended_B::ea[_uc - my_first_letter]);
    }

    Line_Break linebreak(const _Char _uc) const {
      if (_uc == 0x0220 || _uc == 0x0221 || _uc > 0x0233 )
	throw UndefinedProperty(_uc, PROP_CHARACTER);

      return Unicode::Line_Break(Latin_Extended_B::lb[_uc - my_first_letter]);
    }
    
  private:
    // Functions
    Latin_Extended_B(const Latin_Extended_B &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const Unicode::_Char upper[180];
    static const Unicode::_Char lower[180];
    static const Unicode::_Char title[180];
    static const unsigned char cat[180];
    static const unsigned char ea[180];
    static const unsigned char lb[180];
    static const Unicode::_Char decompStr[180][2];
  }; // class Latin_Extended_B

  const Unicode::_Char Latin_Extended_B::upper[] = {
    0x0180, 0x0181, 0x0182, 0x0182, 0x0184, 0x0184, 0x0186, 0x0187, 
    0x0187, 0x0189, 0x018A, 0x018B, 0x018B, 0x018D, 0x018E, 0x018F, 
    0x0190, 0x0191, 0x0191, 0x0193, 0x0194, 0x01F6, 0x0196, 0x0197, 
    0x0198, 0x0198, 0x019A, 0x019B, 0x019C, 0x019D, 0x019E, 0x019F, 
    0x01A0, 0x01A0, 0x01A2, 0x01A2, 0x01A4, 0x01A4, 0x01A6, 0x01A7, 
    0x01A7, 0x01A9, 0x01AA, 0x01AB, 0x01AC, 0x01AC, 0x01AE, 0x01AF, 
    0x01AF, 0x01B1, 0x01B2, 0x01B3, 0x01B3, 0x01B5, 0x01B5, 0x01B7, 
    0x01B8, 0x01B8, 0x01BA, 0x01BB, 0x01BC, 0x01BC, 0x01BE, 0x01F7, 
    0x01C0, 0x01C1, 0x01C2, 0x01C3, 0x01C4, 0x01C4, 0x01C4, 0x01C7, 
    0x01C7, 0x01C7, 0x01CA, 0x01CA, 0x01CA, 0x01CD, 0x01CD, 0x01CF, 
    0x01CF, 0x01D1, 0x01D1, 0x01D3, 0x01D3, 0x01D5, 0x01D5, 0x01D7, 
    0x01D7, 0x01D9, 0x01D9, 0x01DB, 0x01DB, 0x018E, 0x01DE, 0x01DE, 
    0x01E0, 0x01E0, 0x01E2, 0x01E2, 0x01E4, 0x01E4, 0x01E6, 0x01E6, 
    0x01E8, 0x01E8, 0x01EA, 0x01EA, 0x01EC, 0x01EC, 0x01EE, 0x01EE, 
    0x01F0, 0x01F1, 0x01F1, 0x01F1, 0x01F4, 0x01F4, 0x01F6, 0x01F7, 
    0x01F8, 0x01F8, 0x01FA, 0x01FA, 0x01FC, 0x01FC, 0x01FE, 0x01FE, 
    0x0200, 0x0200, 0x0202, 0x0202, 0x0204, 0x0204, 0x0206, 0x0206, 
    0x0208, 0x0208, 0x020A, 0x020A, 0x020C, 0x020C, 0x020E, 0x020E, 
    0x0210, 0x0210, 0x0212, 0x0212, 0x0214, 0x0214, 0x0216, 0x0216, 
    0x0218, 0x0218, 0x021A, 0x021A, 0x021C, 0x021C, 0x021E, 0x021E,
    0x0000, // spaceholder: this character is not defined
    0x0000, // spaceholder: this character is not defined
    0x0222, 0x0222, 0x0224, 0x0224, 0x0226, 0x0226, 0x0228, 0x0228, 
    0x022A, 0x022A, 0x022C, 0x022C, 0x022E, 0x022E, 0x0230, 0x0230, 
    0x0232, 0x0232
  };

   const Unicode::_Char Latin_Extended_B::lower[] = {
     0x0180, 0x0253, 0x0183, 0x0183, 0x0185, 0x0185, 0x0254, 0x0188, 
     0x0188, 0x0256, 0x0257, 0x018C, 0x018C, 0x018D, 0x01DD, 0x0259, 
     0x025B, 0x0192, 0x0192, 0x0260, 0x0263, 0x0195, 0x0269, 0x0268, 
     0x0199, 0x0199, 0x019A, 0x019B, 0x026F, 0x0272, 0x019E, 0x0275, 
     0x01A1, 0x01A1, 0x01A3, 0x01A3, 0x01A5, 0x01A5, 0x0280, 0x01A8, 
     0x01A8, 0x0283, 0x01AA, 0x01AB, 0x01AD, 0x01AD, 0x0288, 0x01B0, 
     0x01B0, 0x028A, 0x028B, 0x01B4, 0x01B4, 0x01B6, 0x01B6, 0x0292, 
     0x01B9, 0x01B9, 0x01BA, 0x01BB, 0x01BD, 0x01BD, 0x01BE, 0x01BF, 
     0x01C0, 0x01C1, 0x01C2, 0x01C3, 0x01C6, 0x01C6, 0x01C6, 0x01C9, 
     0x01C9, 0x01C9, 0x01CC, 0x01CC, 0x01CC, 0x01CE, 0x01CE, 0x01D0, 
     0x01D0, 0x01D2, 0x01D2, 0x01D4, 0x01D4, 0x01D6, 0x01D6, 0x01D8, 
     0x01D8, 0x01DA, 0x01DA, 0x01DC, 0x01DC, 0x01DD, 0x01DF, 0x01DF, 
     0x01E1, 0x01E1, 0x01E3, 0x01E3, 0x01E5, 0x01E5, 0x01E7, 0x01E7, 
     0x01E9, 0x01E9, 0x01EB, 0x01EB, 0x01ED, 0x01ED, 0x01EF, 0x01EF, 
     0x01F0, 0x01F3, 0x01F3, 0x01F3, 0x01F5, 0x01F5, 0x0195, 0x01BF, 
     0x01F9, 0x01F9, 0x01FB, 0x01FB, 0x01FD, 0x01FD, 0x01FF, 0x01FF, 
     0x0201, 0x0201, 0x0203, 0x0203, 0x0205, 0x0205, 0x0207, 0x0207, 
     0x0209, 0x0209, 0x020B, 0x020B, 0x020D, 0x020D, 0x020F, 0x020F, 
     0x0211, 0x0211, 0x0213, 0x0213, 0x0215, 0x0215, 0x0217, 0x0217, 
     0x0219, 0x0219, 0x021B, 0x021B, 0x021D, 0x021D, 0x021F, 0x021F,
     0x0000, // spaceholder: this character is not defined
     0x0000, // spacehoolder: this character is not defined 
     0x0223, 0x0223, 0x0225, 0x0225, 0x0227, 0x0227, 0x0229, 0x0229, 
     0x022B, 0x022B, 0x022D, 0x022D, 0x022F, 0x022F, 0x0231, 0x0231, 
     0x0233, 0x0233
  };

  const Unicode::_Char Latin_Extended_B::title[] = {
    0x0180, 0x0181, 0x0182, 0x0182, 0x0184, 0x0184, 0x0186, 0x0187, 
    0x0187, 0x0189, 0x018A, 0x018B, 0x018B, 0x018D, 0x018E, 0x018F, 
    0x0190, 0x0191, 0x0191, 0x0193, 0x0194, 0x01F6, 0x0196, 0x0197, 
    0x0198, 0x0198, 0x019A, 0x019B, 0x019C, 0x019D, 0x019E, 0x019F, 
    0x01A0, 0x01A0, 0x01A2, 0x01A2, 0x01A4, 0x01A4, 0x01A6, 0x01A7, 
    0x01A7, 0x01A9, 0x01AA, 0x01AB, 0x01AC, 0x01AC, 0x01AE, 0x01AF, 
    0x01AF, 0x01B1, 0x01B2, 0x01B3, 0x01B3, 0x01B5, 0x01B5, 0x01B7, 
    0x01B8, 0x01B8, 0x01BA, 0x01BB, 0x01BC, 0x01BC, 0x01BE, 0x01F7, 
    0x01C0, 0x01C1, 0x01C2, 0x01C3, 0x01C5, 0x01C5, 0x01C5, 0x01C8, 
    0x01C8, 0x01C8, 0x01CB, 0x01CB, 0x01CB, 0x01CD, 0x01CD, 0x01CF, 
    0x01CF, 0x01D1, 0x01D1, 0x01D3, 0x01D3, 0x01D5, 0x01D5, 0x01D7, 
    0x01D7, 0x01D9, 0x01D9, 0x01DB, 0x01DB, 0x018E, 0x01DE, 0x01DE, 
    0x01E0, 0x01E0, 0x01E2, 0x01E2, 0x01E4, 0x01E4, 0x01E6, 0x01E6, 
    0x01E8, 0x01E8, 0x01EA, 0x01EA, 0x01EC, 0x01EC, 0x01EE, 0x01EE, 
    0x01F0, 0x01F2, 0x01F2, 0x01F2, 0x01F4, 0x01F4, 0x01F6, 0x01F7, 
    0x01F8, 0x01F8, 0x01FA, 0x01FA, 0x01FC, 0x01FC, 0x01FE, 0x01FE, 
    0x0200, 0x0200, 0x0202, 0x0202, 0x0204, 0x0204, 0x0206, 0x0206, 
    0x0208, 0x0208, 0x020A, 0x020A, 0x020C, 0x020C, 0x020E, 0x020E, 
    0x0210, 0x0210, 0x0212, 0x0212, 0x0214, 0x0214, 0x0216, 0x0216, 
    0x0218, 0x0218, 0x021A, 0x021A, 0x021C, 0x021C, 0x021E, 0x021E, 
    0x0000, // spaceholder: this character is not defined
    0x0000, // spaceholder: this character is not defined
    0x0222, 0x0222, 0x0224, 0x0224, 0x0226, 0x0226, 0x0228, 0x0228, 
    0x022A, 0x022A, 0x022C, 0x022C, 0x022E, 0x022E, 0x0230, 0x0230, 
    0x0232, 0x0232
  };


  const unsigned char Latin_Extended_B::cat[]= {
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lo, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lu, CAT_Lt, CAT_Ll, CAT_Lu, 
    CAT_Lt, CAT_Ll, CAT_Lu, CAT_Lt, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Lu, CAT_Lt, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, // spaceholder: this character is not defined
    CAT_Lu, // spaceholder. this caracter is not defined
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll
  };
  
  const unsigned char Latin_Extended_B::ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, // spaceholder: this character is not defined
    EA_WIDTH_N, // spaceholder: this character is not defined 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

  const unsigned char Latin_Extended_B::lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, 
    LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, 
    LB_AI, LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL,
    LB_AL, // spaceholder: this character is not defined
    LB_AL, // spaceholder: this character is not defined
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL
  };

  const Unicode::_Char Latin_Extended_B::decompStr[][2] = {
    { 0x0180, 0x0000 }, { 0x0181, 0x0000 }, { 0x0182, 0x0000 }, { 0x0183, 0x0000 },
    { 0x0184, 0x0000 }, { 0x0185, 0x0000 }, { 0x0186, 0x0000 }, { 0x0187, 0x0000 }, 
    { 0x0188, 0x0000 }, { 0x0189, 0x0000 }, { 0x018A, 0x0000 }, { 0x018B, 0x0000 }, 
    { 0x018C, 0x0000 }, { 0x018D, 0x0000 }, { 0x018E, 0x0000 }, { 0x018F, 0x0000 }, 
    { 0x0190, 0x0000 }, { 0x0191, 0x0000 }, { 0x0192, 0x0000 }, { 0x0193, 0x0000 }, 
    { 0x0194, 0x0000 }, { 0x0195, 0x0000 }, { 0x0196, 0x0000 }, { 0x0197, 0x0000 }, 
    { 0x0198, 0x0000 }, { 0x0199, 0x0000 }, { 0x019A, 0x0000 }, { 0x019B, 0x0000 }, 
    { 0x019C, 0x0000 }, { 0x019D, 0x0000 }, { 0x019E, 0x0000 }, { 0x019F, 0x0000 }, 
    { 0x004F, 0x031B }, { 0x006F, 0x031B }, { 0x01A2, 0x0000 }, { 0x01A3, 0x0000 }, 
    { 0x01A4, 0x0000 }, { 0x01A5, 0x0000 }, { 0x01A6, 0x0000 }, { 0x01A7, 0x0000 }, 
    { 0x01A8, 0x0000 }, { 0x01A9, 0x0000 }, { 0x01AA, 0x0000 }, { 0x01AB, 0x0000 }, 
    { 0x01AC, 0x0000 }, { 0x01AD, 0x0000 }, { 0x01AE, 0x0000 }, { 0x0055, 0x031B }, 
    { 0x0075, 0x031B }, { 0x01B1, 0x0000 }, { 0x01B2, 0x0000 }, { 0x01B3, 0x0000 }, 
    { 0x01B4, 0x0000 }, { 0x01B5, 0x0000 }, { 0x01B6, 0x0000 }, { 0x01B7, 0x0000 }, 
    { 0x01B8, 0x0000 }, { 0x01B9, 0x0000 }, { 0x01BA, 0x0000 }, { 0x01BB, 0x0000 }, 
    { 0x01BC, 0x0000 }, { 0x01BD, 0x0000 }, { 0x01BE, 0x0000 }, { 0x01BF, 0x0000 }, 
    { 0x01C0, 0x0000 }, { 0x01C1, 0x0000 }, { 0x01C2, 0x0000 }, { 0x01C3, 0x0000 }, 
    { 0x0044, 0x017D }, { 0x0044, 0x017E }, { 0x0064, 0x017E }, { 0x004C, 0x004A }, 
    { 0x004C, 0x006A }, { 0x006C, 0x006A }, { 0x004E, 0x004A }, { 0x004E, 0x006A }, 
    { 0x006E, 0x006A }, { 0x0041, 0x030C }, { 0x0061, 0x030C }, { 0x0049, 0x030C }, 
    { 0x0069, 0x030C }, { 0x004F, 0x030C }, { 0x006F, 0x030C }, { 0x0055, 0x030C }, 
    { 0x0075, 0x030C }, { 0x00DC, 0x0304 }, { 0x00FC, 0x0304 }, { 0x00DC, 0x0301 }, 
    { 0x00FC, 0x0301 }, { 0x00DC, 0x030C }, { 0x00FC, 0x030C }, { 0x00DC, 0x0300 }, 
    { 0x00FC, 0x0300 }, { 0x01DD, 0x0000 }, { 0x00C4, 0x0304 }, { 0x00E4, 0x0304 }, 
    { 0x0226, 0x0304 }, { 0x0227, 0x0304 }, { 0x00C6, 0x0304 }, { 0x00E6, 0x0304 }, 
    { 0x01E4, 0x0000 }, { 0x01E5, 0x0000 }, { 0x0047, 0x030C }, { 0x0067, 0x030C }, 
    { 0x004B, 0x030C }, { 0x006B, 0x030C }, { 0x004F, 0x0328 }, { 0x006F, 0x0328 }, 
    { 0x01EA, 0x0304 }, { 0x01EB, 0x0304 }, { 0x01B7, 0x030C }, { 0x0292, 0x030C }, 
    { 0x006A, 0x030C }, { 0x0044, 0x005A }, { 0x0044, 0x007A }, { 0x0064, 0x007A }, 
    { 0x0047, 0x0301 }, { 0x0067, 0x0301 }, { 0x01F6, 0x0000 }, { 0x01F7, 0x0000 }, 
    { 0x004E, 0x0300 }, { 0x006E, 0x0300 }, { 0x00C5, 0x0301 }, { 0x00E5, 0x0301 }, 
    { 0x00C6, 0x0301 }, { 0x00E6, 0x0301 }, { 0x00D8, 0x0301 }, { 0x00F8, 0x0301 }, 
    { 0x0041, 0x030F }, { 0x0061, 0x030F }, { 0x0041, 0x0311 }, { 0x0061, 0x0311 }, 
    { 0x0045, 0x030F }, { 0x0065, 0x030F }, { 0x0045, 0x0311 }, { 0x0065, 0x0311 }, 
    { 0x0049, 0x030F }, { 0x0069, 0x030F }, { 0x0049, 0x0311 }, { 0x0069, 0x0311 }, 
    { 0x004F, 0x030F }, { 0x006F, 0x030F }, { 0x004F, 0x0311 }, { 0x006F, 0x0311 }, 
    { 0x0052, 0x030F }, { 0x0072, 0x030F }, { 0x0052, 0x0311 }, { 0x0072, 0x0311 }, 
    { 0x0055, 0x030F }, { 0x0075, 0x030F }, { 0x0055, 0x0311 }, { 0x0075, 0x0311 }, 
    { 0x0053, 0x0326 }, { 0x0073, 0x0326 }, { 0x0054, 0x0326 }, { 0x0074, 0x0326 }, 
    { 0x021C, 0x0000 }, { 0x021D, 0x0000 }, { 0x0048, 0x030C }, { 0x0068, 0x030C }, 
    { 0x0000, 0x0000 }, // spaceholder: this character is not defined
    { 0x0000, 0x0000 }, // spaceholder: this character is not defined
    { 0x0222, 0x0000 }, { 0x0223, 0x0000 }, { 0x0224, 0x0000 }, { 0x0225, 0x0000 }, 
    { 0x0041, 0x0307 }, { 0x0061, 0x0307 }, { 0x0045, 0x0327 }, { 0x0065, 0x0327 }, 
    { 0x00D6, 0x0304 }, { 0x00F6, 0x0304 }, { 0x00D5, 0x0304 }, { 0x00F5, 0x0304 }, 
    { 0x004F, 0x0307 }, { 0x006F, 0x0307 }, { 0x022E, 0x0304 }, { 0x022F, 0x0304 }, 
    { 0x0059, 0x0304 }, { 0x0079, 0x0304 }
  };

}; // namespace Unicode

dload(Unicode::Latin_Extended_B);
  
