/*
 * Plugin for Unicode Block "Latin Extended-A"
 * (0100-017F)
 * Unicode Version: 3.0
 *
 * by Tobias Hunger (Tobias_Hunger@gmx.de)
 * 
 */

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Dictionary.hh>

namespace Unicode {

  class Latin_Extended_A : public Unicode::Dictionary::Block {
  public:
    void clean() {
    };

    Latin_Extended_A() {
      my_first_letter=0x0100;
      my_last_letter =0x017F;
      // my_version="3.0"
    }
      
    ~Latin_Extended_A() {
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
      return Latin_Extended_A::upper[_uc - my_first_letter];
    }

    _Char lowercase(const _Char _uc) const {
      return Latin_Extended_A::lower[_uc - my_first_letter];
    }

    _Char titlecase(const _Char _uc) const {
      return Latin_Extended_A::title[_uc - my_first_letter];
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

    // Allways "Latin Extended-A"
    string blockname(const _Char _uc) const {
      return "Latin Extended-A";
    }

    Gen_Cat category(const _Char _uc) const {
      return Unicode::Gen_Cat(Latin_Extended_A::cat[_uc - my_first_letter]);
    }

    // Allways 0 with "Latin Extended-A"
    Can_Comb_Class combClass(const _Char _uc) const {
      return Unicode::Can_Comb_Class(0);
    }

    // Allways BIDIR_L with "Latin Extended-A"
    Bidir_Props bidirProps(const _Char _uc) const {
      return Unicode::BIDIR_L;
    }

    Char_Decomp decompType(const _Char _uc) const {
      switch (_uc) {
      case 0x0132:
      case 0x0133:
      case 0x013F:
      case 0x0140:
      case 0x0149:
      case 0x017F:
	return Unicode::DECOMP_COMPAT;
      default:
	return Unicode::DECOMP_NO_DECOMP;
      }
    }

    String decompString(const _Char _uc) const {
      Unicode::String us;
      us.resize(2);
      us[0]=Latin_Extended_A::decompStr[_uc - my_first_letter][0];
      us[1]=Latin_Extended_A::decompStr[_uc - my_first_letter][1];  
      if (us[1] == 0x0000)
	us.resize(1);
      return us;
    }

    // Allways False with Latin Extended-A
    bool mustMirror(const _Char _uc) const {
      return 0;
    }

    EA_Width EAWidth(const _Char _uc) const {
      return Unicode::EA_Width(Latin_Extended_A::ea[_uc - my_first_letter]);
    }

    Line_Break linebreak(const _Char _uc) const {
      return Unicode::Line_Break(Latin_Extended_A::lb[_uc - my_first_letter]);
    }
    
  private:
    // Functions
    Latin_Extended_A(const Latin_Extended_A &) {}

    Unicode::_Char my_first_letter;
    Unicode::_Char my_last_letter;

    static const Unicode::_Char upper[128];
    static const Unicode::_Char lower[128];
    static const Unicode::_Char title[128];
    static const unsigned char cat[128];
    static const unsigned char ea[128];
    static const unsigned char lb[128];
    static const Unicode::_Char decompStr[128][2];
  }; // class Latin1_Supplement

  const Unicode::_Char Latin_Extended_A::upper[] = {
    0x0100, 0x0100, 0x0102, 0x0102, 0x0104, 0x0104, 0x0106, 0x0106, 
    0x0108, 0x0108, 0x010A, 0x010A, 0x010C, 0x010C, 0x010E, 0x010E, 
    0x0110, 0x0110, 0x0112, 0x0112, 0x0114, 0x0114, 0x0116, 0x0116, 
    0x0118, 0x0118, 0x011A, 0x011A, 0x011C, 0x011C, 0x011E, 0x011E, 
    0x0120, 0x0120, 0x0122, 0x0122, 0x0124, 0x0124, 0x0126, 0x0126, 
    0x0128, 0x0128, 0x012A, 0x012A, 0x012C, 0x012C, 0x012E, 0x012E, 
    0x0130, 0x0049, 0x0132, 0x0132, 0x0134, 0x0134, 0x0136, 0x0136, 
    0x0138, 0x0139, 0x0139, 0x013B, 0x013B, 0x013D, 0x013D, 0x013F, 
    0x013F, 0x0141, 0x0141, 0x0143, 0x0143, 0x0145, 0x0145, 0x0147, 
    0x0147, 0x0149, 0x014A, 0x014A, 0x014C, 0x014C, 0x014E, 0x014E, 
    0x0150, 0x0150, 0x0152, 0x0152, 0x0154, 0x0154, 0x0156, 0x0156, 
    0x0158, 0x0158, 0x015A, 0x015A, 0x015C, 0x015C, 0x015E, 0x015E, 
    0x0160, 0x0160, 0x0162, 0x0162, 0x0164, 0x0164, 0x0166, 0x0166, 
    0x0168, 0x0168, 0x016A, 0x016A, 0x016C, 0x016C, 0x016E, 0x016E, 
    0x0170, 0x0170, 0x0172, 0x0172, 0x0174, 0x0174, 0x0176, 0x0176, 
    0x0178, 0x0179, 0x0179, 0x017B, 0x017B, 0x017D, 0x017D, 0x0053
  };

   const Unicode::_Char Latin_Extended_A::lower[] = {
     0x0101, 0x0101, 0x0103, 0x0103, 0x0105, 0x0105, 0x0107, 0x0107, 
     0x0109, 0x0109, 0x010B, 0x010B, 0x010D, 0x010D, 0x010F, 0x010F, 
     0x0111, 0x0111, 0x0113, 0x0113, 0x0115, 0x0115, 0x0117, 0x0117, 
     0x0119, 0x0119, 0x011B, 0x011B, 0x011D, 0x011D, 0x011F, 0x011F, 
     0x0121, 0x0121, 0x0123, 0x0123, 0x0125, 0x0125, 0x0127, 0x0127, 
     0x0129, 0x0129, 0x012B, 0x012B, 0x012D, 0x012D, 0x012F, 0x012F, 
     0x0069, 0x0131, 0x0133, 0x0133, 0x0135, 0x0135, 0x0137, 0x0137, 
     0x0138, 0x013A, 0x013A, 0x013C, 0x013C, 0x013E, 0x013E, 0x0140, 
     0x0140, 0x0142, 0x0142, 0x0144, 0x0144, 0x0146, 0x0146, 0x0148, 
     0x0148, 0x0149, 0x014B, 0x014B, 0x014D, 0x014D, 0x014F, 0x014F, 
     0x0151, 0x0151, 0x0153, 0x0153, 0x0155, 0x0155, 0x0157, 0x0157, 
     0x0159, 0x0159, 0x015B, 0x015B, 0x015D, 0x015D, 0x015F, 0x015F, 
     0x0161, 0x0161, 0x0163, 0x0163, 0x0165, 0x0165, 0x0167, 0x0167, 
     0x0169, 0x0169, 0x016B, 0x016B, 0x016D, 0x016D, 0x016F, 0x016F, 
     0x0171, 0x0171, 0x0173, 0x0173, 0x0175, 0x0175, 0x0177, 0x0177, 
     0x00FF, 0x017A, 0x017A, 0x017C, 0x017C, 0x017E, 0x017E, 0x017F 
  };

  const Unicode::_Char Latin_Extended_A::title[] = {
    0x0100, 0x0100, 0x0102, 0x0102, 0x0104, 0x0104, 0x0106, 0x0106, 
    0x0108, 0x0108, 0x010A, 0x010A, 0x010C, 0x010C, 0x010E, 0x010E, 
    0x0110, 0x0110, 0x0112, 0x0112, 0x0114, 0x0114, 0x0116, 0x0116, 
    0x0118, 0x0118, 0x011A, 0x011A, 0x011C, 0x011C, 0x011E, 0x011E, 
    0x0120, 0x0120, 0x0122, 0x0122, 0x0124, 0x0124, 0x0126, 0x0126, 
    0x0128, 0x0128, 0x012A, 0x012A, 0x012C, 0x012C, 0x012E, 0x012E, 
    0x0130, 0x0049, 0x0132, 0x0132, 0x0134, 0x0134, 0x0136, 0x0136, 
    0x0138, 0x0139, 0x0139, 0x013B, 0x013B, 0x013D, 0x013D, 0x013F, 
    0x013F, 0x0141, 0x0141, 0x0143, 0x0143, 0x0145, 0x0145, 0x0147, 
    0x0147, 0x0149, 0x014A, 0x014A, 0x014C, 0x014C, 0x014E, 0x014E, 
    0x0150, 0x0150, 0x0152, 0x0152, 0x0154, 0x0154, 0x0156, 0x0156, 
    0x0158, 0x0158, 0x015A, 0x015A, 0x015C, 0x015C, 0x015E, 0x015E, 
    0x0160, 0x0160, 0x0162, 0x0162, 0x0164, 0x0164, 0x0166, 0x0166, 
    0x0168, 0x0168, 0x016A, 0x016A, 0x016C, 0x016C, 0x016E, 0x016E, 
    0x0170, 0x0170, 0x0172, 0x0172, 0x0174, 0x0174, 0x0176, 0x0176, 
    0x0178, 0x0179, 0x0179, 0x017B, 0x017B, 0x017D, 0x017D, 0x0053 
  };


  const unsigned char Latin_Extended_A::cat[]= {
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, 
    CAT_Ll, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, 
    CAT_Lu, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Lu, CAT_Ll, CAT_Ll
  };
  
  const unsigned char Latin_Extended_A::ea[] = {
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

  const unsigned char Latin_Extended_A::lb[] = {
    LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AI, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, 
    LB_AI, LB_AI, LB_AI, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AI, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AI, LB_AI, 
    LB_AL, LB_AL, LB_AL, LB_AI, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

  const Unicode::_Char Latin_Extended_A::decompStr[][2] = {
    { 0x0041, 0x0304 }, { 0x0061, 0x0304 }, { 0x0041, 0x0306 }, { 0x0061, 0x0306 }, 
    { 0x0041, 0x0328 }, { 0x0061, 0x0328 }, { 0x0043, 0x0301 }, { 0x0063, 0x0301 }, 
    { 0x0043, 0x0302 }, { 0x0063, 0x0302 }, { 0x0043, 0x0307 }, { 0x0063, 0x0307 }, 
    { 0x0043, 0x030C }, { 0x0063, 0x030C }, { 0x0044, 0x030C }, { 0x0064, 0x030C }, 
    { 0x0110, 0x0000 }, { 0x0111, 0x0000 }, { 0x0045, 0x0304 }, { 0x0065, 0x0304 }, 
    { 0x0045, 0x0306 }, { 0x0065, 0x0306 }, { 0x0045, 0x0307 }, { 0x0065, 0x0307 }, 
    { 0x0045, 0x0328 }, { 0x0065, 0x0328 }, { 0x0045, 0x030C }, { 0x0065, 0x030C }, 
    { 0x0047, 0x0302 }, { 0x0067, 0x0302 }, { 0x0047, 0x0306 }, { 0x0067, 0x0306 }, 
    { 0x0047, 0x0307 }, { 0x0067, 0x0307 }, { 0x0047, 0x0327 }, { 0x0067, 0x0327 }, 
    { 0x0048, 0x0302 }, { 0x0068, 0x0302 }, { 0x0126, 0x0000 }, { 0x0127, 0x0000 }, 
    { 0x0049, 0x0303 }, { 0x0069, 0x0303 }, { 0x0049, 0x0304 }, { 0x0069, 0x0304 }, 
    { 0x0049, 0x0306 }, { 0x0069, 0x0306 }, { 0x0049, 0x0328 }, { 0x0069, 0x0328 }, 
    { 0x0049, 0x0307 }, { 0x0131, 0x0000 }, { 0x0049, 0x004A }, { 0x0069, 0x006A }, 
    { 0x004A, 0x0302 }, { 0x006A, 0x0302 }, { 0x004B, 0x0327 }, { 0x006B, 0x0327 }, 
    { 0x0138, 0x0000 }, { 0x004C, 0x0301 }, { 0x006C, 0x0301 }, { 0x004C, 0x0327 }, 
    { 0x006C, 0x0327 }, { 0x004C, 0x030C }, { 0x006C, 0x030C }, { 0x004C, 0x00B7 }, 
    { 0x006C, 0x00B7 }, { 0x0141, 0x0000 }, { 0x0142, 0x0000 }, { 0x004E, 0x0301 }, 
    { 0x006E, 0x0301 }, { 0x004E, 0x0327 }, { 0x006E, 0x0327 }, { 0x004E, 0x030C }, 
    { 0x006E, 0x030C }, { 0x02BC, 0x006E }, { 0x014A, 0x0000 }, { 0x014B, 0x0000 }, 
    { 0x004F, 0x0304 }, { 0x006F, 0x0304 }, { 0x004F, 0x0306 }, { 0x006F, 0x0306 }, 
    { 0x004F, 0x030B }, { 0x006F, 0x030B }, { 0x0152, 0x0000 }, { 0x0153, 0x0000 }, 
    { 0x0052, 0x0301 }, { 0x0072, 0x0301 }, { 0x0052, 0x0327 }, { 0x0072, 0x0327 }, 
    { 0x0052, 0x030C }, { 0x0072, 0x030C }, { 0x0053, 0x0301 }, { 0x0073, 0x0301 }, 
    { 0x0053, 0x0302 }, { 0x0073, 0x0302 }, { 0x0053, 0x0327 }, { 0x0073, 0x0327 }, 
    { 0x0053, 0x030C }, { 0x0073, 0x030C }, { 0x0054, 0x0327 }, { 0x0074, 0x0327 }, 
    { 0x0054, 0x030C }, { 0x0074, 0x030C }, { 0x0166, 0x0000 }, { 0x0167, 0x0000 }, 
    { 0x0055, 0x0303 }, { 0x0075, 0x0303 }, { 0x0055, 0x0304 }, { 0x0075, 0x0304 }, 
    { 0x0055, 0x0306 }, { 0x0075, 0x0306 }, { 0x0055, 0x030A }, { 0x0075, 0x030A }, 
    { 0x0055, 0x030B }, { 0x0075, 0x030B }, { 0x0055, 0x0328 }, { 0x0075, 0x0328 }, 
    { 0x0057, 0x0302 }, { 0x0077, 0x0302 }, { 0x0059, 0x0302 }, { 0x0079, 0x0302 }, 
    { 0x0059, 0x0308 }, { 0x005A, 0x0301 }, { 0x007A, 0x0301 }, { 0x005A, 0x0307 }, 
    { 0x007A, 0x0307 }, { 0x005A, 0x030C }, { 0x007A, 0x030C }, { 0x0073, 0x0000 }
  };

}; // namespace Unicode

dload(Unicode::Latin_Extended_A);
  
