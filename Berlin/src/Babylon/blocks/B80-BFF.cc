/*$Id: B80-BFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:04 +0200.
 *
 * This plugin to libPrague is free software; you can redistribute it
 * and/or  modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA
 */

#include <Babylon/defs.hh>
#include <Babylon/Dictionary.hh>
#include <bitset>
#include <map>

namespace Babylon {

  class TamilB80 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    TamilB80() {
      m_first_letter = 0xB80;
      m_last_letter  = 0xBFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000B92, 0x00000BD7)] = 0x0B94;
      m_composeMap[make_pair(0x00000BC6, 0x00000BBE)] = 0x0BCA;
      m_composeMap[make_pair(0x00000BC6, 0x00000BD7)] = 0x0BCC;
      m_composeMap[make_pair(0x00000BC7, 0x00000BBE)] = 0x0BCB;

    }


    ~TamilB80() {
    }

    UCS4 first_letter() const {
      return m_first_letter;
    }

    UCS4 last_letter() const {
      return m_last_letter;
    }

    bool is_undef_block() const {
      return 0;
    }

    // query functions:

    std::string blockname(const UCS4 uc) const {
      return "Tamil";
    }

    bool is_defined(const UCS4 uc) const {
      return (m_is_defined.test(uc - m_first_letter));
    }

    UCS4 uppercase(const UCS4 uc) const {
      return uc;
    }

    UCS4 lowercase(const UCS4 uc) const {
      return uc;
    }

    UCS4 titlecase(const UCS4 uc) const {
      return uc;
    }

    int dec_digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0BE7u:
        return 1;
        break;
      case 0x0BE8u:
        return 2;
        break;
      case 0x0BE9u:
        return 3;
        break;
      case 0x0BEAu:
        return 4;
        break;
      case 0x0BEBu:
        return 5;
        break;
      case 0x0BECu:
        return 6;
        break;
      case 0x0BEDu:
        return 7;
        break;
      case 0x0BEEu:
        return 8;
        break;
      case 0x0BEFu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0BE7u:
      case 0x0BE8u:
      case 0x0BE9u:
      case 0x0BEAu:
      case 0x0BEBu:
      case 0x0BECu:
      case 0x0BEDu:
      case 0x0BEEu:
      case 0x0BEFu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0BE7u:
        return 1;
        break;
      case 0x0BE8u:
        return 2;
        break;
      case 0x0BE9u:
        return 3;
        break;
      case 0x0BEAu:
        return 4;
        break;
      case 0x0BEBu:
        return 5;
        break;
      case 0x0BECu:
        return 6;
        break;
      case 0x0BEDu:
        return 7;
        break;
      case 0x0BEEu:
        return 8;
        break;
      case 0x0BEFu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0BE7u:
      case 0x0BE8u:
      case 0x0BE9u:
      case 0x0BEAu:
      case 0x0BEBu:
      case 0x0BECu:
      case 0x0BEDu:
      case 0x0BEEu:
      case 0x0BEFu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0BE7u:
        return 1.000000;
        break;
      case 0x0BE8u:
        return 2.000000;
        break;
      case 0x0BE9u:
        return 3.000000;
        break;
      case 0x0BEAu:
        return 4.000000;
        break;
      case 0x0BEBu:
        return 5.000000;
        break;
      case 0x0BECu:
        return 6.000000;
        break;
      case 0x0BEDu:
        return 7.000000;
        break;
      case 0x0BEEu:
        return 8.000000;
        break;
      case 0x0BEFu:
        return 9.000000;
        break;
      case 0x0BF0u:
        return 10.000000;
        break;
      case 0x0BF1u:
        return 100.000000;
        break;
      case 0x0BF2u:
        return 1000.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0BE7u:
      case 0x0BE8u:
      case 0x0BE9u:
      case 0x0BEAu:
      case 0x0BEBu:
      case 0x0BECu:
      case 0x0BEDu:
      case 0x0BEEu:
      case 0x0BEFu:
      case 0x0BF0u:
      case 0x0BF1u:
      case 0x0BF2u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(TamilB80::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(TamilB80::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(TamilB80::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = TamilB80::m_decompStr[uc - m_first_letter][0];
      us[1] = TamilB80::m_decompStr[uc - m_first_letter][1];
      if (us[1] == 0x0000u) {
        us.resize(1);
      }

      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return 0;
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(TamilB80::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_N);
    }

    UCS4 compose (const UCS4 start, const UCS4 last) {
      return m_composeMap[make_pair(start, last)];
    }

    bool is_White_space(const UCS4 uc) const {
      return 0;
    }

    bool is_Bidi_Control(const UCS4 uc) const {
      return 0;
    }

    bool is_Join_Control(const UCS4 uc) const {
      return 0;
    }

    bool is_Dash(const UCS4 uc) const {
      return 0;
    }

    bool is_Hyphen(const UCS4 uc) const {
      return 0;
    }

    bool is_Quotation_Mark(const UCS4 uc) const {
      return 0;
    }

    bool is_Terminal_Punctuation(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Math(const UCS4 uc) const {
      return 0;
    }

    bool is_Hex_Digit(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Alphabetic(const UCS4 uc) const {
      return m_Other_Alphabetic.test(uc - m_first_letter);
    }

    bool is_Ideographic(const UCS4 uc) const {
      return 0;
    }

    bool is_Diacritic(const UCS4 uc) const {
      return 0;
    }

    bool is_Extender(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Lowercase(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Uppercase(const UCS4 uc) const {
      return 0;
    }

    bool is_Noncharacter_Code_Point(const UCS4 uc) const {
      return 0;
    }


  private:
    // functions
    TamilB80(const TamilB80 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<128> m_is_defined;
    static const unsigned char _cat[128];
    static const unsigned char _comb_cl[128];
    static const unsigned char m_bidir[128];
    static const UCS2 m_decompStr[128][2];
    static const unsigned char m_lb[128];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<128> m_Other_Alphabetic;

  }; // class TamilB80

    const std::bitset<128> TamilB80::m_is_defined(std::string("00000000000001111111111110000000000000001000000000111101110001111100001110111111110001110001100011010110001111011100011111101100"));

  const unsigned char TamilB80::_cat[] = {
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Lo, CAT_Mn, CAT_Lo, CAT_Lo, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mn, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_No, CAT_No, CAT_No, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn
  };

  const unsigned char TamilB80::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 9, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char TamilB80::m_bidir[] = {
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM
  };

  const UCS2 TamilB80::m_decompStr[][2] = {
    { 0x0B80u, 0x0000u }, { 0x0B81u, 0x0000u }, { 0x0B82u, 0x0000u }, { 0x0B83u, 0x0000u }, 
    { 0x0B84u, 0x0000u }, { 0x0B85u, 0x0000u }, { 0x0B86u, 0x0000u }, { 0x0B87u, 0x0000u }, 
    { 0x0B88u, 0x0000u }, { 0x0B89u, 0x0000u }, { 0x0B8Au, 0x0000u }, { 0x0B8Bu, 0x0000u }, 
    { 0x0B8Cu, 0x0000u }, { 0x0B8Du, 0x0000u }, { 0x0B8Eu, 0x0000u }, { 0x0B8Fu, 0x0000u }, 
    { 0x0B90u, 0x0000u }, { 0x0B91u, 0x0000u }, { 0x0B92u, 0x0000u }, { 0x0B93u, 0x0000u }, 
    { 0x0B92u, 0x0BD7u }, { 0x0B95u, 0x0000u }, { 0x0B96u, 0x0000u }, { 0x0B97u, 0x0000u }, 
    { 0x0B98u, 0x0000u }, { 0x0B99u, 0x0000u }, { 0x0B9Au, 0x0000u }, { 0x0B9Bu, 0x0000u }, 
    { 0x0B9Cu, 0x0000u }, { 0x0B9Du, 0x0000u }, { 0x0B9Eu, 0x0000u }, { 0x0B9Fu, 0x0000u }, 
    { 0x0BA0u, 0x0000u }, { 0x0BA1u, 0x0000u }, { 0x0BA2u, 0x0000u }, { 0x0BA3u, 0x0000u }, 
    { 0x0BA4u, 0x0000u }, { 0x0BA5u, 0x0000u }, { 0x0BA6u, 0x0000u }, { 0x0BA7u, 0x0000u }, 
    { 0x0BA8u, 0x0000u }, { 0x0BA9u, 0x0000u }, { 0x0BAAu, 0x0000u }, { 0x0BABu, 0x0000u }, 
    { 0x0BACu, 0x0000u }, { 0x0BADu, 0x0000u }, { 0x0BAEu, 0x0000u }, { 0x0BAFu, 0x0000u }, 
    { 0x0BB0u, 0x0000u }, { 0x0BB1u, 0x0000u }, { 0x0BB2u, 0x0000u }, { 0x0BB3u, 0x0000u }, 
    { 0x0BB4u, 0x0000u }, { 0x0BB5u, 0x0000u }, { 0x0BB6u, 0x0000u }, { 0x0BB7u, 0x0000u }, 
    { 0x0BB8u, 0x0000u }, { 0x0BB9u, 0x0000u }, { 0x0BBAu, 0x0000u }, { 0x0BBBu, 0x0000u }, 
    { 0x0BBCu, 0x0000u }, { 0x0BBDu, 0x0000u }, { 0x0BBEu, 0x0000u }, { 0x0BBFu, 0x0000u }, 
    { 0x0BC0u, 0x0000u }, { 0x0BC1u, 0x0000u }, { 0x0BC2u, 0x0000u }, { 0x0BC3u, 0x0000u }, 
    { 0x0BC4u, 0x0000u }, { 0x0BC5u, 0x0000u }, { 0x0BC6u, 0x0000u }, { 0x0BC7u, 0x0000u }, 
    { 0x0BC8u, 0x0000u }, { 0x0BC9u, 0x0000u }, { 0x0BC6u, 0x0BBEu }, { 0x0BC7u, 0x0BBEu }, 
    { 0x0BC6u, 0x0BD7u }, { 0x0BCDu, 0x0000u }, { 0x0BCEu, 0x0000u }, { 0x0BCFu, 0x0000u }, 
    { 0x0BD0u, 0x0000u }, { 0x0BD1u, 0x0000u }, { 0x0BD2u, 0x0000u }, { 0x0BD3u, 0x0000u }, 
    { 0x0BD4u, 0x0000u }, { 0x0BD5u, 0x0000u }, { 0x0BD6u, 0x0000u }, { 0x0BD7u, 0x0000u }, 
    { 0x0BD8u, 0x0000u }, { 0x0BD9u, 0x0000u }, { 0x0BDAu, 0x0000u }, { 0x0BDBu, 0x0000u }, 
    { 0x0BDCu, 0x0000u }, { 0x0BDDu, 0x0000u }, { 0x0BDEu, 0x0000u }, { 0x0BDFu, 0x0000u }, 
    { 0x0BE0u, 0x0000u }, { 0x0BE1u, 0x0000u }, { 0x0BE2u, 0x0000u }, { 0x0BE3u, 0x0000u }, 
    { 0x0BE4u, 0x0000u }, { 0x0BE5u, 0x0000u }, { 0x0BE6u, 0x0000u }, { 0x0BE7u, 0x0000u }, 
    { 0x0BE8u, 0x0000u }, { 0x0BE9u, 0x0000u }, { 0x0BEAu, 0x0000u }, { 0x0BEBu, 0x0000u }, 
    { 0x0BECu, 0x0000u }, { 0x0BEDu, 0x0000u }, { 0x0BEEu, 0x0000u }, { 0x0BEFu, 0x0000u }, 
    { 0x0BF0u, 0x0000u }, { 0x0BF1u, 0x0000u }, { 0x0BF2u, 0x0000u }, { 0x0BF3u, 0x0000u }, 
    { 0x0BF4u, 0x0000u }, { 0x0BF5u, 0x0000u }, { 0x0BF6u, 0x0000u }, { 0x0BF7u, 0x0000u }, 
    { 0x0BF8u, 0x0000u }, { 0x0BF9u, 0x0000u }, { 0x0BFAu, 0x0000u }, { 0x0BFBu, 0x0000u }, 
    { 0x0BFCu, 0x0000u }, { 0x0BFDu, 0x0000u }, { 0x0BFEu, 0x0000u }, { 0x0BFFu, 0x0000u }
  };

  const unsigned char TamilB80::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, 
    LB_CM, LB_AL, LB_AL, LB_CM, LB_AL, LB_CM, LB_AL, LB_AL, 
    LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_AL, LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> TamilB80::m_Other_Alphabetic(std::string("00000000000000000000000000000000000000000000000000011101110001101100000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::TamilB80);
