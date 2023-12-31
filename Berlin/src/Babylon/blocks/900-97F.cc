/*$Id: 900-97F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:46:44 +0200.
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

  class Devanagari900 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Devanagari900() {
      m_first_letter = 0x900;
      m_last_letter  = 0x97F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000915, 0x0000093C)] = 0x0958;
      m_composeMap[make_pair(0x00000916, 0x0000093C)] = 0x0959;
      m_composeMap[make_pair(0x00000917, 0x0000093C)] = 0x095A;
      m_composeMap[make_pair(0x0000091C, 0x0000093C)] = 0x095B;
      m_composeMap[make_pair(0x00000921, 0x0000093C)] = 0x095C;
      m_composeMap[make_pair(0x00000922, 0x0000093C)] = 0x095D;
      m_composeMap[make_pair(0x00000928, 0x0000093C)] = 0x0929;
      m_composeMap[make_pair(0x0000092B, 0x0000093C)] = 0x095E;
      m_composeMap[make_pair(0x0000092F, 0x0000093C)] = 0x095F;
      m_composeMap[make_pair(0x00000930, 0x0000093C)] = 0x0931;
      m_composeMap[make_pair(0x00000933, 0x0000093C)] = 0x0934;

    }


    ~Devanagari900() {
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
      return "Devanagari";
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
      case 0x0966u:
        return 0;
        break;
      case 0x0967u:
        return 1;
        break;
      case 0x0968u:
        return 2;
        break;
      case 0x0969u:
        return 3;
        break;
      case 0x096Au:
        return 4;
        break;
      case 0x096Bu:
        return 5;
        break;
      case 0x096Cu:
        return 6;
        break;
      case 0x096Du:
        return 7;
        break;
      case 0x096Eu:
        return 8;
        break;
      case 0x096Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0966u:
      case 0x0967u:
      case 0x0968u:
      case 0x0969u:
      case 0x096Au:
      case 0x096Bu:
      case 0x096Cu:
      case 0x096Du:
      case 0x096Eu:
      case 0x096Fu:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0966u:
        return 0;
        break;
      case 0x0967u:
        return 1;
        break;
      case 0x0968u:
        return 2;
        break;
      case 0x0969u:
        return 3;
        break;
      case 0x096Au:
        return 4;
        break;
      case 0x096Bu:
        return 5;
        break;
      case 0x096Cu:
        return 6;
        break;
      case 0x096Du:
        return 7;
        break;
      case 0x096Eu:
        return 8;
        break;
      case 0x096Fu:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0966u:
      case 0x0967u:
      case 0x0968u:
      case 0x0969u:
      case 0x096Au:
      case 0x096Bu:
      case 0x096Cu:
      case 0x096Du:
      case 0x096Eu:
      case 0x096Fu:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0966u:
        return 0.000000;
        break;
      case 0x0967u:
        return 1.000000;
        break;
      case 0x0968u:
        return 2.000000;
        break;
      case 0x0969u:
        return 3.000000;
        break;
      case 0x096Au:
        return 4.000000;
        break;
      case 0x096Bu:
        return 5.000000;
        break;
      case 0x096Cu:
        return 6.000000;
        break;
      case 0x096Du:
        return 7.000000;
        break;
      case 0x096Eu:
        return 8.000000;
        break;
      case 0x096Fu:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0966u:
      case 0x0967u:
      case 0x0968u:
      case 0x0969u:
      case 0x096Au:
      case 0x096Bu:
      case 0x096Cu:
      case 0x096Du:
      case 0x096Eu:
      case 0x096Fu:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Devanagari900::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Devanagari900::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Devanagari900::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Devanagari900::m_decompStr[uc - m_first_letter][0];
      us[1] = Devanagari900::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Devanagari900::m_lb[uc - m_first_letter]);
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
      return m_Terminal_Punctuation.test(uc - m_first_letter);
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
      return m_Diacritic.test(uc - m_first_letter);
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
    Devanagari900(const Devanagari900 &) {}

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
    static const std::bitset<128> m_Terminal_Punctuation;
    static const std::bitset<128> m_Other_Alphabetic;
    static const std::bitset<128> m_Diacritic;

  }; // class Devanagari900

    const std::bitset<128> Devanagari900::m_is_defined(std::string("00000000000000011111111111111111111111110001111100111111111111111111001111111111111111111111111111111111111111111111111111101110"));

  const unsigned char Devanagari900::_cat[] = {
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mn, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Lo, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mn, CAT_Mn, CAT_Po, CAT_Po, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Po, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, 
    CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mn
  };

  const unsigned char Devanagari900::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 7, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 9, 0, 0, 
    0, 230, 220, 230, 230, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char Devanagari900::m_bidir[] = {
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, 
    BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM
  };

  const UCS2 Devanagari900::m_decompStr[][2] = {
    { 0x0900u, 0x0000u }, { 0x0901u, 0x0000u }, { 0x0902u, 0x0000u }, { 0x0903u, 0x0000u }, 
    { 0x0904u, 0x0000u }, { 0x0905u, 0x0000u }, { 0x0906u, 0x0000u }, { 0x0907u, 0x0000u }, 
    { 0x0908u, 0x0000u }, { 0x0909u, 0x0000u }, { 0x090Au, 0x0000u }, { 0x090Bu, 0x0000u }, 
    { 0x090Cu, 0x0000u }, { 0x090Du, 0x0000u }, { 0x090Eu, 0x0000u }, { 0x090Fu, 0x0000u }, 
    { 0x0910u, 0x0000u }, { 0x0911u, 0x0000u }, { 0x0912u, 0x0000u }, { 0x0913u, 0x0000u }, 
    { 0x0914u, 0x0000u }, { 0x0915u, 0x0000u }, { 0x0916u, 0x0000u }, { 0x0917u, 0x0000u }, 
    { 0x0918u, 0x0000u }, { 0x0919u, 0x0000u }, { 0x091Au, 0x0000u }, { 0x091Bu, 0x0000u }, 
    { 0x091Cu, 0x0000u }, { 0x091Du, 0x0000u }, { 0x091Eu, 0x0000u }, { 0x091Fu, 0x0000u }, 
    { 0x0920u, 0x0000u }, { 0x0921u, 0x0000u }, { 0x0922u, 0x0000u }, { 0x0923u, 0x0000u }, 
    { 0x0924u, 0x0000u }, { 0x0925u, 0x0000u }, { 0x0926u, 0x0000u }, { 0x0927u, 0x0000u }, 
    { 0x0928u, 0x0000u }, { 0x0928u, 0x093Cu }, { 0x092Au, 0x0000u }, { 0x092Bu, 0x0000u }, 
    { 0x092Cu, 0x0000u }, { 0x092Du, 0x0000u }, { 0x092Eu, 0x0000u }, { 0x092Fu, 0x0000u }, 
    { 0x0930u, 0x0000u }, { 0x0930u, 0x093Cu }, { 0x0932u, 0x0000u }, { 0x0933u, 0x0000u }, 
    { 0x0933u, 0x093Cu }, { 0x0935u, 0x0000u }, { 0x0936u, 0x0000u }, { 0x0937u, 0x0000u }, 
    { 0x0938u, 0x0000u }, { 0x0939u, 0x0000u }, { 0x093Au, 0x0000u }, { 0x093Bu, 0x0000u }, 
    { 0x093Cu, 0x0000u }, { 0x093Du, 0x0000u }, { 0x093Eu, 0x0000u }, { 0x093Fu, 0x0000u }, 
    { 0x0940u, 0x0000u }, { 0x0941u, 0x0000u }, { 0x0942u, 0x0000u }, { 0x0943u, 0x0000u }, 
    { 0x0944u, 0x0000u }, { 0x0945u, 0x0000u }, { 0x0946u, 0x0000u }, { 0x0947u, 0x0000u }, 
    { 0x0948u, 0x0000u }, { 0x0949u, 0x0000u }, { 0x094Au, 0x0000u }, { 0x094Bu, 0x0000u }, 
    { 0x094Cu, 0x0000u }, { 0x094Du, 0x0000u }, { 0x094Eu, 0x0000u }, { 0x094Fu, 0x0000u }, 
    { 0x0950u, 0x0000u }, { 0x0951u, 0x0000u }, { 0x0952u, 0x0000u }, { 0x0953u, 0x0000u }, 
    { 0x0954u, 0x0000u }, { 0x0955u, 0x0000u }, { 0x0956u, 0x0000u }, { 0x0957u, 0x0000u }, 
    { 0x0915u, 0x093Cu }, { 0x0916u, 0x093Cu }, { 0x0917u, 0x093Cu }, { 0x091Cu, 0x093Cu }, 
    { 0x0921u, 0x093Cu }, { 0x0922u, 0x093Cu }, { 0x092Bu, 0x093Cu }, { 0x092Fu, 0x093Cu }, 
    { 0x0960u, 0x0000u }, { 0x0961u, 0x0000u }, { 0x0962u, 0x0000u }, { 0x0963u, 0x0000u }, 
    { 0x0964u, 0x0000u }, { 0x0965u, 0x0000u }, { 0x0966u, 0x0000u }, { 0x0967u, 0x0000u }, 
    { 0x0968u, 0x0000u }, { 0x0969u, 0x0000u }, { 0x096Au, 0x0000u }, { 0x096Bu, 0x0000u }, 
    { 0x096Cu, 0x0000u }, { 0x096Du, 0x0000u }, { 0x096Eu, 0x0000u }, { 0x096Fu, 0x0000u }, 
    { 0x0970u, 0x0000u }, { 0x0971u, 0x0000u }, { 0x0972u, 0x0000u }, { 0x0973u, 0x0000u }, 
    { 0x0974u, 0x0000u }, { 0x0975u, 0x0000u }, { 0x0976u, 0x0000u }, { 0x0977u, 0x0000u }, 
    { 0x0978u, 0x0000u }, { 0x0979u, 0x0000u }, { 0x097Au, 0x0000u }, { 0x097Bu, 0x0000u }, 
    { 0x097Cu, 0x0000u }, { 0x097Du, 0x0000u }, { 0x097Eu, 0x0000u }, { 0x097Fu, 0x0000u }
  };

  const unsigned char Devanagari900::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_CM, LB_AL, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_CM, LB_AL, LB_AL, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_AL, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> Devanagari900::m_Terminal_Punctuation(std::string("00000000000000000000000000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<128> Devanagari900::m_Other_Alphabetic(std::string("00000000000000000000000000001100000000000000000000011111111111111100000000000000000000000000000000000000000000000000000000000110"));

    const std::bitset<128> Devanagari900::m_Diacritic(std::string("00000000000000000000000000000000000000000001111000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Devanagari900);
