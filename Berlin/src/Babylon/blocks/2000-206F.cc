/*$Id: 2000-206F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:48:38 +0200.
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

namespace Babylon {

  class General_Punctuation2000 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    General_Punctuation2000() {
      m_first_letter = 0x2000;
      m_last_letter  = 0x206F;
      // m_version="3.1" // Not yet supported!

    }


    ~General_Punctuation2000() {
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
      return "General Punctuation";
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
      return 0;
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      return 0;
    }

    int digit_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Digit(const UCS4 uc) const {
      return 0;
    }

    float numeric_value(const UCS4 uc) const {
      return 0;
    }

    bool is_Numeric(const UCS4 uc) const {
      return 0;
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(General_Punctuation2000::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(General_Punctuation2000::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(General_Punctuation2000::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = General_Punctuation2000::m_decompStr[uc - m_first_letter][0];
      us[1] = General_Punctuation2000::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0x2026:
        us.resize(3);
        us[2u] = 0x002Eu;
        break;

      case 0x2034:
        us.resize(3);
        us[2u] = 0x2032u;
        break;

      case 0x2037:
        us.resize(3);
        us[2u] = 0x2035u;
        break;
      }
      if (us[1] == 0x0000u) {
        us.resize(1);
      }

      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return m_mirror.test(uc - m_first_letter);
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(General_Punctuation2000::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(General_Punctuation2000::m_ea[uc - m_first_letter]);
    }

    UCS4 compose (const UCS4 start, const UCS4 last) {
      return 0;
    }

    bool is_White_space(const UCS4 uc) const {
      return m_White_space.test(uc - m_first_letter);
    }

    bool is_Bidi_Control(const UCS4 uc) const {
      return m_Bidi_Control.test(uc - m_first_letter);
    }

    bool is_Join_Control(const UCS4 uc) const {
      return m_Join_Control.test(uc - m_first_letter);
    }

    bool is_Dash(const UCS4 uc) const {
      return m_Dash.test(uc - m_first_letter);
    }

    bool is_Hyphen(const UCS4 uc) const {
      return m_Hyphen.test(uc - m_first_letter);
    }

    bool is_Quotation_Mark(const UCS4 uc) const {
      return m_Quotation_Mark.test(uc - m_first_letter);
    }

    bool is_Terminal_Punctuation(const UCS4 uc) const {
      return m_Terminal_Punctuation.test(uc - m_first_letter);
    }

    bool is_Other_Math(const UCS4 uc) const {
      return m_Other_Math.test(uc - m_first_letter);
    }

    bool is_Hex_Digit(const UCS4 uc) const {
      return 0;
    }

    bool is_Other_Alphabetic(const UCS4 uc) const {
      return 0;
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
    General_Punctuation2000(const General_Punctuation2000 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<112> m_is_defined;
    static const unsigned char _cat[112];
    static const unsigned char m_bidir[112];
    static const unsigned char _decomp[112];
    static const UCS2 m_decompStr[112][2];
    static const std::bitset<112> m_mirror;
    static const unsigned char m_lb[112];
    static const unsigned char m_ea[112];
    static const std::bitset<112> m_White_space;
    static const std::bitset<112> m_Bidi_Control;
    static const std::bitset<112> m_Join_Control;
    static const std::bitset<112> m_Dash;
    static const std::bitset<112> m_Hyphen;
    static const std::bitset<112> m_Quotation_Mark;
    static const std::bitset<112> m_Terminal_Punctuation;
    static const std::bitset<112> m_Other_Math;

  }; // class General_Punctuation2000

    const std::bitset<112> General_Punctuation2000::m_is_defined(std::string("1111110000000000000000000000000000111111011111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char General_Punctuation2000::_cat[] = {
    CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, 
    CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, 
    CAT_Pd, CAT_Pd, CAT_Pd, CAT_Pd, CAT_Pd, CAT_Pd, CAT_Po, CAT_Po, 
    CAT_Pi, CAT_Pf, CAT_Ps, CAT_Pi, CAT_Pi, CAT_Pf, CAT_Ps, CAT_Pi, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Zl, CAT_Zp, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Zs, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Po, CAT_Pi, CAT_Pf, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Pc, 
    CAT_Pc, CAT_Po, CAT_Po, CAT_Po, CAT_Sm, CAT_Ps, CAT_Pe, CAT_Zs, 
    CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Po, CAT_Zs, CAT_Zs, 
    CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, 
    CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, 
    CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, CAT_Zs, 
    CAT_Zs, CAT_Zs, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf, CAT_Cf
  };

  const unsigned char General_Punctuation2000::m_bidir[] = {
    BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, 
    BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_L, BIDIR_R, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_WS, BIDIR_B, BIDIR_LRE, BIDIR_RLE, BIDIR_PDF, BIDIR_LRO, BIDIR_RLO, BIDIR_WS, 
    BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_WS, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_WS, BIDIR_WS, 
    BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, 
    BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, 
    BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, BIDIR_WS, 
    BIDIR_WS, BIDIR_WS, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN
  };

  const unsigned char General_Punctuation2000::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_NOBREAK, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_NOBREAK, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_NOBREAK, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS2 General_Punctuation2000::m_decompStr[][2] = {
    { 0x2002u, 0x0000u }, { 0x2003u, 0x0000u }, { 0x0020u, 0x0000u }, { 0x0020u, 0x0000u }, 
    { 0x0020u, 0x0000u }, { 0x0020u, 0x0000u }, { 0x0020u, 0x0000u }, { 0x0020u, 0x0000u }, 
    { 0x0020u, 0x0000u }, { 0x0020u, 0x0000u }, { 0x0020u, 0x0000u }, { 0x200Bu, 0x0000u }, 
    { 0x200Cu, 0x0000u }, { 0x200Du, 0x0000u }, { 0x200Eu, 0x0000u }, { 0x200Fu, 0x0000u }, 
    { 0x2010u, 0x0000u }, { 0x2010u, 0x0000u }, { 0x2012u, 0x0000u }, { 0x2013u, 0x0000u }, 
    { 0x2014u, 0x0000u }, { 0x2015u, 0x0000u }, { 0x2016u, 0x0000u }, { 0x0020u, 0x0333u }, 
    { 0x2018u, 0x0000u }, { 0x2019u, 0x0000u }, { 0x201Au, 0x0000u }, { 0x201Bu, 0x0000u }, 
    { 0x201Cu, 0x0000u }, { 0x201Du, 0x0000u }, { 0x201Eu, 0x0000u }, { 0x201Fu, 0x0000u }, 
    { 0x2020u, 0x0000u }, { 0x2021u, 0x0000u }, { 0x2022u, 0x0000u }, { 0x2023u, 0x0000u }, 
    { 0x002Eu, 0x0000u }, { 0x002Eu, 0x002Eu }, { 0x002Eu, 0x002Eu }, { 0x2027u, 0x0000u }, 
    { 0x2028u, 0x0000u }, { 0x2029u, 0x0000u }, { 0x202Au, 0x0000u }, { 0x202Bu, 0x0000u }, 
    { 0x202Cu, 0x0000u }, { 0x202Du, 0x0000u }, { 0x202Eu, 0x0000u }, { 0x0020u, 0x0000u }, 
    { 0x2030u, 0x0000u }, { 0x2031u, 0x0000u }, { 0x2032u, 0x0000u }, { 0x2032u, 0x2032u }, 
    { 0x2032u, 0x2032u }, { 0x2035u, 0x0000u }, { 0x2035u, 0x2035u }, { 0x2035u, 0x2035u }, 
    { 0x2038u, 0x0000u }, { 0x2039u, 0x0000u }, { 0x203Au, 0x0000u }, { 0x203Bu, 0x0000u }, 
    { 0x0021u, 0x0021u }, { 0x203Du, 0x0000u }, { 0x0020u, 0x0305u }, { 0x203Fu, 0x0000u }, 
    { 0x2040u, 0x0000u }, { 0x2041u, 0x0000u }, { 0x2042u, 0x0000u }, { 0x2043u, 0x0000u }, 
    { 0x2044u, 0x0000u }, { 0x2045u, 0x0000u }, { 0x2046u, 0x0000u }, { 0x2047u, 0x0000u }, 
    { 0x003Fu, 0x0021u }, { 0x0021u, 0x003Fu }, { 0x204Au, 0x0000u }, { 0x204Bu, 0x0000u }, 
    { 0x204Cu, 0x0000u }, { 0x204Du, 0x0000u }, { 0x204Eu, 0x0000u }, { 0x204Fu, 0x0000u }, 
    { 0x2050u, 0x0000u }, { 0x2051u, 0x0000u }, { 0x2052u, 0x0000u }, { 0x2053u, 0x0000u }, 
    { 0x2054u, 0x0000u }, { 0x2055u, 0x0000u }, { 0x2056u, 0x0000u }, { 0x2057u, 0x0000u }, 
    { 0x2058u, 0x0000u }, { 0x2059u, 0x0000u }, { 0x205Au, 0x0000u }, { 0x205Bu, 0x0000u }, 
    { 0x205Cu, 0x0000u }, { 0x205Du, 0x0000u }, { 0x205Eu, 0x0000u }, { 0x205Fu, 0x0000u }, 
    { 0x2060u, 0x0000u }, { 0x2061u, 0x0000u }, { 0x2062u, 0x0000u }, { 0x2063u, 0x0000u }, 
    { 0x2064u, 0x0000u }, { 0x2065u, 0x0000u }, { 0x2066u, 0x0000u }, { 0x2067u, 0x0000u }, 
    { 0x2068u, 0x0000u }, { 0x2069u, 0x0000u }, { 0x206Au, 0x0000u }, { 0x206Bu, 0x0000u }, 
    { 0x206Cu, 0x0000u }, { 0x206Du, 0x0000u }, { 0x206Eu, 0x0000u }, { 0x206Fu, 0x0000u }
  };

  const std::bitset<112> General_Punctuation2000::m_mirror(std::string("0000000000000000000000000000000000000000011000000000011000000000000000000000000000000000000000000000000000000000"));

  const unsigned char General_Punctuation2000::m_lb[] = {
    LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_GL, 
    LB_BA, LB_BA, LB_BA, LB_ZW, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_BA, LB_GL, LB_BA, LB_BA, LB_B2, LB_AI, LB_AI, LB_AL, 
    LB_QU, LB_QU, LB_OP, LB_QU, LB_QU, LB_QU, LB_OP, LB_QU, 
    LB_AI, LB_AI, LB_AL, LB_AL, LB_IN, LB_IN, LB_IN, LB_BA, 
    LB_BK, LB_BK, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_GL, 
    LB_PO, LB_PO, LB_PO, LB_PO, LB_PO, LB_PO, LB_PO, LB_PO, 
    LB_AL, LB_QU, LB_QU, LB_AI, LB_NS, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_NS, LB_OP, LB_CL, LB_BA, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_BA, LB_BA, 
    LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, 
    LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, 
    LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, LB_BA, 
    LB_BA, LB_BA, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

  const unsigned char General_Punctuation2000::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_A, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_A, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N
  };

    const std::bitset<112> General_Punctuation2000::m_White_space(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111"));

    const std::bitset<112> General_Punctuation2000::m_Bidi_Control(std::string("0000000000000000000000000000000000000000000000000000000000000000011111000000000000000000000000001100000000000000"));

    const std::bitset<112> General_Punctuation2000::m_Join_Control(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011000000000000"));

    const std::bitset<112> General_Punctuation2000::m_Dash(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111110000000000000000"));

    const std::bitset<112> General_Punctuation2000::m_Hyphen(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110000000000000000"));

    const std::bitset<112> General_Punctuation2000::m_Quotation_Mark(std::string("0000000000000000000000000000000000000000000000000000000000000000000000000000000000011000000000000000000000000000"));

    const std::bitset<112> General_Punctuation2000::m_Terminal_Punctuation(std::string("0000000000000000000000000000000000000011000000000011000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<112> General_Punctuation2000::m_Other_Math(std::string("0000000000000000000000000000000000000000000000000000000000011100000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::General_Punctuation2000);
