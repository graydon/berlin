/*$Id: D80-DFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:47:18 +0200.
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

  class SinhalaD80 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    SinhalaD80() {
      m_first_letter = 0xD80;
      m_last_letter  = 0xDFF;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00000DD9, 0x00000DCA)] = 0x0DDA;
      m_composeMap[make_pair(0x00000DD9, 0x00000DCF)] = 0x0DDC;
      m_composeMap[make_pair(0x00000DD9, 0x00000DDF)] = 0x0DDE;
      m_composeMap[make_pair(0x00000DDC, 0x00000DCA)] = 0x0DDD;

    }


    ~SinhalaD80() {
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
      return "Sinhala";
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
      return Babylon::Gen_Cat(SinhalaD80::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(SinhalaD80::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(SinhalaD80::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = SinhalaD80::m_decompStr[uc - m_first_letter][0];
      us[1] = SinhalaD80::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(SinhalaD80::m_lb[uc - m_first_letter]);
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
    SinhalaD80(const SinhalaD80 &) {}

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

  }; // class SinhalaD80

    const std::bitset<128> SinhalaD80::m_is_defined(std::string("00000000000111000000000000000000111111110101111110000100011111110010111111111011111111111111111111111100011111111111111111101100"));

  const unsigned char SinhalaD80::_cat[] = {
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, CAT_Lo, CAT_Mc, CAT_Mc, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mn, CAT_Mn, CAT_Mn, CAT_Mc, CAT_Mn, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Po, CAT_Mc, CAT_Mc, CAT_Mc, 
    CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc, CAT_Mc
  };

  const unsigned char SinhalaD80::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 9, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0
  };

  const unsigned char SinhalaD80::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_NSM, BIDIR_L, BIDIR_NSM, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const UCS2 SinhalaD80::m_decompStr[][2] = {
    { 0x0D80u, 0x0000u }, { 0x0D81u, 0x0000u }, { 0x0D82u, 0x0000u }, { 0x0D83u, 0x0000u }, 
    { 0x0D84u, 0x0000u }, { 0x0D85u, 0x0000u }, { 0x0D86u, 0x0000u }, { 0x0D87u, 0x0000u }, 
    { 0x0D88u, 0x0000u }, { 0x0D89u, 0x0000u }, { 0x0D8Au, 0x0000u }, { 0x0D8Bu, 0x0000u }, 
    { 0x0D8Cu, 0x0000u }, { 0x0D8Du, 0x0000u }, { 0x0D8Eu, 0x0000u }, { 0x0D8Fu, 0x0000u }, 
    { 0x0D90u, 0x0000u }, { 0x0D91u, 0x0000u }, { 0x0D92u, 0x0000u }, { 0x0D93u, 0x0000u }, 
    { 0x0D94u, 0x0000u }, { 0x0D95u, 0x0000u }, { 0x0D96u, 0x0000u }, { 0x0D97u, 0x0000u }, 
    { 0x0D98u, 0x0000u }, { 0x0D99u, 0x0000u }, { 0x0D9Au, 0x0000u }, { 0x0D9Bu, 0x0000u }, 
    { 0x0D9Cu, 0x0000u }, { 0x0D9Du, 0x0000u }, { 0x0D9Eu, 0x0000u }, { 0x0D9Fu, 0x0000u }, 
    { 0x0DA0u, 0x0000u }, { 0x0DA1u, 0x0000u }, { 0x0DA2u, 0x0000u }, { 0x0DA3u, 0x0000u }, 
    { 0x0DA4u, 0x0000u }, { 0x0DA5u, 0x0000u }, { 0x0DA6u, 0x0000u }, { 0x0DA7u, 0x0000u }, 
    { 0x0DA8u, 0x0000u }, { 0x0DA9u, 0x0000u }, { 0x0DAAu, 0x0000u }, { 0x0DABu, 0x0000u }, 
    { 0x0DACu, 0x0000u }, { 0x0DADu, 0x0000u }, { 0x0DAEu, 0x0000u }, { 0x0DAFu, 0x0000u }, 
    { 0x0DB0u, 0x0000u }, { 0x0DB1u, 0x0000u }, { 0x0DB2u, 0x0000u }, { 0x0DB3u, 0x0000u }, 
    { 0x0DB4u, 0x0000u }, { 0x0DB5u, 0x0000u }, { 0x0DB6u, 0x0000u }, { 0x0DB7u, 0x0000u }, 
    { 0x0DB8u, 0x0000u }, { 0x0DB9u, 0x0000u }, { 0x0DBAu, 0x0000u }, { 0x0DBBu, 0x0000u }, 
    { 0x0DBCu, 0x0000u }, { 0x0DBDu, 0x0000u }, { 0x0DBEu, 0x0000u }, { 0x0DBFu, 0x0000u }, 
    { 0x0DC0u, 0x0000u }, { 0x0DC1u, 0x0000u }, { 0x0DC2u, 0x0000u }, { 0x0DC3u, 0x0000u }, 
    { 0x0DC4u, 0x0000u }, { 0x0DC5u, 0x0000u }, { 0x0DC6u, 0x0000u }, { 0x0DC7u, 0x0000u }, 
    { 0x0DC8u, 0x0000u }, { 0x0DC9u, 0x0000u }, { 0x0DCAu, 0x0000u }, { 0x0DCBu, 0x0000u }, 
    { 0x0DCCu, 0x0000u }, { 0x0DCDu, 0x0000u }, { 0x0DCEu, 0x0000u }, { 0x0DCFu, 0x0000u }, 
    { 0x0DD0u, 0x0000u }, { 0x0DD1u, 0x0000u }, { 0x0DD2u, 0x0000u }, { 0x0DD3u, 0x0000u }, 
    { 0x0DD4u, 0x0000u }, { 0x0DD5u, 0x0000u }, { 0x0DD6u, 0x0000u }, { 0x0DD7u, 0x0000u }, 
    { 0x0DD8u, 0x0000u }, { 0x0DD9u, 0x0000u }, { 0x0DD9u, 0x0DCAu }, { 0x0DDBu, 0x0000u }, 
    { 0x0DD9u, 0x0DCFu }, { 0x0DDCu, 0x0DCAu }, { 0x0DD9u, 0x0DDFu }, { 0x0DDFu, 0x0000u }, 
    { 0x0DE0u, 0x0000u }, { 0x0DE1u, 0x0000u }, { 0x0DE2u, 0x0000u }, { 0x0DE3u, 0x0000u }, 
    { 0x0DE4u, 0x0000u }, { 0x0DE5u, 0x0000u }, { 0x0DE6u, 0x0000u }, { 0x0DE7u, 0x0000u }, 
    { 0x0DE8u, 0x0000u }, { 0x0DE9u, 0x0000u }, { 0x0DEAu, 0x0000u }, { 0x0DEBu, 0x0000u }, 
    { 0x0DECu, 0x0000u }, { 0x0DEDu, 0x0000u }, { 0x0DEEu, 0x0000u }, { 0x0DEFu, 0x0000u }, 
    { 0x0DF0u, 0x0000u }, { 0x0DF1u, 0x0000u }, { 0x0DF2u, 0x0000u }, { 0x0DF3u, 0x0000u }, 
    { 0x0DF4u, 0x0000u }, { 0x0DF5u, 0x0000u }, { 0x0DF6u, 0x0000u }, { 0x0DF7u, 0x0000u }, 
    { 0x0DF8u, 0x0000u }, { 0x0DF9u, 0x0000u }, { 0x0DFAu, 0x0000u }, { 0x0DFBu, 0x0000u }, 
    { 0x0DFCu, 0x0000u }, { 0x0DFDu, 0x0000u }, { 0x0DFEu, 0x0000u }, { 0x0DFFu, 0x0000u }
  };

  const unsigned char SinhalaD80::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, 
    LB_CM, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_CM, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, LB_AL, LB_CM, LB_CM, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_AL, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM
  };

    const std::bitset<128> SinhalaD80::m_Other_Alphabetic(std::string("00000000000011000000000000000000111111110001111110000000000000000000000000000000000000000000000000000000000000000000000000001100"));

}; // namespace Babylon

dload(Babylon::SinhalaD80);
