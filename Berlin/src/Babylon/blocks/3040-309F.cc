/*$Id: 3040-309F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:49:47 +0200.
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

  class Hiragana3040 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Hiragana3040() {
      m_first_letter = 0x3040;
      m_last_letter  = 0x309F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x00003046, 0x00003099)] = 0x3094;
      m_composeMap[make_pair(0x0000304B, 0x00003099)] = 0x304C;
      m_composeMap[make_pair(0x0000304D, 0x00003099)] = 0x304E;
      m_composeMap[make_pair(0x0000304F, 0x00003099)] = 0x3050;
      m_composeMap[make_pair(0x00003051, 0x00003099)] = 0x3052;
      m_composeMap[make_pair(0x00003053, 0x00003099)] = 0x3054;
      m_composeMap[make_pair(0x00003055, 0x00003099)] = 0x3056;
      m_composeMap[make_pair(0x00003057, 0x00003099)] = 0x3058;
      m_composeMap[make_pair(0x00003059, 0x00003099)] = 0x305A;
      m_composeMap[make_pair(0x0000305B, 0x00003099)] = 0x305C;
      m_composeMap[make_pair(0x0000305D, 0x00003099)] = 0x305E;
      m_composeMap[make_pair(0x0000305F, 0x00003099)] = 0x3060;
      m_composeMap[make_pair(0x00003061, 0x00003099)] = 0x3062;
      m_composeMap[make_pair(0x00003064, 0x00003099)] = 0x3065;
      m_composeMap[make_pair(0x00003066, 0x00003099)] = 0x3067;
      m_composeMap[make_pair(0x00003068, 0x00003099)] = 0x3069;
      m_composeMap[make_pair(0x0000306F, 0x00003099)] = 0x3070;
      m_composeMap[make_pair(0x0000306F, 0x0000309A)] = 0x3071;
      m_composeMap[make_pair(0x00003072, 0x00003099)] = 0x3073;
      m_composeMap[make_pair(0x00003072, 0x0000309A)] = 0x3074;
      m_composeMap[make_pair(0x00003075, 0x00003099)] = 0x3076;
      m_composeMap[make_pair(0x00003075, 0x0000309A)] = 0x3077;
      m_composeMap[make_pair(0x00003078, 0x00003099)] = 0x3079;
      m_composeMap[make_pair(0x00003078, 0x0000309A)] = 0x307A;
      m_composeMap[make_pair(0x0000307B, 0x00003099)] = 0x307C;
      m_composeMap[make_pair(0x0000307B, 0x0000309A)] = 0x307D;
      m_composeMap[make_pair(0x0000309D, 0x00003099)] = 0x309E;

    }


    ~Hiragana3040() {
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
      return "Hiragana";
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
      return Babylon::Gen_Cat(Hiragana3040::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Can_Comb_Class(Hiragana3040::_comb_cl[uc - m_first_letter]);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Hiragana3040::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Hiragana3040::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Hiragana3040::m_decompStr[uc - m_first_letter][0];
      us[1] = Hiragana3040::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(Hiragana3040::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_W);
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
      return 0;
    }

    bool is_Ideographic(const UCS4 uc) const {
      return 0;
    }

    bool is_Diacritic(const UCS4 uc) const {
      return m_Diacritic.test(uc - m_first_letter);
    }

    bool is_Extender(const UCS4 uc) const {
      return m_Extender.test(uc - m_first_letter);
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
    Hiragana3040(const Hiragana3040 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<96> m_is_defined;
    static const unsigned char _cat[96];
    static const unsigned char _comb_cl[96];
    static const unsigned char m_bidir[96];
    static const unsigned char _decomp[96];
    static const UCS4 m_decompStr[96][2];
    static const unsigned char m_lb[96];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<96> m_Diacritic;
    static const std::bitset<96> m_Extender;

  }; // class Hiragana3040

    const std::bitset<96> Hiragana3040::m_is_defined(std::string("011111100001111111111111111111111111111111111111111111111111111111111111111111111111111111111110"));

  const unsigned char Hiragana3040::_cat[] = {
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Mn, CAT_Mn, CAT_Sk, CAT_Sk, CAT_Lm, CAT_Lm, CAT_Lo
  };

  const unsigned char Hiragana3040::_comb_cl[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 8, 8, 0, 0, 0, 0, 0
  };

  const unsigned char Hiragana3040::m_bidir[] = {
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_NSM, BIDIR_NSM, BIDIR_ON, BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L
  };

  const unsigned char Hiragana3040::_decomp[] = {
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_COMPAT, DECOMP_COMPAT, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS4 Hiragana3040::m_decompStr[][2] = {
    { 0x3040u, 0x0000u }, { 0x3041u, 0x0000u }, { 0x3042u, 0x0000u }, { 0x3043u, 0x0000u }, 
    { 0x3044u, 0x0000u }, { 0x3045u, 0x0000u }, { 0x3046u, 0x0000u }, { 0x3047u, 0x0000u }, 
    { 0x3048u, 0x0000u }, { 0x3049u, 0x0000u }, { 0x304Au, 0x0000u }, { 0x304Bu, 0x0000u }, 
    { 0x304Bu, 0x3099u }, { 0x304Du, 0x0000u }, { 0x304Du, 0x3099u }, { 0x304Fu, 0x0000u }, 
    { 0x304Fu, 0x3099u }, { 0x3051u, 0x0000u }, { 0x3051u, 0x3099u }, { 0x3053u, 0x0000u }, 
    { 0x3053u, 0x3099u }, { 0x3055u, 0x0000u }, { 0x3055u, 0x3099u }, { 0x3057u, 0x0000u }, 
    { 0x3057u, 0x3099u }, { 0x3059u, 0x0000u }, { 0x3059u, 0x3099u }, { 0x305Bu, 0x0000u }, 
    { 0x305Bu, 0x3099u }, { 0x305Du, 0x0000u }, { 0x305Du, 0x3099u }, { 0x305Fu, 0x0000u }, 
    { 0x305Fu, 0x3099u }, { 0x3061u, 0x0000u }, { 0x3061u, 0x3099u }, { 0x3063u, 0x0000u }, 
    { 0x3064u, 0x0000u }, { 0x3064u, 0x3099u }, { 0x3066u, 0x0000u }, { 0x3066u, 0x3099u }, 
    { 0x3068u, 0x0000u }, { 0x3068u, 0x3099u }, { 0x306Au, 0x0000u }, { 0x306Bu, 0x0000u }, 
    { 0x306Cu, 0x0000u }, { 0x306Du, 0x0000u }, { 0x306Eu, 0x0000u }, { 0x306Fu, 0x0000u }, 
    { 0x306Fu, 0x3099u }, { 0x306Fu, 0x309Au }, { 0x3072u, 0x0000u }, { 0x3072u, 0x3099u }, 
    { 0x3072u, 0x309Au }, { 0x3075u, 0x0000u }, { 0x3075u, 0x3099u }, { 0x3075u, 0x309Au }, 
    { 0x3078u, 0x0000u }, { 0x3078u, 0x3099u }, { 0x3078u, 0x309Au }, { 0x307Bu, 0x0000u }, 
    { 0x307Bu, 0x3099u }, { 0x307Bu, 0x309Au }, { 0x307Eu, 0x0000u }, { 0x307Fu, 0x0000u }, 
    { 0x3080u, 0x0000u }, { 0x3081u, 0x0000u }, { 0x3082u, 0x0000u }, { 0x3083u, 0x0000u }, 
    { 0x3084u, 0x0000u }, { 0x3085u, 0x0000u }, { 0x3086u, 0x0000u }, { 0x3087u, 0x0000u }, 
    { 0x3088u, 0x0000u }, { 0x3089u, 0x0000u }, { 0x308Au, 0x0000u }, { 0x308Bu, 0x0000u }, 
    { 0x308Cu, 0x0000u }, { 0x308Du, 0x0000u }, { 0x308Eu, 0x0000u }, { 0x308Fu, 0x0000u }, 
    { 0x3090u, 0x0000u }, { 0x3091u, 0x0000u }, { 0x3092u, 0x0000u }, { 0x3093u, 0x0000u }, 
    { 0x3046u, 0x3099u }, { 0x3095u, 0x0000u }, { 0x3096u, 0x0000u }, { 0x3097u, 0x0000u }, 
    { 0x3098u, 0x0000u }, { 0x3099u, 0x0000u }, { 0x309Au, 0x0000u }, { 0x0020u, 0x3099u }, 
    { 0x0020u, 0x309Au }, { 0x309Du, 0x0000u }, { 0x309Du, 0x3099u }, { 0x309Fu, 0x0000u }
  };

  const unsigned char Hiragana3040::m_lb[] = {
    LB_NS, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS, 
    LB_ID, LB_NS, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, LB_NS, LB_ID, LB_NS, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_NS, LB_ID, 
    LB_ID, LB_ID, LB_ID, LB_ID, LB_ID, LB_NS, LB_NS, LB_NS, 
    LB_NS, LB_CM, LB_CM, LB_NS, LB_NS, LB_NS, LB_NS, LB_NS
  };

    const std::bitset<96> Hiragana3040::m_Diacritic(std::string("000111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<96> Hiragana3040::m_Extender(std::string("011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Hiragana3040);
