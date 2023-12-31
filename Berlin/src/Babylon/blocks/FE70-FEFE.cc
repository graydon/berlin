/*$Id: FE70-FEFE.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:05:11 +0200.
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

  class Arabic_Presentation_FormsBFE70 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Arabic_Presentation_FormsBFE70() {
      m_first_letter = 0xFE70;
      m_last_letter  = 0xFEFE;
      // m_version="3.1" // Not yet supported!

    }


    ~Arabic_Presentation_FormsBFE70() {
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
      return "Arabic Presentation Forms-B";
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
      return Babylon::Gen_Cat(CAT_Lo);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(BIDIR_AL);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Arabic_Presentation_FormsBFE70::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Arabic_Presentation_FormsBFE70::m_decompStr[uc - m_first_letter][0];
      us[1] = Arabic_Presentation_FormsBFE70::m_decompStr[uc - m_first_letter][1];
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
      return Babylon::Line_Break(LB_AL);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(EA_WIDTH_N);
    }

    UCS4 compose (const UCS4 start, const UCS4 last) {
      return 0;
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
    Arabic_Presentation_FormsBFE70(const Arabic_Presentation_FormsBFE70 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<143> m_is_defined;
    static const unsigned char _decomp[143];
    static const UCS4 m_decompStr[143][2];

  }; // class Arabic_Presentation_FormsBFE70

    const std::bitset<143> Arabic_Presentation_FormsBFE70::m_is_defined(std::string("00111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111010111"));

  const unsigned char Arabic_Presentation_FormsBFE70::_decomp[] = {
    DECOMP_ISOLATED, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_CANONICAL, DECOMP_ISOLATED, DECOMP_CANONICAL, DECOMP_ISOLATED, DECOMP_MEDIAL, 
    DECOMP_ISOLATED, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_MEDIAL, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, 
    DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, 
    DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, 
    DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, 
    DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, 
    DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS4 Arabic_Presentation_FormsBFE70::m_decompStr[][2] = {
    { 0x0020u, 0x064Bu }, { 0x0640u, 0x064Bu }, { 0x0020u, 0x064Cu }, { 0xFE73u, 0x0000u }, 
    { 0x0020u, 0x064Du }, { 0xFE75u, 0x0000u }, { 0x0020u, 0x064Eu }, { 0x0640u, 0x064Eu }, 
    { 0x0020u, 0x064Fu }, { 0x0640u, 0x064Fu }, { 0x0020u, 0x0650u }, { 0x0640u, 0x0650u }, 
    { 0x0020u, 0x0651u }, { 0x0640u, 0x0651u }, { 0x0020u, 0x0652u }, { 0x0640u, 0x0652u }, 
    { 0x0621u, 0x0000u }, { 0x0622u, 0x0000u }, { 0x0622u, 0x0000u }, { 0x0623u, 0x0000u }, 
    { 0x0623u, 0x0000u }, { 0x0624u, 0x0000u }, { 0x0624u, 0x0000u }, { 0x0625u, 0x0000u }, 
    { 0x0625u, 0x0000u }, { 0x0626u, 0x0000u }, { 0x0626u, 0x0000u }, { 0x0626u, 0x0000u }, 
    { 0x0626u, 0x0000u }, { 0x0627u, 0x0000u }, { 0x0627u, 0x0000u }, { 0x0628u, 0x0000u }, 
    { 0x0628u, 0x0000u }, { 0x0628u, 0x0000u }, { 0x0628u, 0x0000u }, { 0x0629u, 0x0000u }, 
    { 0x0629u, 0x0000u }, { 0x062Au, 0x0000u }, { 0x062Au, 0x0000u }, { 0x062Au, 0x0000u }, 
    { 0x062Au, 0x0000u }, { 0x062Bu, 0x0000u }, { 0x062Bu, 0x0000u }, { 0x062Bu, 0x0000u }, 
    { 0x062Bu, 0x0000u }, { 0x062Cu, 0x0000u }, { 0x062Cu, 0x0000u }, { 0x062Cu, 0x0000u }, 
    { 0x062Cu, 0x0000u }, { 0x062Du, 0x0000u }, { 0x062Du, 0x0000u }, { 0x062Du, 0x0000u }, 
    { 0x062Du, 0x0000u }, { 0x062Eu, 0x0000u }, { 0x062Eu, 0x0000u }, { 0x062Eu, 0x0000u }, 
    { 0x062Eu, 0x0000u }, { 0x062Fu, 0x0000u }, { 0x062Fu, 0x0000u }, { 0x0630u, 0x0000u }, 
    { 0x0630u, 0x0000u }, { 0x0631u, 0x0000u }, { 0x0631u, 0x0000u }, { 0x0632u, 0x0000u }, 
    { 0x0632u, 0x0000u }, { 0x0633u, 0x0000u }, { 0x0633u, 0x0000u }, { 0x0633u, 0x0000u }, 
    { 0x0633u, 0x0000u }, { 0x0634u, 0x0000u }, { 0x0634u, 0x0000u }, { 0x0634u, 0x0000u }, 
    { 0x0634u, 0x0000u }, { 0x0635u, 0x0000u }, { 0x0635u, 0x0000u }, { 0x0635u, 0x0000u }, 
    { 0x0635u, 0x0000u }, { 0x0636u, 0x0000u }, { 0x0636u, 0x0000u }, { 0x0636u, 0x0000u }, 
    { 0x0636u, 0x0000u }, { 0x0637u, 0x0000u }, { 0x0637u, 0x0000u }, { 0x0637u, 0x0000u }, 
    { 0x0637u, 0x0000u }, { 0x0638u, 0x0000u }, { 0x0638u, 0x0000u }, { 0x0638u, 0x0000u }, 
    { 0x0638u, 0x0000u }, { 0x0639u, 0x0000u }, { 0x0639u, 0x0000u }, { 0x0639u, 0x0000u }, 
    { 0x0639u, 0x0000u }, { 0x063Au, 0x0000u }, { 0x063Au, 0x0000u }, { 0x063Au, 0x0000u }, 
    { 0x063Au, 0x0000u }, { 0x0641u, 0x0000u }, { 0x0641u, 0x0000u }, { 0x0641u, 0x0000u }, 
    { 0x0641u, 0x0000u }, { 0x0642u, 0x0000u }, { 0x0642u, 0x0000u }, { 0x0642u, 0x0000u }, 
    { 0x0642u, 0x0000u }, { 0x0643u, 0x0000u }, { 0x0643u, 0x0000u }, { 0x0643u, 0x0000u }, 
    { 0x0643u, 0x0000u }, { 0x0644u, 0x0000u }, { 0x0644u, 0x0000u }, { 0x0644u, 0x0000u }, 
    { 0x0644u, 0x0000u }, { 0x0645u, 0x0000u }, { 0x0645u, 0x0000u }, { 0x0645u, 0x0000u }, 
    { 0x0645u, 0x0000u }, { 0x0646u, 0x0000u }, { 0x0646u, 0x0000u }, { 0x0646u, 0x0000u }, 
    { 0x0646u, 0x0000u }, { 0x0647u, 0x0000u }, { 0x0647u, 0x0000u }, { 0x0647u, 0x0000u }, 
    { 0x0647u, 0x0000u }, { 0x0648u, 0x0000u }, { 0x0648u, 0x0000u }, { 0x0649u, 0x0000u }, 
    { 0x0649u, 0x0000u }, { 0x064Au, 0x0000u }, { 0x064Au, 0x0000u }, { 0x064Au, 0x0000u }, 
    { 0x064Au, 0x0000u }, { 0x0644u, 0x0622u }, { 0x0644u, 0x0622u }, { 0x0644u, 0x0623u }, 
    { 0x0644u, 0x0623u }, { 0x0644u, 0x0625u }, { 0x0644u, 0x0625u }, { 0x0644u, 0x0627u }, 
    { 0x0644u, 0x0627u }, { 0xFEFDu, 0x0000u }, { 0xFEFEu, 0x0000u }
  };

}; // namespace Babylon

dload(Babylon::Arabic_Presentation_FormsBFE70);
