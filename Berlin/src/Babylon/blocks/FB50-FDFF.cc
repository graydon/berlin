/*$Id: FB50-FDFF.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 18:04:49 +0200.
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

  class Arabic_Presentation_FormsAFB50 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Arabic_Presentation_FormsAFB50() {
      m_first_letter = 0xFB50;
      m_last_letter  = 0xFDFF;
      // m_version="3.1" // Not yet supported!

    }


    ~Arabic_Presentation_FormsAFB50() {
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
      return "Arabic Presentation Forms-A";
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
      return Babylon::Gen_Cat(Arabic_Presentation_FormsAFB50::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Arabic_Presentation_FormsAFB50::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(Arabic_Presentation_FormsAFB50::_decomp[uc - m_first_letter]);
    }

    UTF32_string decompose(const UCS4 uc) const {
      Babylon::UTF32_string us;
      us.resize(2);
      us[0] = Arabic_Presentation_FormsAFB50::m_decompStr[uc - m_first_letter][0];
      us[1] = Arabic_Presentation_FormsAFB50::m_decompStr[uc - m_first_letter][1];

      switch (uc) {

      case 0xFC5E:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFC5F:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFC60:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFC61:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFC62:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFC63:
        us.resize(3);
        us[2u] = 0x0670u;
        break;

      case 0xFCF2:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFCF3:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFCF4:
        us.resize(3);
        us[2u] = 0x0651u;
        break;

      case 0xFD50:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD51:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD52:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD53:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD54:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD55:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD56:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD57:
        us.resize(3);
        us[2u] = 0x062Eu;
        break;

      case 0xFD58:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD59:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD5A:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD5B:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD5C:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD5D:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD5E:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD5F:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD60:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD61:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD62:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD63:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD64:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD65:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD66:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD67:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD68:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD69:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD6A:
        us.resize(3);
        us[2u] = 0x062Eu;
        break;

      case 0xFD6B:
        us.resize(3);
        us[2u] = 0x062Eu;
        break;

      case 0xFD6C:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD6D:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD6E:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD6F:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD70:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD71:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD72:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD73:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD74:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD75:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD76:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD77:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD78:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD79:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD7A:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD7B:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD7C:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD7D:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD7E:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD7F:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD80:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD81:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD82:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD83:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD84:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD85:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD86:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD87:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD88:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD89:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD8A:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD8B:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD8C:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFD8D:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD8E:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD8F:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD92:
        us.resize(3);
        us[2u] = 0x062Eu;
        break;

      case 0xFD93:
        us.resize(3);
        us[2u] = 0x062Cu;
        break;

      case 0xFD94:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD95:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD96:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD97:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD98:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD99:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD9A:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD9B:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFD9C:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD9D:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFD9E:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFD9F:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDA0:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDA1:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDA2:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDA3:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDA4:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDA5:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDA6:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDA7:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDA8:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDA9:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDAA:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDAB:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDAC:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDAD:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDAE:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDAF:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB0:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB1:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB2:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB3:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB4:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFDB5:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDB6:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB7:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDB8:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFDB9:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDBA:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDBB:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDBC:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDBD:
        us.resize(3);
        us[2u] = 0x062Du;
        break;

      case 0xFDBE:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDBF:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDC0:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDC1:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDC2:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDC3:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDC4:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDC5:
        us.resize(3);
        us[2u] = 0x0645u;
        break;

      case 0xFDC6:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDC7:
        us.resize(3);
        us[2u] = 0x064Au;
        break;

      case 0xFDF0:
        us.resize(3);
        us[2u] = 0x06D2u;
        break;

      case 0xFDF1:
        us.resize(3);
        us[2u] = 0x06D2u;
        break;

      case 0xFDF2:
        us.resize(4);
        us[2u] = 0x0644u;
        us[3u] = 0x0647u;
        break;

      case 0xFDF3:
        us.resize(4);
        us[2u] = 0x0628u;
        us[3u] = 0x0631u;
        break;

      case 0xFDF4:
        us.resize(4);
        us[2u] = 0x0645u;
        us[3u] = 0x062Fu;
        break;

      case 0xFDF5:
        us.resize(4);
        us[2u] = 0x0639u;
        us[3u] = 0x0645u;
        break;

      case 0xFDF6:
        us.resize(4);
        us[2u] = 0x0648u;
        us[3u] = 0x0644u;
        break;

      case 0xFDF7:
        us.resize(4);
        us[2u] = 0x064Au;
        us[3u] = 0x0647u;
        break;

      case 0xFDF8:
        us.resize(4);
        us[2u] = 0x0644u;
        us[3u] = 0x0645u;
        break;

      case 0xFDF9:
        us.resize(3);
        us[2u] = 0x0649u;
        break;

      case 0xFDFA:
        us.resize(18);
        us[2u] = 0x0649u;
        us[3u] = 0x0020u;
        us[4u] = 0x0627u;
        us[5u] = 0x0644u;
        us[6u] = 0x0644u;
        us[7u] = 0x0647u;
        us[8u] = 0x0020u;
        us[9u] = 0x0639u;
        us[10u] = 0x0644u;
        us[11u] = 0x064Au;
        us[12u] = 0x0647u;
        us[13u] = 0x0020u;
        us[14u] = 0x0648u;
        us[15u] = 0x0633u;
        us[16u] = 0x0644u;
        us[17u] = 0x0645u;
        break;

      case 0xFDFB:
        us.resize(8);
        us[2u] = 0x0020u;
        us[3u] = 0x062Cu;
        us[4u] = 0x0644u;
        us[5u] = 0x0627u;
        us[6u] = 0x0644u;
        us[7u] = 0x0647u;
        break;
      }
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
      return Babylon::Line_Break(Arabic_Presentation_FormsAFB50::m_lb[uc - m_first_letter]);
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
      return m_Noncharacter_Code_Point.test(uc - m_first_letter);
    }


  private:
    // functions
    Arabic_Presentation_FormsAFB50(const Arabic_Presentation_FormsAFB50 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const std::bitset<688> m_is_defined;
    static const unsigned char _cat[688];
    static const unsigned char m_bidir[688];
    static const unsigned char _decomp[688];
    static const UCS4 m_decompStr[688][2];
    static const unsigned char m_lb[688];
    static const std::bitset<688> m_Noncharacter_Code_Point;

  }; // class Arabic_Presentation_FormsAFB50

    const std::bitset<688> Arabic_Presentation_FormsAFB50::m_is_defined(std::string("0000111111111111000000000000000000000000000000000000000011111111111111111111111111111111111111111111111111111100111111111111111111111111111111111111111111111111111111111111111100000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"));

  const unsigned char Arabic_Presentation_FormsAFB50::_cat[] = {
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
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Ps, CAT_Pe, 
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
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, 
    CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo, CAT_Lo
  };

  const unsigned char Arabic_Presentation_FormsAFB50::m_bidir[] = {
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_ON, BIDIR_ON, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, 
    BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL, BIDIR_AL
  };

  const unsigned char Arabic_Presentation_FormsAFB50::_decomp[] = {
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, 
    DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, 
    DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_MEDIAL, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_MEDIAL, 
    DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, 
    DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, 
    DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, 
    DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_MEDIAL, DECOMP_FINAL, DECOMP_ISOLATED, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_INITIAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_FINAL, DECOMP_FINAL, DECOMP_FINAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_INITIAL, DECOMP_FINAL, DECOMP_FINAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, 
    DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_ISOLATED, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL, DECOMP_CANONICAL
  };

  const UCS4 Arabic_Presentation_FormsAFB50::m_decompStr[][2] = {
    { 0x0671u, 0x0000u }, { 0x0671u, 0x0000u }, { 0x067Bu, 0x0000u }, { 0x067Bu, 0x0000u }, 
    { 0x067Bu, 0x0000u }, { 0x067Bu, 0x0000u }, { 0x067Eu, 0x0000u }, { 0x067Eu, 0x0000u }, 
    { 0x067Eu, 0x0000u }, { 0x067Eu, 0x0000u }, { 0x0680u, 0x0000u }, { 0x0680u, 0x0000u }, 
    { 0x0680u, 0x0000u }, { 0x0680u, 0x0000u }, { 0x067Au, 0x0000u }, { 0x067Au, 0x0000u }, 
    { 0x067Au, 0x0000u }, { 0x067Au, 0x0000u }, { 0x067Fu, 0x0000u }, { 0x067Fu, 0x0000u }, 
    { 0x067Fu, 0x0000u }, { 0x067Fu, 0x0000u }, { 0x0679u, 0x0000u }, { 0x0679u, 0x0000u }, 
    { 0x0679u, 0x0000u }, { 0x0679u, 0x0000u }, { 0x06A4u, 0x0000u }, { 0x06A4u, 0x0000u }, 
    { 0x06A4u, 0x0000u }, { 0x06A4u, 0x0000u }, { 0x06A6u, 0x0000u }, { 0x06A6u, 0x0000u }, 
    { 0x06A6u, 0x0000u }, { 0x06A6u, 0x0000u }, { 0x0684u, 0x0000u }, { 0x0684u, 0x0000u }, 
    { 0x0684u, 0x0000u }, { 0x0684u, 0x0000u }, { 0x0683u, 0x0000u }, { 0x0683u, 0x0000u }, 
    { 0x0683u, 0x0000u }, { 0x0683u, 0x0000u }, { 0x0686u, 0x0000u }, { 0x0686u, 0x0000u }, 
    { 0x0686u, 0x0000u }, { 0x0686u, 0x0000u }, { 0x0687u, 0x0000u }, { 0x0687u, 0x0000u }, 
    { 0x0687u, 0x0000u }, { 0x0687u, 0x0000u }, { 0x068Du, 0x0000u }, { 0x068Du, 0x0000u }, 
    { 0x068Cu, 0x0000u }, { 0x068Cu, 0x0000u }, { 0x068Eu, 0x0000u }, { 0x068Eu, 0x0000u }, 
    { 0x0688u, 0x0000u }, { 0x0688u, 0x0000u }, { 0x0698u, 0x0000u }, { 0x0698u, 0x0000u }, 
    { 0x0691u, 0x0000u }, { 0x0691u, 0x0000u }, { 0x06A9u, 0x0000u }, { 0x06A9u, 0x0000u }, 
    { 0x06A9u, 0x0000u }, { 0x06A9u, 0x0000u }, { 0x06AFu, 0x0000u }, { 0x06AFu, 0x0000u }, 
    { 0x06AFu, 0x0000u }, { 0x06AFu, 0x0000u }, { 0x06B3u, 0x0000u }, { 0x06B3u, 0x0000u }, 
    { 0x06B3u, 0x0000u }, { 0x06B3u, 0x0000u }, { 0x06B1u, 0x0000u }, { 0x06B1u, 0x0000u }, 
    { 0x06B1u, 0x0000u }, { 0x06B1u, 0x0000u }, { 0x06BAu, 0x0000u }, { 0x06BAu, 0x0000u }, 
    { 0x06BBu, 0x0000u }, { 0x06BBu, 0x0000u }, { 0x06BBu, 0x0000u }, { 0x06BBu, 0x0000u }, 
    { 0x06C0u, 0x0000u }, { 0x06C0u, 0x0000u }, { 0x06C1u, 0x0000u }, { 0x06C1u, 0x0000u }, 
    { 0x06C1u, 0x0000u }, { 0x06C1u, 0x0000u }, { 0x06BEu, 0x0000u }, { 0x06BEu, 0x0000u }, 
    { 0x06BEu, 0x0000u }, { 0x06BEu, 0x0000u }, { 0x06D2u, 0x0000u }, { 0x06D2u, 0x0000u }, 
    { 0x06D3u, 0x0000u }, { 0x06D3u, 0x0000u }, { 0xFBB2u, 0x0000u }, { 0xFBB3u, 0x0000u }, 
    { 0xFBB4u, 0x0000u }, { 0xFBB5u, 0x0000u }, { 0xFBB6u, 0x0000u }, { 0xFBB7u, 0x0000u }, 
    { 0xFBB8u, 0x0000u }, { 0xFBB9u, 0x0000u }, { 0xFBBAu, 0x0000u }, { 0xFBBBu, 0x0000u }, 
    { 0xFBBCu, 0x0000u }, { 0xFBBDu, 0x0000u }, { 0xFBBEu, 0x0000u }, { 0xFBBFu, 0x0000u }, 
    { 0xFBC0u, 0x0000u }, { 0xFBC1u, 0x0000u }, { 0xFBC2u, 0x0000u }, { 0xFBC3u, 0x0000u }, 
    { 0xFBC4u, 0x0000u }, { 0xFBC5u, 0x0000u }, { 0xFBC6u, 0x0000u }, { 0xFBC7u, 0x0000u }, 
    { 0xFBC8u, 0x0000u }, { 0xFBC9u, 0x0000u }, { 0xFBCAu, 0x0000u }, { 0xFBCBu, 0x0000u }, 
    { 0xFBCCu, 0x0000u }, { 0xFBCDu, 0x0000u }, { 0xFBCEu, 0x0000u }, { 0xFBCFu, 0x0000u }, 
    { 0xFBD0u, 0x0000u }, { 0xFBD1u, 0x0000u }, { 0xFBD2u, 0x0000u }, { 0x06ADu, 0x0000u }, 
    { 0x06ADu, 0x0000u }, { 0x06ADu, 0x0000u }, { 0x06ADu, 0x0000u }, { 0x06C7u, 0x0000u }, 
    { 0x06C7u, 0x0000u }, { 0x06C6u, 0x0000u }, { 0x06C6u, 0x0000u }, { 0x06C8u, 0x0000u }, 
    { 0x06C8u, 0x0000u }, { 0x0677u, 0x0000u }, { 0x06CBu, 0x0000u }, { 0x06CBu, 0x0000u }, 
    { 0x06C5u, 0x0000u }, { 0x06C5u, 0x0000u }, { 0x06C9u, 0x0000u }, { 0x06C9u, 0x0000u }, 
    { 0x06D0u, 0x0000u }, { 0x06D0u, 0x0000u }, { 0x06D0u, 0x0000u }, { 0x06D0u, 0x0000u }, 
    { 0x0649u, 0x0000u }, { 0x0649u, 0x0000u }, { 0x0626u, 0x0627u }, { 0x0626u, 0x0627u }, 
    { 0x0626u, 0x06D5u }, { 0x0626u, 0x06D5u }, { 0x0626u, 0x0648u }, { 0x0626u, 0x0648u }, 
    { 0x0626u, 0x06C7u }, { 0x0626u, 0x06C7u }, { 0x0626u, 0x06C6u }, { 0x0626u, 0x06C6u }, 
    { 0x0626u, 0x06C8u }, { 0x0626u, 0x06C8u }, { 0x0626u, 0x06D0u }, { 0x0626u, 0x06D0u }, 
    { 0x0626u, 0x06D0u }, { 0x0626u, 0x0649u }, { 0x0626u, 0x0649u }, { 0x0626u, 0x0649u }, 
    { 0x06CCu, 0x0000u }, { 0x06CCu, 0x0000u }, { 0x06CCu, 0x0000u }, { 0x06CCu, 0x0000u }, 
    { 0x0626u, 0x062Cu }, { 0x0626u, 0x062Du }, { 0x0626u, 0x0645u }, { 0x0626u, 0x0649u }, 
    { 0x0626u, 0x064Au }, { 0x0628u, 0x062Cu }, { 0x0628u, 0x062Du }, { 0x0628u, 0x062Eu }, 
    { 0x0628u, 0x0645u }, { 0x0628u, 0x0649u }, { 0x0628u, 0x064Au }, { 0x062Au, 0x062Cu }, 
    { 0x062Au, 0x062Du }, { 0x062Au, 0x062Eu }, { 0x062Au, 0x0645u }, { 0x062Au, 0x0649u }, 
    { 0x062Au, 0x064Au }, { 0x062Bu, 0x062Cu }, { 0x062Bu, 0x0645u }, { 0x062Bu, 0x0649u }, 
    { 0x062Bu, 0x064Au }, { 0x062Cu, 0x062Du }, { 0x062Cu, 0x0645u }, { 0x062Du, 0x062Cu }, 
    { 0x062Du, 0x0645u }, { 0x062Eu, 0x062Cu }, { 0x062Eu, 0x062Du }, { 0x062Eu, 0x0645u }, 
    { 0x0633u, 0x062Cu }, { 0x0633u, 0x062Du }, { 0x0633u, 0x062Eu }, { 0x0633u, 0x0645u }, 
    { 0x0635u, 0x062Du }, { 0x0635u, 0x0645u }, { 0x0636u, 0x062Cu }, { 0x0636u, 0x062Du }, 
    { 0x0636u, 0x062Eu }, { 0x0636u, 0x0645u }, { 0x0637u, 0x062Du }, { 0x0637u, 0x0645u }, 
    { 0x0638u, 0x0645u }, { 0x0639u, 0x062Cu }, { 0x0639u, 0x0645u }, { 0x063Au, 0x062Cu }, 
    { 0x063Au, 0x0645u }, { 0x0641u, 0x062Cu }, { 0x0641u, 0x062Du }, { 0x0641u, 0x062Eu }, 
    { 0x0641u, 0x0645u }, { 0x0641u, 0x0649u }, { 0x0641u, 0x064Au }, { 0x0642u, 0x062Du }, 
    { 0x0642u, 0x0645u }, { 0x0642u, 0x0649u }, { 0x0642u, 0x064Au }, { 0x0643u, 0x0627u }, 
    { 0x0643u, 0x062Cu }, { 0x0643u, 0x062Du }, { 0x0643u, 0x062Eu }, { 0x0643u, 0x0644u }, 
    { 0x0643u, 0x0645u }, { 0x0643u, 0x0649u }, { 0x0643u, 0x064Au }, { 0x0644u, 0x062Cu }, 
    { 0x0644u, 0x062Du }, { 0x0644u, 0x062Eu }, { 0x0644u, 0x0645u }, { 0x0644u, 0x0649u }, 
    { 0x0644u, 0x064Au }, { 0x0645u, 0x062Cu }, { 0x0645u, 0x062Du }, { 0x0645u, 0x062Eu }, 
    { 0x0645u, 0x0645u }, { 0x0645u, 0x0649u }, { 0x0645u, 0x064Au }, { 0x0646u, 0x062Cu }, 
    { 0x0646u, 0x062Du }, { 0x0646u, 0x062Eu }, { 0x0646u, 0x0645u }, { 0x0646u, 0x0649u }, 
    { 0x0646u, 0x064Au }, { 0x0647u, 0x062Cu }, { 0x0647u, 0x0645u }, { 0x0647u, 0x0649u }, 
    { 0x0647u, 0x064Au }, { 0x064Au, 0x062Cu }, { 0x064Au, 0x062Du }, { 0x064Au, 0x062Eu }, 
    { 0x064Au, 0x0645u }, { 0x064Au, 0x0649u }, { 0x064Au, 0x064Au }, { 0x0630u, 0x0670u }, 
    { 0x0631u, 0x0670u }, { 0x0649u, 0x0670u }, { 0x0020u, 0x064Cu }, { 0x0020u, 0x064Du }, 
    { 0x0020u, 0x064Eu }, { 0x0020u, 0x064Fu }, { 0x0020u, 0x0650u }, { 0x0020u, 0x0651u }, 
    { 0x0626u, 0x0631u }, { 0x0626u, 0x0632u }, { 0x0626u, 0x0645u }, { 0x0626u, 0x0646u }, 
    { 0x0626u, 0x0649u }, { 0x0626u, 0x064Au }, { 0x0628u, 0x0631u }, { 0x0628u, 0x0632u }, 
    { 0x0628u, 0x0645u }, { 0x0628u, 0x0646u }, { 0x0628u, 0x0649u }, { 0x0628u, 0x064Au }, 
    { 0x062Au, 0x0631u }, { 0x062Au, 0x0632u }, { 0x062Au, 0x0645u }, { 0x062Au, 0x0646u }, 
    { 0x062Au, 0x0649u }, { 0x062Au, 0x064Au }, { 0x062Bu, 0x0631u }, { 0x062Bu, 0x0632u }, 
    { 0x062Bu, 0x0645u }, { 0x062Bu, 0x0646u }, { 0x062Bu, 0x0649u }, { 0x062Bu, 0x064Au }, 
    { 0x0641u, 0x0649u }, { 0x0641u, 0x064Au }, { 0x0642u, 0x0649u }, { 0x0642u, 0x064Au }, 
    { 0x0643u, 0x0627u }, { 0x0643u, 0x0644u }, { 0x0643u, 0x0645u }, { 0x0643u, 0x0649u }, 
    { 0x0643u, 0x064Au }, { 0x0644u, 0x0645u }, { 0x0644u, 0x0649u }, { 0x0644u, 0x064Au }, 
    { 0x0645u, 0x0627u }, { 0x0645u, 0x0645u }, { 0x0646u, 0x0631u }, { 0x0646u, 0x0632u }, 
    { 0x0646u, 0x0645u }, { 0x0646u, 0x0646u }, { 0x0646u, 0x0649u }, { 0x0646u, 0x064Au }, 
    { 0x0649u, 0x0670u }, { 0x064Au, 0x0631u }, { 0x064Au, 0x0632u }, { 0x064Au, 0x0645u }, 
    { 0x064Au, 0x0646u }, { 0x064Au, 0x0649u }, { 0x064Au, 0x064Au }, { 0x0626u, 0x062Cu }, 
    { 0x0626u, 0x062Du }, { 0x0626u, 0x062Eu }, { 0x0626u, 0x0645u }, { 0x0626u, 0x0647u }, 
    { 0x0628u, 0x062Cu }, { 0x0628u, 0x062Du }, { 0x0628u, 0x062Eu }, { 0x0628u, 0x0645u }, 
    { 0x0628u, 0x0647u }, { 0x062Au, 0x062Cu }, { 0x062Au, 0x062Du }, { 0x062Au, 0x062Eu }, 
    { 0x062Au, 0x0645u }, { 0x062Au, 0x0647u }, { 0x062Bu, 0x0645u }, { 0x062Cu, 0x062Du }, 
    { 0x062Cu, 0x0645u }, { 0x062Du, 0x062Cu }, { 0x062Du, 0x0645u }, { 0x062Eu, 0x062Cu }, 
    { 0x062Eu, 0x0645u }, { 0x0633u, 0x062Cu }, { 0x0633u, 0x062Du }, { 0x0633u, 0x062Eu }, 
    { 0x0633u, 0x0645u }, { 0x0635u, 0x062Du }, { 0x0635u, 0x062Eu }, { 0x0635u, 0x0645u }, 
    { 0x0636u, 0x062Cu }, { 0x0636u, 0x062Du }, { 0x0636u, 0x062Eu }, { 0x0636u, 0x0645u }, 
    { 0x0637u, 0x062Du }, { 0x0638u, 0x0645u }, { 0x0639u, 0x062Cu }, { 0x0639u, 0x0645u }, 
    { 0x063Au, 0x062Cu }, { 0x063Au, 0x0645u }, { 0x0641u, 0x062Cu }, { 0x0641u, 0x062Du }, 
    { 0x0641u, 0x062Eu }, { 0x0641u, 0x0645u }, { 0x0642u, 0x062Du }, { 0x0642u, 0x0645u }, 
    { 0x0643u, 0x062Cu }, { 0x0643u, 0x062Du }, { 0x0643u, 0x062Eu }, { 0x0643u, 0x0644u }, 
    { 0x0643u, 0x0645u }, { 0x0644u, 0x062Cu }, { 0x0644u, 0x062Du }, { 0x0644u, 0x062Eu }, 
    { 0x0644u, 0x0645u }, { 0x0644u, 0x0647u }, { 0x0645u, 0x062Cu }, { 0x0645u, 0x062Du }, 
    { 0x0645u, 0x062Eu }, { 0x0645u, 0x0645u }, { 0x0646u, 0x062Cu }, { 0x0646u, 0x062Du }, 
    { 0x0646u, 0x062Eu }, { 0x0646u, 0x0645u }, { 0x0646u, 0x0647u }, { 0x0647u, 0x062Cu }, 
    { 0x0647u, 0x0645u }, { 0x0647u, 0x0670u }, { 0x064Au, 0x062Cu }, { 0x064Au, 0x062Du }, 
    { 0x064Au, 0x062Eu }, { 0x064Au, 0x0645u }, { 0x064Au, 0x0647u }, { 0x0626u, 0x0645u }, 
    { 0x0626u, 0x0647u }, { 0x0628u, 0x0645u }, { 0x0628u, 0x0647u }, { 0x062Au, 0x0645u }, 
    { 0x062Au, 0x0647u }, { 0x062Bu, 0x0645u }, { 0x062Bu, 0x0647u }, { 0x0633u, 0x0645u }, 
    { 0x0633u, 0x0647u }, { 0x0634u, 0x0645u }, { 0x0634u, 0x0647u }, { 0x0643u, 0x0644u }, 
    { 0x0643u, 0x0645u }, { 0x0644u, 0x0645u }, { 0x0646u, 0x0645u }, { 0x0646u, 0x0647u }, 
    { 0x064Au, 0x0645u }, { 0x064Au, 0x0647u }, { 0x0640u, 0x064Eu }, { 0x0640u, 0x064Fu }, 
    { 0x0640u, 0x0650u }, { 0x0637u, 0x0649u }, { 0x0637u, 0x064Au }, { 0x0639u, 0x0649u }, 
    { 0x0639u, 0x064Au }, { 0x063Au, 0x0649u }, { 0x063Au, 0x064Au }, { 0x0633u, 0x0649u }, 
    { 0x0633u, 0x064Au }, { 0x0634u, 0x0649u }, { 0x0634u, 0x064Au }, { 0x062Du, 0x0649u }, 
    { 0x062Du, 0x064Au }, { 0x062Cu, 0x0649u }, { 0x062Cu, 0x064Au }, { 0x062Eu, 0x0649u }, 
    { 0x062Eu, 0x064Au }, { 0x0635u, 0x0649u }, { 0x0635u, 0x064Au }, { 0x0636u, 0x0649u }, 
    { 0x0636u, 0x064Au }, { 0x0634u, 0x062Cu }, { 0x0634u, 0x062Du }, { 0x0634u, 0x062Eu }, 
    { 0x0634u, 0x0645u }, { 0x0634u, 0x0631u }, { 0x0633u, 0x0631u }, { 0x0635u, 0x0631u }, 
    { 0x0636u, 0x0631u }, { 0x0637u, 0x0649u }, { 0x0637u, 0x064Au }, { 0x0639u, 0x0649u }, 
    { 0x0639u, 0x064Au }, { 0x063Au, 0x0649u }, { 0x063Au, 0x064Au }, { 0x0633u, 0x0649u }, 
    { 0x0633u, 0x064Au }, { 0x0634u, 0x0649u }, { 0x0634u, 0x064Au }, { 0x062Du, 0x0649u }, 
    { 0x062Du, 0x064Au }, { 0x062Cu, 0x0649u }, { 0x062Cu, 0x064Au }, { 0x062Eu, 0x0649u }, 
    { 0x062Eu, 0x064Au }, { 0x0635u, 0x0649u }, { 0x0635u, 0x064Au }, { 0x0636u, 0x0649u }, 
    { 0x0636u, 0x064Au }, { 0x0634u, 0x062Cu }, { 0x0634u, 0x062Du }, { 0x0634u, 0x062Eu }, 
    { 0x0634u, 0x0645u }, { 0x0634u, 0x0631u }, { 0x0633u, 0x0631u }, { 0x0635u, 0x0631u }, 
    { 0x0636u, 0x0631u }, { 0x0634u, 0x062Cu }, { 0x0634u, 0x062Du }, { 0x0634u, 0x062Eu }, 
    { 0x0634u, 0x0645u }, { 0x0633u, 0x0647u }, { 0x0634u, 0x0647u }, { 0x0637u, 0x0645u }, 
    { 0x0633u, 0x062Cu }, { 0x0633u, 0x062Du }, { 0x0633u, 0x062Eu }, { 0x0634u, 0x062Cu }, 
    { 0x0634u, 0x062Du }, { 0x0634u, 0x062Eu }, { 0x0637u, 0x0645u }, { 0x0638u, 0x0645u }, 
    { 0x0627u, 0x064Bu }, { 0x0627u, 0x064Bu }, { 0xFD3Eu, 0x0000u }, { 0xFD3Fu, 0x0000u }, 
    { 0xFD40u, 0x0000u }, { 0xFD41u, 0x0000u }, { 0xFD42u, 0x0000u }, { 0xFD43u, 0x0000u }, 
    { 0xFD44u, 0x0000u }, { 0xFD45u, 0x0000u }, { 0xFD46u, 0x0000u }, { 0xFD47u, 0x0000u }, 
    { 0xFD48u, 0x0000u }, { 0xFD49u, 0x0000u }, { 0xFD4Au, 0x0000u }, { 0xFD4Bu, 0x0000u }, 
    { 0xFD4Cu, 0x0000u }, { 0xFD4Du, 0x0000u }, { 0xFD4Eu, 0x0000u }, { 0xFD4Fu, 0x0000u }, 
    { 0x062Au, 0x062Cu }, { 0x062Au, 0x062Du }, { 0x062Au, 0x062Du }, { 0x062Au, 0x062Du }, 
    { 0x062Au, 0x062Eu }, { 0x062Au, 0x0645u }, { 0x062Au, 0x0645u }, { 0x062Au, 0x0645u }, 
    { 0x062Cu, 0x0645u }, { 0x062Cu, 0x0645u }, { 0x062Du, 0x0645u }, { 0x062Du, 0x0645u }, 
    { 0x0633u, 0x062Du }, { 0x0633u, 0x062Cu }, { 0x0633u, 0x062Cu }, { 0x0633u, 0x0645u }, 
    { 0x0633u, 0x0645u }, { 0x0633u, 0x0645u }, { 0x0633u, 0x0645u }, { 0x0633u, 0x0645u }, 
    { 0x0635u, 0x062Du }, { 0x0635u, 0x062Du }, { 0x0635u, 0x0645u }, { 0x0634u, 0x062Du }, 
    { 0x0634u, 0x062Du }, { 0x0634u, 0x062Cu }, { 0x0634u, 0x0645u }, { 0x0634u, 0x0645u }, 
    { 0x0634u, 0x0645u }, { 0x0634u, 0x0645u }, { 0x0636u, 0x062Du }, { 0x0636u, 0x062Eu }, 
    { 0x0636u, 0x062Eu }, { 0x0637u, 0x0645u }, { 0x0637u, 0x0645u }, { 0x0637u, 0x0645u }, 
    { 0x0637u, 0x0645u }, { 0x0639u, 0x062Cu }, { 0x0639u, 0x0645u }, { 0x0639u, 0x0645u }, 
    { 0x0639u, 0x0645u }, { 0x063Au, 0x0645u }, { 0x063Au, 0x0645u }, { 0x063Au, 0x0645u }, 
    { 0x0641u, 0x062Eu }, { 0x0641u, 0x062Eu }, { 0x0642u, 0x0645u }, { 0x0642u, 0x0645u }, 
    { 0x0644u, 0x062Du }, { 0x0644u, 0x062Du }, { 0x0644u, 0x062Du }, { 0x0644u, 0x062Cu }, 
    { 0x0644u, 0x062Cu }, { 0x0644u, 0x062Eu }, { 0x0644u, 0x062Eu }, { 0x0644u, 0x0645u }, 
    { 0x0644u, 0x0645u }, { 0x0645u, 0x062Du }, { 0x0645u, 0x062Du }, { 0x0645u, 0x062Du }, 
    { 0x0645u, 0x062Cu }, { 0x0645u, 0x062Cu }, { 0x0645u, 0x062Eu }, { 0x0645u, 0x062Eu }, 
    { 0xFD90u, 0x0000u }, { 0xFD91u, 0x0000u }, { 0x0645u, 0x062Cu }, { 0x0647u, 0x0645u }, 
    { 0x0647u, 0x0645u }, { 0x0646u, 0x062Du }, { 0x0646u, 0x062Du }, { 0x0646u, 0x062Cu }, 
    { 0x0646u, 0x062Cu }, { 0x0646u, 0x062Cu }, { 0x0646u, 0x0645u }, { 0x0646u, 0x0645u }, 
    { 0x064Au, 0x0645u }, { 0x064Au, 0x0645u }, { 0x0628u, 0x062Eu }, { 0x062Au, 0x062Cu }, 
    { 0x062Au, 0x062Cu }, { 0x062Au, 0x062Eu }, { 0x062Au, 0x062Eu }, { 0x062Au, 0x0645u }, 
    { 0x062Au, 0x0645u }, { 0x062Cu, 0x0645u }, { 0x062Cu, 0x062Du }, { 0x062Cu, 0x0645u }, 
    { 0x0633u, 0x062Eu }, { 0x0635u, 0x062Du }, { 0x0634u, 0x062Du }, { 0x0636u, 0x062Du }, 
    { 0x0644u, 0x062Cu }, { 0x0644u, 0x0645u }, { 0x064Au, 0x062Du }, { 0x064Au, 0x062Cu }, 
    { 0x064Au, 0x0645u }, { 0x0645u, 0x0645u }, { 0x0642u, 0x0645u }, { 0x0646u, 0x062Du }, 
    { 0x0642u, 0x0645u }, { 0x0644u, 0x062Du }, { 0x0639u, 0x0645u }, { 0x0643u, 0x0645u }, 
    { 0x0646u, 0x062Cu }, { 0x0645u, 0x062Eu }, { 0x0644u, 0x062Cu }, { 0x0643u, 0x0645u }, 
    { 0x0644u, 0x062Cu }, { 0x0646u, 0x062Cu }, { 0x062Cu, 0x062Du }, { 0x062Du, 0x062Cu }, 
    { 0x0645u, 0x062Cu }, { 0x0641u, 0x0645u }, { 0x0628u, 0x062Du }, { 0x0643u, 0x0645u }, 
    { 0x0639u, 0x062Cu }, { 0x0635u, 0x0645u }, { 0x0633u, 0x062Eu }, { 0x0646u, 0x062Cu }, 
    { 0xFDC8u, 0x0000u }, { 0xFDC9u, 0x0000u }, { 0xFDCAu, 0x0000u }, { 0xFDCBu, 0x0000u }, 
    { 0xFDCCu, 0x0000u }, { 0xFDCDu, 0x0000u }, { 0xFDCEu, 0x0000u }, { 0xFDCFu, 0x0000u }, 
    { 0xFDD0u, 0x0000u }, { 0xFDD1u, 0x0000u }, { 0xFDD2u, 0x0000u }, { 0xFDD3u, 0x0000u }, 
    { 0xFDD4u, 0x0000u }, { 0xFDD5u, 0x0000u }, { 0xFDD6u, 0x0000u }, { 0xFDD7u, 0x0000u }, 
    { 0xFDD8u, 0x0000u }, { 0xFDD9u, 0x0000u }, { 0xFDDAu, 0x0000u }, { 0xFDDBu, 0x0000u }, 
    { 0xFDDCu, 0x0000u }, { 0xFDDDu, 0x0000u }, { 0xFDDEu, 0x0000u }, { 0xFDDFu, 0x0000u }, 
    { 0xFDE0u, 0x0000u }, { 0xFDE1u, 0x0000u }, { 0xFDE2u, 0x0000u }, { 0xFDE3u, 0x0000u }, 
    { 0xFDE4u, 0x0000u }, { 0xFDE5u, 0x0000u }, { 0xFDE6u, 0x0000u }, { 0xFDE7u, 0x0000u }, 
    { 0xFDE8u, 0x0000u }, { 0xFDE9u, 0x0000u }, { 0xFDEAu, 0x0000u }, { 0xFDEBu, 0x0000u }, 
    { 0xFDECu, 0x0000u }, { 0xFDEDu, 0x0000u }, { 0xFDEEu, 0x0000u }, { 0xFDEFu, 0x0000u }, 
    { 0x0635u, 0x0644u }, { 0x0642u, 0x0644u }, { 0x0627u, 0x0644u }, { 0x0627u, 0x0643u }, 
    { 0x0645u, 0x062Du }, { 0x0635u, 0x0644u }, { 0x0631u, 0x0633u }, { 0x0639u, 0x0644u }, 
    { 0x0648u, 0x0633u }, { 0x0635u, 0x0644u }, { 0x0635u, 0x0644u }, { 0x062Cu, 0x0644u }, 
    { 0xFDFCu, 0x0000u }, { 0xFDFDu, 0x0000u }, { 0xFDFEu, 0x0000u }, { 0xFDFFu, 0x0000u }
  };

  const unsigned char Arabic_Presentation_FormsAFB50::m_lb[] = {
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_OP, LB_CL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL
  };

    const std::bitset<688> Arabic_Presentation_FormsAFB50::m_Noncharacter_Code_Point(std::string("0000000000000000111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Arabic_Presentation_FormsAFB50);
