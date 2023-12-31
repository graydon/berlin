/*$Id: 0-7F.cc
 *
 * This source file is a part of the Berlin Project
 * Copyright (C) 1999 Tobias Hunger <tobias@berlin-consortium.org>
 * http://www.berlin-consortium.org
 *
 * It was automatically created from the files available at
 * ftp.unicode.org on Fri, 30 Mar 2001 17:45:53 +0200.
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

  class Basic_Latin0 : public Babylon::Dictionary::Block {
  public:
    void clean () {
    };

    Basic_Latin0() {
      m_first_letter = 0x0;
      m_last_letter  = 0x7F;
      // m_version="3.1" // Not yet supported!
      m_composeMap[make_pair(0x0000003C, 0x00000338)] = 0x226E;
      m_composeMap[make_pair(0x0000003D, 0x00000338)] = 0x2260;
      m_composeMap[make_pair(0x0000003E, 0x00000338)] = 0x226F;
      m_composeMap[make_pair(0x00000041, 0x00000300)] = 0x00C0;
      m_composeMap[make_pair(0x00000041, 0x00000301)] = 0x00C1;
      m_composeMap[make_pair(0x00000041, 0x00000302)] = 0x00C2;
      m_composeMap[make_pair(0x00000041, 0x00000303)] = 0x00C3;
      m_composeMap[make_pair(0x00000041, 0x00000304)] = 0x0100;
      m_composeMap[make_pair(0x00000041, 0x00000306)] = 0x0102;
      m_composeMap[make_pair(0x00000041, 0x00000307)] = 0x0226;
      m_composeMap[make_pair(0x00000041, 0x00000308)] = 0x00C4;
      m_composeMap[make_pair(0x00000041, 0x00000309)] = 0x1EA2;
      m_composeMap[make_pair(0x00000041, 0x0000030A)] = 0x00C5;
      m_composeMap[make_pair(0x00000041, 0x0000030C)] = 0x01CD;
      m_composeMap[make_pair(0x00000041, 0x0000030F)] = 0x0200;
      m_composeMap[make_pair(0x00000041, 0x00000311)] = 0x0202;
      m_composeMap[make_pair(0x00000041, 0x00000323)] = 0x1EA0;
      m_composeMap[make_pair(0x00000041, 0x00000325)] = 0x1E00;
      m_composeMap[make_pair(0x00000041, 0x00000328)] = 0x0104;
      m_composeMap[make_pair(0x00000042, 0x00000307)] = 0x1E02;
      m_composeMap[make_pair(0x00000042, 0x00000323)] = 0x1E04;
      m_composeMap[make_pair(0x00000042, 0x00000331)] = 0x1E06;
      m_composeMap[make_pair(0x00000043, 0x00000301)] = 0x0106;
      m_composeMap[make_pair(0x00000043, 0x00000302)] = 0x0108;
      m_composeMap[make_pair(0x00000043, 0x00000307)] = 0x010A;
      m_composeMap[make_pair(0x00000043, 0x0000030C)] = 0x010C;
      m_composeMap[make_pair(0x00000043, 0x00000327)] = 0x00C7;
      m_composeMap[make_pair(0x00000044, 0x00000307)] = 0x1E0A;
      m_composeMap[make_pair(0x00000044, 0x0000030C)] = 0x010E;
      m_composeMap[make_pair(0x00000044, 0x00000323)] = 0x1E0C;
      m_composeMap[make_pair(0x00000044, 0x00000327)] = 0x1E10;
      m_composeMap[make_pair(0x00000044, 0x0000032D)] = 0x1E12;
      m_composeMap[make_pair(0x00000044, 0x00000331)] = 0x1E0E;
      m_composeMap[make_pair(0x00000045, 0x00000300)] = 0x00C8;
      m_composeMap[make_pair(0x00000045, 0x00000301)] = 0x00C9;
      m_composeMap[make_pair(0x00000045, 0x00000302)] = 0x00CA;
      m_composeMap[make_pair(0x00000045, 0x00000303)] = 0x1EBC;
      m_composeMap[make_pair(0x00000045, 0x00000304)] = 0x0112;
      m_composeMap[make_pair(0x00000045, 0x00000306)] = 0x0114;
      m_composeMap[make_pair(0x00000045, 0x00000307)] = 0x0116;
      m_composeMap[make_pair(0x00000045, 0x00000308)] = 0x00CB;
      m_composeMap[make_pair(0x00000045, 0x00000309)] = 0x1EBA;
      m_composeMap[make_pair(0x00000045, 0x0000030C)] = 0x011A;
      m_composeMap[make_pair(0x00000045, 0x0000030F)] = 0x0204;
      m_composeMap[make_pair(0x00000045, 0x00000311)] = 0x0206;
      m_composeMap[make_pair(0x00000045, 0x00000323)] = 0x1EB8;
      m_composeMap[make_pair(0x00000045, 0x00000327)] = 0x0228;
      m_composeMap[make_pair(0x00000045, 0x00000328)] = 0x0118;
      m_composeMap[make_pair(0x00000045, 0x0000032D)] = 0x1E18;
      m_composeMap[make_pair(0x00000045, 0x00000330)] = 0x1E1A;
      m_composeMap[make_pair(0x00000046, 0x00000307)] = 0x1E1E;
      m_composeMap[make_pair(0x00000047, 0x00000301)] = 0x01F4;
      m_composeMap[make_pair(0x00000047, 0x00000302)] = 0x011C;
      m_composeMap[make_pair(0x00000047, 0x00000304)] = 0x1E20;
      m_composeMap[make_pair(0x00000047, 0x00000306)] = 0x011E;
      m_composeMap[make_pair(0x00000047, 0x00000307)] = 0x0120;
      m_composeMap[make_pair(0x00000047, 0x0000030C)] = 0x01E6;
      m_composeMap[make_pair(0x00000047, 0x00000327)] = 0x0122;
      m_composeMap[make_pair(0x00000048, 0x00000302)] = 0x0124;
      m_composeMap[make_pair(0x00000048, 0x00000307)] = 0x1E22;
      m_composeMap[make_pair(0x00000048, 0x00000308)] = 0x1E26;
      m_composeMap[make_pair(0x00000048, 0x0000030C)] = 0x021E;
      m_composeMap[make_pair(0x00000048, 0x00000323)] = 0x1E24;
      m_composeMap[make_pair(0x00000048, 0x00000327)] = 0x1E28;
      m_composeMap[make_pair(0x00000048, 0x0000032E)] = 0x1E2A;
      m_composeMap[make_pair(0x00000049, 0x00000300)] = 0x00CC;
      m_composeMap[make_pair(0x00000049, 0x00000301)] = 0x00CD;
      m_composeMap[make_pair(0x00000049, 0x00000302)] = 0x00CE;
      m_composeMap[make_pair(0x00000049, 0x00000303)] = 0x0128;
      m_composeMap[make_pair(0x00000049, 0x00000304)] = 0x012A;
      m_composeMap[make_pair(0x00000049, 0x00000306)] = 0x012C;
      m_composeMap[make_pair(0x00000049, 0x00000307)] = 0x0130;
      m_composeMap[make_pair(0x00000049, 0x00000308)] = 0x00CF;
      m_composeMap[make_pair(0x00000049, 0x00000309)] = 0x1EC8;
      m_composeMap[make_pair(0x00000049, 0x0000030C)] = 0x01CF;
      m_composeMap[make_pair(0x00000049, 0x0000030F)] = 0x0208;
      m_composeMap[make_pair(0x00000049, 0x00000311)] = 0x020A;
      m_composeMap[make_pair(0x00000049, 0x00000323)] = 0x1ECA;
      m_composeMap[make_pair(0x00000049, 0x00000328)] = 0x012E;
      m_composeMap[make_pair(0x00000049, 0x00000330)] = 0x1E2C;
      m_composeMap[make_pair(0x0000004A, 0x00000302)] = 0x0134;
      m_composeMap[make_pair(0x0000004B, 0x00000301)] = 0x1E30;
      m_composeMap[make_pair(0x0000004B, 0x0000030C)] = 0x01E8;
      m_composeMap[make_pair(0x0000004B, 0x00000323)] = 0x1E32;
      m_composeMap[make_pair(0x0000004B, 0x00000327)] = 0x0136;
      m_composeMap[make_pair(0x0000004B, 0x00000331)] = 0x1E34;
      m_composeMap[make_pair(0x0000004C, 0x00000301)] = 0x0139;
      m_composeMap[make_pair(0x0000004C, 0x0000030C)] = 0x013D;
      m_composeMap[make_pair(0x0000004C, 0x00000323)] = 0x1E36;
      m_composeMap[make_pair(0x0000004C, 0x00000327)] = 0x013B;
      m_composeMap[make_pair(0x0000004C, 0x0000032D)] = 0x1E3C;
      m_composeMap[make_pair(0x0000004C, 0x00000331)] = 0x1E3A;
      m_composeMap[make_pair(0x0000004D, 0x00000301)] = 0x1E3E;
      m_composeMap[make_pair(0x0000004D, 0x00000307)] = 0x1E40;
      m_composeMap[make_pair(0x0000004D, 0x00000323)] = 0x1E42;
      m_composeMap[make_pair(0x0000004E, 0x00000300)] = 0x01F8;
      m_composeMap[make_pair(0x0000004E, 0x00000301)] = 0x0143;
      m_composeMap[make_pair(0x0000004E, 0x00000303)] = 0x00D1;
      m_composeMap[make_pair(0x0000004E, 0x00000307)] = 0x1E44;
      m_composeMap[make_pair(0x0000004E, 0x0000030C)] = 0x0147;
      m_composeMap[make_pair(0x0000004E, 0x00000323)] = 0x1E46;
      m_composeMap[make_pair(0x0000004E, 0x00000327)] = 0x0145;
      m_composeMap[make_pair(0x0000004E, 0x0000032D)] = 0x1E4A;
      m_composeMap[make_pair(0x0000004E, 0x00000331)] = 0x1E48;
      m_composeMap[make_pair(0x0000004F, 0x00000300)] = 0x00D2;
      m_composeMap[make_pair(0x0000004F, 0x00000301)] = 0x00D3;
      m_composeMap[make_pair(0x0000004F, 0x00000302)] = 0x00D4;
      m_composeMap[make_pair(0x0000004F, 0x00000303)] = 0x00D5;
      m_composeMap[make_pair(0x0000004F, 0x00000304)] = 0x014C;
      m_composeMap[make_pair(0x0000004F, 0x00000306)] = 0x014E;
      m_composeMap[make_pair(0x0000004F, 0x00000307)] = 0x022E;
      m_composeMap[make_pair(0x0000004F, 0x00000308)] = 0x00D6;
      m_composeMap[make_pair(0x0000004F, 0x00000309)] = 0x1ECE;
      m_composeMap[make_pair(0x0000004F, 0x0000030B)] = 0x0150;
      m_composeMap[make_pair(0x0000004F, 0x0000030C)] = 0x01D1;
      m_composeMap[make_pair(0x0000004F, 0x0000030F)] = 0x020C;
      m_composeMap[make_pair(0x0000004F, 0x00000311)] = 0x020E;
      m_composeMap[make_pair(0x0000004F, 0x0000031B)] = 0x01A0;
      m_composeMap[make_pair(0x0000004F, 0x00000323)] = 0x1ECC;
      m_composeMap[make_pair(0x0000004F, 0x00000328)] = 0x01EA;
      m_composeMap[make_pair(0x00000050, 0x00000301)] = 0x1E54;
      m_composeMap[make_pair(0x00000050, 0x00000307)] = 0x1E56;
      m_composeMap[make_pair(0x00000052, 0x00000301)] = 0x0154;
      m_composeMap[make_pair(0x00000052, 0x00000307)] = 0x1E58;
      m_composeMap[make_pair(0x00000052, 0x0000030C)] = 0x0158;
      m_composeMap[make_pair(0x00000052, 0x0000030F)] = 0x0210;
      m_composeMap[make_pair(0x00000052, 0x00000311)] = 0x0212;
      m_composeMap[make_pair(0x00000052, 0x00000323)] = 0x1E5A;
      m_composeMap[make_pair(0x00000052, 0x00000327)] = 0x0156;
      m_composeMap[make_pair(0x00000052, 0x00000331)] = 0x1E5E;
      m_composeMap[make_pair(0x00000053, 0x00000301)] = 0x015A;
      m_composeMap[make_pair(0x00000053, 0x00000302)] = 0x015C;
      m_composeMap[make_pair(0x00000053, 0x00000307)] = 0x1E60;
      m_composeMap[make_pair(0x00000053, 0x0000030C)] = 0x0160;
      m_composeMap[make_pair(0x00000053, 0x00000323)] = 0x1E62;
      m_composeMap[make_pair(0x00000053, 0x00000326)] = 0x0218;
      m_composeMap[make_pair(0x00000053, 0x00000327)] = 0x015E;
      m_composeMap[make_pair(0x00000054, 0x00000307)] = 0x1E6A;
      m_composeMap[make_pair(0x00000054, 0x0000030C)] = 0x0164;
      m_composeMap[make_pair(0x00000054, 0x00000323)] = 0x1E6C;
      m_composeMap[make_pair(0x00000054, 0x00000326)] = 0x021A;
      m_composeMap[make_pair(0x00000054, 0x00000327)] = 0x0162;
      m_composeMap[make_pair(0x00000054, 0x0000032D)] = 0x1E70;
      m_composeMap[make_pair(0x00000054, 0x00000331)] = 0x1E6E;
      m_composeMap[make_pair(0x00000055, 0x00000300)] = 0x00D9;
      m_composeMap[make_pair(0x00000055, 0x00000301)] = 0x00DA;
      m_composeMap[make_pair(0x00000055, 0x00000302)] = 0x00DB;
      m_composeMap[make_pair(0x00000055, 0x00000303)] = 0x0168;
      m_composeMap[make_pair(0x00000055, 0x00000304)] = 0x016A;
      m_composeMap[make_pair(0x00000055, 0x00000306)] = 0x016C;
      m_composeMap[make_pair(0x00000055, 0x00000308)] = 0x00DC;
      m_composeMap[make_pair(0x00000055, 0x00000309)] = 0x1EE6;
      m_composeMap[make_pair(0x00000055, 0x0000030A)] = 0x016E;
      m_composeMap[make_pair(0x00000055, 0x0000030B)] = 0x0170;
      m_composeMap[make_pair(0x00000055, 0x0000030C)] = 0x01D3;
      m_composeMap[make_pair(0x00000055, 0x0000030F)] = 0x0214;
      m_composeMap[make_pair(0x00000055, 0x00000311)] = 0x0216;
      m_composeMap[make_pair(0x00000055, 0x0000031B)] = 0x01AF;
      m_composeMap[make_pair(0x00000055, 0x00000323)] = 0x1EE4;
      m_composeMap[make_pair(0x00000055, 0x00000324)] = 0x1E72;
      m_composeMap[make_pair(0x00000055, 0x00000328)] = 0x0172;
      m_composeMap[make_pair(0x00000055, 0x0000032D)] = 0x1E76;
      m_composeMap[make_pair(0x00000055, 0x00000330)] = 0x1E74;
      m_composeMap[make_pair(0x00000056, 0x00000303)] = 0x1E7C;
      m_composeMap[make_pair(0x00000056, 0x00000323)] = 0x1E7E;
      m_composeMap[make_pair(0x00000057, 0x00000300)] = 0x1E80;
      m_composeMap[make_pair(0x00000057, 0x00000301)] = 0x1E82;
      m_composeMap[make_pair(0x00000057, 0x00000302)] = 0x0174;
      m_composeMap[make_pair(0x00000057, 0x00000307)] = 0x1E86;
      m_composeMap[make_pair(0x00000057, 0x00000308)] = 0x1E84;
      m_composeMap[make_pair(0x00000057, 0x00000323)] = 0x1E88;
      m_composeMap[make_pair(0x00000058, 0x00000307)] = 0x1E8A;
      m_composeMap[make_pair(0x00000058, 0x00000308)] = 0x1E8C;
      m_composeMap[make_pair(0x00000059, 0x00000300)] = 0x1EF2;
      m_composeMap[make_pair(0x00000059, 0x00000301)] = 0x00DD;
      m_composeMap[make_pair(0x00000059, 0x00000302)] = 0x0176;
      m_composeMap[make_pair(0x00000059, 0x00000303)] = 0x1EF8;
      m_composeMap[make_pair(0x00000059, 0x00000304)] = 0x0232;
      m_composeMap[make_pair(0x00000059, 0x00000307)] = 0x1E8E;
      m_composeMap[make_pair(0x00000059, 0x00000308)] = 0x0178;
      m_composeMap[make_pair(0x00000059, 0x00000309)] = 0x1EF6;
      m_composeMap[make_pair(0x00000059, 0x00000323)] = 0x1EF4;
      m_composeMap[make_pair(0x0000005A, 0x00000301)] = 0x0179;
      m_composeMap[make_pair(0x0000005A, 0x00000302)] = 0x1E90;
      m_composeMap[make_pair(0x0000005A, 0x00000307)] = 0x017B;
      m_composeMap[make_pair(0x0000005A, 0x0000030C)] = 0x017D;
      m_composeMap[make_pair(0x0000005A, 0x00000323)] = 0x1E92;
      m_composeMap[make_pair(0x0000005A, 0x00000331)] = 0x1E94;
      m_composeMap[make_pair(0x00000061, 0x00000300)] = 0x00E0;
      m_composeMap[make_pair(0x00000061, 0x00000301)] = 0x00E1;
      m_composeMap[make_pair(0x00000061, 0x00000302)] = 0x00E2;
      m_composeMap[make_pair(0x00000061, 0x00000303)] = 0x00E3;
      m_composeMap[make_pair(0x00000061, 0x00000304)] = 0x0101;
      m_composeMap[make_pair(0x00000061, 0x00000306)] = 0x0103;
      m_composeMap[make_pair(0x00000061, 0x00000307)] = 0x0227;
      m_composeMap[make_pair(0x00000061, 0x00000308)] = 0x00E4;
      m_composeMap[make_pair(0x00000061, 0x00000309)] = 0x1EA3;
      m_composeMap[make_pair(0x00000061, 0x0000030A)] = 0x00E5;
      m_composeMap[make_pair(0x00000061, 0x0000030C)] = 0x01CE;
      m_composeMap[make_pair(0x00000061, 0x0000030F)] = 0x0201;
      m_composeMap[make_pair(0x00000061, 0x00000311)] = 0x0203;
      m_composeMap[make_pair(0x00000061, 0x00000323)] = 0x1EA1;
      m_composeMap[make_pair(0x00000061, 0x00000325)] = 0x1E01;
      m_composeMap[make_pair(0x00000061, 0x00000328)] = 0x0105;
      m_composeMap[make_pair(0x00000062, 0x00000307)] = 0x1E03;
      m_composeMap[make_pair(0x00000062, 0x00000323)] = 0x1E05;
      m_composeMap[make_pair(0x00000062, 0x00000331)] = 0x1E07;
      m_composeMap[make_pair(0x00000063, 0x00000301)] = 0x0107;
      m_composeMap[make_pair(0x00000063, 0x00000302)] = 0x0109;
      m_composeMap[make_pair(0x00000063, 0x00000307)] = 0x010B;
      m_composeMap[make_pair(0x00000063, 0x0000030C)] = 0x010D;
      m_composeMap[make_pair(0x00000063, 0x00000327)] = 0x00E7;
      m_composeMap[make_pair(0x00000064, 0x00000307)] = 0x1E0B;
      m_composeMap[make_pair(0x00000064, 0x0000030C)] = 0x010F;
      m_composeMap[make_pair(0x00000064, 0x00000323)] = 0x1E0D;
      m_composeMap[make_pair(0x00000064, 0x00000327)] = 0x1E11;
      m_composeMap[make_pair(0x00000064, 0x0000032D)] = 0x1E13;
      m_composeMap[make_pair(0x00000064, 0x00000331)] = 0x1E0F;
      m_composeMap[make_pair(0x00000065, 0x00000300)] = 0x00E8;
      m_composeMap[make_pair(0x00000065, 0x00000301)] = 0x00E9;
      m_composeMap[make_pair(0x00000065, 0x00000302)] = 0x00EA;
      m_composeMap[make_pair(0x00000065, 0x00000303)] = 0x1EBD;
      m_composeMap[make_pair(0x00000065, 0x00000304)] = 0x0113;
      m_composeMap[make_pair(0x00000065, 0x00000306)] = 0x0115;
      m_composeMap[make_pair(0x00000065, 0x00000307)] = 0x0117;
      m_composeMap[make_pair(0x00000065, 0x00000308)] = 0x00EB;
      m_composeMap[make_pair(0x00000065, 0x00000309)] = 0x1EBB;
      m_composeMap[make_pair(0x00000065, 0x0000030C)] = 0x011B;
      m_composeMap[make_pair(0x00000065, 0x0000030F)] = 0x0205;
      m_composeMap[make_pair(0x00000065, 0x00000311)] = 0x0207;
      m_composeMap[make_pair(0x00000065, 0x00000323)] = 0x1EB9;
      m_composeMap[make_pair(0x00000065, 0x00000327)] = 0x0229;
      m_composeMap[make_pair(0x00000065, 0x00000328)] = 0x0119;
      m_composeMap[make_pair(0x00000065, 0x0000032D)] = 0x1E19;
      m_composeMap[make_pair(0x00000065, 0x00000330)] = 0x1E1B;
      m_composeMap[make_pair(0x00000066, 0x00000307)] = 0x1E1F;
      m_composeMap[make_pair(0x00000067, 0x00000301)] = 0x01F5;
      m_composeMap[make_pair(0x00000067, 0x00000302)] = 0x011D;
      m_composeMap[make_pair(0x00000067, 0x00000304)] = 0x1E21;
      m_composeMap[make_pair(0x00000067, 0x00000306)] = 0x011F;
      m_composeMap[make_pair(0x00000067, 0x00000307)] = 0x0121;
      m_composeMap[make_pair(0x00000067, 0x0000030C)] = 0x01E7;
      m_composeMap[make_pair(0x00000067, 0x00000327)] = 0x0123;
      m_composeMap[make_pair(0x00000068, 0x00000302)] = 0x0125;
      m_composeMap[make_pair(0x00000068, 0x00000307)] = 0x1E23;
      m_composeMap[make_pair(0x00000068, 0x00000308)] = 0x1E27;
      m_composeMap[make_pair(0x00000068, 0x0000030C)] = 0x021F;
      m_composeMap[make_pair(0x00000068, 0x00000323)] = 0x1E25;
      m_composeMap[make_pair(0x00000068, 0x00000327)] = 0x1E29;
      m_composeMap[make_pair(0x00000068, 0x0000032E)] = 0x1E2B;
      m_composeMap[make_pair(0x00000068, 0x00000331)] = 0x1E96;
      m_composeMap[make_pair(0x00000069, 0x00000300)] = 0x00EC;
      m_composeMap[make_pair(0x00000069, 0x00000301)] = 0x00ED;
      m_composeMap[make_pair(0x00000069, 0x00000302)] = 0x00EE;
      m_composeMap[make_pair(0x00000069, 0x00000303)] = 0x0129;
      m_composeMap[make_pair(0x00000069, 0x00000304)] = 0x012B;
      m_composeMap[make_pair(0x00000069, 0x00000306)] = 0x012D;
      m_composeMap[make_pair(0x00000069, 0x00000308)] = 0x00EF;
      m_composeMap[make_pair(0x00000069, 0x00000309)] = 0x1EC9;
      m_composeMap[make_pair(0x00000069, 0x0000030C)] = 0x01D0;
      m_composeMap[make_pair(0x00000069, 0x0000030F)] = 0x0209;
      m_composeMap[make_pair(0x00000069, 0x00000311)] = 0x020B;
      m_composeMap[make_pair(0x00000069, 0x00000323)] = 0x1ECB;
      m_composeMap[make_pair(0x00000069, 0x00000328)] = 0x012F;
      m_composeMap[make_pair(0x00000069, 0x00000330)] = 0x1E2D;
      m_composeMap[make_pair(0x0000006A, 0x00000302)] = 0x0135;
      m_composeMap[make_pair(0x0000006A, 0x0000030C)] = 0x01F0;
      m_composeMap[make_pair(0x0000006B, 0x00000301)] = 0x1E31;
      m_composeMap[make_pair(0x0000006B, 0x0000030C)] = 0x01E9;
      m_composeMap[make_pair(0x0000006B, 0x00000323)] = 0x1E33;
      m_composeMap[make_pair(0x0000006B, 0x00000327)] = 0x0137;
      m_composeMap[make_pair(0x0000006B, 0x00000331)] = 0x1E35;
      m_composeMap[make_pair(0x0000006C, 0x00000301)] = 0x013A;
      m_composeMap[make_pair(0x0000006C, 0x0000030C)] = 0x013E;
      m_composeMap[make_pair(0x0000006C, 0x00000323)] = 0x1E37;
      m_composeMap[make_pair(0x0000006C, 0x00000327)] = 0x013C;
      m_composeMap[make_pair(0x0000006C, 0x0000032D)] = 0x1E3D;
      m_composeMap[make_pair(0x0000006C, 0x00000331)] = 0x1E3B;
      m_composeMap[make_pair(0x0000006D, 0x00000301)] = 0x1E3F;
      m_composeMap[make_pair(0x0000006D, 0x00000307)] = 0x1E41;
      m_composeMap[make_pair(0x0000006D, 0x00000323)] = 0x1E43;
      m_composeMap[make_pair(0x0000006E, 0x00000300)] = 0x01F9;
      m_composeMap[make_pair(0x0000006E, 0x00000301)] = 0x0144;
      m_composeMap[make_pair(0x0000006E, 0x00000303)] = 0x00F1;
      m_composeMap[make_pair(0x0000006E, 0x00000307)] = 0x1E45;
      m_composeMap[make_pair(0x0000006E, 0x0000030C)] = 0x0148;
      m_composeMap[make_pair(0x0000006E, 0x00000323)] = 0x1E47;
      m_composeMap[make_pair(0x0000006E, 0x00000327)] = 0x0146;
      m_composeMap[make_pair(0x0000006E, 0x0000032D)] = 0x1E4B;
      m_composeMap[make_pair(0x0000006E, 0x00000331)] = 0x1E49;
      m_composeMap[make_pair(0x0000006F, 0x00000300)] = 0x00F2;
      m_composeMap[make_pair(0x0000006F, 0x00000301)] = 0x00F3;
      m_composeMap[make_pair(0x0000006F, 0x00000302)] = 0x00F4;
      m_composeMap[make_pair(0x0000006F, 0x00000303)] = 0x00F5;
      m_composeMap[make_pair(0x0000006F, 0x00000304)] = 0x014D;
      m_composeMap[make_pair(0x0000006F, 0x00000306)] = 0x014F;
      m_composeMap[make_pair(0x0000006F, 0x00000307)] = 0x022F;
      m_composeMap[make_pair(0x0000006F, 0x00000308)] = 0x00F6;
      m_composeMap[make_pair(0x0000006F, 0x00000309)] = 0x1ECF;
      m_composeMap[make_pair(0x0000006F, 0x0000030B)] = 0x0151;
      m_composeMap[make_pair(0x0000006F, 0x0000030C)] = 0x01D2;
      m_composeMap[make_pair(0x0000006F, 0x0000030F)] = 0x020D;
      m_composeMap[make_pair(0x0000006F, 0x00000311)] = 0x020F;
      m_composeMap[make_pair(0x0000006F, 0x0000031B)] = 0x01A1;
      m_composeMap[make_pair(0x0000006F, 0x00000323)] = 0x1ECD;
      m_composeMap[make_pair(0x0000006F, 0x00000328)] = 0x01EB;
      m_composeMap[make_pair(0x00000070, 0x00000301)] = 0x1E55;
      m_composeMap[make_pair(0x00000070, 0x00000307)] = 0x1E57;
      m_composeMap[make_pair(0x00000072, 0x00000301)] = 0x0155;
      m_composeMap[make_pair(0x00000072, 0x00000307)] = 0x1E59;
      m_composeMap[make_pair(0x00000072, 0x0000030C)] = 0x0159;
      m_composeMap[make_pair(0x00000072, 0x0000030F)] = 0x0211;
      m_composeMap[make_pair(0x00000072, 0x00000311)] = 0x0213;
      m_composeMap[make_pair(0x00000072, 0x00000323)] = 0x1E5B;
      m_composeMap[make_pair(0x00000072, 0x00000327)] = 0x0157;
      m_composeMap[make_pair(0x00000072, 0x00000331)] = 0x1E5F;
      m_composeMap[make_pair(0x00000073, 0x00000301)] = 0x015B;
      m_composeMap[make_pair(0x00000073, 0x00000302)] = 0x015D;
      m_composeMap[make_pair(0x00000073, 0x00000307)] = 0x1E61;
      m_composeMap[make_pair(0x00000073, 0x0000030C)] = 0x0161;
      m_composeMap[make_pair(0x00000073, 0x00000323)] = 0x1E63;
      m_composeMap[make_pair(0x00000073, 0x00000326)] = 0x0219;
      m_composeMap[make_pair(0x00000073, 0x00000327)] = 0x015F;
      m_composeMap[make_pair(0x00000074, 0x00000307)] = 0x1E6B;
      m_composeMap[make_pair(0x00000074, 0x00000308)] = 0x1E97;
      m_composeMap[make_pair(0x00000074, 0x0000030C)] = 0x0165;
      m_composeMap[make_pair(0x00000074, 0x00000323)] = 0x1E6D;
      m_composeMap[make_pair(0x00000074, 0x00000326)] = 0x021B;
      m_composeMap[make_pair(0x00000074, 0x00000327)] = 0x0163;
      m_composeMap[make_pair(0x00000074, 0x0000032D)] = 0x1E71;
      m_composeMap[make_pair(0x00000074, 0x00000331)] = 0x1E6F;
      m_composeMap[make_pair(0x00000075, 0x00000300)] = 0x00F9;
      m_composeMap[make_pair(0x00000075, 0x00000301)] = 0x00FA;
      m_composeMap[make_pair(0x00000075, 0x00000302)] = 0x00FB;
      m_composeMap[make_pair(0x00000075, 0x00000303)] = 0x0169;
      m_composeMap[make_pair(0x00000075, 0x00000304)] = 0x016B;
      m_composeMap[make_pair(0x00000075, 0x00000306)] = 0x016D;
      m_composeMap[make_pair(0x00000075, 0x00000308)] = 0x00FC;
      m_composeMap[make_pair(0x00000075, 0x00000309)] = 0x1EE7;
      m_composeMap[make_pair(0x00000075, 0x0000030A)] = 0x016F;
      m_composeMap[make_pair(0x00000075, 0x0000030B)] = 0x0171;
      m_composeMap[make_pair(0x00000075, 0x0000030C)] = 0x01D4;
      m_composeMap[make_pair(0x00000075, 0x0000030F)] = 0x0215;
      m_composeMap[make_pair(0x00000075, 0x00000311)] = 0x0217;
      m_composeMap[make_pair(0x00000075, 0x0000031B)] = 0x01B0;
      m_composeMap[make_pair(0x00000075, 0x00000323)] = 0x1EE5;
      m_composeMap[make_pair(0x00000075, 0x00000324)] = 0x1E73;
      m_composeMap[make_pair(0x00000075, 0x00000328)] = 0x0173;
      m_composeMap[make_pair(0x00000075, 0x0000032D)] = 0x1E77;
      m_composeMap[make_pair(0x00000075, 0x00000330)] = 0x1E75;
      m_composeMap[make_pair(0x00000076, 0x00000303)] = 0x1E7D;
      m_composeMap[make_pair(0x00000076, 0x00000323)] = 0x1E7F;
      m_composeMap[make_pair(0x00000077, 0x00000300)] = 0x1E81;
      m_composeMap[make_pair(0x00000077, 0x00000301)] = 0x1E83;
      m_composeMap[make_pair(0x00000077, 0x00000302)] = 0x0175;
      m_composeMap[make_pair(0x00000077, 0x00000307)] = 0x1E87;
      m_composeMap[make_pair(0x00000077, 0x00000308)] = 0x1E85;
      m_composeMap[make_pair(0x00000077, 0x0000030A)] = 0x1E98;
      m_composeMap[make_pair(0x00000077, 0x00000323)] = 0x1E89;
      m_composeMap[make_pair(0x00000078, 0x00000307)] = 0x1E8B;
      m_composeMap[make_pair(0x00000078, 0x00000308)] = 0x1E8D;
      m_composeMap[make_pair(0x00000079, 0x00000300)] = 0x1EF3;
      m_composeMap[make_pair(0x00000079, 0x00000301)] = 0x00FD;
      m_composeMap[make_pair(0x00000079, 0x00000302)] = 0x0177;
      m_composeMap[make_pair(0x00000079, 0x00000303)] = 0x1EF9;
      m_composeMap[make_pair(0x00000079, 0x00000304)] = 0x0233;
      m_composeMap[make_pair(0x00000079, 0x00000307)] = 0x1E8F;
      m_composeMap[make_pair(0x00000079, 0x00000308)] = 0x00FF;
      m_composeMap[make_pair(0x00000079, 0x00000309)] = 0x1EF7;
      m_composeMap[make_pair(0x00000079, 0x0000030A)] = 0x1E99;
      m_composeMap[make_pair(0x00000079, 0x00000323)] = 0x1EF5;
      m_composeMap[make_pair(0x0000007A, 0x00000301)] = 0x017A;
      m_composeMap[make_pair(0x0000007A, 0x00000302)] = 0x1E91;
      m_composeMap[make_pair(0x0000007A, 0x00000307)] = 0x017C;
      m_composeMap[make_pair(0x0000007A, 0x0000030C)] = 0x017E;
      m_composeMap[make_pair(0x0000007A, 0x00000323)] = 0x1E93;
      m_composeMap[make_pair(0x0000007A, 0x00000331)] = 0x1E95;

    }


    ~Basic_Latin0() {
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
      return "Basic Latin";
    }

    bool is_defined(const UCS4 uc) const {
      return 1;
    }

    UCS4 uppercase(const UCS4 uc) const {
      return Basic_Latin0::m_upper[uc - m_first_letter];
    }

    UCS4 lowercase(const UCS4 uc) const {
      return Basic_Latin0::m_lower[uc - m_first_letter];
    }

    UCS4 titlecase(const UCS4 uc) const {
      return Basic_Latin0::m_title[uc - m_first_letter];
    }

    int dec_digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0030u:
        return 0;
        break;
      case 0x0031u:
        return 1;
        break;
      case 0x0032u:
        return 2;
        break;
      case 0x0033u:
        return 3;
        break;
      case 0x0034u:
        return 4;
        break;
      case 0x0035u:
        return 5;
        break;
      case 0x0036u:
        return 6;
        break;
      case 0x0037u:
        return 7;
        break;
      case 0x0038u:
        return 8;
        break;
      case 0x0039u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Decimal_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0030u:
      case 0x0031u:
      case 0x0032u:
      case 0x0033u:
      case 0x0034u:
      case 0x0035u:
      case 0x0036u:
      case 0x0037u:
      case 0x0038u:
      case 0x0039u:
        return 1;
      default:
        return 0;
      }
    }

    int digit_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0030u:
        return 0;
        break;
      case 0x0031u:
        return 1;
        break;
      case 0x0032u:
        return 2;
        break;
      case 0x0033u:
        return 3;
        break;
      case 0x0034u:
        return 4;
        break;
      case 0x0035u:
        return 5;
        break;
      case 0x0036u:
        return 6;
        break;
      case 0x0037u:
        return 7;
        break;
      case 0x0038u:
        return 8;
        break;
      case 0x0039u:
        return 9;
        break;
      default:
        return 0;
      }
    }

    bool is_Digit(const UCS4 uc) const {
      switch(uc) {
      case 0x0030u:
      case 0x0031u:
      case 0x0032u:
      case 0x0033u:
      case 0x0034u:
      case 0x0035u:
      case 0x0036u:
      case 0x0037u:
      case 0x0038u:
      case 0x0039u:
        return 1;
      default:
        return 0;
      }
    }

    float numeric_value(const UCS4 uc) const {
      if (!is_defined(uc))
        return 0;
      switch(uc) {
      case 0x0030u:
        return 0.000000;
        break;
      case 0x0031u:
        return 1.000000;
        break;
      case 0x0032u:
        return 2.000000;
        break;
      case 0x0033u:
        return 3.000000;
        break;
      case 0x0034u:
        return 4.000000;
        break;
      case 0x0035u:
        return 5.000000;
        break;
      case 0x0036u:
        return 6.000000;
        break;
      case 0x0037u:
        return 7.000000;
        break;
      case 0x0038u:
        return 8.000000;
        break;
      case 0x0039u:
        return 9.000000;
        break;
      default:
        return 0;
      }
    }

    bool is_Numeric(const UCS4 uc) const {
      switch(uc) {
      case 0x0030u:
      case 0x0031u:
      case 0x0032u:
      case 0x0033u:
      case 0x0034u:
      case 0x0035u:
      case 0x0036u:
      case 0x0037u:
      case 0x0038u:
      case 0x0039u:
        return 1;
      default:
        return 0;
      }
    }

    Gen_Cat category(const UCS4 uc) const {
      if (!is_defined(uc))
        return CAT_MAX;
      return Babylon::Gen_Cat(Basic_Latin0::_cat[uc - m_first_letter]);
    }

    Can_Comb_Class comb_class(const UCS4 uc) const {
      if (!is_defined(uc))
        return CC_MAX;
      return Babylon::Can_Comb_Class(0);
    }

    Bidir_Props bidir_props(const UCS4 uc) const {
      if (!is_defined(uc))
        return BIDIR_MAX;
      return Babylon::Bidir_Props(Basic_Latin0::m_bidir[uc - m_first_letter]);
    }

    Char_Decomp decomp_type(const UCS4 uc) const {
      if (!is_defined(uc))
        return DECOMP_MAX;
      return Babylon::Char_Decomp(DECOMP_CANONICAL);
    }

    UTF32_string decompose(const UCS4 uc) const {
      UTF32_string us;
      us.resize(1); us[0] = uc;
      return us;
    }

    bool must_mirror(const UCS4 uc) const {
      return m_mirror.test(uc - m_first_letter);
    }

    Line_Break linebreak(const UCS4 uc) const {
      if (!is_defined(uc))
        return LB_MAX;
      return Babylon::Line_Break(Basic_Latin0::m_lb[uc - m_first_letter]);
    }

    EA_Width EA_width(const UCS4 uc) const {
      if (!is_defined(uc))
        return EA_WIDTH_MAX;
      return Babylon::EA_Width(Basic_Latin0::m_ea[uc - m_first_letter]);
    }

    UCS4 compose (const UCS4 start, const UCS4 last) {
      return m_composeMap[make_pair(start, last)];
    }

    bool is_White_space(const UCS4 uc) const {
      return m_White_space.test(uc - m_first_letter);
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
      return m_Hex_Digit.test(uc - m_first_letter);
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
    Basic_Latin0(const Basic_Latin0 &) {}

    Babylon::UCS4 m_first_letter;
    Babylon::UCS4 m_last_letter;
    // Babylon::UCS4_string m_version;
    static const UCS4 m_upper[128];
    static const UCS4 m_lower[128];
    static const UCS4 m_title[128];
    static const unsigned char _cat[128];
    static const unsigned char m_bidir[128];
    static const std::bitset<128> m_mirror;
    static const unsigned char m_lb[128];
    static const unsigned char m_ea[128];
    std::map<pair<UCS4, UCS4>, UCS4> m_composeMap;
    static const std::bitset<128> m_White_space;
    static const std::bitset<128> m_Terminal_Punctuation;
    static const std::bitset<128> m_Hex_Digit;

  }; // class Basic_Latin0

  const UCS4 Basic_Latin0::m_upper[] = {
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 
    0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, 
    0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 
    0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, 
    0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 
    0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F, 
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, 
    0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
    0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F, 
    0x0060, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
    0x0058, 0x0059, 0x005A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F
  };

  const UCS4 Basic_Latin0::m_lower[] = {
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 
    0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, 
    0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 
    0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, 
    0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 
    0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F, 
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, 
    0x0040, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 
    0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 
    0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 
    0x0078, 0x0079, 0x007A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F, 
    0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 
    0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 
    0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 
    0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F
  };

  const UCS4 Basic_Latin0::m_title[] = {
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 
    0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, 
    0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 
    0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, 
    0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 
    0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F, 
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, 
    0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
    0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F, 
    0x0060, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
    0x0058, 0x0059, 0x005A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F
  };

  const unsigned char Basic_Latin0::_cat[] = {
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, CAT_Cc, 
    CAT_Zs, CAT_Po, CAT_Po, CAT_Po, CAT_Sc, CAT_Po, CAT_Po, CAT_Po, 
    CAT_Ps, CAT_Pe, CAT_Po, CAT_Sm, CAT_Po, CAT_Pd, CAT_Po, CAT_Po, 
    CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, CAT_Nd, 
    CAT_Nd, CAT_Nd, CAT_Po, CAT_Po, CAT_Sm, CAT_Sm, CAT_Sm, CAT_Po, 
    CAT_Po, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, CAT_Lu, 
    CAT_Lu, CAT_Lu, CAT_Lu, CAT_Ps, CAT_Po, CAT_Pe, CAT_Sk, CAT_Pc, 
    CAT_Sk, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ll, 
    CAT_Ll, CAT_Ll, CAT_Ll, CAT_Ps, CAT_Sm, CAT_Pe, CAT_Sm, CAT_Cc
  };

  const unsigned char Basic_Latin0::m_bidir[] = {
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_S, BIDIR_B, BIDIR_S, BIDIR_WS, BIDIR_B, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, 
    BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_BN, BIDIR_B, BIDIR_B, BIDIR_B, BIDIR_S, 
    BIDIR_WS, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_ET, BIDIR_ET, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ET, BIDIR_CS, BIDIR_ET, BIDIR_CS, BIDIR_ES, 
    BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, BIDIR_EN, 
    BIDIR_EN, BIDIR_EN, BIDIR_CS, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, 
    BIDIR_ON, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_L, 
    BIDIR_L, BIDIR_L, BIDIR_L, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_ON, BIDIR_BN
  };

  const std::bitset<128> Basic_Latin0::m_mirror(std::string("00101000000000000000000000000000001010000000000000000000000000000101000000000000000000110000000000000000000000000000000000000000"));

  const unsigned char Basic_Latin0::m_lb[] = {
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_BA, LB_LF, LB_CM, LB_BK, LB_CR, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, LB_CM, 
    LB_SP, LB_EX, LB_QU, LB_AL, LB_PR, LB_PO, LB_AL, LB_QU, 
    LB_OP, LB_CL, LB_AL, LB_PR, LB_IS, LB_HY, LB_IS, LB_SY, 
    LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, LB_NU, 
    LB_NU, LB_NU, LB_IS, LB_IS, LB_AL, LB_AL, LB_AL, LB_EX, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_OP, LB_PR, LB_CL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, LB_AL, 
    LB_AL, LB_AL, LB_AL, LB_OP, LB_BA, LB_CL, LB_AL, LB_CM
  };

  const unsigned char Basic_Latin0::m_ea[] = {
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, EA_WIDTH_N, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, 
    EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_Na, EA_WIDTH_N
  };

    const std::bitset<128> Basic_Latin0::m_White_space(std::string("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011111000000000"));

    const std::bitset<128> Basic_Latin0::m_Terminal_Punctuation(std::string("00000000000000000000000000000000000000000000000000000000000000000000110000000000000000000000000000000000000000000000000000000000"));

    const std::bitset<128> Basic_Latin0::m_Hex_Digit(std::string("00000000000000000000000001111110000000000000000000000000011111100000001111111111000000000000000000000000000000000000000000000000"));

}; // namespace Babylon

dload(Babylon::Basic_Latin0);
