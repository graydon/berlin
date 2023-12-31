/*$Id: Dictionary.hh,v 1.10 1999/11/10 21:57:36 stefan Exp $
 *
 * This source file is a part of the Berlin Project.
 * Copyright (C) 1999 Tobias Hunger <Tobias_Hunger@gmx.de>
 * http://www.berlin-consortium.org
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */
#ifndef _Dictionary_hh_
#define _Dictionary_hh_

#include <Prague/Unicode/UnicodeDefs.hh>
#include <Prague/Unicode/Unistring.hh>
#include <Prague/Sys/Plugin.hh>
#include <fstream>

namespace Unicode {

  class Dictionary
  {
    struct Guard { ~Guard() { delete Dictionary::dictionary;}};
    friend struct Guard;
    
  public:
    void update(const string &);

  private:
    static Dictionary * instance();

    // Query functions:
    _Char uppercase(_Char _UC) {
      return find_char(_UC)->uppercase(_UC); }
    _Char lowercase(_Char _UC) {
      return find_char(_UC)->lowercase(_UC); }
    _Char titlecase(_Char _UC) {
      return find_char(_UC)->titlecase(_UC); }
    float numericValue(_Char _UC) {
      return find_char(_UC)->numericValue(_UC); }
    int decDigitValue(_Char _UC) {
      return find_char(_UC)->decDigitValue(_UC); }
    int digitValue(_Char _UC) {
      return find_char(_UC)->digitValue(_UC); } 
    string blockname(_Char _UC) {
      return find_char(_UC)->blockname(_UC); }
    Gen_Cat category(_Char _UC) {
      return find_char(_UC)->category(_UC); }
    Can_Comb_Class combClass(_Char _UC) {
      return find_char(_UC)->combClass(_UC); }
    Bidir_Props bidirProps(_Char _UC) {
      return find_char(_UC)->bidirProps(_UC); }
    Char_Decomp decompType(_Char _UC) {
      return find_char(_UC)->decompType(_UC); }
    String decompString(_Char _UC) {
      return find_char(_UC)->decompString(_UC); }
    bool mustMirror(_Char _UC) {
      return find_char(_UC)->mustMirror(_UC); }
    EA_Width EAWidth(_Char _UC) {
      return find_char(_UC)->EAWidth(_UC); }
    Line_Break linebreak(_Char _UC) {
      return find_char(_UC)->linebreak(_UC); }

    class Block {
    public:
      Block() { }
      virtual ~Block() {}
      virtual void clean() {}
      
      // Query Functions:
      virtual _Char unicodeValue(const _Char) const = 0;
      virtual _Char uppercase(const _Char) const = 0;
      virtual _Char lowercase(const _Char) const = 0;
      virtual _Char titlecase(const _Char) const = 0;
      virtual float numericValue(const _Char) const = 0;
      virtual int decDigitValue(const _Char) const = 0;
      virtual int digitValue(const _Char) const = 0;
      virtual string blockname(const _Char) const = 0;
      virtual Gen_Cat category(const _Char) const = 0;
      virtual Can_Comb_Class combClass(const _Char) const = 0;
      virtual Bidir_Props bidirProps(const _Char) const = 0;
      virtual Char_Decomp decompType(const _Char) const = 0;
      virtual String decompString(const _Char) const = 0;
      virtual bool mustMirror(const _Char) const = 0;
      virtual EA_Width EAWidth(const _Char) const = 0;
      virtual Line_Break linebreak(const _Char) const = 0;

      virtual _Char firstLetter() = 0;
      virtual _Char lastLetter() = 0;
    protected:

    private:
    }; // class Block

    struct Data {
      _Char start;
      _Char end;
      string file;
      bool canRemove;
      Prague::Plugin<Dictionary::Block> * block;
    }; // struct Data

    Block * find_char(_Char);

    Dictionary();
    Dictionary(const Dictionary &) {}
    ~Dictionary();
    void clean();
    void find_next_in_file(ifstream &);

    static Dictionary *dictionary;
    static Guard guard;
    static Prague::Mutex singletonMutex;

    Data *data;
    size_t size;
    String my_version;
    Prague::Mutex mutex;

    string plugindir;

    // friends:
    friend class Unicode::Char;
    friend class Unicode::String;
  }; // class Dictionary

}; // namespace Unicode

#endif // _Dictionary_hh_
