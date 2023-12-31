#ifndef __domDefs_hh__
#define __domDefs_hh__

#ifndef USE_omniORB_logStream
#define USE_omniORB_logStream
#endif

#ifndef __CORBA_H_EXTERNAL_GUARD__
#define __CORBA_H_EXTERNAL_GUARD__
#include <omniORB2/CORBA.h>
#endif

#ifndef __OMNILC_H_EXTERNAL_GUARD__
#define __OMNILC_H_EXTERNAL_GUARD__
#include <omniORB2/omniLC.h>
#endif


#ifdef _LC_attr
#error "A local CPP macro _LC_attr has already been defined."
#else
#ifdef  USE_stub_in_nt_dll
#define _LC_attr _OMNIORB_NTDLL_IMPORT
#else
#define _LC_attr
#endif
#endif

_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_wchar;
typedef CORBA::UShort wchar;
_CORBA_GLOBAL_VAR const CORBA::TypeCode_ptr _tc_wstring;
typedef _CORBA_Unbounded_Sequence_w_FixSizeElement<wchar,2,2> wstring;
typedef _CORBA_Sequence_Var<wstring, wchar > wstring_var;

#ifndef __04RL__IDL_SEQUENCE_CORBA_UShort__
#define __04RL__IDL_SEQUENCE_CORBA_UShort__

inline void operator<<=(CORBA::Any& _a, const wstring& _s) {
  MemBufferedStream _0RL_mbuf;
  _tc_wstring->NP_fillInit(_0RL_mbuf);
  _s >>= _0RL_mbuf;
  _a.NP_replaceData(_tc_wstring,_0RL_mbuf);
}

inline void operator<<=(CORBA::Any& _a, wstring* _sp) {
  _a <<= *_sp;
  delete _sp;
}

inline void _03RL_wstring_delete(void* _data) {
  wstring* _0RL_t = (wstring*) _data;
  delete _0RL_t;
}

inline CORBA::Boolean operator>>=(const CORBA::Any& _a, wstring*& _sp) {
  CORBA::TypeCode_var _0RL_any_tc = _a.type();
  if (!_0RL_any_tc->NP_expandEqual(_tc_wstring,1)) {
    _sp = 0;
    return 0;
  }
  else {
    void* _0RL_data = _a.NP_data();

    if (!_0RL_data) {
      MemBufferedStream _0RL_tmp_mbuf;
      _a.NP_getBuffer(_0RL_tmp_mbuf);
      wstring* _0RL_tmp = new wstring;
      *_0RL_tmp <<= _0RL_tmp_mbuf;
      _0RL_data = (void*) _0RL_tmp;
      _a.NP_holdData(_0RL_data,_03RL_wstring_delete);
    }

    _sp = (wstring*) _0RL_data;
    return 1;
  }
}

#endif


#undef _LC_attr

#endif // __domDefs_hh__
