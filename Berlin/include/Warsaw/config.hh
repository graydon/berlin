/* ../include/Warsaw/config.hh.  Generated automatically by configure.  */
#ifndef _config_hh
#define _config_hh

/* define whether we are using omniORB2 */
#define __OMNIORB2__
 
// this cruft is intended to help handle differences in ORBs. it's sad, really.
#ifdef __OMNIORB2__
#  define interface(name) name##_IntfRepoID
#  define applyscope(scope,thing) scope ## :: ## thing
#  define skeletonize(interfacename) _sk_ ## interfacename
#  define lcskeletonize(interfacename) _lc_sk_ ## interfacename
#  define implements(interface) virtual public skeletonize(interface)
#  define lcimplements(interface) virtual public lcskeletonize(interface)
#  define implementsscoped(scope,interface) virtual public applyscope(scope,skeletonize(interface))
#  define lcimplementsscoped(scope,interface) virtual public applyscope(scope,lcskeletonize(interface))
#  define obtain(sc,in) applyscope(in,_narrow(sc->create(interface(in))))
#  define declare_corba_ptr_type(foo) class foo; \
	typedef foo* foo ##_ptr; \
	typedef foo ##_ptr foo ##Ref; \
	class foo ##_Helper; \
	typedef _CORBA_ObjRef_Var<foo,foo ##_Helper> foo ##_var;

#else
#  error "sorry, currently only OMNIORB2 is supported"
#endif

#endif /* _config_hh */
