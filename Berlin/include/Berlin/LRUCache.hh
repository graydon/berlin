#ifndef _LRUCache_hh
#define _LRUCache_hh

#include <map>
#include <list>

/* this is an ultra-simple LRU cache. it requires that your key objects are
  comparable by <, that your value objects are lightweight enough to be handled
  by value, you have a factory object for your value type which has 2 methods:
  produce (which takes a single parameter of key type and copies into it) and
  recycle (which takes a single parameter of value type and frees its storage),
  and you do synchronizaiton externally.
  
  on the other hand, it's very simple to use.  */

template 
<class kT, class vT, class factoryT, class cacheT = std::map<kT,vT> > 
class LRUCache {

private:
  unsigned int max;
  cacheT cache;
  std::list<kT> queue;
  factoryT factory;

public:

  LRUCache(factoryT fact, unsigned int i = 256) : max(i), factory(fact) {}

  void get(const kT &k, vT &v) throw () 
  {
    typename cacheT::iterator iter = cache.find(k);
    if (iter != cache.end())
      {
	v = iter->second;
	return;
      }
    else
      {
	v = factory.produce(k);
	cache.insert(pair<kT,vT>(k,v));
	queue.push_front(k);
	if (queue.size() >= max)
	  {
	    kT victim = queue.back();
	    factory.recycle(cache[victim]);
	    cache.erase(victim);
	    queue.pop_back();
	  }
      }
  }
};


#endif
