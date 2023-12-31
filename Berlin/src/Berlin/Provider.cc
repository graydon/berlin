#include "Berlin/Provider.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/TransformImpl.hh"
#include "Berlin/AllocationImpl.hh"

using namespace Prague;

std::stack<RegionImpl *> Provider<RegionImpl>::pool;
Mutex Provider<RegionImpl>::mutex;
std::stack<TransformImpl *> Provider<TransformImpl>::pool;
Mutex Provider<TransformImpl>::mutex;
std::stack<AllocationImpl *> Provider<AllocationImpl>::pool;  
Mutex Provider<AllocationImpl>::mutex;
