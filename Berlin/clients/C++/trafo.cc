#include "Berlin/TransformImpl.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/GraphicImpl.hh"
#include "Berlin/ImplVar.hh"
#include "Warsaw/IO.hh"

int main(int argc, char **argv)
{
  if (argc != 4)
    {
      cerr << "Usage : " << argv[0] << " <angle> <xalign> <yalign>" << endl;
      exit(-1);
    }
  Coord angle = atof(argv[1]);
  Coord xalign = atof(argv[2]);
  Coord yalign = atof(argv[3]);
  CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");
  CORBA::BOA_ptr boa = orb->BOA_init(argc,argv,"omniORB2_BOA");
  boa->impl_is_ready(0,1);
  Impl_var<TransformImpl> trafo1(new TransformImpl);
//   Vertex v = {0.8, 0.8, 1.};
  trafo1->rotate(angle, zaxis);
//   trafo1->scale(v);
//   trafo1->translate(v);
  Transform::Matrix matrix;
  trafo1->storeMatrix(matrix);
  cout << "* zero step: a transformation matrix" << endl;
  cout << matrix;
  cout << "* first step: set childs requisition :\n";
  Graphic::Requisition requisition;
  requisition.x.natural = requisition.x.minimum = requisition.x.maximum = 1000.;
  requisition.x.align = xalign;
  requisition.x.defined = true;
  requisition.y.natural = requisition.y.minimum = requisition.y.maximum = 1000.;
  requisition.y.align = yalign;
  requisition.y.defined = true;
  requisition.z.natural = requisition.z.minimum = requisition.z.maximum = 1000.;
  requisition.z.align = 0.;
  requisition.z.defined = true;
  cout << requisition << endl;

  cout << "* second step: calculate the requisition in the parent's CS :\n";
  Graphic::Requisition tr = requisition;
  GraphicImpl::transformRequest(tr, Transform_var(trafo1->_this()));
  cout << tr << endl;

  cout << "* third step: allocate a region in the parent's CS :\n";
  RegionImpl region;
  region.lower.x = - xalign * 1000.;
  region.lower.y = - yalign * 1000.;
  region.lower.z =    0.;
  region.upper.x = region.lower.x + 1000.;
  region.upper.y = region.lower.y + 1000.;
  region.upper.z = 1000.;
  region.xalign = xalign;
  region.yalign = yalign;
  region.zalign = 0.;
  region.valid = true;
  cout << "desired region  : " << region << '\n';
  region.applyTransform(Transform_var(trafo1->_this()));
  cout << "parent's region : " << region << '\n';
  
  cout << "* fourth step: allocate the child's region given the parent's region :\n";
  Vertex offset = GraphicImpl::transformAllocate(region, requisition, Transform_var(trafo1->_this()));
//   region.lower.x += offset.x;
//   region.upper.x += offset.x;
//   region.lower.y += offset.y;
//   region.upper.y += offset.y;
//   region.lower.z += offset.z;
//   region.upper.z += offset.z;
  cout << region << endl;
  cout << "offset " << offset << endl;
};
