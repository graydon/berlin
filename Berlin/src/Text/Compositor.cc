
#include "Text/Compositor.hh"
#include "Berlin/RegionImpl.hh"
#include "Berlin/GraphicImpl.hh"
#include "Warsaw/DrawingKit.hh"
#include "Berlin/Math.hh"

Compositor::Compositor(Axis a) : axis(a) {}
Compositor::~Compositor(){}
void Compositor::setSpan(RegionImpl *r, Axis a, Coord origin, Coord length, Alignment align)
{
    Coord begin = origin - length * align;
    Coord end = begin + length;
    switch (a)
	{
	case xaxis:
	    r->lower.x = begin;
	    r->upper.x = end;
	    r->xalign = align;
	    break;
	case yaxis:
	    r->lower.y = begin;
	    r->upper.y = end;
	    r->yalign = align;
	    break;
	case zaxis:
	    r->lower.z = begin;
	    r->upper.z = end;
	    r->zalign = align;
	    break;
	}
}

//
// does no compensation -- to be used when current dk == canonical dk
//

IdentityCompositor::IdentityCompositor(Axis a) : Compositor(a) {}
IdentityCompositor::~IdentityCompositor() {}
void IdentityCompositor::compose(long n, Graphic_ptr *chunks, DrawingKit_ptr dk, 
				 Region_ptr given, Compositor::Allocations result) {
    Graphic::Requirement* r;
    Region::Allotment a;
    given->span(axis, a);
    for (long i = 0; i < n; i++)
	{
	    Graphic::Requisition req;
	    chunks[i]->request(req);
	    r = GraphicImpl::requirement(req, axis);
	    if (r->defined)
		{
		    Coord length = Math::max(Math::min(a.end - a.begin, r->maximum), r->minimum);
		    setSpan(result[i], axis, a.begin + a.align*(a.end-a.begin), length, r->align);
		}
	    else setSpan(result[i], axis, Coord(0), Coord(0), Coord(0));
	}
}
