#include "Text/TextViewer.hh"
#include "Warsaw/DrawingKit.hh"
#include "Warsaw/TextBuffer.hh"
#include "Text/Compositor.hh"

TextViewer::TextViewer(TextBuffer_ptr txt, DrawingKit_ptr dk, Compositor *c)  {
    myCompositor = c;
    myCanonicalDK = DrawingKit::_duplicate(dk);
    myTextBuffer = TextBuffer::_duplicate(txt);    
}

void TextViewer::draw(DrawTraversal_ptr dt) {
}
void TextViewer::update(Subject_ptr s, const CORBA::Any &a) {
}


TextViewer::~TextViewer() {}

