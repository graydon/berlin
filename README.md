**Note**: the repository https://github.com/stefanseefeld/fresco is actually what you want to look at rather than this one (unless you want to see a crawl of an old website). I made this snapshot thinking sourceforge was the only residual source; but of course Stefan has already done a better job of preservation.

# Berlin / Fresco

From 1998 through 2003, myself, Stefan Seefeld, Tobias Hunger, Nathaniel Smith and many others collaborated on a project to build a new windowing system for Unix systems.
The project was started and briefly led by Jim Fetters and Matt Messier, and was originally called "Berlin", but later it merged with an older codebase called "Fresco" and adopted that name.

The goal was to push _far_ beyond what the X window system of the time supported, providing a basis for the fabled future "year of the Linux desktop".
It used a resolution-independent high-color imaging model rendered through OpenGL, supported antialiased Unicode text, and had a retained server-side scene graph.

It was initially based on local RPC but quickly transitioned to CORBA for network transparency and language neutrality.
It is _possible_ that our ambition in doing so encouraged the GNOME and KDE projects, which were launched concurrently, in their ill-fated and ultimately abandoned CORBA adventures.
If so, I deeply apologize! We were young and foolish and did not understand what we were getting into.

In any event the project was ultimately unsuccessful both technically and competitively, so people moved on and it faded into obscurity.
This is a historical snapshot of some material -- source releases and website -- still recoverable from sourceforge at the time of writing.

-Graydon Hoare, Dec 2023
