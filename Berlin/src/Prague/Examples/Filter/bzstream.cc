#include <fstream>
#include <Prague/Filter/bzbuf.hh>

int main()
{
  ofstream ofs("output.bz2");
  bzbuf filter(ofs.rdbuf(), ios::out);
  ostream os(&filter);
  os << "this is a little test using the bzip2 library" << endl;
};
