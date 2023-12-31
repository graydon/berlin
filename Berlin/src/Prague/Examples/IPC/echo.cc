#include <iostream>
#include <string>

int main(int, char **)
{
  string line;
  while (getline(cin, line) && line != "end")
    cout << line << endl;
}
