// This progran is to check wether the translation from and
// to Babylon::String works.

#include <Babylon/Babylon.hh>
#include <Warsaw/Types.hh>

int main (int argc, char * argv) {
    Babylon::String bs("TEST-STRING");
    Babylon::UTF8_string us;

    us = Babylon::recode<Babylon::UTF8_string>(bs);

    for (unsigned int i = 0; i < us.length(); i++)
	cout << char(us[i]);
    cout << endl;
}
