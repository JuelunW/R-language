/*
    I defined the option in callOption.h, given all the information needed.
    1. complete the constructor definition with input parameters.
    2. complete the pricing function using path independent simulation method.
    3. construct a call option using default constructor, then test the pricing function.
    4. construct another call option using the other constructor, but changing the volatility
    to 0.1 while keeping other values unchanged, then test the pricing function again and observe the option price change.
 */

#include "std_lib_facilities.h"
#include "callOption.h"

int main()
{
    // construct an option using default constructor
    callOption option1;
    // print the call option price for option1
    cout << "price for the call option1: " << option1.price() << endl;

    // construct an option using the other constructor, change the option vol to 0.1
    callOption option2 (100, 0.001, 0.1, 100, 30);
    // print the call option price for option2
    cout << "price for the call option2: " << option2.price() << endl;

    return 0;
}
