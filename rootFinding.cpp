#include "std_lib_facilities.h"
double f (double x); // define a formula need to be solved
double bisectionRoot (double low, double high, int maximumIterations = 10000, double tolerance = .000001);
double secantRoot (double low, double high, int maximumIterations = 10000, double tolerance = .000001);

int main()
{
    //bisectionRoot
    double low = 0.1;
    double high = 10.1;

    cout << "The bisection root of function is " << bisectionRoot(low, high) << endl;
    cout << "The secant root of function is " << secantRoot(low, high) << endl;

    return 0;
}


double f (double x){
    return (pow((x - 1), 3) - 125);
}

double bisectionRoot (double low, double high, int maximumIterations, double tolerance){
    if(f(low) * f(high) > 0){
        cout << "Bisection is not suitable for this function." << endl;
        return 1;
    }

    if (f(low) > 0){
        double tmp = low;
        low = high;
        high = tmp;
    }

    double midPoint;
    for (int i = 0; i < maximumIterations; i++){
        midPoint = (low + high)/2;
        if(f(midPoint) == 0 || abs(low - high)/2 < tolerance){
            break;
        }
        else if(f(midPoint) < 0) low = midPoint;
        else high = midPoint;
    }
    return midPoint;
}

double secantRoot (double low, double high, int maximumIterations, double tolerance){
    double xm1,xm2;

    for (int i = 0; i < maximumIterations; i++){
        xm1 = (low * f(high) - high * f(low)) / (f(high) - f(low));
        low = high;
        high = xm1;

        xm2 = (low * f(high) - high * f(low)) / (f(high) - f(low));
        if (xm1 == 0 || fabs(xm2 - xm1) < tolerance) break;
    }

    return xm1;
}
