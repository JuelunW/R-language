#include "std_lib_facilities.h"

// Lab Exercise 4: On a two dimensional Cartesian coordinate,
// given a triangle by providing coordinates of its vertices (A, B, C).
// The goal is to approximate the optimal point D using Monte Carlo Simulation.
// The optimal point is defined as D = argmin{ dist(A, D) + dist(B,D) + dist(C,D) }.

// helper function used to generate uniformly distributed random variable with lower bound and upper bound
double uniformRandom(double lower, double upper){
    random_device r;
    default_random_engine engine(r());
    uniform_real_distribution<double> uniform(lower, upper);
    return uniform(engine);
}

// helper function used to calculate the sum distance
double sumDistance(double x, double y, const vector<double>& xx, const vector<double>& yy) {
    // helpful function you could use: pow(), sqrt()
    /*-----------------Your code----------------------*/
    double dist = 0;
    for(int i = 0; i < xx.size(); i++){
        dist += sqrt(pow(xx[i] - x, 2) + pow(yy[i] - y, 2));
    }

    return dist;
    /*-----------------Your code----------------------*/
}

pair<double, vector<double>> minimumDistance(const vector<double>& xx, const vector<double>& yy){
    // The vector result
    pair <double, vector<double>> result;
    // The optimal point
    vector<double> optimalPoint = {0,0};
    // the optimal value
    double optimalDistance = 10000;
    // simulation time
    int N = 100000;
    /*-----------------Your code----------------------*/
    for(int i = 0; i < N; i++){
        double x = uniformRandom(1, 9);
        double y = uniformRandom(1, 9);
        double dist = sumDistance(x, y, xx, yy);
        if(dist < optimalDistance){
            optimalDistance =  dist;
            optimalPoint[0] = x;
            optimalPoint[1] = y;
        }
    }
    /*-----------------Your code----------------------*/
    // Assign values to result
    result.first = optimalDistance;
    result.second = optimalPoint;
    return result;
}



int main()
{
    // x and y coordinates of triangle vertices
    vector<double> xx = {1,9,5};
    vector<double> yy = {1,1,3};
    pair<double, vector<double>> solution = minimumDistance(xx, yy);
    cout << "The optimal point is: (" << solution.second[0] << ", " << solution.second[1] << ")" << endl;
    cout << "The optimal value is: " << solution.first << endl;
    return 0;
}