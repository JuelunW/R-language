

#include "std_lib_facilities.h"

/*1. Define a function which calculates the outer product of two vectors. The function return is a matrix. (15 points) */
vector<vector<int>> outerProduct(vector<int>& A, vector<int>& B)
{
	// implementation

    vector<vector<int>> outerProductMatrix;
    vector<int> tmp;

    for (int row = 0; row < A.size(); row++){
        for (int col = 0; col < B.size(); col++){
            tmp.push_back(A[row] * B[col]);
        }
        outerProductMatrix.push_back(tmp);
        tmp.clear();
    }

    return outerProductMatrix;
}

/*2. Define a function which transposes a matrix. (15 points)*/
vector<vector<int>> transpose(vector<vector<int>>& A)
{
	// implementation

    vector<vector<int>> tmp;
    vector<int> ttmp;
    for (int i = 0; i < A[0].size(); i++){
        for (int j = 0; j < A.size(); j++) {
            ttmp.push_back(A[j][i]);
        }
        tmp.push_back(ttmp);
        ttmp.clear();
    }

    return tmp;
}

/*3. Define a function which can properly print out the matrix* (10 points)*/ 
void printMatrix(vector<vector<int>>& A)
{
    cout << '\n';
    cout << "{" << '\n';
    for (int i = 0; i < A.size(); i++) {
        cout << '\t' << "{ ";
        for (int j = 0; j < A[0].size(); j++) {
            cout << A[i][j];
            if (j < A[0].size() - 1) {
                cout << ',';
            }
            cout << ' ';
        }
        cout << '}';
        if (i < A.size() - 1) {
            cout << ',';
        }
        cout << '\n';
    }
    cout << "};";
    cout << '\n';
}




int main()
{
	// Define both vector A and vector B
	vector<int> A = {1 ,2, 3, 4, 5};
	vector<int> B = {1, 2, 3};

	/*Test outerProduct function, the expected output is:
	1 2 3
	2 4 6
	3 6 9
	4 8 12
	5 10 15
	*/
	vector<vector<int>> M = outerProduct(A, B);
	printMatrix(M);

	/*Test transpose function, the expected output is:
	1 2 3 4 5
	2 4 6 8 10
	3 6 9 12 15
	*/
    vector<vector<int>> tM = transpose(M);
    printMatrix(tM);

    keep_window_open();
    return 0;
}
