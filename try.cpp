#include <iostream>
#include <string>
#include <cstring>
#include <algorithm>
#include <math.h>
#include <cmath>
using namespace std;
int errorcount = 0;
void MatrixParameters(char* matrixstring, int Arraysize[]){
    int RowCounter = 0;
    int ColCounter = 0;
   if(strlen(matrixstring) == 2 && matrixstring[0] == '[')
   {
       errorcount++;
       return;
   }
    for (unsigned int i = 1; i < strlen(matrixstring) - 1; i++)
    {
        if (!isdigit(matrixstring[i]) && matrixstring[i] != ',' && matrixstring[i] != ' ' && matrixstring[i] != '-' && matrixstring[i] != '.')
        {
            
            errorcount++;
            break;
            return;
        }
         else if(matrixstring[i] == ',' && matrixstring[i+1] == ',')
         {
             errorcount++;
             break;
             return;
         }
        else if(matrixstring[i] == '.' && matrixstring[i+1] == '.')
         {
             errorcount++;
             break;
             return;
         }
        else if(matrixstring[i] == ' ' && matrixstring[i+1] == ' ')
         {
             errorcount++;
             break;
             return;
         }
        else if(strlen(matrixstring) <= 3 && !isdigit(matrixstring[1]))
        {
            errorcount++;
            break;
            return;
        }
        else if(matrixstring[i] == '-' && matrixstring[i+1] == '-')
        {
            errorcount++;
            break;
            return;
        }
    }
    for (unsigned int i = 1; i < strlen(matrixstring); i++){
        if (matrixstring[i] == ' ')
        {
            ColCounter++;
        }
        if (matrixstring[i] == ',') 
        {
            break;
        }
    }    
    /* for (unsigned int i = 1; i < strlen(matrixstring); i++){
        if (matrixstring[i] == ' ')
        {
            y++;
        }
    } */
    for (unsigned int i = 1; i < strlen(matrixstring); i++){
        if (matrixstring[i] == ','){
            RowCounter++;
        }
    }
    if (RowCounter == 0 && ColCounter == 0 && matrixstring[0] == '[' && !isdigit(matrixstring[1])){
        errorcount++;
        return;
    }
    if  (RowCounter >= 100 || ColCounter >= 100)
    {
        errorcount++;
        return;
    }
    
    Arraysize[0] = ColCounter + 1;
    Arraysize[1] = RowCounter + 1;
}

void tokenize(char *s, string StrMatrix[]){
    int i = 0;
    char *tok = strtok(s," [,");
    while (tok)
    {
        StrMatrix[i] = tok;
        tok = strtok(NULL," ,]");
        i++;
    }
}

float ConvertToFloat(string s){
        char char_array[s.length() + 1];
        strcpy(char_array, s.c_str());
        double num = atof(char_array);
        return num;
}

void fill2d(float ConvertedMatrix[], float twod[][100], int MatrixSize[]){
    int x = 0;
    for(int i = 0; i < MatrixSize[1]; i++){
        for (int j = 0; j < MatrixSize[0]; j++){
            twod[i][j] = ConvertedMatrix[x];
            x++;
        }
    }
}
void getCofactor(float A[100][100], float temp[100][100], int p, int q, int n)
{
    int i = 0, j = 0;
    float temp2;
    // Looping for each element of the matrix
    for (int row = 0; row < n; row++)
    {
        for (int col = 0; col < n; col++)
        {
            // Copying into temporary matrix only those element
            // which are not in given row and column
            if (row != p && col != q)
            {
                temp2 = 100 *A[row][col];
                temp2 = round(temp2);
                A[row][col] = (float)temp2 / 100;
                temp[i][j++] = A[row][col];
                
                // Row is filled, so increase row index and
                // reset col index
                if (j == n - 1)
                {
                    j = 0;
                    i++;
                }
            }
        }
    }
}

float MatrixDeterminant( float matrix[100][100], int n) {
   float det = 0;
    float temp;
    int sign = 1;
   float submatrix[100][100];
    if (n == 1)
        return matrix[0][0];
   if (n == 2)
   return ((matrix[0][0] * matrix[1][1]) - (matrix[1][0] * matrix[0][1]));
   else {
      for (int f = 0; f < n; f++) 
    {
        // Getting Cofactor of mat[0][f]
        getCofactor(matrix, submatrix, 0, f, n);
        det += sign * matrix[0][f]
             * MatrixDeterminant(submatrix, n - 1);

        // terms are to be added with alternate sign
        sign = -sign;
    }

      }
    temp = det * 100;
    temp = round(temp);
    det = (float)temp / 100;
   return det;
}
void MatrixTranspose(float matrix[100][100], int col, int row)
{
    int temp;
    float prectemp;
    float transpose[100][100];
    for (int i = 0; i < row; i++)
    {
        for (int j = 0; j < col; j++)
        {
            transpose[j][i] = matrix[i][j];
            prectemp = transpose[j][i] * 100;
            prectemp = round(prectemp);
            transpose[j][i] = (float)prectemp / 100;
        }
        
    }
    temp = col;
    col = row;
    row = temp;
    cout << '[';
    for (int i = 0; i < row; i++){
        for(int j = 0; j < col; j++){
            cout << transpose[i][j];
            if (j < col - 1)   cout << " ";
        }
        if (i < row - 1)   cout << ", ";
    }
    cout << ']';
}
void adjoint(float A[100][100],float adj[100][100],int Matrix1Size[])
{
    int N = Matrix1Size[0];
    if (N == 1)
    {
        adj[0][0] = 1;
        return;
    }

    // temp is used to store cofactors of A[][]
    float sign = 1, temp[100][100];

    for (int i=0; i<Matrix1Size[0]; i++)
    {
        for (int j=0; j<Matrix1Size[0]; j++)
        {
            // Get cofactor of A[i][j]
            getCofactor(A, temp, i, j, Matrix1Size[0]);

            // sign of adj[j][i] positive if sum of row
            // and column indexes is even.
            sign = ((i+j)%2==0)? 1: -1;

            // Interchanging rows and columns to get the
            // transpose of the cofactor matrix
            adj[j][i] = (sign)*(MatrixDeterminant(temp, N-1));
        }
    }
}

void MatrixInverse(float matrix[100][100],int Matrix1Size[])    
{
    long temp;
    float inverse[100][100];
    //int N = Matrix1Size[0];
    if (Matrix1Size[0] != Matrix1Size[1])
    {
        cout << "ERROR!";
        return;
    }
    
    // Find determinant of A[][]
    float det = MatrixDeterminant(matrix, Matrix1Size[0]);
    if (det == 0)
    {
        cout << "ERROR!";
        return;
    }

    // Find adjoint
    float adj[100][100];
    adjoint(matrix, adj,Matrix1Size);
    cout << '[';
    // Find Inverse using formula "inverse(A) = adj(A)/det(A)"
    for (int i=0; i<Matrix1Size[1]; i++)
    {
        for (int j=0; j<Matrix1Size[0]; j++)
        {
            inverse[i][j] = adj[i][j]/float(det);
            temp = 100 * (inverse[i][j]);
            inverse[i][j] = (float)temp / 100;
            cout << inverse[i][j];
            if (j < Matrix1Size[0] - 1)   cout << " ";
        }
        if (i < Matrix1Size[1] - 1)   cout << ", ";
    }
    cout << ']';
    return;
}    



int main()
{
    int detern;
    char firstmatrix[90000];
    float ConvertedMatrix[80000];
    float MatrixArr1[100][100];
    //float Answer1[100][100] = {0};
    string StrMatrix[80000] = {"null"};
    int Matrix1Size[2];
    char operators[1];
    
    cin.getline(firstmatrix, 80000, '\n');
    cin.getline(operators, 2, '\n');
    
    MatrixParameters(firstmatrix, Matrix1Size);
    tokenize(firstmatrix, StrMatrix);

    for (unsigned int i = 0; i < sizeof(StrMatrix) / sizeof(StrMatrix[0]) - 1; i++){
        string MatrixString = StrMatrix[i];
        if (MatrixString.length() == 0)    break;
        ConvertedMatrix[i] = ConvertToFloat(MatrixString);
    }

    fill2d(ConvertedMatrix, MatrixArr1, Matrix1Size);
    if(Matrix1Size[0] == Matrix1Size[1])
    {
        detern = Matrix1Size[0];
    }
    else{
        detern = 0;
    }

    switch(operators[0])
    {
    case 'D':
        if (detern == 0){
            cout << "ERROR!";
            break;
        }
        else if(Matrix1Size[0] == 1 || Matrix1Size[1] == 1)
        {
            cout << "ERROR!";
            break;
        }
        else if (errorcount > 0)
        {
            cout << "ERROR!";
            break;
        }
        else
        {
           cout << MatrixDeterminant(MatrixArr1, detern);
           break;
        }
        break;
    case 'T':
            if (errorcount > 0)
            {
                cout << "ERROR!";
                break;
            }
            else
            {
                 MatrixTranspose(MatrixArr1,Matrix1Size[0], Matrix1Size[1]);
                 break;
            }
            break;
    case 'I':
        if (detern == 0)
            cout << "ERROR!";
        else if(errorcount > 0)
            cout << "ERROR!";
        else{
            MatrixInverse(MatrixArr1, Matrix1Size);
            break;
        }
        break;
    default:
        cout << "ERROR!";
        break;

    }

    return 0;
}
