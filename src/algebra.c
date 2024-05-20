#include "algebra.h"
#include <stdio.h>
#include <math.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

Matrix add_matrix(Matrix a, Matrix b)
{
    if (a.cols == b.cols && a.rows == b.rows)
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] + b.data[i][j];
            }
        }
        return c;
    }
    printf("Error: Matrix a and b must have the same rows and cols.");
    return create_matrix(0, 0);
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    if (a.cols == b.cols && a.rows == b.rows)
    {
        Matrix c = create_matrix(a.rows, a.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < a.cols; j++)
            {
                c.data[i][j] = a.data[i][j] - b.data[i][j];
            }
        }
        return c;
    }
    printf("Error: Matrix a and b must have the same rows and cols.");
    return create_matrix(0, 0);
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    if (a.cols == b.rows)
    {
        Matrix c = create_matrix(a.rows, b.cols);
        for (int i = 0; i < a.rows; i++)
        {
            for (int j = 0; j < b.cols; j++)
            {
                for (int k = 0; k < a.cols; k++)
                {
                    c.data[i][j] = a.data[i][k] * b.data[k][j];
                }
            }
        }
        return c;
    }
    printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.");
    return create_matrix(0, 0);
}

Matrix scale_matrix(Matrix a, double k)
{
    Matrix b = create_matrix(a.rows, a.cols);
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            b.data[i][j] = a.data[i][j] * k;
        }
    }
    return b;
}

Matrix transpose_matrix(Matrix a)
{
    Matrix b = create_matrix(a.cols, a.rows);
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            b.data[i][j] = a.data[j][i];
        }
    }
    return b;
}

double det_matrix(Matrix a)
{
    if (a.cols == a.rows)
    {
        if (a.cols == 1)
            return a.data[0][0];
        else if (a.cols == 2)
        {
            return a.data[0][0] * a.data[1][1] - a.data[0][1] * a.data[1][0];
        }
        else
        {
            int n = a.cols;
            double res = 0;
            double s = 1;               // 符号交替为±
            for (int i = 0; i < n; i++) // 按列展开
            {
                Matrix b = create_matrix(n - 1, n - 1); // 存储子行列式
                for (int j = 1; j < n; j++)
                {
                    int ni = 0, nj = 0;
                    for (int k = 0; k < n; k++)
                    {
                        if (k != 0)
                        {
                            for (int l = 0; l < n; l++)
                            {
                                if (l != i)
                                {
                                    b.data[ni][nj++] = a.data[k][l];
                                    if (nj == n - 1)
                                    {
                                        ni++;
                                        nj = 0;
                                    }
                                }
                            }
                        }
                    }

                    res += s * a.data[i][j] * det_matrix(b);
                    s = -s; // 改变符号
                }
            }
            return res;
        }
    }
    printf("Error: The matrix must be a square matrix.");
    return 0;
}

Matrix inv_matrix(Matrix a)
{
    if (a.cols == a.rows)
        {
            printf("Error: The matrix must be a square matrix.");
            return create_matrix(0, 0);
        }
    else if (det_matrix(a) == 0)
    {
        printf("Error: The matrix is singular.");
        return create_matrix(0, 0);
    }
    // ToDo 真心懒了
}

int rank_matrix(Matrix a)
{
    int M = a.cols, N = a.rows;
    int i, j, k;
    float temp;
    int r = 0, d = 0; // r表示秩，d表示当前正在哪一行。
    for (i = 0; i < M; i++)
    {
        k = d; /*a[i][i] a[i+1][i] ... a[n][i]中绝对值最大的行位置*/
        for (j = d + 1; j < N; j++)
            if (fabs(a.data[k][i]) < fabs(a.data[j][i]))
                k = j;
        if (k != d) /*交换第i行和第k行，行列式该变号*/
        {
            for (j = i; j < M; j++)
            {
                temp = a.data[d][j];
                a.data[d][j] = a.data[k][j];
                a.data[k][j] = temp;
            }
        }
        if (a.data[d][i] == 0) /*当a[i][i]为零是时，行列式为零*/
        {
            continue;
        }
        else
        {
            r = r + 1;
            for (j = 0; j < N; j++)
            {
                if (j != d)
                {
                    temp = -1 * a.data[j][i] / a.data[d][i];
                    for (k = i; k < M; k++)
                        a.data[j][k] = a.data[j][k] + temp * a.data[d][k];
                }
            }
            temp = a.data[d][i];
            for (j = i; j < M; j++)
                a.data[d][j] = a.data[d][j] / temp;
        }
        d = d + 1;
        if (d >= N)
            return r;
    }
    return 0;
}

double trace_matrix(Matrix a)
{
    if (a.cols == a.rows)
    {
        double t = 0;
        for(int i = 0; i < a.cols; i++)
        {
            t += a.data[i][i];
        }
        return t;
    }
    printf("Error: The matrix must be a square matrix.");
    return 0;
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}