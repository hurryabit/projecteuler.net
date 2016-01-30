// problem258.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const int DIM = 2000;
const long long int MOD = 20092010;

typedef int* UTMatrix;

inline int* get_cell(const UTMatrix matrix, int i, int j) {
  int index = (i-1)*DIM + (j-1);
  return matrix+index;
}

inline int get_value(const UTMatrix matrix, int i, int j) {
  return *get_cell(matrix,i,j);
}

inline long long int get_value_ll(const UTMatrix matrix, int i, int j) {
  return (long long int) get_value(matrix,i,j);
}

UTMatrix new_matrix() {
  return malloc(sizeof(int) * DIM * DIM);
}

void mult(UTMatrix result, const UTMatrix a, const UTMatrix b) {
  int i, j, k;
  long long int res;

  for (i = 1; i <= DIM; ++i) {
    for (j = 1; j <= DIM; ++j) {
      res = 0;
      for (k = 1; k <= DIM; ++k) {
        res += get_value_ll(a,i,k)*get_value_ll(b,k,j);
      }
      *get_cell(result,i,j) = (int) (res % MOD);
    }
  }
}

inline void mult_with_base(UTMatrix matrix) {
/*  int* row = malloc(sizeof(int) * (DIM+1));
  int j;
  for (j = 1; j <= DIM; ++j) {
    row[j] = (get_value(matrix,1,j) + get_value(matrix,2,j)) % MOD;
  }

  memmove(matrix,matrix+DIM,sizeof(int)*DIM*(DIM-1));
  memcpy(matrix+DIM*(DIM-1),row+1,sizeof(int)*DIM);*/

  int i, i1, i2, j;
  int* cell;
  for (i = 0; i < DIM; ++i) {
    i1 = i+1;
    i2 = i1 % DIM + 1;
    for (j = 1; j <= DIM; ++j) {
      cell = get_cell(matrix,i1,j);
      *cell += get_value(matrix,i2,j);
      *cell %= MOD;
    }
  }
}

void power(UTMatrix result, long long int n, UTMatrix helper) {
  if (n == 0) {
    int i;
    for (i = 0; i < DIM * DIM; ++i) {
      result[i] = 0;
    }
    for (i = 1; i <= DIM; ++i) {
      *get_cell(result,i,i) = 1;
    }
  }
  else {
    power(helper, n >> 1, result);
    mult(result, helper, helper);
    if (n % 2 == 1) {
      mult_with_base(result);
    }
  }

  printf("Calculated power for n=%lld.\n", n);
}

void print_matrix(const UTMatrix matrix) {
  int i, j;
  for (i = 1; i <= DIM; ++i) {
    for (j = 1; j <= DIM; ++j) {
      printf("%4d  ", get_value(matrix,i,j));
    }
    printf("\n");
  }
  printf("\n");
}

int main1() {
  UTMatrix matrix1 = new_matrix();
  UTMatrix matrix2 = new_matrix();

  power(matrix1, 0, matrix2);
  mult_with_base(matrix1);
  mult(matrix2,matrix1,matrix1);
  print_matrix(matrix2);

  return 0;
}

int main(int argc, char** argv) {
  UTMatrix matrix = new_matrix();
  long long int n = 500000000000000; //atoi(argv[1]);
  int j;

  power(matrix, n, new_matrix());
  n = 0;
  for (j = 1; j <= DIM; ++j) {
    n += get_value_ll(matrix,1,j);
  }
  n %= MOD;

  printf("%lld\n", n);

  return 0;
}
