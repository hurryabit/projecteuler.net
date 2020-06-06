// problem181.cc

#include<iostream>
#include<vector>

using namespace std;

long monochrome_exact(int x, int g);
long monochrome_bound(int x, int g);
long bichrome_exact(int x, int y, int g);
long bichrome_bound(int x, int y, int g);

long** monochrome_exact_table;

long monochrome_exact(int x, int g) {
  long& entry = monochrome_exact_table[x][g];
  if( entry < 0 ) {
    if( x < g )
      entry = 0;
    else
      entry = monochrome_bound(x-g, g);
  }

  return entry;
}

long** monochrome_bound_table;

long monochrome_bound(int x, int g) {
  static vector<vector<long>> table(61, vector<long>(101, -1));

  long& entry = table[x][g];
  if( entry < 0 ) {
    if( x == 0 )
      entry = 1;
    else if( g == 0 )
      entry = 0;
    else
      entry = monochrome_bound(x, g-1) + monochrome_exact(x, g);
  }

  return entry;
}

long bichrome_exact(int x, int y, int g) {
  static vector<vector<vector<long>>> table(61, vector<vector<long>>(41, vector<long>(101, -1)));

  long& entry = table[x][y][g];
  if( entry < 0 ) {
    if( x < y )
      entry = bichrome_exact(y, x, g);
    else if( x == 0 )
      entry = monochrome_exact(y, g);
    else if( y == 0 )
      entry = monochrome_exact(x, g);
    else {
      entry = 0;
      int b_xy = min(g,min(x,y));
      for( int g_xy = 0; g_xy <= b_xy; ++g_xy ) {
        int b_x = min(g,x) - g_xy;
        for( int g_x = max(g - y, 0); g_x <= b_x; ++g_x ) {
          int g_y = g - g_xy - g_x;
          for( int x_cut = g_x; x_cut <= x - g_xy; ++x_cut ) {
            for( int y_cut = g_y; y_cut <= y - g_xy; ++y_cut ) {
              entry += monochrome_exact(x_cut, g_x)
                     * monochrome_exact(y_cut, g_y)
                     * bichrome_bound(x - x_cut - g_xy, y - y_cut - g_xy, g_xy);
            }
          }
        }
      }
    }
  }

  return entry;
}

long bichrome_bound(int x, int y, int g) {
  static vector<vector<vector<long>>> table(61, vector<vector<long>>(41, vector<long>(101, -1)));

  long& entry = table[x][y][g];
  if( entry < 0 ) {
    if( x < y )
      entry = bichrome_bound(y, x, g);
    else if( x == 0 && y == 0 )
      entry = 1;
    else if( g == 0 )
      entry = 0;
    else
      entry = bichrome_bound(x, y, g-1) + bichrome_exact(x, y, g);
  }

  return entry;
}

int main() {
  monochrome_exact_table = new long*[61];
  monochrome_bound_table = new long*[61];
  for( int i = 0; i <= 60; ++i ) {
    long* monochrome_exact_row = new long[101];
    long* monochrome_bound_row = new long[101];
    for( int j = 0; j <= 100; ++j ) {
      monochrome_exact_row[j] = -1;
      monochrome_bound_row[j] = -1;
    }
    monochrome_exact_table[i] = monochrome_exact_row;
    monochrome_bound_table[i] = monochrome_bound_row;
  }

  cout << bichrome_bound(60, 40, 100) << endl;
}
