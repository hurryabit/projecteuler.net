// Problem313.java

import java.util.*;

class Problem313 {
  static final int M = 13, N = 13, MN = M * N;
  
  static class Point implements Comparable<Point> {
    public final int x, y;
    
    public static final Point first = new Point(0,0);
    public static final Point last  = new Point(M-1,N-1);
    
    public Point(final int x, final int y) {
      this.x = x;
      this.y = y;
    }
    
    public Point(final int c) {
      x = c / N;
      y = c - x * N;
    }
    
    public Point add(final Point p) {
      return new Point(x + p.x, y + p.y);
    }

    public boolean isInBounds() {
      return 0 <= x && x < M && 0 <= y && y < N;
    }

    public int encode() {
      return x * N + y;
    }
    
    public String toString() {
      return "(" + String.valueOf(x) + ", " + String.valueOf(y) + ")";
    }
    
    public boolean equals(Object obj) {
      if( !(obj instanceof Point) )
        return false;
      final Point p = (Point) obj;
      return x == p.x && y == p.y;
    }
    
    public int compareTo(Point p) {
      int cmp = Integer.compare(x, p.x);
      return cmp == 0 ? Integer.compare(y, p.y) : cmp;
    }
    
    public List<Point> adjacency() {
      final Vector<Point> adjs = new Vector<Point>(4);
      final Point[] dirs = {new Point(1,0), new Point( 0, 1), new Point(-1, 0), new Point( 0,-1)};
      for( final Point dir: dirs ) {
        final Point adj = add(dir);
        if( adj.isInBounds() )
          adjs.add(adj);
      }
      return adjs;
    }
  }
  
  static class Config implements Comparable<Config> {
    public final Point red, gap;
    
    public Config(final Point red, final Point gap) {
      this.red = red;
      this.gap = gap;
    }
    
    public static final Config initial = new Config(Point.first, Point.last);
    
    public boolean isFinal() {
      return red.equals(Point.last);
    }
    
    public List<Config> adjacency() {
      final Vector<Config> adjs = new Vector<Config>(4);
      for( final Point gap_new: gap.adjacency() ) {
        final Point red_new = gap_new.equals(red) ? gap : red;
        adjs.add(new Config(red_new, gap_new));
      }
      return adjs;
    }
    
    public String toString() {
      return "Config { red = " + red.toString() + ", gap = " + gap.toString() + " }";
    }
    
    public boolean equals(Object obj) {
      if( !(obj instanceof Config) )
        return false;
      final Config cfg = (Config) obj;
      return red.equals(cfg.red) && gap.equals(cfg.gap);
    }

    public int compareTo(Config cfg) {
      int cmp = red.compareTo(cfg.red);
      return cmp == 0 ? gap.compareTo(cfg.gap) : cmp;
    }
    
    public void prettyPrint() {
      for( int y = 0; y < N; ++y ) {
        for( int x = 0; x < M; ++x ) {
          char c = '.';
          final Point p = new Point(x, y);
          if( p.equals(red) )
            c = '#';
          else if( p.equals(gap) )
            c = 'O';
          System.out.print(c);
        }
        System.out.println();
      }
      System.out.println();
    }
  }
  
  public static int f(int m, int n) {
    if( m > n )
      return 6*m + 2*n - 13;
    else if( m == n)
      return 8*m - 11;
    else
      return 2*m + 6*n - 13;
  }
  
  public static void main(String[] args) {
    TreeMap<Config, Config> pred = new TreeMap<Config, Config>();
    Queue<Config> queue = new LinkedList<Config>();
    
    pred.put(Config.initial, Config.initial);
    queue.add(Config.initial);
    
    while( !queue.element().isFinal() ) {
      final Config curr = queue.remove();
      for( final Config next: curr.adjacency() ) {
        if( !pred.containsKey(next) ) {
          pred.put(next, curr);
          queue.add(next);
        }
      }
    }
    
    final Deque<Config> stack = new LinkedList<Config>();
    Config curr = queue.element();
    stack.addFirst(curr);
    while( !curr.equals(pred.get(curr)) ) {
      curr = pred.get(curr);
      stack.addFirst(curr);
    }
/*    int i = 0;
    while( !stack.isEmpty() ) {
      System.out.println(i);
      stack.removeFirst().prettyPrint();
      ++i;
    }*/
    System.out.println(stack.size()-1);
    System.out.println(f(M, N));
  }
}
