/**
 * Title:        StanfordMaxEnt<p>
 * Description:  A Maximum Entropy Toolkit<p>
 * Copyright:    Copyright (c) Kristina Toutanova<p>
 * Company:      Stanford University<p>
 */

package edu.stanford.nlp.maxent;

import edu.stanford.nlp.io.InDataStreamFile;
import edu.stanford.nlp.io.OutDataStreamFile;
import edu.stanford.nlp.io.PrintFile;
import edu.stanford.nlp.util.Index;
import edu.stanford.nlp.util.IntPair;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

/**
 * This class represents the training samples. It can return statistics of them,
 * for example the frequency of each x or y
 * in the training data.
 *
 * @author Kristina Toutanova
 * @version 1.0
 */
public class Experiments {

  /**
   * vArray has dimensions [numTraining][2] and holds the x and y for each training sample.
   * Its length is the number of data points.
   */
  protected int[][] vArray;

  /**
   * px[x] holds the number of times the history x appeared in training data
   */
  protected int[] px; // 4MB, may be compress it

  /**
   * py[y] holds the number of times the outcome y appeared in training data
   */
  protected int[] py;

  protected int[] maxY; // for each x, which is the maximum possible y

  /**
   * pxy[x][y]=# times (x,y) occurred in training
   */
  protected static int[][] pxy; // maybe there is a better way to keep that, if it is zero or 1 , else the number // check whether it is non-deterministic, and how much

  public static int xSize, ySize;

  /**
   * v may hold the actual Experiments, i.e. Objects of type Experiments
   */
  private ArrayList<Experiments> v = new ArrayList<Experiments>();

  /**
   * Maximum ySize.
   * CDM May 2007: What is this and what does it control?  Why isn't it set
   * dynamically?  Is it the number of y values that one x value can have?
   * If so, although it was set to 5, it should be 7 for the WSJ PTB.
   * But that doesn't solve the problem for the data set after that....
   */
  static int dim = 7;  // was 5 before CDM fiddled


  /**
   * The value of classification y for x.
   * Used for ranking.
   */

  public double[][] values;


  public Experiments() {
  }

  /**
   * If this constructor is used, the maximum possible class overall is found and all classes are assumed possible
   * for all instances
   *
   * @param vArray
   */
  public Experiments(int[][] vArray) {
    this.vArray = vArray;
    ptilde();
  }


  /**
   * The number of possible classes for each instance is contained in the array maxYs
   * then the possible classes for x are from 0 to maxYs[x]-1
   *
   * @param vArray
   */
  public Experiments(int[][] vArray, int[] maxYs) {
    this.vArray = vArray;
    ptilde();
    this.maxY = maxYs;
  }

  public Experiments(int[][] vArray, int ySize) {
    this.vArray = vArray;
    Experiments.ySize = ySize;
    ptilde(ySize);
  }

  public Index<IntPair> createIndex() {
    Index<IntPair> index = new Index<IntPair>();
    for (int x = 0; x < px.length; x++) {
      int numberY = numY(x);
      for (int y = 0; y < numberY; y++) {
        index.add(new IntPair(x, y));
      }
    }
    return index;
  }

  /**
   * The filename has format: <data><xSize>xSize</xSize><ySize>ySize</ySize>
   * x1 y1
   * x2 y2
   * ..
   * </data>
   * ..
   */
  public Experiments(String filename) {
    try {
      Exception e1 = new Exception("Incorrect data file format");
      BufferedReader in = new BufferedReader(new FileReader(filename));
      String head = in.readLine();
      if (!head.equals("<data>")) {
        throw e1;
      }
      String xLine = in.readLine();
      if (!xLine.startsWith("<xSize>")) {
        throw e1;
      }
      if (!xLine.endsWith("</xSize>")) {
        throw e1;
      }
      int index1 = xLine.indexOf('>');
      int index2 = xLine.lastIndexOf('<');
      String xSt = xLine.substring(index1 + 1, index2);
      System.out.println(xSt);
      xSize = Integer.parseInt(xSt);
      System.out.println("xSize is " + xSize);
      String yLine = in.readLine();
      if (!yLine.startsWith("<ySize>")) {
        throw e1;
      }
      if (!yLine.endsWith("</ySize>")) {
        throw e1;
      }
      index1 = yLine.indexOf('>');
      index2 = yLine.lastIndexOf('<');
      ySize = Integer.parseInt(yLine.substring(index1 + 1, index2));
      System.out.println("ySize is " + ySize);
      String nLine = in.readLine();
      if (!nLine.startsWith("<number>")) {
        throw e1;
      }
      if (!nLine.endsWith("</number>")) {
        throw e1;
      }
      index1 = nLine.indexOf('>');
      index2 = nLine.lastIndexOf('<');
      int number = Integer.parseInt(nLine.substring(index1 + 1, index2));
      System.out.println("number is " + number);
      vArray = new int[number][2];
      int current = 0;
      while (current < number) {
        String experiment = in.readLine();
        int index = experiment.indexOf(' ');
        int x = Integer.parseInt(experiment.substring(0, index));
        int y = Integer.parseInt(experiment.substring(index + 1));
        vArray[current][0] = x;
        vArray[current][1] = y;
        current++;
      }
      ptilde(ySize);
    } catch (Exception e) {
      System.out.println("Incorrect data file format");
      e.printStackTrace();
    }
  }

  public void add(Experiments m) {
    v.add(m);
  }


  public final void ptilde() {
    int maxX = 0;
    int maxY = 0;
    for (int[] sample : vArray) {
      if (maxX < sample[0]) {
        maxX = sample[0];
      }
      if (maxY < sample[1]) {
        maxY = sample[1];
      }
    }
    px = new int[maxX + 1];
    py = new int[maxY + 1];
    pxy = new int[maxX + 1][dim];
    xSize = maxX + 1;
    ySize = maxY + 1;
    //GlobalHolder.xSize=xSize;
    //GlobalHolder.ySize=ySize;
    int[] yArr = new int[dim];
    for (int[] sample : vArray) {
      int xC = sample[0];
      int yC = sample[1];
      px[xC]++;
      py[yC]++;
      for (int j = 0; j < dim; j++) {
        yArr[j] = pxy[xC][j] > 0 ? pxy[xC][j] % ySize : -1;
      }
      for (int j = 0; j < dim; j++) {
        if (yArr[j] == -1) {
          pxy[xC][j] = ySize + yC;
          break;
        }
        if (yC == yArr[j]) {
          pxy[xC][j] += ySize;
          break;
        }
      } // for dim

      //System.out.println(" Exception more than  "+dim);

    }// for i

    // check for same x with different y
    for (int y = 0; y < ySize; y++) {
      double sum = 0.0;
      for (int x = 0; x < xSize; x++) {
        double p1 = ptildeXY(x, y);
        sum = sum + p1;
      }
      if (Math.abs(ptildeY(y) - sum) > 0.00001) {
        System.out.println("Experiments error: for y=" + y + ", ptildeY(y)=" + ptildeY(y) + " but Sum_x ptildeXY(x,y)=" + sum);
      }
    }// for y

    this.maxY = new int[xSize];
    for (int j = 0; j < xSize; j++) {
      this.maxY[j] = ySize;
    }
  } // end ptilde()


  public void setMaxY(int[] maxY) {
    this.maxY = maxY;
  }


  public int numY(int x) {
    return maxY[x];
  }



  /** When we want a pre-given number of classes.
   */
  public void ptilde(int ySize) {
    int maxX = 0;
    int maxY = 0;
    Experiments.ySize = ySize;
    for (int[] sample : vArray) {
      if (maxX < sample[0]) {
        maxX = sample[0];
      }
      if (maxY < sample[1]) {
        maxY = sample[1];
      }
    }
    px = new int[maxX + 1];
    maxY = ySize - 1;
    py = new int[ySize];
    pxy = new int[maxX + 1][dim];
    xSize = maxX + 1;
    ySize = maxY + 1;
    //GlobalHolder.xSize=xSize;
    //GlobalHolder.ySize=ySize;
    int[] yArr = new int[dim];
    for (int[] sample : vArray) {
      int xC = sample[0];
      int yC = sample[1];
      px[xC]++;
      py[yC]++;
      for (int j = 0; j < dim; j++) {
        yArr[j] = pxy[xC][j] > 0 ? pxy[xC][j] % ySize : -1;
      }
      for (int j = 0; j < dim; j++) {
        if (yArr[j] == -1) {
          pxy[xC][j] = ySize + yC;
          break;
        }
        if (yC == yArr[j]) {
          pxy[xC][j] += ySize;
          break;
        }
      } // for dim

      //System.out.println(" Exception more than  "+dim);

    }// for i
    // check for same x with different y

    System.out.println("ySize is" + ySize);

    for (int y = 0; y < ySize; y++) {
      double sum = 0.0;
      for (int x = 0; x < xSize; x++) {
        double p1 = ptildeXY(x, y);
        sum = sum + p1;
      }
      if (Math.abs(ptildeY(y) - sum) > 0.00001) {
        System.out.println("Experiments error: for y=" + y + ", ptildeY(y)=" + ptildeY(y) + " but Sum_x ptildeXY(x,y)=" + sum);
      } else {
        System.out.println("Experiments: for y " + y + " Sum_x ptildeXY(x,y)=" + sum);
      }
    } // for y
  }


  public double ptildeX(int x) {
    if (x > xSize - 1) {
      return 0.0;
    }
    return px[x] / (double) vArray.length;
  }


  public double ptildeY(int y) {
    if (y > ySize - 1) {
      return 0.0;
    }
    return py[y] / (double) size();
  }

  public double ptildeXY(int x, int y) {
    for (int j = 0; j < dim; j++) {
      if (y == pxy[x][j] % ySize) {
        return (pxy[x][j] / ySize) / (double) size();
      }
    }
    return 0.0;
  }

  public int[] get(int index) {
    return vArray[index];
  }

  public int size() {
    return vArray.length;
  }

  public int getNumber() {
    return vArray.length;
  }

  public void print() {
    System.out.println(" Experiments : ");
    for (int i = 0; i < size(); i++) {
      System.out.println(vArray[i][0] + " : " + vArray[i][1]);
    }
    System.out.println(" p(x) ");
    for (int i = 0; i < xSize; i++) {
      System.out.println(i + " : " + ptildeX(i));
    }
    System.out.println(" p(y) ");
    for (int i = 0; i < ySize; i++) {
      System.out.println(i + " : " + ptildeY(i));
    }


  }

  public void print(PrintFile pf) {
    pf.println(" Experiments : ");
    for (int i = 0; i < size(); i++) {
      pf.println(vArray[i][0] + " : " + vArray[i][1]);
    }
    pf.println(" p(x) ");
    for (int i = 0; i < xSize; i++) {
      pf.println(i + " : " + ptildeX(i));
    }
    pf.println(" p(y) ");
    for (int i = 0; i < ySize; i++) {
      pf.println(i + " : " + ptildeY(i));
    }

  }


  public void save(String filename) {
    try {
      OutDataStreamFile rF = new OutDataStreamFile(filename);
      rF.writeInt(xSize);
      rF.writeInt(ySize);
      rF.writeInt(vArray.length);
      for (int i = 0; i < xSize; i++) {
        rF.writeInt(px[i]);
      }
      for (int j = 0; j < ySize; j++) {
        rF.writeInt(py[j]);
      }
      for (int i = 0; i < xSize; i++) {
        for (int j = 0; j < dim; j++) {
          rF.writeInt(pxy[i][j]);
        }
      }

      rF.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }


  public void read(String filename) {
    try {
      InDataStreamFile rF = new InDataStreamFile(filename);
      xSize = rF.readInt();
      ySize = rF.readInt();
      int number = rF.readInt();
      px = new int[xSize];
      py = new int[ySize];
      pxy = new int[xSize][ySize];
      for (int i = 0; i < xSize; i++) {
        px[i] = rF.readInt();
      }
      for (int j = 0; j < ySize; j++) {
        py[j] = rF.readInt();
      }
      for (int i = 0; i < xSize; i++) {
        for (int j = 0; j < dim; j++) {
          pxy[i][j] = rF.readInt();
        }
      }
      rF.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }


  /*
  public static void main(String[] args) {
    int[] hPos = {0, 1, 2, -1, -2};
    boolean[] isTag = {false, false, false, true, true};
    TaggerExperiments gophers = new TaggerExperiments("trainhuge.txt",null);
    gophers.ptilde();
  }
  */

}
