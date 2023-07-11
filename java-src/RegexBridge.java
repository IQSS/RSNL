import java.io.*;
import java.util.*;
import java.util.regex.*;

class RegexBridge {

  // Takes a pattern and a string and returns an array of all the
  // matches of the pattern in the string
  public static String[] findall (Pattern pat, String str) {
    ArrayList<String> tokens = new ArrayList<String>();

    Matcher mat = pat.matcher(str);
    while (mat.find())
      tokens.add(mat.group());

    return((String[]) tokens.toArray(new String[0]));
  }

  /* Find for multiple input strings */
  public static boolean[] matchesPart (Pattern pat, String[] input) {
    boolean[] res = new boolean[input.length];
    Matcher matcher = pat.matcher("");
    for (int i = 0; i < input.length; ++i) {
      matcher.reset(input[i]);
      res[i] = matcher.find();
    }
    return res;
  }

  public static String[] sub (Pattern pat, String[] input, String[] repl) {
    String[] res = new String[input.length];
    Matcher matcher = pat.matcher("");
    for (int i = 0; i < input.length; ++i) {
      matcher.reset(input[i]);
      res[i] = matcher.replaceAll(repl[i]);
    }
    return res;
  }

  public static String[] sub (Pattern pat, String[] input, String repl) {
    String[] res = new String[input.length];
    Matcher matcher = pat.matcher("");
    for (int i = 0; i < input.length; ++i) {
      matcher.reset(input[i]);
      res[i] = matcher.replaceAll(repl);
    }
    return res;
  }

  public static void main (String args[]) {
  }


}


