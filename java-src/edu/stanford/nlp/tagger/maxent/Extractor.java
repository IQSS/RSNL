/**
 * Title:        StanfordMaxEnt<p>
 * Description:  A Maximum Entropy Toolkit<p>
 * Copyright:    Copyright (c) Kristina Toutanova<p>
 * Company:      Stanford University<p>
 */

package edu.stanford.nlp.tagger.maxent;

import java.io.Serializable;
import java.util.regex.Pattern;
import java.util.regex.Matcher;


/**
 * This class serves as the base class for classes which extract relevant
 * information from a history to give it to the features. Every feature has
 * an associated extractor or maybe more.  GlobalHolder keeps all the
 * extractors; two histories are considered equal if all extractors return
 * equal values for them.  The main functionality of the Extractors is
 * provided by the method extract which takes a History as an argument.
 * The Extractor looks at the history and takes out something important for
 * the features - e.g. specific words and tags at specific positions or
 * some function of the History. The histories x are effectively vectors
 * of values, with each dimension being the output of some extractor.
 *
 * @author Kristina Toutanova
 * @version 1.0
 */
public class Extractor implements Serializable {

  private static final long serialVersionUID = -4694133872973560083L;

  static final String zeroSt = "0";

  private final int position;
  private final boolean isTag;

  public Extractor() {
    this(Integer.MAX_VALUE, false);
  }


  /**
   * This constructor creates an extractor which extracts either the tag or
   * the word from position position in the history.
   *
   * @param position The position of the thing to be extracted. This is
   *                 relative to the current word. For example, position 0
   *                 will be the current word, -1 will be
   *                 the word before +1 will be the word after, etc.
   * @param isTag    If true this means that the POS tag is extracted from
   *                 position, otherwise the word is extracted.
   */
  public Extractor(int position, boolean isTag) {
    this.position = position;
    this.isTag = isTag;
  }


  /** This evaluates any precondition for a feature being applicable based
   *  on a certain tag. It returns true if the feature is applicable.
   *  By default an Extractor is applicable everywhere, but some
   *  subclasses limit application.
   *
   *  @param tag The possible tag that the feature will be generated for
   *  @return Whether the feature extractor is applicable (true) or not (false)
   */
  @SuppressWarnings({"MethodMayBeStatic", "UnusedDeclaration"})
  public boolean precondition(String tag) {
    return true;
  }


  /**
   * @return the number of positions to the left the extractor looks at (only tags, because words are fixed.)
   */
  public int leftContext() {
    if (isTag) {
      if (position < 0) {
        return -position;
      }
    }

    return 0;
  }


  /**
   * @return the number of positions to the right the etxractor looks at (only tags, because words are fixed.)
   */
  public int rightContext() {
    if (isTag) {
      if (position > 0) {
        return position;
      }
    }

    return 0;
  }


  // CDM May 2007: This feature is currently never used. Maybe we should
  // change things so it is, and each feature template has a threshold, but
  // need to then work out what a TaggerFeature is and whether we should still
  // be using one of those to index with.
  // At present real threshold check happens in TaggerExperiments with
  // the populated(int, int) method.
  //  public boolean isPopulated(TaggerFeature f) {
  //    return (f.indexedValues.length > GlobalHolder.minFeatureThresh);
  //  }


  /** Subclasses should only override the two argument version
   *  of this method.
   *
   *  @param h The history to extract from
   *  @return The feature value
   */
  final String extract(History h) {
    return extract(h, GlobalHolder.pairs);
  }


  String extract(History h, PairsHolder pH) {
    return pH.get(h, position, isTag);
  }


  @SuppressWarnings({"MethodMayBeStatic"})
  String extractLV(History h, PairsHolder pH) {
    // should extract last verbal word and also the current word
    int start = h.start;
    String lastverb = "NA";
    int current = h.current;
    int index = current - 1;
    while (index >= start) {
      String tag = pH.get(index, true);
      if (tag.startsWith("VB")) {
        lastverb = pH.get(index, false);
        break;
      }
      if (tag.startsWith(",")) {
        break;
      }
      index--;
    }
    return lastverb;
  }


  String extractLV(History h, PairsHolder pH, int bound) {
    // should extract last verbal word and also the current word
    int start = h.start;
    String lastverb = "NA";
    int current = h.current;
    int index = current - 1;
    while ((index >= start) && (index >= current - bound)) {
      String tag = pH.get(index, true);
      if (tag.startsWith("VB")) {
        lastverb = pH.get(index, false);
        break;
      }
      if (tag.startsWith(",")) {
        break;
      }
      index--;
    }
    return lastverb;
  }


  /** By default the bound is ignored, but a few subclasses make use of it.
   */
  @SuppressWarnings({"UnusedDeclaration"})
  String extract(History h, PairsHolder pH, int bound) {
    return extract(h, pH);
  }


  @Override
  public String toString() {
    String cl = getClass().getName();
    int ind = cl.lastIndexOf('.');
    // MAX_VALUE is the default value and means we aren't using these two arguments
    String args = (position == Integer.MAX_VALUE) ? "": (position + "," + (isTag ? "tag" : "word"));
    return cl.substring(ind + 1) + '(' + args + ')';
  }


  /** This is used for argument parsing in arch variable.
   *
   *  @param str arch variable component input
   *  @return The value of the parenthesized integer, or 0 if none.
   */
  static int getParenthesizedNum(String str) {
    Pattern argP = Pattern.compile("\\(([0-9]+)\\)");
    int ans = 0;
    Matcher m = argP.matcher(str);
    if (m.find()) {
      String bit = m.group(1);
      try {
        ans = Integer.parseInt(bit);
      } catch (NumberFormatException nfe) {
        // just leave ans as 0
      }
    }
    return ans;
  }

  /** This is used for argument parsing in arch variable.
   *
   *  @param str arch variable component input
   *  @return The parenthesized String, or null if none.
   */
  static String getParenthesizedString(String str) {
    Pattern argP = Pattern.compile("\\(([^)]+)\\)");
    Matcher m = argP.matcher(str);
    if (m.find()) {
      return m.group(1);
    } else {
      return null;
    }
  }

}
