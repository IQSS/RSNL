import edu.stanford.nlp.ling.Sentence;
import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.ling.Word;
import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import edu.stanford.nlp.process.Tokenizer;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.objectbank.TokenizerFactory;
import edu.stanford.nlp.process.WhitespaceTokenizer;

import java.io.*;
import java.util.*;

/* A simple little interface to the stanford nlp group's Penn Tree
 * Bank Parser.
 */
class PTBTokenizerBridge {

  // Takes a string and returns an array of string tokens.
  public static String[] PTBtokenize (String input) {
    // Instantiate a PTBtokenizer
    TokenizerFactory tokFactory = PTBTokenizer.factory();
    Tokenizer tok = 
      tokFactory.getTokenizer(new BufferedReader(new StringReader(input)));

    // Get a list of tokenized
    List<Word> ttoks = (List<Word>) tok.tokenize();

    // We need an array for R, so copy over
    String[] toks = new String[ttoks.size()];
    for (int i = 0; i < ttoks.size(); i++)
      toks[i] = ttoks.get(i).value();

    return toks;
  }

  public static void main (String args[]) {
  }

}
