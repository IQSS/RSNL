import edu.stanford.nlp.ling.Sentence;
import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.ling.Word;
import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import edu.stanford.nlp.tagger.maxent.TaggerConfig;
import edu.stanford.nlp.process.Tokenizer;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.objectbank.TokenizerFactory;
import edu.stanford.nlp.process.WhitespaceTokenizer;

import edu.stanford.nlp.process.WordToSentenceProcessor;

import java.io.*;
import java.util.*;

class MaxentTaggerBridge {

  static ArrayList<String> tokens;
  static ArrayList<String> tags;

  public static void init(String filename, String props) throws Exception {
    String[] args = {"-model", filename, "-props", props};
    MaxentTagger.init(filename, new TaggerConfig(args));
  }

  public static String[] getTokens() {
    return((String[]) tokens.toArray(new String[0]));
  }

  public static String[] getTags() {
    return((String[]) tags.toArray(new String[0]));
  }

  // Tag a raw string.  This does the tokenization on the fly.
  public static void tagString (String text) {
    tokens = new ArrayList<String>();
    tags = new ArrayList<String>();

    List<Sentence<? extends HasWord>> sentences = 
      MaxentTagger.tokenizeText(new BufferedReader(new StringReader(text)));
    for (Sentence<? extends HasWord> sentence : sentences) {
      Sentence<TaggedWord> tSentence = MaxentTagger.tagSentence(sentence);
      for (int i = 0; i < tSentence.size(); i++) {
        tokens.add(tSentence.get(i).value());
        tags.add(tSentence.get(i).tag());
      }
    }
  }

  // Takes a sequence of tokens and applies the tagger.  Note that the
  // output may not match the input in length because eol tokens are
  // discarded.
  public static void tagTokens(String[] text) {
    tokens = new ArrayList<String>();
    tags = new ArrayList<String>();
    
    // Convert to the necessary type
    ArrayList<Word> inTokens = new ArrayList<Word>();
    for (int i = 0; i < text.length; ++i)
      inTokens.add(new Word(text[i]));

    // Break up sentences
    WordToSentenceProcessor wtsp = new WordToSentenceProcessor();
    List<List<Word>> sentences = wtsp.process(inTokens);

    for (List<Word> preSentence : sentences) {
      Sentence<Word> sentence = new Sentence(preSentence);
      Sentence<TaggedWord> tSentence = MaxentTagger.tagSentence(sentence);
      for (int i = 0; i < tSentence.size(); i++) {
        tokens.add(tSentence.get(i).value());
        tags.add(tSentence.get(i).tag());
      }
    }
  }

  public static void main (String args[]) throws Exception {
  }

}
