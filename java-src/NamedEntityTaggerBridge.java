import edu.stanford.nlp.ie.crf.*;
import edu.stanford.nlp.ie.AbstractSequenceClassifier;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations.AnswerAnnotation;
import edu.stanford.nlp.util.StringUtils;

import edu.stanford.nlp.ling.Sentence;
import edu.stanford.nlp.ling.TaggedWord;
import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.ling.Word;
import edu.stanford.nlp.tagger.maxent.MaxentTagger;
import edu.stanford.nlp.process.WhitespaceTokenizer;

import edu.stanford.nlp.process.WordToSentenceProcessor;


import java.io.*;
import java.util.*;

class   NamedEntityTaggerBridge {

  AbstractSequenceClassifier classifier;
  ArrayList<String> tokens;
  ArrayList<String> tags;

  public NamedEntityTaggerBridge (String filename) throws Exception {
    classifier = 
      CRFClassifier.getClassifierNoExceptions(filename);
  }

  public String[] getTokens() {
    return((String[]) tokens.toArray(new String[0]));
  }

  public String[] getTags() {
    return((String[]) tags.toArray(new String[0]));
  }

  // Tag a raw string.  This does the tokenization on the fly.
  public void tagString (String text) {
    this.tokens = new ArrayList<String>();
    this.tags = new ArrayList<String>();

    List<List<CoreLabel>> out = this.classifier.classify(text);
    for (List<CoreLabel> sentence : out) {
      for (CoreLabel word : sentence) {
        this.tokens.add(word.word());
        this.tags.add(word.get(AnswerAnnotation.class));
      }
    }
  }

  // Takes a sequence of tokens and applies the tagger.  Note that the
  // output may not match the input in length because eol tokens are
  // discarded.
  public void tagTokens(String[] text) {
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

      List<CoreLabel> tSentence = 
        this.classifier.classifySentence(sentence);
      for (CoreLabel word : tSentence) {
        this.tokens.add(word.word());
        this.tags.add(word.get(AnswerAnnotation.class));
      }
    }
  }

  public static void main (String args[]) throws Exception {
  }

}
