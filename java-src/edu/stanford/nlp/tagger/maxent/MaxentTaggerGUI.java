// MaxentTaggerGUI -- StanfordMaxEnt, A Maximum Entropy Toolkit
// Copyright (c) 2002-2008 Leland Stanford Junior University
//
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// For more information, bug reports, fixes, contact:
//    Christopher Manning
//    Dept of Computer Science, Gates 1A
//    Stanford CA 94305-9010
//    USA
//    Support/Questions: java-nlp-user@lists.stanford.edu
//    Licensing: java-nlp-support@lists.stanford.edu
//    http://www-nlp.stanford.edu/software/tagger.shtml
package edu.stanford.nlp.tagger.maxent;

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/** A very simple GUI for displaying the POS tagger tagging text.
 *
 * Simple usage:
 * <code>java -mx500m edu.stanford.nlp.tagger.maxent.MaxentTaggerGUI <path to POS tagger model></code>
 * <p>
 *  <i>Note:</i> Could still use some more work, but probably a reasonable demo as of 16 Jan 08 (Anna).
 * @author Kristina Toutanova
 * @author Anna Rafferty (improvements on original gui)
 * @version 1.0
 */
@SuppressWarnings("serial")
public class MaxentTaggerGUI extends JFrame {

  private JTextArea inputBox = new JTextArea();
  private JTextArea outputBox = new JTextArea();
  private JButton tagButton = new JButton();
  //private PrintFile pf;

  public MaxentTaggerGUI() {
    super("Maximum Entropy Part of Speech Tagger");
    try {
      jbInit();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static void main(final String[] args) {

    Thread t = new Thread() {
      @Override
      public void run() {
        String file;

        try {
          if (args.length > 0) {
            file = args[args.length - 1];
          } else {
            file = MaxentTagger.DEFAULT_DISTRIBUTION_PATH;
          }
          GlobalHolder.readModelAndInit(file);
        } catch (Exception e) {
          try {
            file = MaxentTagger.DEFAULT_NLP_GROUP_MODEL_PATH;
            GlobalHolder.readModelAndInit(file);
          } catch (Exception e2) {
            e.printStackTrace();
            return;
          }
          e.printStackTrace();
        }
      }
    };
    t.start();


    MaxentTaggerGUI mainFrame1 = new MaxentTaggerGUI();
    mainFrame1.setPreferredSize(new Dimension(400, 200));
    mainFrame1.pack();
    mainFrame1.setVisible(true);
  }

  private void jbInit() throws Exception {
    //pf = new PrintFile("out");
    this.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });


    //Set up the input/output fields and let them scroll.
    inputBox.setLineWrap(true);
    inputBox.setWrapStyleWord(true);
    outputBox.setLineWrap(true);
    outputBox.setWrapStyleWord(true);
    outputBox.setEditable(false);
    JScrollPane scroll1 = new JScrollPane(inputBox);
    JScrollPane scroll2 = new JScrollPane(outputBox);
    scroll1.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Type a sentence to tag: "));
    scroll2.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Tagged sentence:"));

    //Set up the button for starting tagging
    JPanel buttonPanel = new JPanel();
    buttonPanel.setBackground(Color.WHITE);
    buttonPanel.applyComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
    FlowLayout fl = new FlowLayout();
    fl.setAlignment(FlowLayout.RIGHT);
    buttonPanel.setLayout(fl);
    tagButton.setText("Show Tagged");
    tagButton.setBackground(Color.WHITE);
    buttonPanel.add(tagButton);

    tagButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        performTagAction(e);
      }
    });


    //Lay it all out
    this.setLayout(new GridBagLayout());
    GridBagConstraints c = new GridBagConstraints();
    c.fill = GridBagConstraints.BOTH;
    c.gridwidth = GridBagConstraints.REMAINDER;
    c.weightx = 4.0;
    c.weighty = 4.0;

    this.add(scroll1, c);
    c.weighty = 1.0;
    this.add(buttonPanel, c);
    c.weighty = 4.0;
    c.gridheight = GridBagConstraints.REMAINDER;
    this.add(scroll2, c);
  }


  private void performTagAction(ActionEvent e) {
    final String s = inputBox.getText();
    Thread t = new Thread() {
      @Override
      public void run() {
        TestSentence ts = new TestSentence(GlobalHolder.getLambdaSolve(), s);
        final String s1 = ts.getTaggedNice();
        SwingUtilities.invokeLater(new Runnable() {
          public void run() {
            outputBox.setText(s1);
          }
        });
      }
    };
    t.start();
  }
}
