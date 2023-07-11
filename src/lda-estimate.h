// (C) Copyright 2004, David M. Blei (blei [at] cs [dot] cmu [dot] edu)

// This file is part of LDA-C.

// LDA-C is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your
// option) any later version.

// LDA-C is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
//
//

#ifndef LDA_ESTIMATE_H
#define LDA_ESTIMATE_H

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <time.h>
#include <Rinternals.h>

#include "lda.h"
#include "lda-data.h"
#include "lda-inference.h"
#include "lda-model.h"
#include "lda-alpha.h"
#include "utils.h"
#include "cokus.h"

int LAG = 5;

float EM_CONVERGED;
int EM_MAX_ITER;
int ESTIMATE_ALPHA;
double INITIAL_ALPHA;
int NTOPICS;

/*
 * perform inference on a document and update sufficient statistics
 *
 */

 double doc_e_step(document* doc, double* gamma, double** phi,
                  lda_model* model, lda_suffstats* ss)
{
    double likelihood;
    int n, k;

    // posterior inference

    likelihood = lda_inference(doc, model, gamma, phi);

    // update sufficient statistics

    double gamma_sum = 0;
    for (k = 0; k < model->num_topics; k++)
    {
        gamma_sum += gamma[k];
        ss->alpha_suffstats += digamma(gamma[k]);
    }
    ss->alpha_suffstats -= model->num_topics * digamma(gamma_sum);

    for (n = 0; n < doc->length; n++)
    {
        for (k = 0; k < model->num_topics; k++)
        {
            ss->class_word[k][doc->words[n]] += doc->counts[n]*phi[n][k];
            ss->class_total[k] += doc->counts[n]*phi[n][k];
        }
    }

    ss->num_docs = ss->num_docs + 1;

    return(likelihood);
}

/*
 * run_em
 *
 */

model_wgamma run_em(char* start, corpus* corpus)
{

    int d, n;
    lda_model *model = NULL;
    double **var_gamma, **phi;
    model_wgamma result;

    // allocate variational parameters

    var_gamma = malloc(sizeof(double*)*(corpus->num_docs));
    for (d = 0; d < corpus->num_docs; d++)
      var_gamma[d] = malloc(sizeof(double) * NTOPICS);

    int max_length = max_corpus_length(corpus);
    phi = malloc(sizeof(double*)*max_length);
    for (n = 0; n < max_length; n++)
      phi[n] = malloc(sizeof(double) * NTOPICS);

    // initialize model

    lda_suffstats* ss = NULL;
    if (strcmp(start, "seeded")==0)
    {
        model = new_lda_model(corpus->num_terms, NTOPICS);
        ss = new_lda_suffstats(model);
        corpus_initialize_ss(ss, model, corpus);
        lda_mle(model, ss, 0);
        model->alpha = INITIAL_ALPHA;
    }
    else if (strcmp(start, "random")==0)
    {
        model = new_lda_model(corpus->num_terms, NTOPICS);
        ss = new_lda_suffstats(model);
        random_initialize_ss(ss, model);
        lda_mle(model, ss, 0);
        model->alpha = INITIAL_ALPHA;
    }
    else
    {
        model = load_lda_model(start);
        ss = new_lda_suffstats(model);
    }

    // run expectation maximization

    int i = 0;
    double likelihood, likelihood_old = 0, converged = 1;

    while (((converged < 0) || (converged > EM_CONVERGED) || (i <= 2)) && (i <= EM_MAX_ITER))
    {
        i++;
        Rprintf("EM Iteration %d", i);
        likelihood = 0;
        zero_initialize_ss(ss, model);

        // e-step

        for (d = 0; d < corpus->num_docs; d++)
        {
            likelihood += doc_e_step(&(corpus->docs[d]),
                                     var_gamma[d],
                                     phi,
                                     model,
                                     ss);
        }
        Rprintf(", Likelihood: %f\n", likelihood);

        // m-step

        lda_mle(model, ss, ESTIMATE_ALPHA);

        // check for convergence

        converged = (likelihood_old - likelihood) / (likelihood_old);
        if (converged < 0) VAR_MAX_ITER = VAR_MAX_ITER * 2;
        likelihood_old = likelihood;
    }

    // Free stuff we don't need any more
    free(phi);

    // output the final model

    result.model = model;
    result.gamma = var_gamma;
    return result;
}


/*
 * inference only
 *
 */

double** infer(lda_model* model, corpus* corpus)
{
    int i, d, n;
    double **var_gamma, likelihood, **phi;
    document* doc;

    var_gamma = malloc(sizeof(double*)*(corpus->num_docs));
    for (i = 0; i < corpus->num_docs; i++)
	    var_gamma[i] = malloc(sizeof(double)*model->num_topics);
    for (d = 0; d < corpus->num_docs; d++)
    {
      if (((d % 100) == 0) && (d>0)) printf("document %d\n",d);

      doc = &(corpus->docs[d]);
      phi = (double**) malloc(sizeof(double*) * doc->length);
      for (n = 0; n < doc->length; n++)
          phi[n] = (double*) malloc(sizeof(double) * model->num_topics);
      likelihood = lda_inference(doc, model, var_gamma[d], phi);

    }

    return var_gamma;
}

// Expects various parts of the sparse matrix representing a
// document-term matrix.
// counts - vector of counts (dtm$v)
// si - doc indices of counts (dtm$i)
// sj - term indices of counts (dtm$j)
// ndocs - total number of docs in matrix (dtm$nrow)
// nterms - vector of terms/doc (sapply(1:ndocs, function(v) sum(dtm$i==v))
// tterms - total number of terms in matrix (dtm$ncol)
 corpus* tdm2corpus (SEXP counts, SEXP si, SEXP sj, SEXP ndocs,
                           SEXP nterms, SEXP tterms)
{
  int length, count, doc, word, i;
  corpus* c;
  int* curword;

  c = malloc(sizeof(corpus));
  c->num_terms = INTEGER(tterms)[0];
  c->num_docs = INTEGER(ndocs)[0];
  c->docs = malloc(sizeof(document)*c->num_docs);
  curword = malloc(sizeof(int) * c->num_docs);

  // Allocate space
  for (i = 0; i < c->num_docs; i++) {
    length = INTEGER(nterms)[i];
    c->docs[i].length = length;
    c->docs[i].words = malloc(sizeof(int)*length);
    c->docs[i].counts = malloc(sizeof(int)*length);
    c->docs[i].total = 0;
    curword[i] = 0;
  }

  // Fill in values
  for (i = 0; i < LENGTH(counts); i++) {
    doc = INTEGER(si)[i] - 1;
    word = INTEGER(sj)[i] - 1;
    count = INTEGER(counts)[i];

    c->docs[doc].words[curword[doc]] = word;
    c->docs[doc].counts[curword[doc]] = count;
    c->docs[doc].total += count;
    curword[doc]++;
  }

  free(curword);
      
  return c;
}

void examine_corpus (corpus* corpus)
{
  int i, j;

  for (i = 0; i < corpus->num_docs; ++i) {
    Rprintf("Document %d - %d:%d\n", i + 1, 
        corpus->docs[i].length, corpus->docs[i].total);
    for (j = 0; j < corpus->docs[i].length; ++j) {
      Rprintf("  Word %d - %d:%d\n", j, 
          corpus->docs[i].words[j], corpus->docs[i].counts[j]);
    }
  }
}


void free_corpus (corpus* corpus)
{
  free(corpus->docs);
  free(corpus);
}

 void free_gamma (double** gamma, int docs)
{
  int i;
  
  for (i = 0; i < docs; i++)
    free(gamma[i]);
  free(gamma);
}


void lda_settings (SEXP var_max_iter, SEXP var_converged, SEXP em_max_iter,
                   SEXP em_converged, SEXP estimate_alpha)
{
  VAR_MAX_ITER = INTEGER(var_max_iter)[0];
  VAR_CONVERGED = REAL(var_converged)[0];
  EM_MAX_ITER = INTEGER(em_max_iter)[0];
  EM_CONVERGED = REAL(em_converged)[0];
  ESTIMATE_ALPHA = INTEGER(estimate_alpha)[0];
}


SEXP infer_lda (SEXP counts, SEXP si, SEXP sj, SEXP ndocs, SEXP nterms,
                SEXP tterms, SEXP alpha, SEXP beta, SEXP mtopics,
                SEXP var_max_iter, SEXP var_converged, SEXP em_max_iter,
                SEXP em_converged, SEXP estimate_alpha, SEXP r_model)
{
  lda_model* model;
  corpus* corpus;
  int i, j, cnt, docs;
  double** var_gamma;
  SEXP gamma;

  // setup model
  model = new_lda_model(INTEGER(tterms)[0], INTEGER(mtopics)[0]);
  model->alpha = REAL(alpha)[0];

  cnt = 0;
  for (j = 0; j < model->num_terms; j++)
    for (i = 0; i < model->num_topics; i++)
      model->log_prob_w[i][j] = REAL(beta)[cnt++];

  // fill in corpus object
  corpus = tdm2corpus(counts, si, sj, ndocs, nterms, tterms);

  // do the inference
  var_gamma = infer(model, corpus);

  // Copy over to R object
  docs = INTEGER(ndocs)[0];
  PROTECT(gamma = allocVector(REALSXP, model->num_topics * docs));
  cnt = 0;
  for (j = 0; j < model->num_topics; j++)
    for (i = 0; i < docs; i++)
      REAL(gamma)[cnt++] = var_gamma[i][j];

  free_lda_model(model);
  free_corpus(corpus);
  free_gamma(var_gamma, docs);

  UNPROTECT(1);
  return gamma;
}

 
SEXP estimate_lda (SEXP counts, SEXP si, SEXP sj, SEXP ndocs, SEXP nterms,
                   SEXP tterms, SEXP alpha, SEXP k, SEXP var_max_iter,
                   SEXP var_converged, SEXP em_max_iter, SEXP em_converged,
                   SEXP estimate_alpha)
{
  model_wgamma fitted;
  corpus* c;
  SEXP beta, gamma, result, final_alpha, num_terms, num_topics;
  int docs, i, j, cnt;
 
  // Construct the corpus struct
  c = tdm2corpus(counts, si, sj, ndocs, nterms, tterms);
  
  // Init settings
  INITIAL_ALPHA = REAL(alpha)[0];
  NTOPICS = INTEGER(k)[0];
  lda_settings(var_max_iter, var_converged, em_max_iter,
      em_converged, estimate_alpha);
 
  // Fit model
  fitted = run_em("random",c);

  // Extract and package results
  PROTECT(result = allocVector(VECSXP, 5));
  docs = INTEGER(ndocs)[0];

  PROTECT(beta = allocVector(REALSXP, NTOPICS * fitted.model->num_terms));
  cnt = 0;
  for (j = 0; j < fitted.model->num_terms; j++)
    for (i = 0; i < NTOPICS; i++)
      REAL(beta)[cnt++] = fitted.model->log_prob_w[i][j];

  PROTECT(gamma = allocVector(REALSXP, NTOPICS*docs));
  cnt = 0;
  for (j = 0; j < NTOPICS; j++)
    for (i = 0; i < docs; i++)
      REAL(gamma)[cnt++] = fitted.gamma[i][j];

  PROTECT(num_terms = allocVector(INTSXP, 1));
  INTEGER(num_terms)[0] = fitted.model->num_terms;

  PROTECT(num_topics = allocVector(INTSXP, 1));
  INTEGER(num_topics)[0] = fitted.model->num_topics;

  PROTECT(final_alpha = allocVector(REALSXP, 1));
  REAL(final_alpha)[0] = fitted.model->alpha;

  SET_VECTOR_ELT(result, 0, num_terms);
  SET_VECTOR_ELT(result, 1, num_topics);
  SET_VECTOR_ELT(result, 2, final_alpha);
  SET_VECTOR_ELT(result, 3, beta);
  SET_VECTOR_ELT(result, 4, gamma);

  free_lda_model(fitted.model);
  free_corpus(c);
  free_gamma(fitted.gamma, docs);

  UNPROTECT(6);

  return result;
}

#endif
