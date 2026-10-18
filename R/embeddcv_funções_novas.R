residualize <- function(X, C=NULL, method="lm", ...) {
  if(is.null(C)) {
    stop("No covariate was specified!")
  } else {
    Y <- as.matrix(X)
    # Linear residualization
    if(method == "lm") {
      return( residuals(lm(Y ~ ., data=as.data.frame(C))) )
      # Natural cubic spline residualization
    } else if(method == "ns") {
      require(splines)
      C_smooth <- C
      for (col in names(C_smooth)) {
        if (is.numeric(C_smooth[[col]])) {
          C_smooth[[col]] <- ns(C_smooth[[col]], ...)
        }
      }
      C_smooth <- do.call(data.frame, C_smooth)
      return( residuals(lm(Y ~ ., data=C_smooth)) )
    } else {
      stop("Unkwon residualization method")
    }
  }
}
source_signals <- function(X, f=1, C=NULL, ...) {
  ## Dimension Reduction
  require(ica)
  if(!is.null(C)) {
    ICA <- ica::ica(residualize(X, C, ...), f)
  } else {
    ICA <- ica::ica(X, f)
  }
  SourceSignals <- ICA$S
  if(!is.null(C)) {
    SourceSignals <- residualize(SourceSignals, C, ...)
  }
  colnames(SourceSignals) <- paste0("Dim",sprintf("%03d", 1:ncol(SourceSignals)))
  return(SourceSignals)
}
topic_modeling <- function(X, ...) {
  require(mclust)
  mixture <- mclust::Mclust(X, ...)
  mixture
}
tfidf_matrix <- function(x, TM, control=list(tolower=TRUE, removePunctuation=TRUE,
                                             removeNumbers=TRUE, stopwords=tm::stopwords(kind="pt"))) {
  require(tm)
  # Step 1: Create a Document-Term Matrix (DTM)
  corpus <- tm::Corpus(tm::VectorSource(x))
  dtm    <- tm::DocumentTermMatrix(corpus, control=control)
  # Step 2: Group the DTM by clusters
  cluster_dtm <- aggregate(as.matrix(dtm), by = list(cluster=TM), FUN=sum)
  rownames(cluster_dtm) <- cluster_dtm$cluster
  cluster_dtm <- cluster_dtm[, -1]  # Remove the cluster column
  # Step 3: Calculate c-TF-IDF
  # TF: Term frequency within each cluster
  tf <- cluster_dtm / rowSums(cluster_dtm)
  # IDF: Inverse document frequency
  # (log of total clusters divided by the number of clusters containing the term)
  idf <- log(nrow(cluster_dtm) / colSums(cluster_dtm > 0))
  # c-TF-IDF: Multiply TF by IDF
  c_tf_idf <- as.matrix(tf * idf)
  c_tf_idf[is.na(c_tf_idf)]       <- 0
  c_tf_idf[is.infinite(c_tf_idf)] <- 0
  return(t(c_tf_idf))
}
words_per_topic <- function(..., top_n_words=5) {
  c_tf_idf <- tfidf_matrix(...)
  # Extract top words for each cluster
  apply(c_tf_idf, 2, function(col) {
    head(rownames(c_tf_idf)[order(-col)], top_n_words)
  })
}