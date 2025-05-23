% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_text.R
\name{summarize_text}
\alias{summarize_text}
\title{Summarize Sentiment in a Text}
\usage{
summarize_text(word, positive, negative)
}
\arguments{
\item{word}{A character vector containing all the words from the document.}

\item{positive}{A character vector of positive sentiment words or prefixes ending with a '*'}

\item{negative}{A character vector of negative sentiment words or prefixes ending with a '*'}
}
\value{
A list containing:
\describe{
  \item{positive}{The total count of positive words matched.}
  \item{negative}{The total count of negative words matched.}
  \item{ratio}{The ratio of positive to negative word matches. If no negative
  words are found, the ratio is set to Inf (infinity).}
}
}
\description{
This function analyzes the sentiment of a text by comparing the words
in a text vector to vectors of positive and negative sentiment words.
It counts how many positive and negative words are present, and
calculates the ratio of positive to negative words.
}
\examples{
word_vector <- c("happy", "sad", "sadly", "joyful", "love", "hate")
positive_words <- c("happy*", "joy*", "love")
negative_words <- c("sad*", "hate")

summarize_text(word_vector, positive_words, negative_words)

}
