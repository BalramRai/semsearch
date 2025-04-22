#' Summarize Sentiment in a Text
#'
#' This function analyzes the sentiment of a text by comparing the words
#' in a text vector to vectors of positive and negative sentiment words.
#' It counts how many positive and negative words are present, and
#' calculates the ratio of positive to negative words.
#'
#' @param word A character vector containing all the words from the document.
#' @param positive A character vector of positive sentiment words or prefixes
#' @param negative A character vector of negative sentiment words or prefixes
#'
#' @return A list containing:
#' \describe{
#'   \item{positive}{The total count of positive words matched.}
#'   \item{negative}{The total count of negative words matched.}
#'   \item{ratio}{The ratio of positive to negative word matches. If no negative
#'   words are found, the ratio is set to Inf (infinity).}
#' }
#'
#' @examples
#' word_vector <- c("happy", "sad", "sadly", "joyful", "love", "hate")
#' positive_words <- c("happy*", "joy*", "love")
#' negative_words <- c("sad*", "hate")
#'
#' summarize_text(word_vector, positive_words, negative_words)
#'
#' @export
summarize_text <- function(word, positive, negative) {

  positive_count <- match_text(word, positive) # counts positive words

  negative_count <- match_text(word_vector, negative) # counts negative words

  total_words <- length(word) # counts total words

  if (negative_count == 0) {
    ratio <- Inf # handles the denominator to be 0
  } else {
    ratio <- positive_count / negative_count # positive/negative ratio
  }

  result <- list(
    total = total_words,
    positive = positive_count,
    negative = negative_count,
    ratio = ratio
  )

  return(result) # returns a list of summaries
}
