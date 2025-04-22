#' Match sentiment Words in a Text
#'
#' This function compares a vector of words from a text document to a vector
#' of sentiment words (positive or negative) and counts how many matches occur.
#' Supports words with prefix as a sentiment word.
#'
#' @param word_vector A character vector of words from the document.
#' @param sentiment_vector A character vector of sentiment words including prefix.
#'
#' @return An integer count of matching sentiment words.
#'
#' @examples
#' match_text(c("happy", "joyful", "sad", "sadly"), c("happy*", "sad*"))
#'
#' @export
match_text <- function(word_vector, sentiment_vector) {
  match_count <- 0 # initialize a match count

  for (sentiment_word in sentiment_vector) {
    if (endsWith(sentiment_word, "*")) # if sentiment word ends with '*', treat it as a prefix search
      {
      prefix <- sub("\\*$", "", sentiment_word) # remove the '*' to get the prefix
      matches <- sum(startsWith(word_vector, prefix)) # matches the prefix word
      match_count <- match_count + matches # updates match count
    } else {
      matches <- sum(word_vector == sentiment_word) # exact matching without prefix
      match_count <- match_count + matches# updates match count
    }
  }

  return(match_count) # returns the total match
}
