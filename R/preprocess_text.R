#' Preprocess Text
#'
#' This function reads a text document, converts it to lowercase, removes punctuation, and splits it into a vector of cleaned words.
#'
#' @param file_path Path to the text file (TXT).
#' @return A vector of cleaned and processed words.
#' @examples
#' words <- preprocess_text("example.txt")
#' head(words)
preprocess_text <- function(file_path) {
  text_file <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE, fill = TRUE)
  text_lower <- tolower(text_file$V1)
  text_clean <- gsub("[[:punct:]]", "", text_lower)
  word_vector <- unlist(strsplit(text_clean, split = " "))
  word_vector <- word_vector[word_vector != ""]
  return(word_vector)
}
