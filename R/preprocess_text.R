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
  text_file <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE, fill = TRUE) # read a text file and handles unequal rows
  text_lower <- tolower(text_file$V1) # convert all the text to lowercase letters
  text_clean <- gsub("[[:punct:]]", "", text_lower) # remove punctuations using POXIS character class [:punct:] including ',',':',';', etc
  word_vector <- unlist(strsplit(text_clean, split = " ")) # split the string into a vector of words using single space delimiter
  word_vector <- word_vector[word_vector != ""] # removes the empty strings if any created
  return(word_vector)
}
