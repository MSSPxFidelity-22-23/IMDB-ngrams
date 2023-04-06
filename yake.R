yake <- function(w, text){

# get the casing
  ## function to return the number of times the word starts with a capital letter when it is not the beginning word of the sentence and the times when the word is in acronym form.
  count_caps_and_acronyms <- function(text) {
    words <- unlist(strsplit(text, "\\s+"))
    cap_count <- 0
    acronym_count <- 0
    for (i in 2:length(words)) {
      word <- words[i]
      if (nchar(word) > 1) {
        if (substr(word, 1, 1) == toupper(substr(word, 1, 1)) & 
            substr(words[i-1], nchar(words[i-1]), nchar(words[i-1])) != ".") {
          cap_count <- cap_count + 1
        }
        if (nchar(word) > 1 & toupper(word) == word) {
          acronym_count <- acronym_count + 1
        }
      }
    }
    max_count <- max(cap_count, acronym_count)
    return(max_count)
  }
  counts <- sapply(text, count_caps_and_acronyms)
  total_count <- sum(sapply(text, function(x) length(strsplit(x, "\\s+")[[1]])))
casing <- max(counts) / log(total_count)

# get the position
position_feature <- function(w,text) {
  # Tokenize the input text into sentences
  sentences <- sapply(text, function(x) {
    strsplit(x, "[.?!]+\\s*")[[1]]
  })
  
  # Find the positions of sentences where the word "w" occurs
  w_positions <- lapply(sentences, function(x) {
    which(sapply(x, function(y) grepl("\\bw\\b", y)))
  })
  
  # Compute the position feature for each text input
  position <- sapply(w_positions, function(x) {
    if (length(x) > 0) {
      log(log(3 + median(x)))
    } else {
      NA
    }
  })
  return(position)
}
 position <-  position_feature(w,text)
# get the word frequency 
  frequency_feature <- function(w, text) {
    # Tokenize the input text and create a vector of word counts
    word_counts <- sapply(text, function(x) {
      word_tokens <- strsplit(x, "[^[:alnum:]_']+")
      sum(sapply(word_tokens, function(y) sum(y == w, na.rm = TRUE)))
    })
    
    # Compute the frequency of the word and normalize by 1-standard deviation from the mean
    frequency <- sapply(word_counts, function(x) {
      x / (mean(word_counts) + sd(word_counts))
    })
    
    return(frequency)
  }
 frequency <- frequency_feature(w,text)
# get the word relatedness to context
  compute_relatedness <- function(w, text) {
    # combine all the texts into a single string
    text_list <- paste(text, collapse = " ")
    
    # calculate the frequency count for each word and bigram
    counts <- table(unlist(strsplit(text_list, "\\s+")))
    bigram_counts <- table(textsquad::ngrams(text, 2))
    
    # calculate the total number of words and bigrams
    total_words <- sum(counts)
    total_bigrams <- sum(bigram_counts)
    
    # calculate the number of unique words and bigrams on the left and right of w
    w_words <- unlist(strsplit(w, "\\s+"))
    left_words <- unlist(strsplit(substr(text_list, 1, regexpr(w_words[1], text_list) - 1), "\\s+"))
    right_words <- unlist(strsplit(substr(text_list, regexpr(w_words[length(w_words)], text_list) + nchar(w_words[length(w_words)])), "\\s+"))
    
    left_bigrams <- NULL
    right_bigrams <- NULL
    
    if(length(w_words) == 1) {
      left_bigrams <- unlist(strsplit(substr(text_list, 1, regexpr(w_words[1], text_list) - 1), "(?<=\\S{2})\\s+(?=\\S{2})"))
      right_bigrams <- unlist(strsplit(substr(text_list, regexpr(w_words[length(w_words)], text_list) + nchar(w_words[length(w_words)])), "(?<=\\S{2})\\s+(?=\\S{2})"))
    } else if(length(w_words) == 2) {
      left_bigrams <- unlist(strsplit(substr(text_list, 1, regexpr(w, text_list) - 1), "(?<=\\S{2})\\s+(?=\\S{2})"))
      right_bigrams <- unlist(strsplit(substr(text_list, regexpr(w, text_list) + nchar(w)), "(?<=\\S{2})\\s+(?=\\S{2})"))
    }
    
    unique_left_words <- length(unique(left_words))
    unique_right_words <- length(unique(right_words))
    unique_left_bigrams <- length(unique(left_bigrams))
    unique_right_bigrams <- length(unique(right_bigrams))
    
    # calculate the count of w
    w_count <- ifelse(length(w_words) == 1, counts[w_words], bigram_counts[w])
    
    # calculate the maximum count of any word or bigram
    max_count <- ifelse(length(w_words) == 1, max(counts), max(bigram_counts))
    
    # calculate the relatedness score
    WR <- (unique_right_words + unique_right_bigrams) / (total_words + total_bigrams - w_count)
    WL <- (unique_left_words + unique_left_bigrams) / (total_words + total_bigrams - w_count)
    PL <- (total_words + total_bigrams - w_count) / max_count
    PR <- (total_words + total_bigrams - w_count) / max_count
    
    relatedness <- 1 + (WR + WL) * w_count / max_count + PL + PR
    
    return(relatedness)
  }
  relatedness <- compute_relatedness(w,text)

# calculate Word Different Sentence
  # define the function to calculate different(w)
  different <- function(w, text) {
    # count the total number of sentences in the text
    total_sentences <- sum(unlist(lapply(text, function(x) str_count(x, "[.?!]"))))
    
    # count the number of sentences that contain the word/bigram w
    num_sentences <- sum(unlist(lapply(text, function(x) str_detect(x, regex(w, ignore_case = TRUE)))))
    
    # calculate the different(w) metric
    different <- num_sentences / total_sentences
    
    return(different)
  }
  different <- different(w,text)
# calculate the Combined Word Score
  score <- relatedness*position/(casing+frequency/relatedness+different/relatedness)


 return(w,score)
}