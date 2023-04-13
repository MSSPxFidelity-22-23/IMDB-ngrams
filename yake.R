yake <- function(word, text){

# get the casing
  ## function to return the number of times the word starts with a capital letter when it is not the beginning word of the sentence and the times when the word is in acronym form.
  count_capital_acronym <- function(word, texts) {
    
    # Initialize variables for counting
    count_capital = 0
    count_acronym = 0
    total_count = 0
    
    # Iterate over each text input
    for (text in texts) {
      
      # Split text into sentences
      sentences <- strsplit(text, "\\.\\s*")[[1]]
      
      # Iterate over each sentence
      for (sentence in sentences) {
        
        # Split sentence into words
        words <- strsplit(sentence, "\\s+")[[1]]
        
        # Iterate over each word
        for (i in 2:length(words)) {
          if (words[i] == word) {
            
            # Check if the word starts with a capital letter
            if (substr(words[i], 1, 1) == toupper(substr(words[i], 1, 1))) {
              count_capital <- count_capital + 1
            }
            
            # Check if the word is an acronym
            if (nchar(words[i]) > 1 && toupper(words[i]) == words[i]) {
              count_acronym <- count_acronym + 1
            }
          }
        }
      }
      
      # Count the total number of occurrences of the word
      total_count <- total_count + length(grep(word, text, ignore.case = TRUE))
    }
    
    # Calculate the final score
    max_count <- max(count_capital, count_acronym)
    score <- max_count / 1 + log(total_count)
    
    return(score)
  }
####
# count_capital_acronym(word = 'cat cat', text = c('Cat Cat Cat', 'cat dog dog','cat CAT CAT'))
####
casing <- count_capital_acronym(word = word, text = text)
  
# get the position
position_feature <- function(word, text) {
  
  # Find sentence positions containing word
  sen <- numeric()
  for (i in seq_along(text)) {
    sentences <- strsplit(text[i], "\\.\\s*")[[1]]
    for (j in seq_along(sentences)) {
      words <- strsplit(sentences[j], "\\s+")[[1]]
      if (length(words) > 1) {
        bigrams <- paste(words[-length(words)], words[-1], sep = " ")
        if (word %in% bigrams) {
          sen <- c(sen, j)
        }
      } else {
        if (word == words) {
          sen <- c(sen, j)
        }
      }
    }
  }
  
  # Calculate position feature
  if (length(sen) > 0) {
    pos <- log(log(3 + median(sen)))
  } else {
    pos <- 0
  }
  
  return(pos)
}



####
 position_feature(word = 'cat', text = c( 'cat cat cat.','cat dog dog.'))
####
position <- position_feature(text = text, word = word)

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