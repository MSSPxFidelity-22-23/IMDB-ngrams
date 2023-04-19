
kwselect <- function(word_list, text, threshold){
  library(stringr)
  library(tokenizers)
  library(tibble)
  library(tidyverse)
  
yake <- function(word_list, text){


# function to get the score of a single word
score <- function(word, text){
# 1. get the casing
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
    casing <- max_count / 1 + log(total_count)
    
    return(casing)
  }
####
# count_capital_acronym(word = 'cat cat', text = c('Cat Cat Cat', 'cat dog dog','cat CAT CAT'))
####
  

casing <- count_capital_acronym(word = word, text = text)
  
# 2. get the position
position_feature <- function(word, text) {
  
  # Split the text into sentences
  sentences <- strsplit(text, "[.?!]")[[1]]
  
  # Find the positions of the bigram 'word' in the sentences
  sen_w <- lapply(sentences, function(s) {
    which(str_detect(s, paste0("\\b", word, "\\b")))
  })
  
  # Flatten the list of sentence positions into a single vector
  sen_w <- unlist(sen_w)
  
  # Calculate the median position of the bigram 'word' in the sentences
  median_pos <- median(sen_w)
  
  # Calculate the position feature of the bigram 'word'
  position_w <- log(log(3 + median_pos))
  
  return(position_w)
}




####
 # position_feature(word = 'cat cat', text = c('cat cat Cat', 'cat dog dog','CAT cat cat'))
####
position <- position_feature(word = word, text = text)

# 3. get the word frequency 
frequency_feature <- function(word, text) {
  
  # Calculate the frequency of the bigram 'word' in the text
  freq_w <- sum(str_count(text, paste0("\\b", word, "\\b")))
  
  # Split the text into individual words
  words <- str_split(text, "\\s+")[[1]]
  
  # Calculate the mean and standard deviation of word counts
  mean_count <- mean(table(words))
  sd_count <- sd(table(words))
  
  # Calculate the frequency feature of the bigram 'word'
  frequency_w <- freq_w / (mean_count + sd_count)
  
  return(frequency_w)
}
####
  # frequency_feature(word = 'cat cat', text = c('cat cat Cat', 'cat dog dog','CAT cat cat'))
####
 frequency <- frequency_feature(w = word,text = text)

# 4. get the word relatedness to context
 relatedness <- function(word, text){
   # split text into words
   words <- unlist(strsplit(tolower(paste(text, collapse = " ")), "\\W+"))
   
   # create bigrams
   bigrams <- paste(words[-length(words)], words[-1], sep = " ")
   
   # count occurrences of each bigram
   counts <- table(bigrams)
   
   # check if the word is present in the bigrams
   if (!(word %in% bigrams)) {
     return(0)
   }
   
   # compute WR, WL, PL, PR
   right_words <- words[which(bigrams == word) + 1]
   left_words <- words[which(bigrams == word) - 1]
   WR <- length(unique(right_words)) / length(right_words)
   WL <- length(unique(left_words)) / length(left_words)
   PL <- length(left_words) / max(counts)
   PR <- length(right_words) / max(counts)
   
   # compute relatedness
   max_count <- max(counts)
   w_count <- counts[word]
   rel <- 1 + (WR + WL) * w_count / max_count + PL + PR
   
   return(as.numeric(rel))
 }
 
 
 
 
 ####
 # relatedness(word = 'cat cat', text = c('cat cat Cat CAT', 'cat dog dog dog','CAT CAT cat cat'))
 ####
  relatedness <- relatedness(word = word,text = text)

# 5. calculate Word Different Sentence
  # define the function to calculate different(w)
  different <- function(word, text) {
    # Split the text into sentences
    sentences <- unlist(strsplit(text, "[.!?]"))
    
    # Count the number of sentences that contain the word
    num_sentences <- sum(sapply(sentences, function(s) grepl(paste0("\\b", word, "\\b"), s)))
    
    # Calculate the different score
    different_score <- num_sentences / length(sentences)
    
    return(different_score)
  }
  
  
  ####
  # different(word = 'cat cat', text = c('cat cat Cat CAT', 'cat dog dog dog','CAT CAT cat cat'))
  ####
  different <- different(word = word,text = text)

# calculate the Combined Word Score
  score <- relatedness*position/(casing+frequency/relatedness+different/relatedness)

  # count of the word

  
  count_bigram <- function(word, text) {
    bigrams <- unlist(tokenize_ngrams(text, n = 2))
    sum(bigrams == word)
  }
  total_counts <- count_bigram(word = word,text = text)
 return(tibble(word,score,total_counts))
}

## testing
score(word = "cat cat", text = c('cat cat Cat CAT', 'cat dog dog dog','CAT CAT cat cat'))
# word_list = c("cat cat", "dog dog")

# Apply the score function to each word in the word list
results_list <- lapply(word_list, score, text=text)

# Combine the resulting tibbles into a single data frame
score_list <- do.call(rbind, results_list)


# adding the keyword score in to the tibble
score_list <- score_list %>% mutate(
  ks = prod(score_list$score)/(1 + sum(score_list$score)*total_counts)
)
return(score_list)
}

#### testing
# yake(word_list = c("cat cat", "dog dog"), text = c("cat cat dog dog", 'cat dog dog cat', 'dog cat cat dog'))
####
candidate <- subset(yake(word_list = word_list, text = text) , ks < threshold)

return(data.frame(candidate$word, candidate$ks))
}


## testing
kwselect(word_list = c("cat cat", "dog dog"), text = c("cat cat dog dog", 'cat dog dog cat', 'dog cat cat dog'), threshold = .1)
