setwd(here::here())
rm(list = ls())



######## Read the text file

a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
          fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("


######## Pre-Processing (split punctuation mark with words)

split_punct <- function(text_vector, punctuation){

  punctuation_indices <- grep(punctuation, text_vector, fixed = TRUE)

  new_vector_length <- length(text_vector) + length(punctuation_indices)
  new_vector <- vector("character", new_vector_length)

  last_index <- 1
  start_index <- 1

  for (index in punctuation_indices) {

    new_vector[last_index:(last_index + (index - start_index))] <- text_vector[start_index:index]
    last_index <- last_index + (index - start_index + 1)
    new_vector[last_index] <- NA
    last_index <- last_index + 1
    start_index <- index + 1
  }

  if (start_index <= length(text_vector)) {
    new_vector[last_index:(last_index + (length(text_vector) - start_index + 1) - 1)] <- text_vector[start_index:length(text_vector)]
  }

  punct_indices_1 <- grep(punctuation, new_vector, fixed = TRUE)
  new_vector <- gsub(punctuation, "", new_vector, fixed=TRUE)
  new_vector[punct_indices_1 + 1] <- punctuation

  return(new_vector)
}

punctuation_vector <- c(",", ".", ";", "!", ":", "?")

a1 <- a

for (punc in punctuation_vector){
  a1 <- split_punct(a1, punc)
}

# saveRDS(a1, file = "word_vector1.rds")
# a1 <- readRDS("word_vector1.rds")



######## Find Top 1000 words of occurrences

a1 <- tolower(a1)

uni <- unique(a1)

cal_occurrences <- match(a1, uni)

occurrences <- tabulate(cal_occurrences)

freq <- 26

b <- uni[which(occurrences > freq)]    # size of b = 1005

length(b)

occurrences_b <- occurrences[which(occurrences > freq)]


######## Make the matrices of common word token sequences

tmp <- match(a1, b)

mlag <- 4
n <- length(a1)

M <- matrix(NA, nrow = (n - mlag), ncol = (mlag + 1))

for (i in 1:(n - mlag)) {
  M[i, ] <- tmp[i:(mlag + i)]
}


######## simulate nw-word sections from my model

nw = 50

M[is.na(M)] <- 0

indices <- grepl("^[A-Za-z]+$", a1)
words <- a1[indices]
freqs <- tmp[indices]
freqs[is.na(freqs)] <- 0

set.seed(2)
start_word <- sample(words, 1, prob = freqs)

sentence <- start_word

for (i in 2:nw) {
  select_word <- ""
  
  for (j in mlag:1) if (i>j) { ## skip lags too long for current i
    indices <- which(a1[1: (n - mlag)] == start_word)
    freqs = M[indices, j + 1]
    
    if (all(freqs == 0)){
      next
    }else{
      select_word <- sample(uni[freqs], 1)
      break
    }
  }
  
  if (select_word == ""){
    select_word <- sample(a1, 1, prob = tmp)
  }
  
  sentence <- paste(sentence, select_word, sep = " ")
  start_word <- select_word
}

cat("\n", sentence, "\n", "\n")


######## Last 3 marks

sentence1 <- sub("^(\\w)", "\\U\\1", sentence, perl = TRUE)

sentence1 <- gsub("\\s+([.,!?])", "\\1", sentence1)

words <- unlist(strsplit(sentence1, " "))
for (i in 1:length(words)) {
  if (i > 1 && nchar(words[i - 1]) > 0 && grepl("[.,!?]", substring(words[i - 1], nchar(words[i - 1]), nchar(words[i - 1])))) {
    words[i] <- paste0(toupper(substring(words[i], 1, 1)), substring(words[i], 2))
  }
}
sentence1 <- paste(words, collapse = " ")
sentence1 <- paste(sentence1, ".", sep = "")

cat("\n", sentence, "\n", "\n")
cat("\n", sentence1, "\n", "\n")







