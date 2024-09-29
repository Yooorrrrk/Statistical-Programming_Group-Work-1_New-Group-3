setwd(here::here())
rm(list = ls())



######## Read the text file

a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
          fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("

a


######## Pre-Processing (split punctuation mark with words)

split_punct <- function(text_vector, punctuation){

  offset <- 0  # split an element will change the length of vecter
  indexes <- grep(punctuation, text_vector, fixed = TRUE)
  for(index in indexes){
    adjusted_index <- index + offset
    text_vector[adjusted_index]<- gsub(punctuation, "", text_vector[adjusted_index], fixed=TRUE)
    text_vector <- append(text_vector, punctuation, after = adjusted_index)
    offset <- offset + 1
  }
  return(text_vector)
}

punctuation_vector <- c(",", ".", ";", "!", ":", "?")


i = 1

for (punc in punctuation_vector){
  print(i)
  i <- i + 1
  a <- split_punct(a, punc)
}

saveRDS(a, file = "word_vector.rds")

a1 <- a
# a1 <- readRDS("word_vector.rds")



######## Find Top 1000 words of occurrences

a1 <- tolower(a1)

uni <- unique(a1)

cal_occurrences <- match(a1, uni)

cal_occurrences

occurrences <- tabulate(cal_occurrences)

freq <- 26

b <- uni[which(occurrences > freq)]    # size of b = 1005

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

set.seed(1111)
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
      select_word_indeices <- sample(1:length(indices), 1, prob = freqs)
      select_word <- a1[indices[select_word_indeices] + j]
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







