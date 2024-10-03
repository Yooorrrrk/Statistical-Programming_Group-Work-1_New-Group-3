# Our Group Members are：
# Name: Rui Yang    | Student ID: S2747080
# Name: Haihan Zhao | Student ID: S2668314
# Name: Di Wu       | Student ID: S2636080

# The workflow of our group is: 
# (1) Conduct multiple group discussions to clarify the assignment's work content and specific details. ->
# (2) Each member independently completes all parts of the assignment (to ensure that every member gets to practice with R programming). ->
# (3) Multiple discussions on the code details, and finalize the submission version.

# The GitHub URL of our repository is: https://github.com/Yooorrrrk/Statistical-Programming_Group-Work-1_New-Group-3.git


setwd(here::here())   # Setup work path at'here'
rm(list = ls())       # Clear all variables in the environment



## 1 Read the text file

a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,
          fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE)                   # remove "_("



## 2 Pre-Processing (split punctuation mark with words)

# 2.1. Define the [split_punct] function.
# 
# The specific steps are as follows:
#   
#   Step 1: Give the input parameters: [text_vector] a text vector; [punctuation] a punctuation mark in the form of a string;
#   Step 2: Find the indices of the punctuation in text vector -> [punctuation_indices];
#   Step 3: Creat a new vector to contain the text and punctuation -> [new_vector];
#   Step 4: Iterate over the [punctuation_indices] variable;
#   Step 5: According to indecies, copy the element in [text_vector] to [new_vector] in which leave a "NA" between the gap;
#   Step 6: Ensure the copy procedure is complete (after the last element in [indecies], it may have a fragment of vector);
#   Step 7: Find the new indices of the [punctuation] in [new_vector] -> [punct_indices_1];
#   Step 8: Remove the [punctuation] in [new_vector];
#   Step 9: Replace the NA with [punctuation].

split_punct <- function(text_vector, punctuation){
  
  punctuation_indices <- grep(punctuation, text_vector, fixed = TRUE)   # Find the indices of the punctuation in text vector
  
  new_vector_length <- length(text_vector) + length(punctuation_indices)
  new_vector <- vector("character", new_vector_length)                  # Creat a new vector to contain the text and punctuation
  
  last_index <- 1
  start_index <- 1
  
  for (index in punctuation_indices) {                                  # Iterate over the [punctuation_indices] variable
    
    new_vector[last_index:(last_index + (index - start_index))] <- text_vector[start_index:index]
    last_index <- last_index + (index - start_index + 1)
    new_vector[last_index] <- NA
    last_index <- last_index + 1
    start_index <- index + 1                                            # According to indecies, copy the element in [text_vector] to [new_vector] in which leave a "NA" between the gap
  }
  
  if (start_index <= length(text_vector)) {                             # Ensure the copy procedure is complete (after the last element in [indecies], it may have a fragment of vector)
    new_vector[last_index:(last_index + (length(text_vector) - start_index + 1) - 1)] <- text_vector[start_index:length(text_vector)]
  }
  
  punct_indices_1 <- grep(punctuation, new_vector, fixed = TRUE)        # Find the new indices of the [punctuation] in [new_vector]
  new_vector <- gsub(punctuation, "", new_vector, fixed=TRUE)           # Remove the [punctuation] in [new_vector]
  new_vector[punct_indices_1 + 1] <- punctuation                        # Replace the NA with [punctuation].
  
  return(new_vector)
}

# Execute [split_punct] function.

punctuation_vector <- c(",", ".", ";", "!", ":", "?")

a1 <- a

for (punc in punctuation_vector){
  a1 <- split_punct(a1, punc)
}



## 3 Find Top ≈1000 words of occurrences & creat common word vector

# The specific steps are as follows:
#   
#   Step 1: Convert text vector [a1] to lower case;
#   Step 2: unique [a1] -> [uni];
#   Step 3: calculate the frequencies of elements in [cal_occurrences] -> [occurrences];
#   Step 4: Choose a suitable threshold, and we find when the threshold is 26, the size of common word vector is 1005;

a1 <- tolower(a1)                                     # Convert text vector [a1] to lower case
uni <- unique(a1)                                     # unique [a1], remove duplicate elements in [a1] (so that they appear only once).

cal_occurrences <- match(a1, uni)                     # Search for the indices of the element in [uni] which occurs in [a1]

occurrences <- tabulate(cal_occurrences)              # calculate the frequencies of elements in [cal_occurrences]

freq <- 26                                            # Choose a suitable threshold, and we find when the threshold is 26, the size of common word vector is 1005

b <- uni[which(occurrences > freq)]                   # size of b = 1005

occurrences_b <- occurrences[which(occurrences > freq)]

print(paste("Length of common word vector: ", length(b)))



## 4 Build the matrices of word token sequences

# Choose a [mlag] means determine a scan-width.
# 
# The 'scan' means:
#   
#   After match function, vector [tmp] can be [1, 2, 3, 4, 5, 6, 7, 8, 9]
# 
# For example:
#   
#   If the [mlag] = 3, [M] will be:
#   
#   [1, 2, 3
#    2, 3, 4
#    3, 4, 5
#    4, 5, 6
#    5, 6, 7
#    6, 7, 8
#    7, 8, 9]
# 
# where the first column stand for itself (as indices in original text vector), the follow columns is their immediate text

tmp <- match(a1, b)

mlag <- 4
n <- length(a1)

M <- matrix(NA, nrow = (n - mlag), ncol = (mlag + 1))

for (i in 1:(n - mlag)) {
  M[i, ] <- tmp[i:(mlag + i)]
}



## 5 Simulate nw-word sections from our model

nw = 50

M[is.na(M)] <- 0

indices <- grepl("^[A-Za-z]+$", a1)                  # Avoid the first word being a punctuation mark; ^[A-Za-z]+$ is a regular expression,to search all the text only make up by letters
words <- a1[indices]
freqs <- tmp[indices]
freqs[is.na(freqs)] <- 0

set.seed(2)
start_word <- sample(words, 1, prob = freqs)         # Generate first word in random (according to their frequencies)

sentence <- start_word

for (i in 2:nw) {                                # The surrounding loop is to make sure the 'generate action' will take place (nw - 1) times
  select_word <- ""
  
  for (j in mlag:1) if (i>j) { ## skip lags too long for current i;      The inner loop is the 'generate action'.
    indices <- which(a1[1: (n - mlag)] == start_word)                 # Find the last word [start_word] (which will be cover at the end of inner loop)
    freqs = M[indices, j + 1]             # Find the indices of [start_word] in origin text vector [a1] & Select the corresponding [pth: mlag] vector (the mlag + 1) column
    
    if (all(freqs == 0)){                 # if the [freqs] is a all-zero vector, then degenerate to (p-1)th model
      next
    }else{                                # else, select a word form uni[freqs]
      select_word <- sample(uni[freqs], 1)
      break
    }
  }
  
  if (select_word == ""){                 # if all pth, (p-1)th, ..., 1th models are failed, choose another word in random
    select_word <- sample(a1, 1, prob = tmp)
  }
  
  sentence <- paste(sentence, select_word, sep = " ")
  start_word <- select_word
}

cat("\n", sentence, "\n", "\n")



##  Generate a sentence from b in random for camparison

sentence0 <- sample(b, nw, prob = occurrences_b)
sentence0 <- paste(sentence0, sep = " ")
cat("\n", sentence0, "\n", "\n")



## Refine the output. (Last 3 marks)

sentence1 <- sub("^(\\w)", "\\U\\1", sentence, perl = TRUE)           # Convert the first letter of the first word to upper form.
sentence1 <- gsub("\\s+([.,!?])", "\\1", sentence1)                   # Remove Spaces before punctuation marks.

words <- unlist(strsplit(sentence1, " "))                             # Convert the first letter of the first word after punctuation mark to upper form.
for (i in 1:length(words)) {
  if (i > 1 && nchar(words[i - 1]) > 0 && grepl("[.,!?]", substring(words[i - 1], nchar(words[i - 1]), nchar(words[i - 1])))) { # Use regular expression to search for the indices of "Mark"
    words[i] <- paste0(toupper(substring(words[i], 1, 1)), substring(words[i], 2))  # [.,!?] means all the punctuation marks in [] will be searched
  }
}
sentence1 <- paste(words, collapse = " ")
sentence1 <- paste(sentence1, ".", sep = "")                          # Add '.' after the last word

cat("\n", "Origin Version: ", sentence, "\n", "\n")
cat("\n", "Refined Version: ", sentence1, "\n", "\n")
