
# Our Group Member are：
# Name: Rui Yang    | Student ID: S2747080
# Name: Haihan Zhao | Student ID: S2668314
# Name: Di Wu       | Student ID: S2636080

# The workflow of our group is: 
# (1) Conduct multiple group discussions to clarify the assignment's work content and specific details. ->
# (2) Each member independently complete all parts of the assignment (to ensure that every member gets practice with R programming). ->
# (3) Multiple discussions on the code details, and finalize the submission version.

# The GitHub URL of our repository is: https://github.com/Yooorrrrk/Statistical-Programming_Group-Work-1_New-Group-3.git

## Step 2 Read the text and perform simple preprocessing.

rm(list = ls())  # Clear the output variable window
setwd("C:\\Program Files\\RStudio") # Read the local path of RStudio

a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73,fileEncoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) # remove "_("


## Step 3 Write a function that can separate a string from a punctuation.
##        Method from notebook

split_punct <- function(vec,puncts)  # where variable "puncts" means the punctuation.
  {
   pros_num <- grep(puncts, vec, fixed=TRUE); # Find indices of words including "puncts" in "vec".
   
   zero_vec <- rep("0", length(vec)+length(pros_num)); # Create zero Vector waiting for fill separated punctuation and single words.
   
   zero_vec[pros_num+ 1:length(pros_num)] <- rep(fixed=TRUE, puncts, length(pros_num)); # Put all punctuation in the specific location in a vector(original indices plus 1,2,...).
                                                                                        # whenever we separate a new punctuation, we need move forward one place.
   
   new_vec <- gsub(fixed=TRUE,puncts, "" , vec); # Maintain the Original indices of punctuation to be a space.
   
   zero_vec[-pros_num -c(1:length(pros_num))] <- new_vec;  # Fill the empty positions in the zero vector with the vector from which we removed the punctuation.
   return(zero_vec) 
}


### ANSWER 3 IS ABOVE



## Step 4 Separate all punctuation marks one by one(",", ".", ";", "!", ":","?").

a1 <- split_punct(a,",")
a2 <- split_punct(a1,".")
a3 <- split_punct(a2,";")
a4 <- split_punct(a3,"!")
a5 <- split_punct(a4,":")

clean_a <- split_punct(a5,"?") #After separating punctuation
## ANSWER 5 IS ABOVE！

## Step 6
little_a <- tolower(clean_a) # Using function "tolower" transfers the letters of each words to lowercase letters

list_a <- unique(little_a)   # Using function "unique" makes a list for all words appearing in list a 

compar_a <- match(little_a, list_a)  # Using function "match" finds the times of words popping in a

times_a <- tabulate(compar_a)       # Count the number of times preparing to find unique words

number_unique <- length(which(times_a == 1)) # Count the number of words which only occur once

sort_a <- sort(times_a,decreasing = TRUE)  # Sort up the times of words from high to low

location_times <- which(times_a >= sort_a[1000]);  # Find the frequency threshold for the top 1,000 most frequent words and the words location in the list

b <- list_a[location_times];  # Forming a new list for top 1,000 most frequent words
length(b)                     # Discovering the length of vector b is more than 1,000 


### ANSWER 6 IS ABOVE



## Step 7 Aim to create a matrix to display the Markov chain of words with a mlag of 4


mlag <- 4
compar_ab <- match(little_a,b)  # Compare vector a with the 1,000 most frequent words


first_col <- compar_ab[1:(length(little_a)-mlag)]  # The following steps aim to define each columns of the M matrix
second_col <- compar_ab[2:(length(little_a)-mlag+1)]
third_col <- compar_ab[3:(length(little_a)-mlag+2)]
forth_col <- compar_ab[4:(length(little_a)-mlag+3)]
fifth_col <- compar_ab[5:(length(little_a)-mlag+4)]


total_data <- c(first_col,second_col, third_col, forth_col, fifth_col)  # Because in R the input of matrix data is from M[1,1] to M[max,1],
                                                                        # then going to the second column(M[1,2] -> M[max,2]).So we can create a
                                                                        # data vector to fill the matrix
M <- matrix(total_data,nrow=length(little_a)-mlag,ncol=mlag +1)         # Define the numbers of rows and columns 


### ANSWER 7 IS ABOVE!



## Step 8 Randomly generate a sentence with 50 words.
##        Identify and resample the index of the first new word, and write a nested loop to generate the sentence.


ns <- 50
number_b <- 1:length(b)


punctions <-c(",", ".", ";", "!", ":", "?")
location_puncts <- ""
for (i in 1:length(punctions))
{location_puncts[i] <- which( b == punctions[i])}  # Search the locations of punctuations.


initial_number <- sample(number_b[-as.numeric(location_puncts)],1)  # Randomly generate the index of the first word and avoid being punctuation in first place. 

new_word <- ""
row_number <- ""  # Name two new empty vectors.

new_word[1] <- initial_number # In order to reduce memory usage, we perform operations using indices

row_number[1] <- sample(which(M[,1] == initial_number),1)  # Find the rows with the same indices and resample them to ensure randomness.

refresh_point <- initial_number # Store indices of each new words

set.seed(520) # Using function "set.seed" lets each producing similarly


for (i in 2:ns)
    {
      for (j in mlag:1) 
        if (i > j)
        {
         database <- which(M[,j]==refresh_point)  
         row_number[i] <- sample(database,1)  # Each times, resampling their new row based on the index of the previous word.
     
         rows <- as.numeric(row_number[i])    # Using function "as.numeric" transfers the type of words from string to data.
         new_word[i] <- M[rows,j+1]           # Generates the new_word. 
     
         if(is.na(new_word[i]))               # If we encounter an NA value for new_word,  we first go through the j lag values(from different columns).
          {next}                              # There are two ways to exit the loop: 1. After experiencing all the j values, if new_word is still NA, the loop is exited reluctantly.
         else                                 # 2. If new_word is not NA, the loop is exited immediately.
          {break}
        }
        if(is.na(new_word[i]))                # After going through all the j lag values, if we get NA for new_word, we need perform a fully random selection.
        {
         new_word[i] <- sample(number_b,1)
        }

      refresh_point <- new_word[i]           # To prevent values from being overwritten.
    }


word_number <- as.numeric(new_word)
new_sentence <- b[word_number]               # Using indices/location to get the new_word


cat(new_sentence)                            # Print new_sentence


### Answer 8 is above!



## Step 9 Use a loop to perform fully random selection.


compar_word <- ""
for (i in 1:ns){compar_word[i] <- sample(number_b,1)} #
compar_sentence <- b[as.numeric(compar_word)]
cat(compar_sentence)#9


### Answer 9 is above!



## Step 10 Capitalize the first letter of the first word and remove the space before punctuation.

new_b <- ""


for (i in 1 : length(b))
  {  
    for (j in 1 : length(location_puncts)) #Go back to Step 8 to find variable
      {
    if (i == location_puncts[j])
        {
       new_b[i] <- b[i]
       break
        }
    else
    {new_b[i] <- paste("",b[i])}
      }  
  }  # Generate a new b sequence, where each word string is preceded by a space, while the punctuation strings remain unchanged.


ult_sentence <- new_b[word_number]  # Be same like the previous sentence
ult_sentence[1] <- substr(ult_sentence[1], 2,nchar(ult_sentence[1])) # This function means move the blank in the first word.

ult_sentence[1] <- paste(toupper(substr(ult_sentence[1], 1, 1)), substr(ult_sentence[1], 2, nchar(ult_sentence[1])) ,sep="") 
# Using function "toupper" to transfer first letter to be upper one.


cat(ult_sentence,sep = "") #We already made a space for each strings, so we don't need a any blank to clue words.

###ANSWER 10 is ABOVE
