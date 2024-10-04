Simulation 1:stochastic models and simulation studies
#setwd(here::here())
#rm(list = ls())

ns <- 50
new_sentence <- rep("",ns)
new_row <- rep("",ns)
random <- 1:length(b)
new_sentence[1] <- sample(random,1)


#K=which(M[,1]== sample(random,1));
#b[M[sample(K,1),2]]
#paste(temp)
#cat(paste(temp),sep=" ")

#empty_vec <- rep("", 50)
#empty <- sample(random,5)
#nw <- 50
#for (i in 2:nw){ empty_vec[i] <- sample 
#  for(j in mlag:1) if (i>j) {## skip lags too long for current i...}


#length(b)
#copy_ab <- c(1:length(compar_ab)-mlag)
#for(i in 1:length(b) - mlag){location_common[i] <- which(compar_ab == i)[1] }
#location_common
#first_col <- little_a[location_common]
#second_col <- little_a[location_common + 1]
#third_col <- little_a[location_common + 2]
#forth_col <- little_a[location_common + 3]
#fifth_col <- little_a[location_common + 4]
#paste(first_col,second_col, third_col, forth_col, fifth_col)

#time_uniword <- tabulate(compar_a)
#length(unique(compar_a))
#little_a[which(time_uniword == 1)]
#rep("0",length(unique(compar_a)))
#dupli_a <- which(compar_a >= 1)#clean_a 中的重复项的位置
#only_a <- little_a [ -dupli_a ];only_a# question b

#rank_a <- sort(time_uniword,decreasing = T)
#location_a <- which(compar_a, rank_a); location_a

for (i in 2:4){
  if ((i-1)%%5==0)
  {data_row <- which(M[,1] == new_sentence[i-1])
  new_row[i] <- sample(data_row, 1)
  middle_value <- as.numeric(new_row[i])
  new_sentence[i] <- M[middle_value, 1]}
  if(i%%5 == 0)
  {data_row <- which(M[,(i-1)%%5] ==  new_sentence[i-1])
  new_row[i] <- sample(data_row,1)
  middle_value <- as.numeric(new_row[i])
  new_sentence[i] <- M[middle_value,5]}
  else
  {data_row <- which(M[,(i-1)%%5] ==  new_sentence[i-1])
  new_row[i] <- sample(data_row,1)
  middle_value <- as.numeric(new_row[i])
  new_sentence[i] <- M[middle_value,i%%5]}
 
}




new_sentence<-as.numeric(new_sentence)
cat(b[new_sentence])


new_row <- which(M[,i-1]==data_row[i])
new_row[i] <- }
for (i in 2:nw){ 
  for(j in mlag:1) if (i>j) {## skip lags too long for current i...}
    nw <- 50
    mlag <- 4
    number_b <- 1:length(b)
    initial_number <- sample(number_b,1)
    new_word <- ""
    row_number <- ""
    row_number[1] <- sample(which(M[,1] == initial_number),1)
    
    new_word[1] <- M[row_number[1],1] 
    refresh_point <- new_word[1]
    
    for (i in 2:ns){
      
      database <- which(M[,j]==refresh_point)
      for (j in 4:1) if (i >j){row_number[i] <- sample(database,1)
      new_word[i] <- M[row_number[i],j]
      if(new_word[i] == NA)
      {next}
      else
      {break}}
      refresh_point <- new_word[i]
    }
    new_sentence <- b[new_word]
    cat(new_sentence)
    
compar_word <- ""
for (i in 1:ns){compar_word[i] <- sample(number_b,1)}
compar_sentence <- b[as.numeric(compar_word)]
cat(compar_sentence)

punctions <-c(",", ".", ";", "!", ":", "?")
p <- ""
for (i in 1:length(punctions))
{p[i] <- which( b == punctions[i])} 


for i in ok
{p <- which(b == ok[i])}



punctions <-c(",", ".", ";", "!", ":", "?")
location_puncts <- ""
for (i in 1:length(punctions))
{location_puncts[i] <- which( b == punctions[i])}  # Search the locations of punctuations.