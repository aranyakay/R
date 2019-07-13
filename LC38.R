# 38. Count and Say
# The count-and-say sequence is the sequence of integers with the first five terms as following:
#   
#   1.     1
# 2.     11
# 3.     21
# 4.     1211
# 5.     111221
# 1 is read off as "one 1" or 11.
# 
# 11 is read off as "two 1s" or 21.
# 
# 21 is read off as "one 2, then one 1" or 1211.
# 
# Given an integer n where 1 ≤ n ≤ 30, generate the nth term of the count-and-say sequence.
# 
# Note: Each term of the sequence of integers will be represented as a string.

countNSay <- function(times){
  if(times > 30) print("only allow 1-30 in this program")
  else{
    n <- read.csv(text="n,read")
    for(i in 1:times){
      if(i==1) n[1,] = c(1, "1")
      else{
        n[i, 1] <- i
        for(j in 1:nchar(n[i-1, 2])){
          if(j==1) {
            m <- read.csv(text="col1,col2")
            m[1,] <- c(1, substring(n[i-1,2],1,1))
          }else{
            if(substring(n[i-1,2],j,j) == substring(n[i-1,2],j-1,j-1)){
              m[nrow(m), 1] <- as.numeric(m[nrow(m), 1]) + 1
            }else{
              #n[i, 2] <- paste(n, m[nrow(m), 2], m[nrow(m), 1], sep="")
              m[nrow(m)+1,] <- c(1, as.numeric(substring(n[i-1,2],j,j)))
            }
          }
        }
        n[i,2] <- gsub("[^0-9]", "", paste(t(m), collapse=""))
      }
    }
    print(n$read[times])
  }
}

countNSay(5)
countNSay(6)
countNSay(7)
countNSay(50)

#matrix n for number and count
#matrix m for element and duplicates
#collapse matrix m for result
#need converse the matrix when output because default is by col


  
