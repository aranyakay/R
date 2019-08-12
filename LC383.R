#Given an arbitrary ransom note string and another string containing letters 
#from all the magazines, write a function that will return true if the ransom 
#note can be constructed from the magazines ; otherwise, it will return false.

#Each letter in the magazine string can only be used once in your ransom note.

#Note:
#You may assume that both strings contain only lowercase letters.

LC383 <- function(ran, mag){
  ran1 <- as.data.frame(table(as.numeric(charToRaw(ran))))
  ran1$Var1 <- as.character(ran1$Var1)
  ran1 <- ran1[order(ran1$Var1),]
  mag1 <- as.data.frame(table(as.numeric(charToRaw(mag))))
  mag1$Var1 <- as.character(mag1$Var1)
  mag1 <- mag1[order(mag1$Var1),]
  result = T
  for(i in 1:nrow(ran1)){
    if(ran1[i,1] %in% mag1[,1]) {
      if(ran1[i,2] > mag1[mag1$Var1==ran1$Var1[i],2]) result=F
    }else{
      result = F
    }
  }
  return(result)
}


LC383("a", "b")

LC383("aa", "ab")

LC383("aa", "aab")
