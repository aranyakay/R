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


#three times faster
#shrink one loop here


LC383 <- function(ran, mag){
  freq_dict = data.frame(id = character(), freq = numeric(), stringsAsFactors = F)
  for(i in 1:nchar(mag)){
    if(substring(mag, i, i) %in% freq_dict$id){
      freq_dict$freq[freq_dict$id==substring(mag, i, i)] = freq_dict$freq[freq_dict$id==substring(mag, i, i)] +1
    }else{
      freq_dict[nrow(freq_dict) + 1, 1] = substring(mag, i, i)
      freq_dict[nrow(freq_dict), 2] = 1
    }
  }
  for(j in 1:nchar(ran)){
    if(substring(ran, j, j) %in% freq_dict$id){
      if(freq_dict$freq[freq_dict$id==substring(ran, j, j)]==0) return(F)
      freq_dict$freq[freq_dict$id==substring(ran, j, j)] = freq_dict$freq[freq_dict$id==substring(ran, j, j)] -1
    }else{
      return(F)
    }
  }
  return(T)
}
