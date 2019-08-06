
#lc680
#Given a non-empty string s, you may delete at most one character. Judge whether you can make it a palindrome

LC680 <- function(S){
  result = F
  if(nchar(S) %% 2 == 1){
    front = substring(S, 1, floor(nchar(S)/2))
    back = substring(S, ceiling(nchar(S)/2)+1, nchar(S))
  }else{
    front = substring(S, 1, nchar(S)/2)
    back = substring(S, (nchar(S)/2 + 1),  nchar(S))
  }
  back = sapply(lapply(strsplit(back, NULL), rev), paste, collapse="")
  if(front == back)result=T
  
  for(i in 1:nchar(S)){
    s = paste(substring(S, 1, i-1), ifelse(i<nchar(S), substring(S, i+1, nchar(S)), ""), sep="")
    if(nchar(s) %% 2 == 1){
    front = substring(s, 1, floor(nchar(s)/2))
    back = substring(s, ceiling(nchar(s)/2)+1,  nchar(s))
    }else{
    front = substring(s, 1, nchar(s)/2)
    back = substring(s, (nchar(s)/2 + 1),  nchar(s))
    }
    back = sapply(lapply(strsplit(back, NULL), rev), paste, collapse="")
    if(front == back)result=T
  }
  return(result)
}


LC680("greg")

LC680("aba")

LC680("abca")
