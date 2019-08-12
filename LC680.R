
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


#a much cleaner way
#does not mean much faster, though

LC680 <- function(S){
  result = F
  if(s == sapply(lapply(strsplit(s, NULL), rev), paste, collapse="")) result = T
  
  for(i in 1:nchar(S)){
    s = paste(substring(S, 1, i-1), ifelse(i<nchar(S), substring(S, i+1, nchar(S)), ""), sep="")
    if(s == sapply(lapply(strsplit(s, NULL), rev), paste, collapse="")) result = T
  }
  return(result)
}


#and this one also works, may looks better
#still not mean faster or space saver

LC680 <- function(S){
  back <- function(a)sapply(lapply(strsplit(a, NULL), rev), paste, collapse="")
  result = F
  if(s == back(s)) result = T
  
  for(i in 1:nchar(S)){
    s = paste(substring(S, 1, i-1), ifelse(i<nchar(S), substring(S, i+1, nchar(S)), ""), sep="")
    if(s == back(s)) result = T
  }
  return(result)
}


#something gonna be so fast


LC680 <- function(S){
  n = nchar(s)
  left = 1
  right = n
  result = T
  comp <- function(s, left, right){
    result = T
    while (left < right) {
      if(substring(s, left, left) != substring(s, right, right) )     return(F)
      left = left + 1
      right = right -1
    }
    return(T)
  }
  while(left < right){ 
    if(substring(s, left, left) != substring(s, right, right)) result = (comp(s, left+1, right)|comp(s, left, right-1))
    left = left + 1
    right = right - 1
  }
  return(result)
  
}

