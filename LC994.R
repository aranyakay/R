#lc994 rotten orange
#DFS method


LC994 <- function(row1 = c(0, 0, 0), row2 = c(0, 0, 0), row3 = c(0, 0, 0)){
  dx = c(1, -1, 0, 0)
  dy = c(0, 0, 1, -1)
  orange = list(row1, row2, row3)
  for(m in 1:length(orange)) while(length(orange[[m]])<3) orange[[m]] = c(orange[[m]],0)
  rotlist = list()
  for (i in 1:length(orange)){
    for (j in 1:length(orange[[1]])) if(orange[[i]][j] == 2) rotlist = append(rotlist, list(c(i, j)))
  }
  minute = 0
  while(length(rotlist) > 0){
    newrotlist = list()
    for(n in 1:length(rotlist)){
      x0 = rotlist[[n]][1]
      y0 = rotlist[[n]][2]
      for (k in 1:4){
        x = x0 + dx[k]
        y = y0 + dy[k]
        if (x >= 1 & x < 4 & y>= 1 & y < 4){
          if(orange[[x]][y] == 1){
          orange[[x]][y] = 2
          newrotlist = append(newrotlist, list(c(x, y)))
          }
        }
      }
    }
    if(length(newrotlist)==0) break
    #print(newrotlist)
    rotlist = newrotlist
    minute = minute + 1   
  }
  for(row in 1:3) for(col in 1:3) if(orange[[row]][col]==1) minute = -1
  return(minute)
}
row1 = c(2, 1, 1)
row2 = c(1, 1, 0)
row3 = c(0, 1, 1)
LC994(row1, row2, row3)

LC994(c(2,1,1),c(0,1,1),c(1,0,1))

LC994(c(0,2))
