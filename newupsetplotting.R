library(UpSetR)
a <- read.table('upset/a.CJ_WT.over.WT_Ctrl.txt',header = T)
b <- read.table('upset/b.Car_Ctrl.over.WT_Ctrl.txt',header = T)
c <- read.table('upset/c.CJ_Carover.WT_Ctrl.txt',header = T)
listInput <- list(one=a$Gene,two=b$Gene,three=c$Gene)
upset(
  fromList(listInput),keep.order = T, sets = c("one", "two", "three"),
  queries = list(list(query = intersects, 
                      params = list("one"),color = "red",active=T),
                 list(query = intersects, 
                      params = list("two"),color = "blue",active=T),
                 list(query = intersects, 
                      params = list("three"),color = "green",active=T)
  ),point.size = 6,matrix.color = 'black')
