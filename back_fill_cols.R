####################### @Anthor: Harry Peng Zhao #################

back_fill_cols<- function(fill_col, growth_col) {
  ####Back fill a list based on growth list (first backwaerd, then forward) ###
  #### return the back-filled list #### 
  
  ##Fill backward (up)
  for (i in (length(fill_col)-1):1) {
    if (is.na(fill_col[i])){
      fill_col[i] <- fill_col[i+1]/ (1 + growth_col[i+1])
    }
  }
  
  ##Fill forward (down)
  
  for (i in (2: length(fill_col))) {
    if (is.na(fill_col[i])){
      fill_col[i] <- fill_col[i-1] * (1 + growth_col[i])
    }
  } 
  
   return (fill_col)
}
