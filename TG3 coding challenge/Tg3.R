f<-file("stdin",'r')
#n1<-scan(f,what="char")
n1<-readLines(f)

for(i in 1:n1[1] ){
  s<-n1[-1]
  s<-t(s)
  
}
for(i in 1:length(s)){
  s1<-unlist(strsplit(s[i],""))
  
  odd=seq(1,length(s1),2)
  even=seq(2,length(s1),2)
  
  oddl=s1[odd]
  evenl=s1[even]
  
  numodd<-as.data.frame(sapply(oddl, utf8ToInt))
  numeven<-as.data.frame(sapply(evenl,utf8ToInt)) 
  sumodd<-colSums(numodd)
  sumeven<-colSums(numeven)
  
  diff<-abs(sumodd-sumeven)
  write(if(diff %% 3 == 0|diff %% 5== 0|diff %% 7== 0) 
  {cat("Prime String")}else{cat("Casual String")},stdout() ) 
}

