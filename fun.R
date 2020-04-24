#install.packages("tm",dependencies = TRUE)
#install.packages("qdapRegex")
#install.packages("sentimentr")
install.packages("gdata")
install.packages("reshape2")
install.packages("shiny")

#clear console
cat("\014")

library("qdapRegex")
library("sentimentr")
library(tm)
library(gdata)
library("shiny")

#setting working directory 
setwd("C:/Users/Shubhangi/Desktop/final-year project/citation_Sentiment_analysis/corpus")
getwd()


# folder with textfiles
dest <- "C:/Users/Shubhangi/Desktop/final-year project/citation_Sentiment_analysis/corpus"
#paper_id=1
Citation_sentiment_analysis<-function(paper_id){
  var1=paper_id
  
  
  # make a vector of PDF file names
  #myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)
  myfiles2 <- list.files(path = dest, pattern = "txt",  full.names = TRUE)
  myfiles2
  # convert each PDF file that is named in the vector into a text file 
  # text file is created in the same directory as the PDFs
  #lapply(myfiles, function(i) system(paste('"C:/Program Files/xpdf/bin32/pdftotext.exe"', paste0('"', i, '"')), wait = FALSE) )
  
  
  #creating corpora
  co<-Corpus(DirSource(directory = dest, pattern="txt"))
  co
  len_corpora<-length(co)
  len_corpora
  
  #ws <- readline(prompt="Enter window size: ")
  pid<-c()
  pname<-c()
  a<-c()
  #reading data from corpora
  #global vector 
  vec<-c()
  vec2<-c()
  sen_im<-c()
  sen_ex<-c()
  y<-co[[var1]]$content
  y
  print(y)
  
  #Extracting paper names
  f<-c()
  f<-myfiles2[[var1]]
  # read the first line of a file
  con <- file(f)
  first_line <- readLines(con,n=1)
  first_line
  close(con)
  #write 
  pid[var1]<-var1
  pname[var1]<-first_line
  pinfo<-data.frame(Paper_ID=pid, Paper_Name=pname)
  print(pinfo)
  
  
  #preprocessing
  #removing "\n" from file
  clean_data<-gsub("[\r\n\f]"," ",y)
  print(clean_data)
  
  #breaking into sentences
  tokenised_sentences<-unlist(strsplit(clean_data, '(?<=(\\.\\s[A-Z]))',perl=TRUE))
  tokenised_sentences
  n<-length(tokenised_sentences)
  n
  
  # adding first character to sentences
  clean_sentences<-c()
  clean_sentences[1]<-tokenised_sentences[1]
  for(ls in 1:n){
    r<-nchar(tokenised_sentences[ls])
    s<-substr(tokenised_sentences[ls],r,r+1)
    clean_sentences[ls+1]<-paste0(s,tokenised_sentences[ls+1])
    clean_sentences[ls+1]<-gsub('.{1}$', '',clean_sentences[ls+1])}
  clean_sentences
  
  #rm_citation(tokenised_sentences)
  total_lines<-ex_citation(clean_sentences)
  total_lines
  
  
  #To remove ""
  x<-gsub("[\"]","",total_lines)
  x
  
  list_to_vector<-unlist(x,use.names = FALSE)
  list_to_vector
  
  #Finding indexes of citation 
  citation_indexes<-which(!is.na(list_to_vector))
  citation_indexes
  length(citation_indexes)
  c1<-0
  for(i in 1:n){
    for(j in citation_indexes){
      if (i==j){
        #for(l in 0:ws-1){
        # f=-(ws/2)+l
        x1<-paste0(clean_sentences[i-1],clean_sentences[i],clean_sentences[i+1])#}
        k<-gsub("[.]","",x1)
        
        #print(extract_sentiment_terms(x1))
        k2<-clean_sentences[i]
        #print(extract_sentiment_terms(k2))
        y1<-sentiment(k)$sentiment
        y2<-sentiment(k2)$sentiment
        
        c1=c1+1
        sen_im[c1]<-x1
        sen_ex[c1]<-k2
        a[c1]=c1
        vec2[c1]<-y2
        vec[c1]<-y1}}}
  
  vec
  vec2
  
  #vector of author and their sentiments
  author<-list_to_vector[!is.na(list_to_vector)]
  res<-data.frame(serial_number=a,author=author,Implicit_sentiment=vec,Explicit_sentiment=vec2)  
  #print(res)
  
  df<-c(pinfo, res)
  df<-c(res)
  #print(df)
  
  # to see what is happening in df
  #str(df)
  #summary(df)
  
  Df<-as.data.frame(df)
  Df
  
  #different file for each paper
  b<-"txt"
  file_name<-paste(var1,b, sep = ".")
  
  #Write data from R to a txt file
  write.fwf(Df, file_name, append = TRUE, sep = "\t")
}
fun1<-function(x1)
{ x2=unlist(x1)
x3<-sen_im[x2]
x4<-sen_ex[x2]
x5<-paste0("Implicit_Citation","\n\n",x3,"\n\n value   ",df$Implicit_sentiment[x2],"\n\n","Explicit_citation","\n\n",x4,"\n\nvalue   ",df$Explicit_sentiment[x2])
return (x5)}

