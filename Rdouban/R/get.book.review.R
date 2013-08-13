get.book.review<-function(bookid,results=100,verbose=TRUE){
  
  .get_review<-function(bookid,start,results,verbose){
    u=paste0("http://api.douban.com/book/subject/",bookid,
             "/reviews?start-index=",start,"&max-results=",results)
    p=getURL(u)
    pagetree <- htmlParse(p,asTree=F)
    
    review_id<-sapply(getNodeSet(pagetree, '//entry//id'), xmlValue)
    review_id<-gsub("[^0-9]","",review_id)
    #sapply(getNodeSet(pagetree, '//entry//link[@rel="alternate"]'),function(x) xmlGetAttr(x, "href"))
    title<-sapply(getNodeSet(pagetree, '//entry//title'), xmlValue)
    Encoding(title)<-"UTF-8"
    
    author<-sapply(getNodeSet(pagetree, '//entry//author//name'), xmlValue)
    Encoding(author)<-"UTF-8"
    author_id<-sapply(getNodeSet(pagetree, '//entry//author//uri'), xmlValue)
    author_id<-gsub("[^0-9]","",author_id)
    published<-gsub("T|\\+08:00"," ",sapply(getNodeSet(pagetree, '//entry//published'), xmlValue))
    updated<-gsub("T|\\+08:00"," ",sapply(getNodeSet(pagetree, '//entry//updated'), xmlValue))
    comments<-sapply(getNodeSet(pagetree, '//entry//comments'),function(x) xmlGetAttr(x, "value"))
    useless<-sapply(getNodeSet(pagetree, '//entry//useless'),function(x) xmlGetAttr(x, "value"))
    votes<-sapply(getNodeSet(pagetree, '//entry//votes'),function(x) xmlGetAttr(x, "value"))
    #rating<-sapply(getNodeSet(pagetree, '//entry//rating'),function(x) xmlGetAttr(x, "value"))
    n=length(review_id)
    review<-vector(length=n)
    rating<-vector(length=n)
    for(i in 1:n){
      u<-paste0("http://book.douban.com/review/",review_id[i],"/")
      if(verbose==TRUE){
        cat("  Getting review form: ",i," ",u," ..... \n")
      }
      p=getURL(u)
      p <- htmlParse(p,asTree=F)
      review0<-sapply(getNodeSet(p, '//span[@property="v:description"]'), xmlValue)
      review0<-gsub("\r","",review0)
      rating[i]<-sapply(getNodeSet(p, '//span[@property="v:rating"]'), xmlValue)
      
      if(nchar(review0)<2)review0<-NA
      review[i]<-review0
    }
    out<-data.frame(title=title,review_id=review_id,review=review,author=author,
                    author_uri=author_id,published=published,updated=updated,
                    comments=comments,useless=useless,votes=votes,
                    rating=rating,stringsAsFactors=F)
    return(out)
  }
  ####################################
  u=paste0("http://api.douban.com/book/subject/",bookid,
           "/reviews?start-index=1&max-results=1")
  p=htmlParse(getURL(u))
  totalresults<-sapply(getNodeSet(p, '//totalresults'), xmlValue)
  cat("There are a total of ",totalresults," reviews.\n")
  results<-min(results,as.integer(totalresults))
  starts<-(0:floor(results/50))*50+1
  n=length(starts)
  ends<-rep(50,n)
  ends[n]<-results-50*(n-1)
  if(results%%50==0){
    starts<-starts[1:(n-1)]
    ends<-ends[1:(n-1)]
  }
  n=length(starts)
  out<-c()
  for(i in 1:n ){
    cat('Getting page: ',i,"\n")
    out0<-.get_review(bookid,start=starts[i],results=ends[i],verbose)
    out<-rbind(out0,out)
  }
  
  return(out)
}
