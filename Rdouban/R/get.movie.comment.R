## movieid:movie id of douban
## results:get the number of comments

get.movie.comment<-function(movieid,fresh=20,results=100,verbose=TRUE){
  u=paste0("http://m.douban.com/movie/subject/",movieid,"/comments")
  p<-htmlParse(postForm(u))
  ## 评论总数
  total<-sapply(getNodeSet(p, '//div[@class="title"]//span'), xmlValue)
  total<-as.integer(gsub("[^0-9]","",total))
  cat("----------There are a total of ",total," short comments.----------\n")
  results<-min(total,results)
  ## 评论屏数
  pages<-ceiling(results/20)
  out<-data.frame(matrix(nrow=pages*20,ncol=4),stringsAsFactors=F)
  colnames(out)<-c("author","author_id","comment","rating")
  nr=1
  for(pg in 1:pages){
    #http://m.douban.com/movie/subject/11627047/comments?page=2&session=4c4f1371
    u=paste0("http://m.douban.com/movie/subject/",movieid,"/comments?page=",pg)
    if(verbose==TRUE){
      cat("Retrieving comments of page ",pg, " from: ",u," .....\n")
    }
    p<- tryCatch(.refreshForm(u,fresh,verbose),error = function(e){NULL})
    
    author<-sapply(getNodeSet(p,'//div[@class="item"]//a[@class="founder"]'),
                   xmlValue)
    author_id<-sapply(getNodeSet(p,'//div[@class="item"]//a[@class="founder"]'),
                      function(x) xmlGetAttr(x, "href"))
    author_id<-unlist(strsplit(author_id,"/movie/people/|/\\?session"))
    author_id<-author_id[-grep("=",author_id)]
    author_id<-author_id[nchar(author_id)>0]
    n1<-getNodeSet(p,'//div[@class="list"]//div[@class="item"]//span')
    tmp<-sapply(n1,xmlValue)
    tmp<-gsub("[\n ]","",tmp)[1:(length(tmp)-3)] 
    cmt<-tmp[seq(1,length(tmp),2)]
    rating<-tmp[seq(2,length(tmp),2)]
    #cat(rating,'\n')
    rating<-gsub("[^\\(0-5星]","",rating)
    #gsub("[^0-5][^星]","",rating)
    rating<-gsub("[0-5]{1,10}\\(","",rating)
    rating<-gsub("[\\(星]","",rating)
    out0<-cbind(author=author,author_id=author_id,comment=cmt,rating=rating)
    nr2=nrow(out0)
    out[nr:(nr+nr2-1),]<-out0
    nr=nr+nr2
  }
  return(out)
}