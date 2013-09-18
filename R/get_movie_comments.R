
get_movie_comments<-function(movieid,results=100,fresh=10,verbose=TRUE,...){
  
  u=paste0('http://movie.douban.com/subject/',movieid,'/comments')
  p<- .refreshURL(u,fresh,verbose)
  
  total<-sapply(getNodeSet(p, '//body//span[@class="total"]'),xmlValue)
  total<-as.integer(gsub("[^0-9]","",total))
  cat('------------There is a total of ',total,' short comments.--------\n')
  
  results<-min(total,results)
  pages<-ceiling(results/20)
  out<-data.frame(matrix(nrow=pages*20,ncol=6),stringsAsFactors=F)
  colnames(out)<-c("author","author_uri","published" ,"comment" ,"votes", "rating")
  kind=1
  for(pg in 1:pages){
    if(verbose==TRUE){
      cat(' Getting short comments from ',(pg-1)*20+1,'--',pg*20,'...\n')
    }
    u=paste0('http://movie.douban.com/subject/',movieid,'/comments?start=',(pg-1)*20+1,'&limit=20&sort=new_score')
    p<- .refreshURL(u,fresh,verbose)
    ## 短评内容
    comment<-gsub('\n| ','',sapply(getNodeSet(p, '//div[@class="comment"]//p'),xmlValue))
    ## 评论时间
    publised<-sapply(getNodeSet(p, '//div[@class="comment"]//span[@class=""]'),xmlValue)
    publised<-gsub("[\n ]","",publised)
    ##作者及其URI
    n1<-getNodeSet(p, '//div[@class="comment"]//span[@class="comment-info"]//a')
    author<-sapply(n1,xmlValue)
    author_uri<-sapply(n1,function(x) xmlGetAttr(x, "href"))
    ##有用的票数
    votes<-sapply(getNodeSet(p, '//div[@class="comment"]//span[@class="comment-vote"]//span'),xmlValue)
    #votes<-gsub("[^0-9]","",votes)
    ##评分
    ##### 调整没有评分的项
    .adj.rating<-function(x){
      m<-nchar(x);n=length(x)
      i=1
      if(nchar(x[i])==0)x<-c(NA,x)
      while(n<40 & i<20){
        if(nchar(x[i])==0 & nchar(x[i+1])==0){
          x<-c(x[1:i],NA,x[(i+1):n])
          n=length(x) 
          i=i+1
          #cat(x,n,"\n",m,"\n")
        }
        i=i+1
       # cat(i,"...\n")
      }
      x
    }
    n2<-getNodeSet(p, '//div[@class="comment"]//span[@class="comment-info"]//span')
    rating0<-gsub("[0a-z ]","",sapply(n2,function(x) xmlGetAttr(x, "class")))
    if(length(rating0)!=length(comment)){
      rating0<-.adj.rating(rating0)
    }
    rating<-rating0[nchar(rating0)>0]
    out0<-cbind(author=author,author_uri=author_uri,
               publised=publised,comment=comment,
               votes=votes,rating=rating)  
   nr=nrow(out0)
    out[kind:(kind+nr-1),]<-out0
    kind<-kind+nr
  }
  out<-out[!is.na(out[,1]),]
  return(out)
}
  

#http://movie.douban.com/subject/5308265/comments
#x=get_movie_comments(movieid=5308265,results=100)
