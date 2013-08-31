
##################################################
## @u:url
## @fresh:refresh number
.get_review<-function(u,fresh=10,verbose=TRUE){
  #p<- tryCatch(htmlParse(postForm(u)),error = function(e){NULL})
  p<- tryCatch(.refreshForm(u,fresh,verbose),error = function(e){NULL})
  
  if(is.null(p)){
    warning("Getting failed:",u,".\n")
    out<-NULL
    return(out)
  }
  if(!is.null(p)){
    ##标题
    title<-sapply(getNodeSet(p, '//head//title'), xmlValue)
    #sapply(getNodeSet(p, '//body//div[@class="itm"]//span'), xmlValue)
    ##作者昵称及URI
    author<-sapply(getNodeSet(p, '//body//div[@class="itm"]//a[@class="founder"]'), xmlValue)
    author_uri<-sapply(getNodeSet(p, '//body//div[@class="itm"]//a[@class="founder"]'), 
                       function(x) xmlGetAttr(x, "href"))
    author_uri<-paste0("http://m.douban.com",author_uri)
    ##推荐次数
    votes<-sapply(getNodeSet(p, '//body//span[@class="forbidden"]'), xmlValue)
    votes<-as.integer(gsub("[^0-9]","",votes))
    ####
    st<-sapply(getNodeSet(p, '//body//div[@class="itm"]//span'), xmlValue)
    ##发表时间
    published<-st[grep("\\d{4}\\-[0-1]",st)]
    ##评分
    rating<-gsub("[^0-9]","",st[grep("\\d{4}\\-[0-1]",st)-1])
    
    ####本评论的显示页数
    pgs<-sapply(getNodeSet(p, '//body//div[@class="paginator"]//span'), xmlValue)
    pgs<-as.integer(gsub("1/| ","",pgs))
    #rev<-vector(length=pgs)
    rev<-NULL
    ##评论内容
    rev0<-paste(sapply(getNodeSet(p, '//body//div//p'), xmlValue),collapse="")
    rev[1]<-gsub("\\(转下页\\)","",rev0)
    preU<-unlist(strsplit(u,"\\?id="))[1]
    review_id<-gsub("[^0-9]","",preU)
    ##分页读取评论内容
    if(pgs>1){
      for(pg in 2:pgs){
        u0<-paste0(preU,"?page=",pg)
        #p<-tryCatch(htmlParse(postForm(u0)),error = function(e){NULL})
        p<- tryCatch(.refreshForm(u0,fresh,verbose),error = function(e){NULL})
        
        if(!is.null(p)){
          rev0<-paste(sapply(getNodeSet(p, '//body//div//p'), xmlValue),collapse="")
          rev[pg]<-gsub("\\(转下页\\)","",rev0)
        }
      }
    }
    review<-paste(rev,collapse="")  
    out<-c(review_id=review_id,title=title,author=author,author_uri=author_uri,
           review=review,published=published,votes=votes,rating=rating)
   #cat("---length:",length(out),"\n",
   #     out[c("title","author","author_uri", "published","votes","rating")],'\n')
    return(out)
  }
  
}
#####################################################################
## movieid:movie id of douban
## results:get the number of reviews
get.movie.review<-function(movieid,results=100,fresh=20,verbose=TRUE){
  u=paste0("http://m.douban.com/movie/subject/",movieid,"/reviews")
  p<-htmlParse(postForm(u))
  ## 评论总数
  total<-sapply(getNodeSet(p, '//div[@class="title"]//span'), xmlValue)
  total<-as.integer(gsub("[^0-9]","",total))
  cat("----------There are a total of ",total," reviews.----------\n")
  results<-min(total,results)
  ## 评论屏数
  pages<-ceiling(results/40)
  
  #n1<-getNodeSet(p, '//div[@class="list"]//div[@class="item"]//a')
  #session<-sapply(n1,function(x) xmlGetAttr(x, "href"))[1]
  #session<-unlist(strsplit(session,"="))
  #session<-session[length(session)]
  ##预定义输出大小
  out<-data.frame(matrix(nrow=pages*40,ncol=8),stringsAsFactors=F)
  colnames(out)<-c("review_id","title","author","author_uri","review", 
                   "published","votes","rating")
  ## k 为out行标
  k=1
  er=1
  for(i in 1:pages){
    #http://m.douban.com/movie/subject/3530403/reviews?page=1&session=368f6186
    #u=paste0("http://m.douban.com/movie/subject/",movieid,
    #         "/reviews?page=",i,"&session=",session)
    u=paste0("http://m.douban.com/movie/subject/",movieid,
             "/reviews?page=",i)
    if(verbose==TRUE){
      cat("Retrieving URLs of page ",i, " of reviews from: ",u," .....\n")
    }
    #p<-tryCatch(htmlParse(postForm(u)),error = function(e){NULL})
    p<- tryCatch(.refreshForm(u,fresh,verbose),error = function(e){NULL})
    if(is.null(p)){
      cat("!!!!!!  Host Forbidden:",u,' .\n')
      next
    }
    n1<-getNodeSet(p, '//div[@class="list"]//div[@class="item"]//a')
    href<-sapply(n1,function(x) xmlGetAttr(x, "href"))
    href<-href[grep("/movie/review/",href)]
    n=length(href)
    for(j in 1:n){
      u0=paste0("http://m.douban.com",href[j])
      if(verbose==TRUE){
        cat("  Retrieving ",k, "reviews from: ",u0," .....\n")
      }
      ### 无法打开页面返回NULL，通常是网络异常,Forbidden
      out0<-tryCatch(.get_review(u=u0,fresh,verbose),error = function(e){NULL})
      if(length(out0)<8){
        cat(er," ------Getting failed:",u0,".\n")
        #write(out0,file=paste0(er,".txt"))
        er=er+1
      }
      if(length(out0)==8){
        out[k,]<-out0
        k=k+1
      }
      
    }
  }
  out<-out[!is.na(out[,1]),]
  return(out)
}
