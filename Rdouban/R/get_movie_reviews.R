##movieid=5308265
## x=get_movie_reviews(movieid=5308265,results=50)
########################################################
### 获取每篇评论的信息
.get_m_review0<-function(u,fresh=10,verbose=TRUE,...){
  p<-.refreshURL(u=u,fresh,verbose)
  ##review_id
  #review_id<-gsub("[^0-9]","",u)
  ## title
  title<- sapply(getNodeSet(p, '//div[@id="content"]//span[@property="v:summary"]'),xmlValue)
  ## 发表时间
  published<-sapply(getNodeSet(p, '//span[@property="v:dtreviewed"]'),xmlValue)
  ## 作者,URI
  author<-sapply(getNodeSet(p, '//span[@property="v:reviewer"]'),xmlValue)
  author_uri<-sapply(getNodeSet(p, '//div[@class="main-hd"]//a[@href]'),
                     function(x) xmlGetAttr(x, "href"))[1]
  ##评分
  rating<-sapply(getNodeSet(p, '//span[@property="v:rating"]'),xmlValue)
  ##影评内容
  review<-sapply(getNodeSet(p, '//span[@property="v:description"]'),xmlValue)
  if(length(review)==0){
    review<-sapply(getNodeSet(p, '//div[@property="v:description"]'),xmlValue)
  }
  review<-gsub("\r","",review)
  ## 有用 & 没用的次数
  x0<-sapply(getNodeSet(p, '//div[@class="main-panel-useful"]//em'),xmlValue)
  useful=x0[1]
  unuseful=x0[2]
  if(length(useful)==0|length(unuseful)==0){
    useful<-sapply(getNodeSet(p, '//span[@class="useful"]//em'),xmlValue)
    unuseful<-sapply(getNodeSet(p, '//span[@class="unuseful"]//em'),xmlValue)
  }
  out<-c(review_uri=u,title=title,published=published,
         author=author,author_uri=author_uri,review=review,
         rating=rating,useful=useful,unuseful=unuseful)
  return(out)
}
###########################################################################
#### 
get_movie_reviews<-function(movieid,results=100,fresh=10,verbose=TRUE,...){
  u=paste0('http://movie.douban.com/subject/',movieid,'/reviews')
  p<-.refreshURL(u,fresh,verbose)
  ## 总评论数
  total<-sapply(getNodeSet(p, '//head//title'),xmlValue)
  total<-unlist(strsplit(total," "))[-1]
  total<-as.integer(gsub('[^0-9]','',total[length(total)]))

  cat('--------------There is a total of',total,'reviews.------------\n')
  
  pages<-ceiling(min(results,total)/20)
  ## 预定义输出dataFrame大小
  out<-data.frame(matrix(nrow=pages*20,ncol=9),stringsAsFactors=F)
  colnames(out)<-c("review_uri","title" , "published" ,"author","author_uri",
                   "review" , "rating", "useful" , "unuseful")
  ## out nrow index
  k=1
  ##
  for(pg in 1:pages){
    cat('Getting',(pg-1)*20+1,'--',pg*20,'reviews...\n')
    u=paste0('http://movie.douban.com/subject/',movieid,
             '/reviews?start=',(pg-1)*20,'&filter=&limit=20')
    p<-.refreshURL(u,fresh,verbose)
    n1<-getNodeSet(p, '//div[@class="review"]//div[@class="review-hd"]//a[@title]')
    href<-unique(sapply(n1,function(x) xmlGetAttr(x, "href")))
    href<-href[grep('/review/',href)]
    n=length(href)
    for(i in 1:n){
      u0=href[i]
      if(verbose==TRUE){
        cat("  %%%% Getting ",k," movie review from URL: ",u0," ...\n")
      }
      out0<-tryCatch(.get_m_review0(u=u0,fresh,verbose),
                     error = function(e){NULL})
      if(length(out0)>0){
        out[k,]<-out0
        k=k+1
      }  
    }    
  }
  out<-out[!is.na(out[,1]),]
  return(out)
}
  
  

  
  
