
get_movie_comments<-function(movieid,n=100,verbose=TRUE,...){
  
  strurl=paste0('http://movie.douban.com/subject/',movieid,'/comments')
  pagetree <- htmlParse(getURL(strurl))
  movie_title<- gsub('短评','',sapply(getNodeSet(pagetree, '//title'),xmlValue))
  comments_amount<-gsub('[^0-9]','',sapply(getNodeSet(pagetree, '//div[@id="content"]//span[@class="fleft"]'),xmlValue)[1])
  comments_amount<-as.integer(comments_amount)
  cat('There is a total of ',comments_amount,'short comments...\n')
  
  pages=ceiling(min(comments_amount,n)/20)
  
  .get_comment<-function(pagetree,verbose=TRUE,...){
    comments<-gsub('\n| ','',sapply(getNodeSet(pagetree, '//div[@class="comment"]//p'),xmlValue))
    time<-sapply(getNodeSet(pagetree, '//div[@class="comment"]//span[@class="fleft ml8"]'),xmlValue)
    
    useful<-sapply(getNodeSet(pagetree,'//div[@class="comment"]//span[@class="votes pr5"]'),xmlValue)
    authornode<-getNodeSet(pagetree, '//div[@class="comment"]//span[@class="fleft"]//a')
    authors<-sapply(authornode,xmlValue)
    authors_url<-sapply(authornode,function(x) xmlGetAttr(x, "href"))
    
    #voteinfo<-getNodeSet(pagetree, '//div[@class="comment"]//h3//span[@title]')
    #voteinfo<-sapply(voteinfo,function(x) xmlGetAttr(x, "class"))
    #rating<-gsub('allstar| rating|0','',voteinfo[grep('rating',voteinfo)])
    cbind(comments,time,useful,authors,authors_url)
  }
  
  short_comments<-.get_comment(pagetree,verbose=verbose)
  
  if(pages>1){
    for(pg in 2:pages){
      if(verbose==TRUE)cat(' Getting short comments from ',(pg-1)*20+1,'--',pg*20,'...\n')
      
      strurl=paste0('http://movie.douban.com/subject/',movieid,'/comments?start=',(pg-1)*20+1,'&limit=20&sort=new_score')
      pagetree <- htmlParse(getURL(strurl))
      short_comments0<-.get_comment(pagetree,verbose=verbose)
      short_comments<-rbind(short_comments,short_comments0)
    }
  }
  short_comments<-data.frame(comment=short_comments[,'comments'],
                             time=short_comments[,'time'],
                             useful=as.integer(short_comments[,'useful']),
                             nickname=short_comments[,'authors'],
                             author_url=short_comments[,'authors_url'],
                             stringsAsFactors=F)
  
  list(movie_title=movie_title,
       comments_amount=comments_amount,
       short_comments=short_comments)
}
#http://movie.douban.com/subject/5308265/comments
#x=get_movie_comments(movieid=5308265,n=100)
