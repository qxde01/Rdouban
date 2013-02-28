

##http://music.douban.com/subject/3843530/reviews?sort=time
##musicid=3843530

get_music_comments<-function(musicid,n=100,verbose=TRUE,...){
  strurl=paste0('http://music.douban.com/subject/',musicid,'/reviews?sort=time')
  pagetree <- htmlParse(getURL(strurl))
  
  title0<-sapply(getNodeSet(pagetree, '//head//title'),xmlValue)
  title<-gsub('[0-9\\(\\) ]','',title0)
  comments_amount<-as.integer(gsub('[^0-9]','',title0))
  
  if (length(comments_amount)==0)
    stop('There is no comments about this music.')
  cat('There is a total of ',comments_amount,'comments...\n')
  
  rating<-sapply(getNodeSet(pagetree, '//div[@class="aside"]//ul'),xmlValue)
	rating<-gsub('[^0-9 ]',' ',rating)
	rating<-unlist(strsplit(rating,' '))
	rating<-rating[nchar(rating)>0]
	rating[-6]<-substr(rating[-6],1,nchar(rating[-6])-1)
	rating<-as.integer(rating)
  names(rating)<-c('votes_amount','stars5','stars4','stars3','stars2','stars1')
  
  .get_comment<-function(pagetree,verbose,...){
    contentnode<-getNodeSet(pagetree, '//div[@id="content"]//a')
    urls<-unique(unlist(sapply(contentnode,function(x) xmlGetAttr(x, "href"))))
    contenturl<-urls[grep('/review/',urls)]
    m=length(contenturl)
    content<-c()
    for(i in 1:m){
      if(verbose==TRUE) cat('  Getting',contenturl[i],'...\n')
      pagetree <- htmlParse(getURL(contenturl[i]))
      
      title<-sapply(getNodeSet(pagetree, '//span[@property="v:summary"]'),xmlValue)
      time<-sapply(getNodeSet(pagetree, '//span[@property="v:dtreviewed"]'),xmlValue)
      comment<-sapply(getNodeSet(pagetree, '//span[@property="v:description"]'),xmlValue)
      rating<-sapply(getNodeSet(pagetree, '//span[@property="v:rating"]'),xmlValue)
      nickname<-sapply(getNodeSet(pagetree, '//span[@property="v:reviewer"]'),xmlValue)
      author_url<-getNodeSet(pagetree, '////div[@id="content"]//span[@class="pl2"]//a')
      author_url<-sapply(author_url[1],function(x) xmlGetAttr(x, "href"))
      useful<-sapply(getNodeSet(pagetree, '//span[@class="useful"]'),xmlValue)
      useful<-as.integer(gsub('[^0-9]','',useful))
      unuseful<-sapply(getNodeSet(pagetree, '//span[@class="unuseful"]'),xmlValue)
      unuseful<-as.integer(gsub('[^0-9]','',unuseful))
      
      content0=c(title=title,time=time,nickname=nickname,
                 comment=comment,rating=rating,useful=useful,
                 unuseful=unuseful,author_url=author_url,comment_url=contenturl[i])
      content<-rbind(content,content0) 
    }
    row.names(content)<-NULL
    content
  }
  pages=ceiling(min(n,comments_amount)/25)
  comments<-.get_comment(pagetree,verbose)
  
  if(pages>1){
    for(pg in 2:pages){
      cat('Getting',(pg-1)*25+1,'--',pg*25,'comments...\n')
      #http://music.douban.com/subject/3843530/reviews?sort=time&start=25
      strurl=paste0('http://music.douban.com/subject/',musicid,'/reviews?sort=time&start=',(pg-1)*25,'/')
      pagetree <- htmlParse(getURL(strurl))
      comments0<-.get_comment(pagetree,verbose)
      comments<-rbind(comments,comments0)
    }
  }
  comments<-data.frame(title=comments[,'title'],
                       time=comments[,'time'],
                       nickname=comments[,'nickname'],
                       comment=comments[,'comment'],
                       rating=as.integer(comments[,'rating']),
                       useful=as.integer(comments[,'useful']),
                       unuseful=as.integer(comments[,'unuseful']),
                       author_url=comments[,'author_url'],
                       comment_url=comments[,'comment_url'],
                       stringsAsFactors=F)
  list(music_title=title,
       comments_amount=comments_amount,
       rating=rating,
       comments=comments)
}

#x<-get_music_comments(musicid=3843530,n=50,verbosr=T)