
get_book_comments<-function(bookid,n=100,verbose=TRUE,...){
  strurl=paste0('http://book.douban.com/subject/',bookid,'/reviews')
  pagetree <- htmlParse(getURL(strurl))
  
  titlenode <- getNodeSet(pagetree, '//title')
  titleinfo<-sapply(titlenode, xmlValue)
  titleinfo<-gsub('\n|的评论| ',' ',titleinfo,fixed = F)
  titleinfo<-unlist(strsplit(titleinfo,' '))
  titleinfo<-titleinfo[nchar(titleinfo)>0]
  book_title<-titleinfo[1]
  comments_amount<-as.integer(gsub('[^0-9]','',titleinfo[2]))
  cat('There is a tatol of',comments_amount,'comments...\n')
  
  .get_comment<-function(pagetree,verbose=TRUE,...){
    ##评论的url及作者的主页url
    cmtnode <- getNodeSet(pagetree, '//a[@title]')
    cmturl<-sapply(cmtnode,function(x) xmlGetAttr(x, "href"))
    
    authorurl<-cmturl[grep('/people/',cmturl)]
    commenturl<-cmturl[grep('/review/',cmturl)]
    m=length(commenturl)
    cmt<-c()
    for(i in 1:m){
      if(verbose==TRUE)
        cat(' Getting book reviews from  ',commenturl[i],' ...\n')
      cmttree <- htmlParse(getURL(commenturl[i]))
      ## the title of comment
      titlenode <- getNodeSet(cmttree, '//title')
      title<-sapply(titlenode, xmlValue)
      title<-gsub('[\n ]|\\([^\\(\\)]*\\)','',title)
      ## 评论作者的昵称
      authornode <- getNodeSet(cmttree, '//span[@property="v:reviewer"]')
      author<-sapply(authornode, xmlValue)
      ## rating of author
      ratingnode <- getNodeSet(cmttree, '//span[@property="v:rating"]')
      rating<-sapply(ratingnode, xmlValue)
      ##评论发表时间
      timenode <- getNodeSet(cmttree, '//span[@property="v:dtreviewed"]')
      time<-sapply(timenode, xmlValue)
      ## a judge by ther douban users :amount of usful and unuseful
      usefulnode <- getNodeSet(cmttree, '//span[@class="useful"]')
      usefulinfo<-sapply(usefulnode, xmlValue)
      useful<-as.integer(gsub('[^0-9]','',usefulinfo))
      
      unusefulnode <- getNodeSet(cmttree, '//span[@class="unuseful"]')
      unusefulinfo<-sapply(unusefulnode, xmlValue)
      unuseful<-as.integer(gsub('[^0-9]','',unusefulinfo))
      ##评论内容
      commentnode <- getNodeSet(cmttree, '//span[@property="v:description"]')
      comment<-sapply(commentnode, xmlValue)
      
      cmt0<-c(titles=title,comments=comment,time=time,authors=author,rating=rating,
              useful=useful,unuseful=unuseful,authors_url=authorurl[i],
              comments_url=commenturl[i])
      cmt<-rbind(cmt,cmt0)
    }
    row.names(cmt)<-NULL
    cmt
  }
  
  pages=ceiling(min(n,comments_amount)/25)
  comment_info<-.get_comment(pagetree,verbose=verbose)
  
  if(pages>1){
    for(pg in 2:pages){
      cat(' Getting',(pg-1)*25+1,'--',pg*25,'comments...\n')
      strurl=paste0('http://book.douban.com/subject/',bookid,'/reviews?score=&start=',(pg-1)*25)
      pagetree <- htmlParse(getURL(strurl))
      #pagetree <- htmlParse(strurl)
      comment_info0<-.get_comment(pagetree)
      comment_info<-rbind(comment_info,comment_info0)   
    }
  }
  row.names(comment_info)<-NULL
  comment_info<-data.frame(title=comment_info[,'titles'],
                           comment=comment_info[,'comments'],
                           time=comment_info[,'time'],
                           nickname=comment_info[,'authors'],
                           rating=as.integer(comment_info[,'rating']),
                           useful=as.integer(comment_info[,'useful']),
                           unuseful=as.integer(comment_info[,'unuseful']),
                           author_url=comment_info[,'authors_url'],
                           comment_url=comment_info[,'comments_url'],
                           stringsAsFactors=F)
  
  
  list(book_title=book_title,
       comments_amount=comments_amount,
       comment_info=comment_info)
}

