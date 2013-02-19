

get_book_comments<-function(bookid,n=50,...){
  strurl=paste0('http://book.douban.com/subject/',bookid,'/reviews')
  pagetree <- htmlParse(getURL(strurl))
  
  titlenode <- getNodeSet(pagetree, '//title')
  titleinfo<-sapply(titlenode, xmlValue)
  titleinfo<-gsub('\n|的评论| ',' ',titleinfo)
  titleinfo<-unlist(strsplit(titleinfo,' '))
  titleinfo<-titleinfo[nchar(titleinfo)>0]
  book_title<-titleinfo[1]
  comments_amount<-as.integer(gsub('[^0-9]','',titleinfo[2]))
  cat('总共',comments_amount,'篇评论...\n')
  
  .get_comment<-function(pagetree,...){
    ##评论的url及作者的主页url
    cmtnode <- getNodeSet(pagetree, '//a[@title]')
    
    cmturl<-sapply(cmtnode,function(x) xmlGetAttr(x, "href"))
    
    authorurl<-cmturl[grep('/people/',cmturl)]
    commenturl<-cmturl[grep('/review/',cmturl)]
    m=length(commenturl)
    cmt<-c()
    for(i in 1:m){
      cat('正在获取 ',commenturl[i],' 的内容...\n')
      cmttree <- htmlParse(getURL(commenturl[i]))
      ## 评论的名称
      titlenode <- getNodeSet(cmttree, '//title')
      title<-sapply(titlenode, xmlValue)
      title<-gsub('\n| ','',title)
      ## 评论作者的昵称、评分及网友的对他的评价（有用和无用的数量）
      authornode <- getNodeSet(cmttree, '//span[@property="v:reviewer"]')
      author<-sapply(authornode, xmlValue)
      
      ratingnode <- getNodeSet(cmttree, '//span[@property="v:rating"]')
      rating<-sapply(ratingnode, xmlValue)
        
      usefulnode <- getNodeSet(cmttree, '//span[@class="useful"]')
      usefulinfo<-sapply(usefulnode, xmlValue)
      useful<-as.integer(gsub('[^0-9]','',usefulinfo))
      
      unusefulnode <- getNodeSet(cmttree, '//span[@class="unuseful"]')
      unusefulinfo<-sapply(unusefulnode, xmlValue)
      unuseful<-as.integer(gsub('[^0-9]','',unusefulinfo))
      ##评论内容
      commentnode <- getNodeSet(cmttree, '//span[@property="v:description"]')
      comment<-sapply(commentnode, xmlValue)
      
      cmt0<-c(titles=title,comments=comment,authors=author,rating=rating,
              useful=useful,unuseful=unuseful,author_url=authorurl[i],commen_turl=commenturl[i])
      cmt<-rbind(cmt,cmt0)
    }
    row.names(cmt)<-NULL
    cmt
  }
  
  if(n>comments_amount) n=comments_amount
  pages=ceiling(n/25)
  
  comment_info<-.get_comment(pagetree)
  
  if(pages>1){
    for(pg in 2:pages){
      cat('正在获取第',(pg-1)*25+1,'～',pg*25,'篇评论...\n')
      strurl=paste0('http://book.douban.com/subject/',bookid,'/reviews?score=&start=',(pg-1)*25)
      pagetree <- htmlParse(getURL(strurl))
      #pagetree <- htmlParse(strurl)
      comment_info0<-.get_comment(pagetree)
      comment_info<-rbind(comment_info,comment_info0)   
    }
  }
  row.names(comment_info)<-NULL
  list(book_title=book_title,
       comments_amount=comments_amount,
       comment_info=as.data.frame(comment_info))
}


