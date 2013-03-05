
get_book_notes<-function(bookid,n=100,verbose=TRUE,...){
  
  strurl=paste0('http://book.douban.com/subject/',bookid,'/annotation')
  pagetree <- htmlParse(getURL(strurl))
  titlenode <- getNodeSet(pagetree, '//title')
  titleinfo<-sapply(titlenode, xmlValue)
  titleinfo<-gsub('\n|的笔记| |\\(|\\)','',titleinfo,fixed = F)
  book_title<-gsub('[0-9]','',titleinfo)
  notes_amount<-as.integer(gsub('[^0-9]','',titleinfo))
  cat('There is a total of',notes_amount,'notes...\n')
  
  .get_note<-function(pagetree,verbose=TRUE,...){
    notenode <- getNodeSet(pagetree, '//a[@href]')
    noteurl<-sapply(notenode,function(x) xmlGetAttr(x, "href"))
    notecmt<-unique(noteurl[grep('http://book.douban.com/annotation/',noteurl)])
    note_url<-notecmt[-grep('#',notecmt)]
    note_comment_url<-notecmt[grep('#',notecmt)]
    authorurl<-unique(noteurl[grep('/people/',noteurl)])
    m=length(note_url)
    
    nt<-c()
    for(i in 1:m){
      if(verbose==TRUE)
        cat(' Getting book note from ',note_url[i],' ...\n')
      
      notetree <- htmlParse(getURL(note_url[i]))
      ##  the title of note
      titlenode <- getNodeSet(notetree, '//title')
      note_title<-gsub('\n| ','',sapply(titlenode, xmlValue))
      ## the time of note realsed
      timenode <- getNodeSet(notetree, '//span[@class="pubtime"]')
      note_time<-sapply(timenode, xmlValue)
      ## the nickname of author
      authornode <- getNodeSet(notetree, '//h6//a')[1]
      author<-sapply(authornode, xmlValue)
      ##the rating of author
      ratingnode <- getNodeSet(notetree, '//div[@class="mod profile clearfix"]//span[@class]')
      rating<-sapply(ratingnode,function(x) xmlGetAttr(x, "class"))[2]
      rating<-gsub('[^0-9]','',rating)
      ##笔记内容
      contentnode <- getNodeSet(notetree, '//pre[@id="link-report"]')
      note_content<-sapply(contentnode, xmlValue)
      
      nt0<-c(titles=note_title,notes=note_content,time=note_time,
             authors=author,rating=rating,author_url=authorurl[i],
             notes_url=note_url[i],notes_comment_url=note_comment_url[i])
      nt<-rbind(nt,nt0)
    }
    row.names(nt)<-NULL
    nt
  }
  
  pages=ceiling(min(n,notes_amount)/10)
  notes_info<-.get_note(pagetree,verbose=verbose)
  if(pages>1){
    for(pg in 2:pages){
      cat('Getting',(pg-1)*10+1,'--',pg*10,'notes...\n')
      strurl=paste0('http://book.douban.com/subject/',bookid,'/annotation?sort=rank&start=',(pg-1)*10)
      pagetree <- htmlParse(getURL(strurl))
      
      notes_info0<-.get_note(pagetree)
      notes_info<-rbind(notes_info,notes_info0)   
    }
  }
  row.names(notes_info)<-NULL
  notes_info<-data.frame(title=notes_info[,'titles'],
                         note=notes_info[,'notes'],
                         time=notes_info[,'time'],
                         nickname=notes_info[,'authors'],
                         rating=as.integer(notes_info[,'rating']),
                         author_url=notes_info[,'author_url'],
                         note_url=notes_info[,'notes_url'],
                         note_comment_url= notes_info[,'notes_comment_url'],
                         stringsAsFactors=F)
  
  list(book_title=book_title,
       notes_amount=notes_amount,
       notes_info=notes_info)
}
