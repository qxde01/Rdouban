
douban_user_statuses<-function(nickname,getNote=TRUE,getReview=TRUE,getList=TRUE,verbose=TRUE,...){

  ## get book labels and book list
  .get_label<-function(nickname,what,getList=TRUE,verbose=TRUE){
    strurl=paste0('http://book.douban.com/people/',nickname,'/',what)
    pagetree <- htmlParse(getURL(strurl))
    label_name<-sapply(getNodeSet(pagetree, '//ul[@class="tag-list mb10"]//a'),xmlValue)
    label_freq<-sapply(getNodeSet(pagetree, '//ul[@class="tag-list mb10"]//span'),xmlValue)
    book_labels<-data.frame(label_name=label_name,
                            label_freq=as.integer(label_freq),
                            stringsAsFactors=F)
    ##book list
    .get_list<-function(pagetree){
      urlnode<-getNodeSet(pagetree, '//li[@class="subject-item"]//a[@title]')
      book_url<-sapply(urlnode,function(x) xmlGetAttr(x, "href"))
      book_tilte<-sapply(urlnode,xmlValue)
      book_tilte<-gsub('\n|  ','',book_tilte)
      
      imgnode<-getNodeSet(pagetree, '//li[@class="subject-item"]//img[@src]')
      img_url<-sapply(imgnode,function(x) xmlGetAttr(x, "src"))
      
      read_date<-sapply(getNodeSet(pagetree, 
                                   '//li[@class="subject-item"]//span[@class="date"]'),xmlValue)
      read_date<-gsub('[\n 读过想在]','',read_date)
      
      book_info<-sapply(getNodeSet(pagetree, 
                                   '//li[@class="subject-item"]//div[@class="pub"]'),xmlValue)
      book_info<-gsub('[\n ]','',book_info)
      
      nlen=length(book_info)
      book_tag<-sapply(getNodeSet(pagetree, 
                                   '//li[@class="subject-item"]//span[@class="tags"]'),xmlValue)
      book_tag<-gsub('标签: ','',book_tag)
      
      book_comment<-sapply(getNodeSet(pagetree, 
                                  '//li[@class="subject-item"]//p[@class="comment"]'),xmlValue)
      book_comment<-gsub('\n|  ','',book_comment)
      book_rating<-getNodeSet(pagetree, c('//span[@class="rating1-t"]',
                                       '//span[@class="rating2-t"]',
                                       '//span[@class="rating3-t"]',
                                       '//span[@class="rating4-t"]',
                                       '//span[@class="rating5-t"]'))
      
      book_rating<-sapply(book_rating,function(x) xmlGetAttr(x, "class"))
      book_rating<-as.integer(gsub('[^0-9]','',book_rating))
      
      if(length(book_tag)<nlen){
        book_tag<-c(book_tag,rep(NA,nlen-length(book_tag)))
        warning('有部分书籍该用户设定没有标签,用NA替代,这将造成书签和书籍名称不对应!')
        }
      if(length(book_comment)<nlen){
        book_comment<-c(book_comment,rep(NA,nlen-length(book_comment)))
        warning('有部分书籍该用户没有发表短评,用NA替代,这将造成短评和书籍名称不对应!')
      }
      if(length(book_rating)<nlen){
        book_rating<-c(book_rating,rep(NA,nlen-length(book_rating)))
        warning('有部分书籍该用户没有评分,用NA替代,这将造成评分和书籍名称不对应!')      
      }
      
      data.frame(book_tilte=book_tilte,
                 read_date=read_date,
                 book_info=book_info,
                 book_tag=book_tag,
                 book_comment=book_comment,
                 book_rating=book_rating,
                 book_url=book_url,
                 img_url=img_url,
                 stringsAsFactors=F)
    }
    
    
    if(getList==TRUE){
      booklist<-.get_list(pagetree)
      num<-sapply(getNodeSet(pagetree, '//head//title'),xmlValue)
      num<-gsub('[\n ]','',num)
      num<-as.integer(unlist(strsplit(num,'\\(|\\)'))[-1])
      #num<-as.integer(substr(num,unlist(gregexpr('\\(',num))[-1]+1,unlist(gregexpr('\\)',num))[-1]-1))
      pages<-ceiling(num/15)
      
      if(pages>1){
        for(pg in 2:pages){
          if(verbose==TRUE)
            cat(' Getting book list abort ',what,'from page=',pg,'\n')
          strurl=paste0('http://book.douban.com/people/',nickname,'/',what,'?start=',15*(pg-1),
                        '&sort=time&rating=all&filter=all&mode=grid')
          pagetree <- htmlParse(getURL(strurl))
          booklist0<-.get_list(pagetree)
          booklist<-rbind(booklist0,booklist)
        }

      }
    }
    else booklist<-NA

    
    list(book_list=booklist,
         book_labels=book_labels)
        
  }
  
  #########################################################
  ##Get author's notes function
  .get_note<-function(nickname=NULL,nextpage=NULL,verbose=TRUE,...){
    if(!is.null(nickname)){
      strurl=paste0('http://www.douban.com/people/',nickname,'/notes')
      pagetree <- htmlParse(getURL(strurl))
    }
    if(!is.null(nextpage)& is.null(nickname)) 
      pagetree <- htmlParse(getURL(nextpage))
    
    urlsnode<-getNodeSet(pagetree, '//span[@class="wrap"]//div//a')
    urls<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
    urls<-urls[grep('/note/',urls)]
    m=length(urls)
    notes<-c()
    for(i in 1:m){
      if(verbose==TRUE)cat(' Getting note from ',urls[i],'...\n')
      
      pagetree1 <- htmlParse(getURL(urls[i]))
      
      title<-sapply(getNodeSet(pagetree1, '//div[@class="note-header"]//h1'),xmlValue)
      time<-sapply(getNodeSet(pagetree1, '//div[@class="note-header"]//span'),xmlValue)
      note<-sapply(getNodeSet(pagetree1, '//div[@class="note"]'),xmlValue)[2]
      rec_num<-sapply(getNodeSet(pagetree1, '//span[@class="rec-num"]'),xmlValue)
      rec_num<-as.integer(gsub('[^0-9]','',rec_num))
      fav_num<-sapply(getNodeSet(pagetree1, '//span[@class="fav-num"]//a'),xmlValue)
      fav_num<-as.integer(gsub('[^0-9]','',fav_num))
      
      notes0<-c(title=title,time=time,note=note,rec_num=rec_num,fav_num=fav_num,note_url=urls[i])
      notes<-rbind(notes,notes0)
    }
    #
    nextpage0<-getNodeSet(pagetree, '//span[@class="next"]//a')
    
    if(length(nextpage0)>0)
      nextpage0<-sapply(nextpage0,function(x) xmlGetAttr(x, "href"))
    if(length(nextpage0)==0) nextpage0=NULL

    if(!is.null(nextpage0)) {
      cat('Getting next page ',nextpage0,'...\n')
      nt<-.get_note(nickname=NULL,nextpage=nextpage0,verbose=verbose)
      notes<-rbind(notes,nt)
    }
    row.names(notes)<-NULL
    notes
  }  
  
  #########################################################
  ####get author's book and movie reviews function
  .get_review<-function(nickname,verbose,...){
    strurl=paste0('http://www.douban.com/people/',nickname,'/reviews')
    pagetree <- htmlParse(getURL(strurl))
    rev_num<-sapply(getNodeSet(pagetree, '//head//title'),xmlValue)
    rev_num<-as.integer(gsub('[^0-9]','',rev_num))
    
    urlsnode<-getNodeSet(pagetree, '//div[@class="article"]//a[@class="pl"]')
    urls<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
    pages=ceiling(rev_num/10)
    
    if(pages>1){
      for(pg in 2:pages){
        strurl<-paste0('http://www.douban.com/people/Quantum_Panda/reviews?start=',(pg-1)*10)
        pagetree <- htmlParse(getURL(strurl))
        urlsnode<-getNodeSet(pagetree, '//div[@class="article"]//a[@class="pl"]')
        urls0<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
        urls=c(urls,urls0)
      }     
    }
    
    movieurls<-urls[grep('movie',urls)]
    bookurls<-urls[grep('book',urls)]
    movie_n=length(movieurls)
    book_n=length(bookurls)
    
    rev<-c()
    if(book_n>0){
      for(i in 1:book_n){
        if(verbose==TRUE) cat(' Getting book review from ',bookurls[i],' ...\n')
        
        pagetree <- htmlParse(getURL(bookurls[i]))
        
        book_title<-sapply(getNodeSet(pagetree,'//span[@property="v:itemreviewed"]'), xmlValue)
        book_title<-gsub('\n','',book_title)
        
        title<-sapply(getNodeSet(pagetree,'//div[@id="content"]//h1'), xmlValue)
        time<-sapply(getNodeSet(pagetree,'//span[@property="v:dtreviewed"]'), xmlValue)
        rating<-sapply(getNodeSet(pagetree,'//span[@property="v:rating"]'), xmlValue)
        if(rating=="None")rating=NA
        review<-sapply(getNodeSet(pagetree,'//span[@property="v:description"]'), xmlValue)
        if(length(review)==0) 
          review<-sapply(getNodeSet(pagetree,'//div[@property="v:description"]'), xmlValue)
        
        useful<-sapply(getNodeSet(pagetree, '//span[@class="useful"]//em'),xmlValue)
        unuseful<-sapply(getNodeSet(pagetree, '//span[@class="unuseful"]//em'),xmlValue)
        
        #unuseful<-sapply(getNodeSet(pagetree, '//span[@class="unuseful"]//em'), xmlValue)
        #unuseful<-as.integer(gsub('unuseful','',unuseful))
        rev0<-c(target_title=book_title,title=title,time=time,review=review,
                rating=rating,useful=useful,unuseful=unuseful,url=bookurls[i],type='book')
        rev<-rbind(rev,rev0)   
      }
    }
    if(movie_n>0){
      for(i in 1:movie_n){
        if(verbose==TRUE)cat(' Getting movie review from  ',movieurls[i],' ...\n')
        
        pagetree <- htmlParse(getURL(movieurls[i]))
        movie_title<-sapply(getNodeSet(pagetree,'//span[@property="v:itemreviewed"]'), xmlValue)
        movie_title<-gsub('\n','',movie_title)
        
        title <- sapply(getNodeSet(pagetree, '//span[@property="v:summary"]'),xmlValue)
        time<-sapply(getNodeSet(pagetree, '//span[@property="v:dtreviewed"]'),xmlValue)
        #nickname<-sapply(getNodeSet(pagetree, '//span[@property="v:reviewer"]'),xmlValue)
        rating<-sapply(getNodeSet(pagetree, '//span[@property="v:rating"]'),xmlValue)
        if(rating=="None")rating=NA
        review<-sapply(getNodeSet(pagetree, '//span[@property="v:description"]'),xmlValue)
        if(length(review)==0) 
          review<-sapply(getNodeSet(pagetree,'//div[@property="v:description"]'), xmlValue)
        useful<-sapply(getNodeSet(pagetree, '//em'),xmlValue)
        #unuseful<-sapply(getNodeSet(pagetree, '//span[@class="unuseful"]//em'),xmlValue)
        
        rev0<-c(target_title=movie_title,title=title,time=time,review=review,
                rating=rating,useful=useful[1],unuseful=useful[2],url=movieurls[i],type='movie')
        rev<-rbind(rev,rev0)
      }
    }
   
    row.names(rev)<-NULL
    rev<-data.frame(target_title=rev[,'target_title'],
                    title=rev[,'title'],
                    time=rev[,'time'],
                    review=rev[,'review'],
                    rating=as.integer(rev[,'rating']),
                    useful=as.integer(rev[,'useful']),
                    unuseful=as.integer(rev[,'unuseful']),
                    url=rev[,'url'],
                    type=rev[,'type'],
                    stringsAsFactors=F)
    rev
  }
  ############################
  strurl=paste0('http://www.douban.com/people/',nickname,'/')
  pagetree <- htmlParse(getURL(strurl))
  author<-gsub('\n','',sapply(getNodeSet(pagetree, '//title'),xmlValue))
  
  book<-sapply(getNodeSet(pagetree, '//div[@id="book"]//span//a'),xmlValue)
  book<-as.integer(gsub('[^0-9]','',book))
  names(book)<-c('do','wish','collect')
  
  magazine<-sapply(getNodeSet(pagetree, '//div[@id="book"]//p//a'),xmlValue)
  magazine<-as.integer(gsub('[^0-9]','',magazine))
  if(length(magazine)==0)magazine=0
  movie<-sapply(getNodeSet(pagetree, '//div[@id="movie"]//span//a'),xmlValue)
  movie<-as.integer(gsub('[^0-9]','',movie))
  if(length(movie)==0) movie=c(0,0)
  if(length(movie)==1) movie=c(movie,0)
  names(movie)<-c('wish','collect')
  
  music<-sapply(getNodeSet(pagetree, '//div[@id="music"]//span//a'),xmlValue)
  music<-as.integer(gsub('[^0-9]','',music))
  if(length(music)==0) music=c(0,0,0)
  if(length(music)==1) music=c(music,0,0)
  if(length(music)==2) music=c(music,0)
  names(music)<-c('do','wish','collect')
  
  ## book labels

  do_labels<-.get_label(nickname=nickname,what='do',getList=getList,verbose=verbose)
  book_do_labels<-do_labels$book_labels
  book_do_list<-do_labels$book_list
    
  wish_labels<-.get_label(nickname=nickname,what='wish',getList=getList,verbose=verbose)
  book_wish_labels<-wish_labels$book_labels
  book_wish_list<-wish_labels$book_list
  
  collect_labels<-.get_label(nickname=nickname,what='collect',getList=getList,verbose=verbose)
  book_collect_labels<-collect_labels$book_labels
  book_collect_list<-collect_labels$book_list
  ####get notes and reviews
  
  notenode<-sapply(getNodeSet(pagetree, '//div[@id="note"]//h2'),xmlValue)
  if(length(notenode)==0)notenode=NULL
  notes<-NA
  if(getNote==TRUE & !is.null(notenode)){
    notes<-.get_note(nickname=nickname,nextpage=NULL,verbose=verbose)
    notes<-data.frame(title=notes[,'title'],time=notes[,'time'],
                        note=notes[,'note'],rec_num=notes[,'rec_num'],
                      fav_num=notes[,'fav_num'],url=notes[,'note_url'],
                      stringsAsFactors=F)
  }
  reviewnode<-sapply(getNodeSet(pagetree, '//div[@id="note"]//h2'),xmlValue)
  if(length(reviewnode)==0)reviewnode=NULL  
  reviews<-NA
  if(getReview==TRUE & !is.null(reviewnode) )reviews<-.get_review(nickname=nickname,verbose=verbose)
  
  list(book=book,
       magazine=magazine,
       movie=movie,
       music=music,
       book_do_labels=book_do_labels,
       book_do_df=book_do_list,
       book_wish_labels=book_wish_labels,
       book_wish_df=book_wish_list,
       book_collect_labels=book_collect_labels,
       book_collect_df=book_collect_list,
       notes=notes,
       reviews=reviews)
  
}

#qxde<-douban_user_statuses(nickname='qxde01',getNote=T,getList=TRUE,verbose=TRUE)
