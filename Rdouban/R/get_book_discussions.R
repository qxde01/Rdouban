##http://book.douban.com/subject/1291204/discussion/
##bookid=1291204
get_book_discussions<-function(bookid,n=100,verbose=TRUE){
  strurl=paste0('http://book.douban.com/subject/',bookid,'/discussion/')
  pagetree <- htmlParse(getURL(strurl))
  discussions_amount<-gsub('[^0-9]','',
                           sapply(getNodeSet(pagetree, '//span[@class="count"]'),xmlValue))
  if (length(discussions_amount)==0)
    stop('There is no discussions about this movie(or TV).')
  cat('There is a total of ',discussions_amount,'discussions...\n')
  
  .get_discussion<-function(pagetree,verbose){
    #title<-gsub('\n','',sapply(getNodeSet(pagetree, '//head//title'),xmlValue))
    urlsnode<-getNodeSet(pagetree, '//table[@class="olt"]//td/a')
    urls<-unique(sapply(urlsnode,function(x) xmlGetAttr(x, "href")))
    disc_urls<-urls[grep('/discussion/',urls)]
    m=length(disc_urls)
    disc<-c()
    for(i in 1:m){
      if(verbose==TRUE) cat(' Getting',disc_urls[i],'...\n')
      
      pagetree <- htmlParse(getURL(disc_urls[i]))
      time<-sapply(getNodeSet(pagetree, '//table[@class="wr"]//span[@class="mn"]'),xmlValue)
      nickname<-sapply(getNodeSet(pagetree, '//table[@class="wr"]//span[@class="pl2"]//a'),xmlValue)[1]
      title<-gsub('\n','',sapply(getNodeSet(pagetree, '//head//title'),xmlValue))
      useful<-sapply(getNodeSet(pagetree, '//span[@class="useful"]//em'),xmlValue)
      unuseful<-sapply(getNodeSet(pagetree, '//span[@class="unuseful"]//em'),xmlValue)
      dicussion<-sapply(getNodeSet(pagetree, '//div[@class="article"]//table[@class="wr"]//div')[1],xmlValue)
      dicussion<-gsub('[\n ]','', dicussion)
      
      disc0<- c(title=title,dicussion=dicussion,
                time=time,nickname=nickname,
                useful=useful,unuseful=unuseful,discussion_url=disc_urls[i])
      disc<-rbind(disc,disc0)
    }
    row.names(disc)<-NULL
    disc
  }
  
  discussions_info<-.get_discussion(pagetree,verbose=verbose)
  pages<-ceiling(min(n,as.integer(discussions_amount))/20)
  
  if(pages>1){
    for(pg in 2:pages){
      if(verbose==TRUE) {cat(' Getting',(pg-1)*20+1,'--',pg*20,'discussions...\n')}
      
      strurl=paste0('http://book.douban.com/subject/',bookid,
                    '/discussion/?start=',(pg-1)*20,'&sort=vote/')
      pagetree <- htmlParse(getURL(strurl))
      discussions_info0<-.get_discussion(pagetree,verbose=verbose)
      discussions_info<-rbind(discussions_info,discussions_info0)
    }
  }
  row.names(discussions_info)<-NULL
  discussions_info<-data.frame(title=discussions_info[,'title'],
                               dicussion=discussions_info[,'dicussion'],
                               nickname=discussions_info[,'nickname'],
                               time=discussions_info[,'time'],
                               useful=as.integer(discussions_info[,'useful']),
                               unuseful=as.integer(discussions_info[,'unuseful']),
                               discussion_url=discussions_info[,'discussion_url'],
                               stringsAsFactors =F)
  discussions_info
}
b<-get_book_discussions(bookid=1291204,n=40,verbose=F)