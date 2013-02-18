
##' 获取豆瓣图书基本信息
##' 
##' @title 获取豆瓣图书基本信息.
##' @param bookid 豆瓣书籍的id号，一串数字，如20326626.
##' @note bookid必须是豆瓣图书有效的id.
##' @return 
##'  一个列表(list), 包括: 
##'  \item{book_info}{书籍的基本信息，如作者、出版社等}
##'  \item{votes_info}{用户评分信息，score为总分，votes指参与的评分人数，
##'  starsx指对应评分用户的所占的比例}
##'  \item{content_intro}{书籍内容简介}
##'  \item{author_intro}{作者简介}
##'  \item{label_all_number}{用户常用标签数量}
##'  \item{labels}{一个data.frame,label_name标签名称及对应的使用频率label_freq}
##'  \item{reader_info}{doings指正在阅读的用户数量，collections指已读的用户数量，
##'  wishes指想读的用户数量} 
##'  
##' @author qxde01 <\email{qxde01@@gmail.com}>
##' @keywords Search
##' 
##' @examples {
##' get_book_info(bookid=20326626)
##' }
##' 
get_book_info<-function(bookid,...){
  strurl=paste0('http://book.douban.com/subject/',bookid,'/')
  pagetree <- htmlParse(strurl)
  
  ## 书籍信息:作者、出版社等
  booknode <- getNodeSet(pagetree, '//div[@id="info"]')
  bookinfo<-sapply(booknode, xmlValue)
  bookinfo<-gsub('\n| ','',bookinfo)
  
  ## 评分信息
  votenode <- getNodeSet(pagetree, '//div[@id="interest_sectl"]')
  voteinfo<-sapply(votenode, xmlValue)
  voteinfo<-gsub('\n| ','',voteinfo)
  voteinfo<-as.double(unlist(strsplit(gsub('\\(|人评价\\)|%',' ',voteinfo),' ')))
  names(voteinfo)<-c('score','votes','stars5','stars4','stars3','stars2','stars1')
  voteinfo[3:7]<-voteinfo[3:7]/100
  ##内容简介、作者简介
  contentnode <- getNodeSet(pagetree, '//div[@class="intro"]')
  contentinfo<-sapply(contentnode, xmlValue)
  contentinfo<-gsub('\n| ','',contentinfo)
  content_intro<-contentinfo[2]
  author_intro<-contentinfo[3]
  
  ##常用标签信息
  labelnode <- getNodeSet(pagetree, '//div[@id="db-tags-section"]')
  labelinfo<-sapply(labelnode, xmlValue)
  labelinfo<-gsub(' ','',labelinfo)
  write(labelinfo,'labelinfo.txt')
  labelinfo<-readLines('labelinfo.txt');file.remove('labelinfo.txt')
  labelinfo<-unlist(strsplit(gsub('<[^><]*>|\n','',labelinfo),'\\(|\\)'))
  label_all_number<-as.integer(gsub('[^0-9]','',labelinfo[2]))
  labelinfo<-labelinfo[c(-1,-2,-3)]
  
  label_name<-labelinfo[-grep('[0-9]',labelinfo)]
  label_freq<-as.integer(labelinfo[grep('[0-9]',labelinfo)])
  ## 读者信息
  readnode <- getNodeSet(pagetree, '//p[@class="pl"]')
  readinfo<-sapply(readnode, xmlValue)
  readinfo<-readinfo[grep('读',readinfo)]
  readerinfo<-as.integer(gsub('[^0-9]','',readinfo))
  names(readerinfo)<-c('doings','collections','wishes')
  
  list(book_info=bookinfo,
       votes_info=voteinfo,
       content_intro=content_intro,
       author_intro=author_intro,
       label_all_number=label_all_number,
       labels=data.frame(label_name,label_freq),
       reader_info=readerinfo)
}


