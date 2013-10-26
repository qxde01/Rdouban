## extract some infomation about a book
.extract_book<-function(x){
  b=unlist(xmlToList(x))
  bookid<-gsub("[^0-9]","",b["h2.a..attrs.href"])
  pub<-gsub("\n|  ","",b[which(b=="pub")-1])
  user_rating<-b[b %in% c("rating1-t","rating2-t",
                          "rating3-t","rating4-t","rating5-t")]
  user_rating<-gsub("[^0-9]","",user_rating)
  if(length(user_rating)==0){user_rating<-NA}
  pa<-"[\n ]|\u8bfb|\u8fc7|\u60f3|\u5728" ##[\n ]|读|过|想|在
  Encoding(pa)<-"UTF-8"
  reading_date<-gsub(pa,"",b[which(b=="date")-1])
  tagg<-"\u6807\u7b7e: " ;Encoding(tagg)<-"UTF-8"  ##标签: 
  user_tags<-gsub(tagg,"",b[which(b=="tags")-1])
  if(length(user_tags)==0){user_tags<-NA}
  comment<-gsub("[\n ]","",b[which(b=="comment")-1])
  if(length(comment)==0){comment<-NA}
  tmp<-get.book.info(bookid)
  title<-tmp$title
  summary<-tmp$summary
  author<-paste(tmp$author,collapse=" ")
  average_rating<-tmp$rating[3]
  image<-tmp$image["medium"]
  pages<-tmp$attribute$pages
  price<-gsub("\u5143","",tmp$attribute$price) #元
  out<-c(bookid,title,author,user_rating,reading_date,user_tags,pub,
         comment,summary,average_rating,image,pages,price)
  names(out)<-NULL
  return(out)
}
########################################################
##coll<-.user_book_what0(userid="qxde01",count=15,what="collect")
##@what:do,collect,wish 
.user_book_what0<-function(userid,count=15,what="collect",verbose=TRUE){
  u<-paste0('http://book.douban.com/people/',userid,'/',what)
  p<-.refreshURL(u)
  tag_label<-sapply(getNodeSet(p, '//ul[@class="tag-list mb10"]//a'),xmlValue)
  tag_freq<-as.integer(sapply(getNodeSet(p, '//ul[@class="tag-list mb10"]//span'),
                              xmlValue))
  tags<-data.frame(tag_label=tag_label,
                   tag_freq=tag_freq, stringsAsFactors=F)
  #####
  ##\\((.*?)\\)  <[^<>]*> \\([^)]+\\) "(\{[^}]+\})(/[^/{}]*/?)?"
  total<-sapply(getNodeSet(p, '//head//title'),xmlValue)
  total<-substr(total,regexpr("\\([^)]+\\)",total),nchar(total))
  total<-gsub("[^0-9]","",total)
  cat("\n--------There is a total of ",total," ",what,"books.----------\n")
  pages<-ceiling(as.integer(total)/count)
  df<-data.frame(matrix(nrow=pages*count,ncol=13),stringsAsFactors=F)
  colnames(df)<-c("bookid","title","author","user_rating","reading_date",
                  "user_tags","pub","comment","summary","average_rating",
                  "image","pages","price")
  k=1
  if(pages>0){
    for(pg in 1:pages){
      u<-paste0('http://book.douban.com/people/',userid,'/',what,
                "?start=",(pg-1)*15,"&sort=time&rating=all&filter=all&mode=grid")
      if(verbose==TRUE){
        cat("Getting ",what," book infomation from:",u,"...\n")
      }
      p<-.refreshURL(u)
      node<-getNodeSet(p, '//li[@class="subject-item"]//div[@class="info"]')
      df0<-t(sapply(node,.extract_book))
      nr=nrow(df0)  
      df[k:(k+nr-1),]<-df0
      k=k+nr
    }
  }
  df<- df[!is.na(df[, 1]), ]
  return(list(tags=tags,df=df))
}
########################################
## 获取所有发表过的书评
##myrev<-.user_book.review0(userid="qxde01")
.user_book.review0<-function(userid,count=5,verbose=TRUE){
  u<-paste0("http://book.douban.com/people/",userid,"/reviews")
  p<-.refreshURL(u)
  total<-sapply(getNodeSet(p, '//head//title'),xmlValue)
  total<-substr(total,regexpr("\\([^)]+\\)",total),nchar(total))
  total<-as.integer(gsub("[^0-9]","",total))
  cat("\n--------There is a total of ",total,"reviews.--------\n")
  pages<-ceiling(total/count)
  out <- data.frame(matrix(nrow = pages * count, ncol = 9), 
                    stringsAsFactors = F)
  colnames(out) <- c("review_uri", "title", "published", "author", 
                     "author_uri", "review", "rating", "useful", "unuseful")
  k = 1
  if(pages>0){
    for(pg in 1:pages){
      u = paste0("http://book.douban.com/people/", userid, 
                 "/reviews?start=", (pg - 1) * count)
      cat("Getting review URLs from page ", pg, ": ", u, " ...\n")
      p <- .refreshURL(u, verbose)
      href <- sapply(getNodeSet(p, '//div[@class="tlst"]//a'), function(x) xmlGetAttr(x, "href"))
      href <- unique(href[grep("/review/", href)])
      href <- href[!href %in% out$review_uri]
      n <- length(href)
      if (n > 0) {
        for (i in 1:n) {
          u0 <- href[i]
          if (verbose == TRUE) {
            cat("   Getting ", k, " movie review from URL: ", u0, " ...\n")
          }
          out0 <- .get_book_review0(u = u0, verbose)
          if (length(out0) == 9) {
            out[k, ] <- out0
            k <- k + 1
          }
          else {
            cat("  !!!! Getting  failed at URL: ", u0," \n")
          }
        }
      }
    }
  }
  out <- out[!is.na(out[, 1]), ]
  return(out)
}
###########################################
# myann<-.user_book.annotation0(userid="qxde01")
.user_book.annotation0<-function(userid,count=5,verbose=TRUE){
  u<-paste0("http://book.douban.com/people/",userid,"/annotation/")
  p<-.refreshURL(u)
  total<-sapply(getNodeSet(p, '//head//title'),xmlValue)
  total<-substr(total,regexpr("\\([^)]+\\)",total),nchar(total))
  total<-as.integer(gsub("[^0-9]","",total))
  cat("\n--------There is a total of ",total,"annotation.--------\n")
  pages<-ceiling(total/count)
  out <- data.frame(matrix(nrow = pages * count, ncol = 9), 
                    stringsAsFactors = F)
  colnames(out) <- c("note_uri", "title", "published", "author", 
                     "author_uri", "note", "rating", "readers", "collectors")
  k=1
  if(pages>0){
    for(pg in 1:pages){
      #http://book.douban.com/people/Quantum_Panda/annotation/?start=5
      u = paste0("http://book.douban.com/people/", userid, 
                 "/annotation/?start=", (pg - 1) * count)
      cat("Getting note URLs from page", pg, ": ", u, "...\n")
      p <- .refreshURL(u, verbose)
      href <- sapply(getNodeSet(p,'//div[@class="annotations-context"]//a[@href]'), 
                     function(x) xmlGetAttr(x, "href"))
      href <- unique(href[grep(".com/annotation/", href)])
      href <- href[-grep("#comments", href)]
      href <- href[!href %in% out$note_uri]
      n = length(href)
      if (n > 0) {
        for (i in 1:n) {
          u0 <- href[i]
          if (verbose == TRUE) {
            cat("   Getting ", k, " book note from URL: ",  u0, " ...\n")
          }
          out0 <- .get_book_note0(u = u0, fresh, verbose)
          if (length(out0) == 9) {
            out[k, ] <- out0
            k = k + 1
          }
          else {
            cat("  !!!! Getting  failed at URL: ", u0," \n")
          }
        }
      }
    }
  }
  out <- out[!is.na(out[, 1]), ]
  return(out)
}
##############################################
##@userid
##@front  
## qxde<-user_book_status(userid="qxde01")
## im<-qxde[[9]]
#png("front.png",width=60*9,height=80*8)
#display(combine(im),method="raster",all=T)
#dev.off()
user_book_status<-function(userid,verbose=TRUE,front=TRUE){
  cat("--------Retrieving information about read books.--------\n")
  tmp<-.user_book_what0(userid,count=15,what="collect",verbose)
  collect_tags<-tmp$tags
  collect_df<-tmp$df
  cat("--------Retrieving information about reading books.--------\n")
  tmp<-.user_book_what0(userid,count=15,what="do",verbose)
  do_tags<-tmp$tags
  do_df<-tmp$df
  cat("--------Retrieving information about the books you want to read.--------\n")
  tmp<-.user_book_what0(userid,count=15,what="wish",verbose)
  wish_tags<-tmp$tags
  wish_df<-tmp$df
  cat("--------Retrieving your published book reviews. --------\n")
  reviews<-.user_book.review0(userid,verbose)
  cat("--------Retrieving your reading notes.--------\n")
  notes<-.user_book.annotation0(userid,verbose)
  
  collect_imags<-list()
  if(front==TRUE){
    if(!require(EBImage)){
      source("http://bioconductor.org/biocLite.R")
      biocLite("EBImage")
      require(EBImage)
    }
    ##cat("--------正在获取已读书籍的封面图片--------...\n")
    cat("--------\u6b63\u5728\u83b7\u53d6\u5df2\u8bfb\u4e66\u7c4d\u7684\u5c01\u9762\u56fe\u7247--------...\n")
    images<-collect_df$image
    m<-length(images)
    for(i in 1:m){
     collect_imags[[i]]<-resize(readImage(images[i]),w=60,h=80) 
    }
  }
  list(collect_tags=collect_tags,
       collect_df=collect_df,
       do_tags=do_tags,
       do_df=do_df,
       wish_tags=wish_tags,
       wish_df=wish_df,
       reviews=reviews,
       notes=notes,
       collect_imags=collect_imags)
}
