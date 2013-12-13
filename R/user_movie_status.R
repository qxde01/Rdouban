## extract some infomation about a movie
.extract_movie<-function(x){
  b=unlist(xmlToList(x))
  Encoding(b)<-"UTF-8"
  movieid<-gsub("[^0-9]","",b["div.a..attrs.href"])
  user_rating<-b[b %in% c("rating1-t","rating2-t",
                          "rating3-t","rating4-t","rating5-t")]
  user_rating<-gsub("[^0-9]","",user_rating)
  if(length(user_rating)==0){user_rating<-NA}

  watching_date<-b[which(b=="date")-1]
  #if(length(watching_date)==0)watching_date<-NA
  tagg<-"\u6807\u7b7e: " ;Encoding(tagg)<-"UTF-8"  ##标签: 
  user_tags<-gsub(tagg,"",b[which(b=="tags")-1])
  if(length(user_tags)==0){user_tags<-NA}
  comment<-gsub("[\n ]","",b[which(b=="comment")-1])
  if(length(comment)==0){comment<-NA}
  
  tmp<-get.movie.info(movieid)
  title<-tmp$title
  summary<-tmp$summary
  author<-paste(tmp$author,collapse=",")
  average_rating<-tmp$rating[2]
  image<-tmp$image
  movie_type<-paste(tmp$attribute$movie_type,collapse=' ')
  year<-tmp$attribute$year
  cast<-paste(tmp$attribute$cast,collapse=',')
  tags<-paste(tmp$tags[,1],collapse=' ')
  duration<-tmp$attribute$movie_duration[1]
  if(length(duration)==0){duration<-NA}
  out<-c(movieid,title,author,user_rating,user_tags,
         comment,summary,average_rating,image,cast,tags,duration)
  names(out)<-NULL
  return(out)
}
########################################################
##coll<-.user_movie_what0(userid="",count=15,what="collect")
##@what:do,collect,wish 
.user_movie_what0<-function(userid,count=15,what="collect",verbose=TRUE){
  u<-paste0('http://movie.douban.com/people/',userid,'/',what)
  p<-.refreshURL(u)
  tag_label<-sapply(getNodeSet(p, '//ul[@class="tag-list mb10"]//a'),xmlValue)
  tag_freq<-as.integer(sapply(getNodeSet(p, '//ul[@class="tag-list mb10"]//span'),
                              xmlValue))
  tags<-data.frame(tag_label=tag_label,
                   tag_freq=tag_freq, stringsAsFactors=F)
  #####
  total<-sapply(getNodeSet(p, '//head//title'),xmlValue)
  total<-substr(total,regexpr("\\([^)]+\\)",total),nchar(total))
  total<-gsub("[^0-9]","",total)
  cat("\n--------There is a total of ",total," ",what,"movies.----------\n")
  pages<-ceiling(as.integer(total)/count)
  df<-data.frame(matrix(nrow=pages*count,ncol=13),stringsAsFactors=F)
  colnames(df)<-c("movieid","title","author","user_rating","user_tags",
                  "comment","summary","average_rating","image",
                  "cast","tags","duration","watching_date")
  k=1
  if(pages>0){
    for(pg in 1:pages){
      u<-paste0('http://movie.douban.com/people/',userid,'/',what,
                "?start=",(pg-1)*15,"&sort=time&rating=all&filter=all&mode=grid")
      if(verbose==TRUE){
        cat("Getting ",what," movies infomation from:",u,"...\n")
      }
      p<-.refreshURL(u)
      node<-getNodeSet(p, '//div[@class="item"]')
      watching_date<-sapply(getNodeSet(p, '//li//span[@class="date"]'),xmlValue)
      
      df0<-t(sapply(node,.extract_movie))
      df0<-cbind(df0,watching_date)
      nr=nrow(df0)  
      df[k:(k+nr-1),]<-df0
      k=k+nr
    }
  }
  df<- df[!is.na(df[, 1]), ]
  return(list(tags=tags,df=df))
}

########################################
## 获取所有发表过的影评
.user_movie.review0<-function(userid,count=5,verbose=TRUE){
  u<-paste0("http://movie.douban.com/people/",userid,"/reviews")
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
      u = paste0("http://movie.douban.com/people/", userid, 
                 "/reviews?start=", (pg - 1) * count)
      cat("Getting review URLs from page ", pg, ": ", u, " ...\n")
      p <- .refreshURL(u, verbose)
      href <- sapply(getNodeSet(p, '//li[@class="nlst"]//a'), function(x) xmlGetAttr(x, "href"))
      href <- unique(href[grep("/review/", href)])
      href <- href[!href %in% out$review_uri]
      n <- length(href)
      if (n > 0) {
        for (i in 1:n) {
          u0 <- href[i]
          if (verbose == TRUE) {
            cat("   Getting ", k, " movie review from URL: ", u0, " ...\n")
          }
          out0 <- .get_movie_review0(u = u0, verbose)
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

##############################################
##@userid
##@front  
user_movie_status<-function(userid,verbose=TRUE,front=TRUE){
  cat("--------Retrieving information about read movies.--------\n")
  tmp<-.user_movie_what0(userid,count=15,what="collect",verbose)
  collect_tags<-tmp$tags
  collect_df<-tmp$df
  cat("--------Retrieving information about reading movies.--------\n")
  tmp<-.user_movie_what0(userid,count=15,what="do",verbose)
  do_tags<-tmp$tags
  do_df<-tmp$df
  cat("--------Retrieving information about the movies you want to read.--------\n")
  tmp<-.user_movie_what0(userid,count=15,what="wish",verbose)
  wish_tags<-tmp$tags
  wish_df<-tmp$df
  cat("--------Retrieving your published movie reviews. --------\n")
  reviews<-.user_movie.review0(userid,verbose)

  collect_images<-list()
  if(front==TRUE){
    if(!require(EBImage)){
      source("http://bioconductor.org/biocLite.R")
      biocLite("EBImage")
      require(EBImage)
    }
    ##cat("--------正在获取已观电影的海报图片--------...\n")
    cat("--------\u6b63\u5728\u83b7\u53d6\u5df2\u89c2\u7535\u5f71\u7684\u6d77\u62a5\u56fe\u7247--------...\n")
    images<-collect_df$image
    m<-length(images)
    for(i in 1:m){
      collect_images[[i]]<-resize(readImage(images[i]),w=60,h=80) 
    }
  }
  list(collect_tags=collect_tags,
       collect_df=collect_df,
       do_tags=do_tags,
       do_df=do_df,
       wish_tags=wish_tags,
       wish_df=wish_df,
       reviews=reviews,
       collect_images=collect_images)
}
