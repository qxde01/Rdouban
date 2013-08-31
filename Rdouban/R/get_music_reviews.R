##http://music.douban.com/subject/3843530/reviews?sort=time
#x<-get_music_reviews(musicid=3843530,results = 100,verbosr=T)
.get_music_review0 <- function(u, fresh = 10, verbose = TRUE, ...) {
  p <- .refreshURL(u = u, fresh, verbose)
  ## review_id review_id<-gsub('[^0-9]','',u) title
  title <- sapply(getNodeSet(p, '//head//title'), xmlValue)
  ## 发表时间
  published <- sapply(getNodeSet(p, "//span[@property=\"v:dtreviewed\"]"), xmlValue)
  ## 作者,URI
  author <- sapply(getNodeSet(p, '//span[@property="v:reviewer"]'), xmlValue)
  author_uri <- sapply(getNodeSet(p, '//div[@id="content"]//span[@class="pl2"]//a'), 
                       function(x) xmlGetAttr(x,  "href"))[1]
  ## 评分
  rating <- sapply(getNodeSet(p, "//span[@property=\"v:rating\"]"), xmlValue)
  ## 影评内容
  review <- sapply(getNodeSet(p, "//span[@property=\"v:description\"]"), xmlValue)
  if (length(review) == 0) {
    review <- sapply(getNodeSet(p, "//div[@property=\"v:description\"]"), xmlValue)
  }
  #review <- gsub("\r", "", review)
  ## 有用 & 没用的次数
  x0 <- sapply(getNodeSet(p, "//div[@class=\"main-panel-useful\"]//em"), xmlValue)
  useful = x0[1]
  unuseful = x0[2]
  if (length(x0) == 0 ) {
    useful <- sapply(getNodeSet(p, "//span[@class=\"useful\"]//em"), xmlValue)
    unuseful <- sapply(getNodeSet(p, "//span[@class=\"unuseful\"]//em"), xmlValue)
  }
  out <- c(review_uri = u, title = title, published = published, author = author, author_uri = author_uri, 
           review = review, rating = rating, useful = useful, unuseful = unuseful)
  return(out)
}
###########################################################################
get_music_reviews<-function(musicid,results = 100, fresh = 10,count=25, verbose = TRUE,...){
  u = paste0('http://music.douban.com/subject/',musicid,'/reviews?sort=time')
  p <- .refreshURL(u, fresh, verbose)
    
  total<-gsub('[^0-9]','',sapply(getNodeSet(p, '//span[@class="count"]'),xmlValue))
  if (length(total)==0)
    stop('There is no reviews about this music.')
  cat('-----There is a tatal of ',total,' music reviews.-----\n')
  pages<-ceiling(min(results,as.integer(total))/count)
  out <- data.frame(matrix(nrow = pages * count, ncol = 9), stringsAsFactors = F)
  colnames(out) <- c("review_uri", "title", "published", "author", "author_uri", "review", 
                     "rating", "useful", "unuseful")
  ## out nrow index
  k = 1
  if(pages>0){
    for (pg in 1:pages) {
      u=paste0('http://music.douban.com/subject/',musicid,'/reviews?sort=time&start=',
               (pg-1)*count,"&filter=&limit=", count)
      if(verbose==TRUE){
        cat(' Getting music comment URLS from page=',pg-1,': ',u,'...\n')
      }
      p <- .refreshURL(u, fresh, verbose)
      n1 <- getNodeSet(p, '//div[@id="content"]//div[@class="article"]//a[@title]')
      href <- unique(sapply(n1, function(x) xmlGetAttr(x, "href")))
      href <- href[grep("/review/", href)]
      href <- href[!href %in% out$review_uri]
      n = length(href)
      if (n > 0) {
        for (i in 1:n) {
          u0 = href[i]
          if (verbose == TRUE) {
            cat("  %%%% Getting ", k, " music review from URL: ", u0, " ...\n")
          }
          out0 <- tryCatch(.get_music_review0(u = u0, fresh, verbose), error = function(e) {
            NULL
          })
          if (length(out0) == 9) {
            out[k, ] <- out0
            k = k + 1
          }
          if (length(out0) != 9) {
            cat("  !!!! Getting  failed at URL: ", u0, " \n")
          }
        }
      }      
    }
  }
  out <- out[!is.na(out[, 1]), ]
  return(out)
}
