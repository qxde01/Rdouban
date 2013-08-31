\encoding{UTF-8}
\name{get_music_reviews}
\alias{get_music_reviews}
\title{获取豆瓣音乐评论信息.}
\description{获取豆瓣音乐评论信息.}
\usage{
get_music_reviews(musicid,results = 100, fresh = 10,count=25, verbose = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{musicid}{豆瓣音乐ID}
  \item{results}{获取评论的数量,默认100}
  \item{fresh}{页面异常时最大刷新次数,默认10}
  \item{count}{每页review_uri数量,默认25，最大25}
  \item{verbose}{若\code{verbose=TRUE则显示详细过程信息，否则不显示，默认\code{TRUE}}}
}
\value{一个data.frame，各列为：
 \item{review_uri }{评论主页URL}
 \item{title }{评论文章名称}
  \item{published }{文章发表时间}
 \item{author}{文章作者昵称}
 \item{author_uri }{作者豆瓣URI}
 \item{review }{评论文章详细内容}
 \item{rating }{作者对电影的评分}
  \item{useful}{被评为useful的次数}
  \item{unuseful}{被评为unuseful的次数}  
}
\author{
qxde01<\email{qxde01@@gmail.com}>
}

\examples{
 ## http://music.douban.com/subject/3843530/reviews
 \dontrun{x<-get_music_reviews(musicid=3843530,results = 100)}
}
\keyword{music}
\keyword{review}
