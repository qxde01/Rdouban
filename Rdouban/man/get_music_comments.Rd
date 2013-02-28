\encoding{UTF-8}
\name{get_music_comments}
\alias{get_music_comments}

\title{获取豆瓣音乐评论信息.}
\description{获取豆瓣音乐评论信息.}
\usage{get_music_comments(musicid,n=100,verbose=TRUE,...)}

\arguments{一个list,包含：
\item{musicid}{豆瓣音乐ID}
\item{n}{获取评论的篇数,默认100}
\item{verbose}{若\code{verbose=TRUE则显示过程信息，否则不显示，默认\code{TRUE}}}
}
 
\value{
  \item{music_title}{音乐专辑名称}
  \item{comments_amount}{参与评分的人数}
  \item{rating}{评分信息,\code{votes_amount}评论用户中参与评论的人数,\code{starsx}分别对应各级评分的人数}
  \item{comments}{一个data.frame，共9列：
    \code{title}评论标题,
    \code{time}评论发表时间,
    \code{nickname}作者昵称,
    \code{comment}评论内容,
    \code{rating}作者评分,
    \code{useful}该评论被赞为有用的次数,
    \code{unuseful}该评论被评为无用的次数,
    \code{author_url}作者主页网址,
    \code{comment_url}该评论主页网址}
}

\author{qxde01<\email{qxde01@@gmail.com}>
}


\note{若实际影评数小于n,则获取的是实际数,若实际数大于n,则获取到的数量是
\code{ceiling(n/25)*25}.}


\examples{
 ## http://music.douban.com/subject/3843530/reviews
 \dontrun{get_music_comments(musicid=3843530,n=50,verbose=T)}
}

\keyword{music}
\keyword{comment}
