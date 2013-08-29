\encoding{UTF-8}
\name{get_movie_info}
\alias{get_movie_info}
\title{获取豆瓣影视的基本信息}
\description{获取豆瓣影视剧的基本信息,包括电影名称、导演、演员、豆瓣评分等信息.
}
\usage{
get_movie_info(movieid,...)
}
\arguments{
  \item{movieid}{豆瓣电影和电视剧的ID,由数字组成的字符串,可通过每部电影或电视剧的豆瓣主页查询.}
}
\value{一个列表(list), 包括:
  \item{title}{电影名称}
  \item{author}{导演}
  \item{rating}{用户评分信息,\code{average}为总分,\code{votes}指参与的评分人数,\code{starsx}指对应评分用户数所占的比例}
  \item{summary}{电影或电视剧简介}
  \item{tags}{\code{tag_label}标签名称及对应的使用频率\code{tag_freq}}
  \item{href}{电影主页URL}
  \item{image}{电影海报图片URL}
  \item{reviews_total}{长影评数量}
  \item{comments_total}{短影评数量}
  \item{audience}{观众信息,\code{doings}指正在观看的用户数量,\code{collections}指已观看的用户数量,\code{wishes}指想看的用户数量}
  \item{attribute}{一个字符串,豆瓣影视剧的基本信息,包括电影名称、导演、演员}
}
\author{<\email{qxde01@@gmail.com}>
}

\examples{
## http://movie.douban.com/subject/5308265/
\dontrun{m<-get_movie_info(movieid='5308265')}
}
\keyword{movie}
