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
  \item{movie_title}{电影名称}
  \item{movie_base_info}{一个字符串,豆瓣影视剧的基本信息,包括电影名称、导演、演员}
  \item{rating}{用户评分信息,\code{score}为总分,\code{votes}指参与的评分人数,\code{starsx}指对应评分用户数所占的比例}
  \item{movie_intro}{电影或电视剧简介}
  \item{labels_amount}{用户常用标签数量}
  \item{labels}{一个data.frame,\code{label_name}标签名称及对应的使用频率\code{label_freq}}
  \item{long_comment_votes}{发表长影评的用户评分情况}
  \item{comments_amount}{分别为短影评数量\code{short_amount},长影评数量\code{long_amount},讨论话题数量\code{discussion_amount}}
  \item{audience}{观众信息,\code{doings}指正在观看的用户数量,\code{collections}指已观看的用户数量,\code{wishes}指想看的用户数量}
}
\author{<\email{qxde01@@gmail.com}>
}
\note{\code{movieid}必须是豆瓣电影或电视剧有效id,比如\link{http://movie.douban.com/subject/5308265/}的ID=5308265.
}

\examples{
## http://movie.douban.com/subject/10527209/
\dontrun{get_movie_info(movieid='5308265')}
}

\keyword{movie}
