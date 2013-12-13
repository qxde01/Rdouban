\encoding{UTF-8}
\name{user_book_status}
\alias{user_book_status}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
豆瓣用户的读书信息
}
\description{
豆瓣用户的读书信息，包括已读书籍、发表书评、笔记等。
}
\usage{
user_book_status(userid,verbose=TRUE,front=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{userid}{豆瓣用户id}
  \item{verbose}{若为\code{TRUE}则显示详细过程信息,否则不显示，默认\code{TRUE}}
  \item{front}{若为TRUE,则下载已读书籍的封面图片,否则不下载}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{一个list，各项为：

  \item{collect_tags }{\code{data.frame},已读书籍的标签}
  \item{collect_df}{\code{data.frame},已读书籍的信息}
  \item{do_tags}{\code{data.frame},正在阅读书籍的标签}
  \item{do_df}{\code{data.frame},正在阅读书籍的信}
  \item{wish_tags}{\code{data.frame},想读书籍的标签}
  \item{wish_df}{\code{data.frame},想读书籍的信息}
  \item{reviews}{\code{data.frame},发表的书评,各列同\code{get_book_reviews}}
  \item{notes}{\code{data.frame},发表的读书笔记,各列同\code{get_book_notes}}
  \item{collect_images}{\code{list},已读书籍的封面图片,分辨率为\code{60x80}}
}

\author{
qxde01<\email{qxde01@gmail.com}>
}

\examples{
## http://book.douban.com/people/qxde01
\dontrun{qxde<-user_book_status(userid="qxde01")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ douban }
\keyword{ book }% __ONLY ONE__ keyword per line
