Rdouban
=======
##获取豆瓣网评论数据的R接口
> ####非官方接口
* douban_user_statuses  获取豆瓣用户的评论、日记等信息
* get_book_info	获取豆瓣图书基本信息
* get_book_reviews	获取豆瓣图书的评论文章
* get_book_discussions	获取豆瓣图书的话题讨论内容
* get_book_notes	获取豆瓣图书的读书笔记
* get_movie_info	获取豆瓣影视的基本信息
* get_movie_reviews	获取豆瓣电影的长篇影评
* get_movie_comments	获取豆瓣电影的短评
* get_movie_discussions	获取关于某个豆瓣电影的话题讨论内容
* get_music_info	获取豆瓣音乐的专辑信息
* get_music_reviews	获取豆瓣音乐评论信息.
* get_music_discussions	获取豆瓣音乐专辑的话题讨论内容


> ####下面的函数是上述部分函数的重写，部分采用了官方API
* get.book.info  豆瓣图书信息API V2
* get.movie.info  豆瓣电影信息API V2
* get.music.info  豆瓣音乐信息API V2
* get.book.review  豆瓣图书评论信息,部分采用API V1
* get.movie.review  豆瓣电影长篇评论,通过移动网页m.douban.com,会被豆瓣阻止
* get.movie.comment  豆瓣电影短评,通过移动网页m.douban.com

>#### 安装(linux)
library("devtools")
install_github("Rdouban","qxde01")