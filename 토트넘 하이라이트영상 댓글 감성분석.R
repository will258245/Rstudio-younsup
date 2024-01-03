#한국어 감성분석 사전 가져오기
library(readr)
positive<- read_lines("pos_pol_word.txt")
negative<- read_lines("neg_pol_word.txt")

#감성분석하기 위한 function 만들기
library(plyr)

#이모티콘을 제거하는 함수
library(stringr)

remove_emojis <- function(text) {
  # 정규 표현식을 사용하여 이모티콘 제거
  cleaned_text <- gsub("[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\u2600-\u26FF\u2700-\u27BF\u2300-\u23FF\u2B50]", "", text, perl = TRUE)
  
  return(cleaned_text)
}

sentimental = function(sentences, positive, negative){
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '  ', sentence) # 문장부호 제거 (POSIX 정규식을 활용)
    sentence = gsub('[[:cntrl:]]', '  ', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '  ', sentence)        # 숫자 제거
    sentence = remove_emojis(sentence)           # 이모티콘 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 \\s+(공백 정규식) +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    # unlist()는 텍스트분석에서 가장 중요한 함수이기도 합니다. 
    # list로 주욱 연결된 텍스트를 객체로 변환하여 개수를 셀 수 있게 해주기 때문입니다.
    
    pos.matches = match(words, positive)           # 긍정어를 pos.matches에 저장합니다
    neg.matches = match(words, negative)           # 부정어를 neg.matches에 저장합니다
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # (긍정 - 부정)하여 숫자를 계산합니다.
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
} # 전체 함수를 실행합니다.



###유튜브 API
#install.packages("tuber")
library(tuber)
library(plyr)

app_id = "803819309217-elpi4pc0bagcvo5l2gg7lpiib48nhucb.apps.googleusercontent.com"
app_secret = "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"

yt_oauth(app_id = app_id,
         app_secret = app_secret,
         token = "")

##유튜브 채널 정보 가져오기
youtuber_meta = data.frame(channel = c("Tottenham Hotspur"),
                           channel_id = "UCEg25rdRZXg32iwai6N6l0w")
ytber_id = get_channel_stats(channel_id = youtuber_meta$channel_id[1])
ytber_stats = data.frame(channel = youtuber_meta$channel[1], ytber_id$statistics)
ytber_stats


##영상 댓글 수집하기 
##토트넘VS브렌트포트
cmt_df = get_all_comments(video_id = "Cp32ljqj5gQ")
TotVsBfc<- cmt_df$textOriginal

##토트넘vs맨유
TOTvsMUFC_df<-get_all_comments(video_id = "_Pu-vYNOw4Q")
TOTvsMUFC<- TOTvsMUFC_df$textOriginal

##토트넘vs본머스
TOTvsBOU_df<- get_all_comments(video_id = "_7NZj67J5R4")
TOTvsBOU<- TOTvsBOU_df$textOriginal

##토트넘vs번리
TOTvsBUR_df<- get_all_comments(video_id = "43FZXOo6oRM")
TOTvsBUR<- TOTvsBUR_df$textOriginal

##토트넘vs셰필드
TOTvsSHU_df<- get_all_comments(video_id = "Q393GX9jEcE")
TOTvsSHU<- TOTvsSHU_df$textOriginal

##토트넘vs아스날
TOTvsARS_df<- get_all_comments(video_id = "LPtCr_l2LM8")
TOTvsARS<- TOTvsARS_df$textOriginal

##토트넘vs리버풀
TOTvsLIV_df<- get_all_comments(video_id = "8LSccN4_ZfI")
TOTvsLIV<- TOTvsLIV_df$textOriginal

##토트넘vs루턴
TOTvsLUT_df<- get_all_comments(video_id = "YbU7dQ_L5sA")
TOTvsLUT<- TOTvsLUT_df$textOriginal

##토트넘vs풀럼
TOTvsFUL_df<- get_all_comments(video_id = "Jmnoe1zompU")
TOTvsFUL<- TOTvsFUL_df$textOriginal

##토트넘vs팰리스
TOTvsCRY_df<- get_all_comments(video_id = "yCXJzcHq3TA")
TOTvsCRY<- TOTvsCRY_df$textOriginal

##토트넘VS첼시
TOTvsCFC_df<- get_all_comments(video_id = "aSQjMW7UrTE")
TOTvsCFC<- TOTvsCFC_df$textOriginal

##토트넘vs울버햄튼
TOTvsWOL_df<- get_all_comments(video_id = "U0jGiGYkIEM")
TOTvsWOL<- TOTvsWOL_df$textOriginal

##토트넘vs아스톤빌라
TOTvsAVL_df<- get_all_comments(video_id = "uqkzNlQTHyY")
TOTvsAVL<- TOTvsAVL_df$textOriginal

##토트넘vs맨시티
TOTvsMCT_df<- get_all_comments(video_id = "bE-HCF97Iw4")
TOTvsMCT<- TOTvsMCT_df$textOriginal

##토트넘vs웨스트햄
TOTvsWHU_df<- get_all_comments(video_id = "CYVzjRD2-yI")
TOTvsWHU<- TOTvsWHU_df$textOriginal

##토트넘vs뉴캐슬
TOTvsNEW_df<- get_all_comments(video_id = "tk0JD3Izb6A")
TOTvsNEW<- TOTvsNEW_df$textOriginal

##토트넘vs노팅엄
TOTvsNFO_df<- get_all_comments(video_id = "vgxGRkhbWuk")
TOTvsNFO<- TOTvsNFO_df$textOriginal

##토트넘vs에버튼
TOTvsEVE_df<- get_all_comments(video_id = "SnZdcOyefLs")
TOTvsEVE<- TOTvsEVE_df$textOriginal


#감성분석
result = sentimental(TotVsBfc, positive, negative)#브렌트포드전(무)
result = sentimental(TOTvsMUFC, positive, negative)#맨유전(승)
result = sentimental(TOTvsBOU, positive, negative)#본머스(승)
result = sentimental(TOTvsBUR, positive, negative)#번리(승)
result = sentimental(TOTvsSHU, positive, negative)#셰필드(승)
result = sentimental(TOTvsARS, positive, negative)#아스날(무)
result = sentimental(TOTvsLIV, positive, negative)#리버풀(승)
result = sentimental(TOTvsLUT, positive, negative)#루턴(승)
result = sentimental(TOTvsFUL, positive, negative)#풀럼(승)
result = sentimental(TOTvsCRY, positive, negative)#팰리스(승)
result = sentimental(TOTvsCFC, positive, negative)#첼시전(패)
result = sentimental(TOTvsWOL, positive, negative)#울버햄튼(패)
result = sentimental(TOTvsAVL, positive, negative)#아스톤빌라(패)
result = sentimental(TOTvsMCT, positive, negative)#맨시티(무)
result = sentimental(TOTvsWHU, positive, negative)#웨스트햄(패)
result = sentimental(TOTvsNEW, positive, negative)#뉴캐슬(승)
result = sentimental(TOTvsNFO, positive, negative)#노팅엄(승)
result = sentimental(TOTvsEVE, positive, negative)#에버튼(승)
#위의 결과를 엑셀로 저장


#단어의 긍정, 부정 점수를 확인할 수 있습니다.
result$remark[result$score >=1] = "긍정"
result$remark[result$score ==0] = "중립"
result$remark[result$score < 0] = "부정"

sentiment_result= table(result$remark)
sentiment_result


pie(sentiment_result, main = "감성분석 결과",
    col = c("blue", "red", "green"))


#댓글 감성분석 총정리df
library(readxl)
library(tidyverse)
result.all <- read_excel("토트넘 하이라이트 댓글 감성분석 결과.xlsx")

result.all<- result.all %>%
  mutate(Number = c(1:18))


#긍.부정어 빈도수 막대그래프프
poswordss<- na.omit(pos.matches) #긍정어 빈도수 막대그래프
poswordsss<- positive[poswordss]
postable<- table(poswordsss)
postable<- postable[postable>1]
possorttable<- sort(postable, decreasing = T)
barplot(possorttable)
possorttabledf<- as.data.frame(possorttable)

negwordss<- na.omit(neg.matches) #부정어 빈도수 막대그래프
negwordsss<- negative[negwordss]
negtable<- table(negwordsss)
negtable<- negtable[negtable>1]
negsorttable<- sort(negtable, decreasing = T)
barplot(negsorttable)
negsorttabledf<- as.data.frame(negsorttable)


###그래프그리기
# 긍정, 부정, 중립댓글 누적막대그래프
result.all.prob<- result.all %>%
  select(ppos, pneg, pmid, Number) %>%
  gather(key = "Number", value = "Prob") %>%
  mutate(GNumber= rep(c(1:18), times=3)) %>%
  arrange(Number)

result.all.prob %>%
  ggplot(aes(x=GNumber, y=Prob, fill=Number)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "누적 막대 그래프 (전체 100%)", x = "경기", y = "누적 확률") +
  theme(axis.text = element_text(size = 15)) +
  scale_fill_manual(values = 
                      c("pmid" = "darkgray", "pneg"="hotpink", "ppos"="skyblue"))



#긍정/부정 그래프
result.all %>%
  ggplot(aes(x=Number, y=posneg)) +
  geom_line(color="skyblue", linewidth=1) +
  geom_point(size=2)+
  xlab("경기") +
  ylab("부정댓글 하나당 긍정댓글수") +
  theme(axis.text = element_text(size = 20))

#승리여부와 긍정/부정
result.all<- result.all %>%
  mutate(
    is.win=ifelse(game == "win", "win", "no-win"),
    is.win=as.factor(is.win)
  ) 

result.all %>%
  ggplot(aes(x=is.win, y=posneg)) +
  geom_point(size=2) +
  geom_segment(aes(x=1, y=1.1334, xend=2, yend=2.7042), 
               color="red", linewidth=1.5) +
  xlab("승리여부") +
  ylab("부정댓글 하나당 긍정댓글수") +
  theme(axis.text = element_text(size = 20))

fit1<- glm(posneg ~ is.win, data = result.all)
summary(fit1) #유의

fit123<-glm(posneg ~is.win + sgoalassi, data = result.all)
summary(fit123)
fit1234<-glm(posneg ~ is.win+sgoalassi + is.win*sgoalassi, data=result.all)
summary(fit1234)

#손흥민 활약과 긍정/부정 산점도
result.all %>%
  ggplot(aes(x=sgoalassi, y=posneg)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se=F) +
  labs(title = "손흥민 활약에 따른 긍정/부정 댓글", 
       x = "손흥민 공격포인트", 
       y = "부정댓글 하나당 긍정댓글수") +
  theme(axis.text = element_text(size = 15))

fit2<- glm(posneg ~ sgoalassi, data = result.all)
summary(fit2)#유의


#팀의 승패와 댓글수
result.all %>%
  ggplot(aes(x=is.win, y=ncomment)) +
  geom_point(size=2) +
  labs(title = "팀의 승리 여부에 따른 댓글 갯수", 
       x = "승리여부", y = "총 댓글갯수") +
  theme(axis.text = element_text(size = 15))

fit3<- glm(ncomment ~ is.win, data = result.all)
summary(fit3) #유의하지 않음

fit321<- glm(ncomment ~ is.win+sgoalassi, data = result.all)
summary(fit321)

#손흥민의 활약과 댓글수
result.all %>%
  ggplot(aes(x=sgoalassi, y=ncomment)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se=F) +
  labs(title = "손흥민 활약에 따른 댓글 개수", 
       x = "손흥민 공격포인트 개수", y = "총 댓글 개수") +
  theme(axis.text = element_text(size = 15))

fit4<- glm(ncomment ~ sgoalassi, data = result.all)
summary(fit4) #유의
