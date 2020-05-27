

  rm(list=ls())
  
  options(scipen=999)
  
  
  
  # install.packages("dplyr")
  # install.packages("VGAM")
  # install.packages("pROC")
  # install.packages("lubridate")
  # install.packages("panelAR")
  # install.packages(c("DBI","RSQLite"))
  
  
  library(dplyr)
  library(tidyverse)
  library(MASS)
  library(VGAM)	
  library(caret)
  library(pROC)
  library(lubridate)
  library(panelAR)
  library(data.table)
  library(ggplot2)
  library(reshape2)
  library(RColorBrewer)
  library(DBI)
  library(RSQLite)

  
  dpath <- "C:\\BA_test\\BA_data"
  
  
  
  
  
  
  
  ########################################################################################################################
  # servicex
  
  
  
  
  # input
  
  user <- read.csv(paste(dpath, "x_users.csv", sep="\\"), header=T, stringsAsFactors=F)
  prod <- read.csv(paste(dpath, "x_product.csv", sep="\\"), header=T, stringsAsFactors=F)
  tist <- read.csv(paste(dpath, "x_transaction_history.csv", sep="\\"), header=T, stringsAsFactors=F)
  
  servicex <- merge(merge(tist,user,all=T, by="uid"),prod,all=T,by="pid")
    servicex <- servicex %>% arrange(uid,pid,tid)
    servicex <- servicex[,c("tid"
                          , "uid", "gender_cd", "age"
                          , "pid", "product_end_at", "info1", "info2"
                          , "channel", "status", "amount", "created_at")]
    servicex$product_end_at <- as.POSIXct(servicex$product_end_at, format="%Y-%m-%d %H:%M:%S")
    servicex$created_at <- as.POSIXct(servicex$created_at, format="%Y-%m-%d %H:%M:%S")
    
    servicex <- servicex %>% distinct(uid,pid,status, .keep_all=T)
    servicex <- cbind(servicex, "aid"=NA, "funnel"=NA)
      servicex[which(servicex$status=="READY"),"funnel"] <- 1
      servicex[which(servicex$status=="FAILED"),"funnel"] <- 2
      servicex[which(servicex$status=="COMPLETED"),"funnel"] <- 3
      servicex[which(servicex$status=="CANCELED"),"funnel"] <- 4
      servicex[which(servicex$status=="REFUNDED"),"funnel"] <- 5
      servicex$aid <- paste(servicex$uid,servicex$pid,sep="_") # action id
  head(servicex);str(servicex)
  
  
  
  
  
  # EDA (service X)
  
  attach(servicex)
  table(gender_cd)
  hist(age);mean(age);sd(age)

  table(pid);mean(table(pid));barplot(table(pid))
  table(channel)
  table(status)
  table(channel, status)
  table(info1);table(info2);table(info1,info2)
  table(amount)
  table(status,amount)
    
  summary(product_end_at)
  summary(created_at)
  detach(servicex)

  
  servicex %>% group_by(channel) %>% summarise(mean(age),sd(age))
  servicex %>% group_by(gender_cd) %>% summarise(mean(age),sd(age))
  servicex %>% group_by(status) %>% summarise(mean(age),sd(age))
  
  servicex %>% group_by(channel) %>% summarise(mean(amount),sd(amount))
  servicex %>% group_by(gender_cd) %>% summarise(mean(amount),sd(amount))
  servicex %>% group_by(status) %>% summarise(sum(amount),mean(amount),sd(amount))

  table(servicex[which(servicex$status=="CANCELED")-1,"status"])
  table(servicex[which(servicex$status=="FAILED")-1,"status"])
  
  ## ERROR
  ## 1. READY duplication
  ## 2. CANCELED with no COMPLETED
  
  with(servicex, table(uid,status))
  with(servicex, table(pid,status))
  with(servicex, table(aid,status))
  
  
  
  
  # 1
  
  servicex %>% filter(status=="COMPLETED") %>% summarise(mean(age))
  servicex %>% filter(status=="REFUNDED") %>% summarise(mean(age))

  par(mfrow=c(2,1))
  hist(servicex$age)
  hist(subset(servicex,status=="REFUNDED")$age)  
  par(mfrow=c(1,1))
    
  refuser <- unique(servicex[which(servicex$status=="REFUNDED"),"uid"])
  comuser <- unique(servicex[which(servicex$status=="COMPLETED"),"uid"])
  comuser <- comuser[which(is.na(match(comuser,refuser)))]

  refuser <- cbind(servicex[match(refuser,servicex$uid),], "ref"=1)
  comuser <- cbind(servicex[match(comuser,servicex$uid),], "ref"=0)

  refuser_binary <- rbind(refuser,comuser)
  boxplot(age ~ ref, data=refuser_binary)
  var.test(age ~ ref, data=refuser_binary)
  t.test(age ~ ref, data=refuser_binary, alternative="greater", var.equal=T, conf.level=0.95)

  model11 <- glm(ref ~ age + gender_cd, data=refuser_binary, family=binomial(link="logit"))
  anova(model11, test="Chisq")
  summary(model11)



  taid <- sample(unique(refuser_binary$aid),0.6*length(unique(refuser_binary$aid)))
  train <- refuser_binary[!is.na(match(refuser_binary$aid,taid)),]
  test <- refuser_binary[is.na(match(refuser_binary$aid,taid)),]
  model12 <- glm(ref ~ age + gender_cd, data=train, family=binomial(link="logit"))
  summary(model12)
  pred12 <- ifelse(predict(model12, test, type="response")>0.5,1,0)
  confusionMatrix(as.factor(pred12),as.factor(test$ref))
  
  plot.roc(roc(test$ref,predict(model12, test, type="response")),   
           col="grey", max.auc.polygon=TRUE, auc.polygon=TRUE,
           print.auc=TRUE, print.thres=TRUE, print.thres.pch=19, print.thres.col="red"
           )
  
  
  
  
  # 2
  
  ## funnel
  grp <- servicex$gender_cd
  grp <- servicex$channel
  grp <- format(servicex$product_end_at,"%Y-%m")
  grp <- format(servicex$created_at,"%Y-%m")

  funnel <- dcast(funnel ~ grp, data=data.frame(with(servicex, table(funnel,grp))), value.var="Freq")
  funnel;
  fnrate <- data.frame(round(cbind(c(funnel[1:3,2]/funnel[1,2],funnel[4:5,2]/funnel[3,2])
                                   ,c(funnel[1:3,3]/funnel[1,3],funnel[4:5,3]/funnel[3,3]))
                             ,4)*100
                       ,row.names=c("Ready", "Failed", "Completed", "Canceled", "Refunded"))
  colnames(fnrate) <- names(table(grp))
  fnrate
  
  
  funneld <- dcast(Var1 ~ Var2, data=data.frame(table(servicex$funnel,format(servicex$created_at,"%Y-%m-%d"))), value.var="Freq")
  funneld
  
  
  turnover <- t(funneld[2,-1]/funneld[1,-1])
  plot(as.Date(rownames(turnover)),turnover)
  abline(h=t.test(turnover)$conf.int[1:2], col="red")
  rownames(turnover)[turnover<=t.test(turnover)$conf.int[1]]
  rownames(turnover)[turnover>=t.test(turnover)$conf.int[2]]
  
  
  servicexCC <- subset(servicex, status=="CANCELED")
  with(servicexCC, table(info1,info2)/sum(table(info1,info2)));with(servicex, table(info1,info2)/sum(table(info1,info2)))
  
  par(mfrow=c(2,1))
  plot(with(servicex, table(format(created_at,"%Y-%m-%d"))), type="l")
  plot(with(servicexCC, table(format(created_at,"%Y-%m-%d"))), type="l")
  
  par(mfrow=c(1,1))
  plot(with(subset(servicex, status=="READY"), table(format(created_at,"%H"))), type="l", col="black")
  par(new=T)
  plot(with(subset(servicex, status=="FAILED"), table(format(created_at,"%H"))), type="l", col="grey")
  par(new=T)
  plot(with(subset(servicex, status=="COMPLETED"), table(format(created_at,"%H"))), type="l", col="blue")
  par(new=T)
  plot(with(subset(servicex, status=="CANCELED"), table(format(created_at,"%H"))), type="l", col="red")
  par(new=T)
  plot(with(subset(servicex, status=="REFUNDED"), table(format(created_at,"%H"))), type="l", col="yellow")
  
  with(servicex, table(difftime(created_at[which(status=="FAILED")],created_at[which(status=="FAILED")-1],units="secs")))
  with(servicex, barplot(table(difftime(created_at[which(status=="CANCELED")],created_at[which(status=="CANCELED")-1],units="min"))))
  
  
  
  
  
  ## count model
  countx <- data.frame("uid"=rownames(with(servicex, table(uid,status))),
                       "ncomplete"=with(servicex, table(uid,status))[,"COMPLETED"],
                       "nready"=with(servicex, table(uid,status))[,"READY"],
                       "age"=servicex[match(rownames(with(servicex, table(uid,status))),servicex$uid),"age"],
                       "gender"=servicex[match(rownames(with(servicex, table(uid,status))),servicex$uid),"gender_cd"]
  )
  model21a <- glm(ncomplete ~ nready + age + gender, data=countx, family=poisson(link="log"))
  summary(model21a)
  
  model21b <- glm.nb(ncomplete ~ nready + age + gender, data=countx)
  summary(model21b)
  
  
  
  
  
  # 3 SQL 
  
  connection <- dbConnect(SQLite(),dbname=paste(dpath,"DataBase.sqlite",sep="\\"))
  
  dbWriteTable(connection, "x_users", user)
  dbWriteTable(connection, "x_product", prod)
  dbWriteTable(connection, "x_transaction_history", tist)
  
  q3_1 <- dbGetQuery(connection,"
                     SELECT uid, count(uid)
                      FROM (SELECT *
                        FROM (SELECT *
                          FROM (SELECT * 
                                  FROM x_transaction_history tist
                                  LEFT OUTER JOIN x_users users ON tist.uid=users.uid
                                  LEFT OUTER JOIN x_product prod ON tist.pid=prod.pid
                          )
                          WHERE amount >= 100000
                          GROUP BY uid, pid, status
                        )
                        GROUP BY uid, pid
                        HAVING count(status)<4 AND status IN ('COMPLETED')
                      )
                      GROUP BY uid
                      HAVING count(uid)>=10
                     ")
  head(q3_1);nrow(q3_1)
 
  q3_2 <- dbGetQuery(connection,"
                     SELECT uid, count(uid)
                      FROM (SELECT *
                        FROM (SELECT *
                          FROM (SELECT * 
                                  FROM x_transaction_history tist
                                  LEFT OUTER JOIN x_users users ON tist.uid=users.uid
                                  LEFT OUTER JOIN x_product prod ON tist.pid=prod.pid
                          )
                          WHERE amount >= 100000
                          GROUP BY uid, pid, status
                        )
                        GROUP BY uid, pid
                        HAVING count(status)<4 AND status IN ('COMPLETED')
                      )
                      GROUP BY uid
                      HAVING count(uid)>=5 AND count(uid)<10
                     ")
  head(q3_2);nrow(q3_2)
  
  match(q3_1$uid,q3_2$uid)

  
  
  
  
  
  
  
  
  
  
  ########################################################################################################################
  # dutch pay
  
  
  
  
  
  ## input
  
  dutchclaim <- read.csv(paste(dpath, "dutchpay_claim_tx.csv", sep="\\"), header=T, stringsAsFactors=F)
  dutchdetail <- read.csv(paste(dpath, "dutchpay_claim_detail.csv", sep="\\"), header=T, stringsAsFactors=F)
  
  dutchpay <- merge(dutchdetail, dutchclaim, all.x=T, all.y=F, by.x="remittance_claim_send_id", by.y="id")
    dutchpay$created_at.x <- as.POSIXct(dutchpay$created_at.x, format="%Y-%m-%dT%H:%M:%SZ")
    dutchpay$created_at.y <- as.POSIXct(dutchpay$created_at.y, format="%Y-%m-%dT%H:%M:%SZ")

    dutchpay <- cbind(dutchpay
                      , "position"=apply(dutchpay[,c("user_id.x", "user_id.y")]
                                         , 1
                                         , function(x) {ifelse(x[1]==x[2],"creditor","deptor")}
                      )
    )
    
  head(dutchpay);str(dutchpay)
  
  
  
  
  # EDA (dutch pay)
  
  head(dutchpay,40)
  table(dutchpay$status)
  
  dutchpay %>% filter(position=="creditor") %>% group_by(status) %>% summarise(n())
  dutchpay %>% filter(position=="deptor") %>% group_by(status) %>% summarise(n())
  dutchpay %>% filter(position=="deptor"& status=="CHECK") #???????
    
  dutchpay %>% filter(position=="creditor") %>% group_by(status) %>% summarise(mean(claim_amount))
  dutchpay %>% filter(position=="deptor") %>% group_by(status) %>% summarise(mean(claim_amount))

  creditor_inf <- dutchpay %>% filter(position=="creditor") %>% group_by(user_id.y) %>% summarise("N"=n(),"Mean"=mean(claim_amount))
  deptor_inf <- dutchpay %>% filter(position=="deptor") %>% group_by(user_id.x) %>% summarise("N"=n())
  cor(creditor_inf[,c("N","Mean")])

  creditor_t <- dutchpay[!is.na(match(dutchpay$user_id.x,creditor_inf$user_id.y)),]
  creditor_t <- creditor_t %>% arrange(user_id.x)
  head(creditor_t,15)
  
  
  
  
  
  # 1
  
  ## 0:CLAIM, 1:SEND
  dept0 <- subset(dutchpay, position=="deptor" & status=="CLAIM")$claim_amount
  dept1 <- subset(dutchpay, position=="deptor" & status=="SEND")$claim_amount
  mean(dept0);sd(dept0)
  mean(dept1);sd(dept1)
  
  var.test(dept0, dept1)
  t.test(dept0, dept1, alternative="greater", var.equal=F)
  
  

  dept_binary <- subset(dutchpay, position=="deptor" & status!="CHECK")
  model21 <- glm(status=="CLAIM" ~ claim_amount, data=dept_binary, family=binomial(link="logit"))
  summary(model21)
  
  
  # 2
  deptor <- dutchpay %>% filter(position=="deptor")
  
  hourtable <- dcast(Var1 ~ status
                     , data=data.frame(with(deptor, table(format(deptor$created_at.x,"%H"),status)))
                     , value.var="Freq")
  hourtable <- hourtable %>% mutate(SRate=SEND/(CLAIM+SEND))

  with(hourtable, plot(CLAIM,type="l", col="black", xlim=c(0,24), xlab="Time Line"))
  with(hourtable, lines(SEND,type="l", col="red"))
  with(hourtable, plot(SRate,type="s", xlab="Time Line"))

  
  
  
  
  # 4,  Retention of completion
  
  ## data
  depts <- dutchpay %>% filter(position=="deptor") %>% group_by(remittance_claim_send_id,status) %>% summarise(total=n()) %>% group_by(remittance_claim_send_id) %>% mutate(percent=total/sum(total))
            depts <- subset(depts, status=="SEND")

  deptmount <- dutchpay %>% filter(position=="deptor") %>% group_by(remittance_claim_send_id) %>% summarise(amount=sum(claim_amount))
  
  remittance <- dutchpay %>% filter(position=="creditor") %>% group_by(user_id.y,remittance_claim_send_id) %>% summarise(n())
  
  resp <- merge(remittance, depts, by="remittance_claim_send_id",all=T)[,c("remittance_claim_send_id","user_id.y","percent")]
          resp <- resp[!is.na(resp$user_id.y),]
          resp <- merge(resp, deptmount, by="remittance_claim_send_id", all.x=T, all.y=F)
          resp[is.na(resp$percent),"percent"] <- 0.01
          resp[is.na(resp$amount),"amount"] <- 1
          resp <- resp %>% arrange(user_id.y, remittance_claim_send_id)
          resp <- cbind(resp, "freq"=1) %>% group_by(user_id.y) %>% mutate(seq=cumsum(freq))
          resp[,"seq"] <- as.integer(resp[,"seq"])

  head(resp)
  
  
  respR <- cbind(aggregate(percent ~ user_id.y, data=resp, mean),"N"=aggregate(percent ~ user_id.y, data=resp, length)[,2])
  head(respR)
  plot(respR[,2:3]) ; cor(respR[,2:3])
  
  
  ## count model
  model41 <-lm(N ~ log(percent), data=respR)
  summary(model41)
  
  model42 <- glm(N ~ log(percent), data=respR, family=poisson(link="log"))
  anova(model42)
  summary(model42);exp(coef(model42))
  
  model43 <- glm.nb(N ~ log(percent), data=respR)
  anova(model43)
  summary(model43);exp(coef(model43))
  
  model44 <- vglm(N ~ log(percent), data=respR, family=posnegbinomial())
  summary(model44)
    
  
  ## Binary model
  respR <- respR %>% mutate(Bin=N>1)
  
  model45 <- glm(Bin ~ log(percent), data=respR, family=binomial(link="logit"))
  anova(model45)
  summary(model45)
  
  
  ## panel AR
  respP <- as.data.frame(resp[!is.na(match(resp$user_id.y,subset(respR, N>=3)$user_id.y)),])
  
  model46 <- panelAR(seq ~ percent + amount
                     , data=respP[!is.na(match(respP$user_id.y, sample(unique(respP$user_id.y),1000,replace=F))),]
                     , panelVar="user_id.y", timeVar="seq", autoCorr="ar1", panelCorrMethod="pwls")
  summary(model46)  
  
  
  
  
  
  # 5,  Serial Retention
  
  
  creditor <- dutchpay %>% filter(position=="creditor")
    table(format(creditor$created_at.y, "%Y-%m"))
  
  dterm <- unique(format(creditor$created_at.y, "%Y-%m-%d"))
    length(as.Date(dterm[1]):as.Date(dterm[length(dterm)]))
    
  dtable <- matrix(as.character(as.Date(0:(length(as.Date(dterm[1]):as.Date(dterm[length(dterm)]))-1), origin=dterm[1]))
                   , ncol=7, byrow=T)
  
  creditor <- cbind(creditor, "init"=NA, "nweek"=NA, "cohort"=NA)
  creditor[duplicated(creditor$user_id.y)==F,"init"] <- "init"
  creditor[duplicated(creditor$user_id.y)==T,"init"] <- "retention"
  creditor$nweek <- paste("week",ceiling((as.numeric(as.Date(format(creditor$created_at.y, "%Y-%m-%d")))-18230)/7))
  
  for(k in 1:nrow(dtable)) {
    print(k)
    creditor[which(creditor$init=="init"
                   & (as.numeric(as.Date(format(creditor$created_at.y, "%Y-%m-%d")))>=as.numeric(as.Date(dtable[k,1]))
                      & as.numeric(as.Date(format(creditor$created_at.y, "%Y-%m-%d")))<=as.numeric(as.Date(dtable[k,7]))
                      ))
             ,"cohort"] <- paste("cohort",k)
  }
  creditor[which(creditor$init=="retention"),"cohort"] <- creditor[match(creditor[which(creditor$init=="retention"),"user_id.y"],creditor$user_id.y),"cohort"]

  creditor$cohort <- factor(creditor$cohort, levels=paste("cohort",1:nrow(dtable)))
  creditor$nweek <- factor(creditor$nweek, levels=paste("week",1:nrow(dtable)))

  lnd_retention <- data.frame(with(creditor, table(cohort,nweek)))
    lnd_retention[which(lnd_retention$Freq==0),"Freq"]<-NA
  mat_retention <- dcast(lnd_retention, cohort ~ nweek, value.var="Freq")
  mat_retentionR <- round((mat_retention / diag(as.matrix(mtx_retention[,-1])))[,-1],4)

  ggplot(lnd_retention, aes(x=nweek, y=Freq, group=cohort, colour=cohort)) +
    geom_line() + geom_point()
  
  mat_retentionRp <- matrix(rep(NA,ncol(mat_retentionR)^2), ncol=ncol(mat_retentionR))
  mat_retentionRm <- matrix(rep(NA,ncol(mat_retentionR)^2), ncol=ncol(mat_retentionR))
  for(j in 2:ncol(mat_retentionR)) {
    mat_retentionRp[,j] <- ifelse(mat_retentionR[,j]-mat_retentionR[,j-1]>0
                                  ,mat_retentionR[,j]-mat_retentionR[,j-1],NA)
    mat_retentionRm[,j] <- ifelse(mat_retentionR[,j]-mat_retentionR[,j-1]<0&mat_retentionR[,j]-mat_retentionR[,j-1]>-0.5
                                  ,abs(mat_retentionR[,j]-mat_retentionR[,j-1]),NA)
  }
  mat_retentionRp
  mat_retentionRm
  
  
  
  
  
