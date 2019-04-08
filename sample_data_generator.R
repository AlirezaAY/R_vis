library(dplyr)
library(sqldf)
library(ggplot2)

set.seed(124)
rm(list=ls())
nrGrps<-12
GrpNm <-factor(c("A1","A2","A3","A4","B1", "B2", "B3", "B4","C1", "C2", "C3", "C4"))
color_hex_base <- rep(c("#a269ff","#00CC00","#FFAA93", "#5DD1FF"),3)

base_surg_df = expand.grid(surg_apprch=c("Approach1","Approach2","Approach3", "Approach4"),
                           condition_grp=c("Benign-1","Benign-2","Malignant")) %>% 
  bind_cols(surg_grp=GrpNm,color_hex=as.character(color_hex_base))



e <- list(mode="vector",length=nrGrps) # a list to store all distributions' values in

for (i in 1:nrGrps) {
  e[[i]] <- rnorm(n = as.integer(sample(70:120,1)), 
                  mean = as.integer(sample(250:400,1)), 
                  sd = as.integer(sample(60:120,1)))
}
for (i in 1:nrGrps) {
  #print(length(e[[i]]))
  
}
#rm(surg_df)
surg_df <- data.frame(surg_grp=factor(levels=(GrpNm)),
                      surg_apprch=factor(levels =c("Approach1","Approach2","Approach3", "Approach4")),
                      condition_grp=factor(levels = c("Benign-1","Benign-2","Malignant")),
                      op_tm=integer(),
                      color_hex=character(),
                      stringsAsFactors=FALSE)
base_comp_df <- data.frame(surg_grp=factor(levels=(GrpNm)), #baseline df to compare densities with
                           surg_apprch=factor(levels =c("Approach1","Approach2","Approach3", "Approach4")),
                           condition_grp=factor(levels = c("Benign-1","Benign-2","Malignant")),
                           op_tm=integer(),
                           color_hex=character(),
                           stringsAsFactors=FALSE)

for (g in 1:nrGrps) {
  surg_grp  <- factor(levels(GrpNm))
  surg_apprch  <- factor(levels =c("Approach1","Approach2","Approach3", "Approach4"))
  condition_grp <- factor(levels = c("Benign-1","Benign-2","Malignant"))
  op_tm  <- numeric()
  op_tm_base  <- numeric()
  color_hex <- character()
  
  print(paste("Condition val:", (ifelse((g>1 & g<5),1,ifelse(g>5&g<9,5,ifelse(g==1||g==5,g,9))))," g:",g,sep=" "))
  for (dist in 1:length(e[[g]])) {
    ifelse_result <-(ifelse((g>1 & g<5),1,ifelse(g>5&g<9,5,ifelse(g==1||g==5,g,9))))
    surg_grp[dist]  <- base_surg_df$surg_grp[g]
    surg_apprch[dist]  <- base_surg_df$surg_apprch[g]
    condition_grp[dist] <- base_surg_df$condition_grp[g]
    color_hex[dist] <- base_surg_df$color_hex[g]
    op_tm[dist]  <- e[[g]][[dist]]
    if (dist<=length(e[[ifelse_result]])) 
    {
      op_tm_base[dist]  <- e[[ifelse_result]][[dist]]          
    }                
    
  }

  if (length(op_tm_base)<length(e[[ifelse_result]])) {
    temp_base_comp_df <- base_comp_df %>%       
      filter(surg_grp==GrpNm[ifelse_result]) %>% 
      select(-surg_grp,-surg_apprch,-color_hex) %>% #the 3 columns' data 2B overwritten by the correct ones
      bind_cols(surg_grp=rep(base_surg_df$surg_grp[g],length(e[[ifelse_result]])),
                surg_apprch=rep(base_surg_df$surg_apprch[g],length(e[[ifelse_result]])),
                color_hex=rep(base_surg_df$color_hex[g],length(e[[ifelse_result]])))
    
  }
  else {
    temp_base_comp_df<- data.frame(surg_grp=surg_grp[1:length(op_tm_base)],
                                   surg_apprch=surg_apprch[1:length(op_tm_base)],
                                   condition_grp=condition_grp[1:length(op_tm_base)], 
                                   op_tm=op_tm_base, 
                                   color_hex=color_hex[1:length(op_tm_base)]) #temporary df
  }      
  
  
  
  
  temp_df<- data.frame(surg_grp,surg_apprch,condition_grp, op_tm, color_hex) #temporary df
  surg_df<- bind_rows(surg_df, temp_df)
  base_comp_df <- bind_rows(base_comp_df, temp_base_comp_df)
  
}


avg_surg_df<-sqldf('SELECT surg_grp, Avg(op_tm) AS avg_op_tm, 
                   Count(op_tm) as Cnt, surg_apprch, color_hex FROM surg_df GROUP BY surg_grp')
avg_comp_df<-sqldf('SELECT surg_grp, Avg(op_tm) AS avg_op_tm,  Count(op_tm) as Cnt, 
                   surg_apprch, color_hex FROM base_comp_df GROUP BY surg_grp')

rm(list= ls()[!(ls() %in% c('base_comp_df','base_surg_df','surg_df'))])
