
rm(list = ls())
gc()


##### PLAYER DATABASE #####

library(XML)
library(stringr)
library(data.table)

league_id <- 102953
player_interval <- 40

scoring_base_table_prefix_name <- "http://espn.go.com/nba/statistics/player/_/stat/scoring-per-game/sort/avgPoints/qualified/false/count/"
assists_base_table_prefix_name <- "http://espn.go.com/nba/statistics/player/_/stat/assists/sort/avgAssists/qualified/false/count/"
steals_base_table_prefix_name <- "http://espn.go.com/nba/statistics/player/_/stat/steals/sort/avgSteals/qualified/false/count/"
blocks_base_table_prefix_name <- "http://espn.go.com/nba/statistics/player/_/stat/blocks/sort/avgBlocks/qualified/false/count/"
rebs_base_table_prefix_name <- "http://espn.go.com/nba/statistics/player/_/stat/rebounds/sort/avgRebounds/qualified/false/count/"
roster_table_prefix_name <- "http://games.espn.go.com/fba/leaguerosters?leagueId="
#player_proj <- read.csv(file="N:\\_MSA\\Analytics\\MSA\\Habib\\nba_analysis_R\\2014_fantasy_projections.csv")

remove_na <- function(x){
  dm <- data.matrix(x)
  dm[is.na(dm)] <- 0
  data.table(dm)
}

player_proj_tables_prefix <- "http://games.espn.go.com/fba/tools/projections?leagueId=154961&startIndex="
sec_player_proj_tables_prefix <- "http://www1.fantasypros.com/nba/projections/avg-overall.php"
league_player_proj_tables_prefix <- "http://games.espn.go.com/fba/freeagency?leagueId=102953&teamId=25&seasonId=2015#&seasonId=2015&avail=4&scoringPeriodId=12&view=stats&context=freeagency&version=projections"

player_proj <- NULL
i <- 0
while (i<800){
  
  table_url_i <- paste0(player_proj_tables_prefix,i)
  table_i <- readHTMLTable(table_url_i)
  player_table_i <- table_i$playertable_0
  names(player_table_i) <- as.character(data.frame(lapply(player_table_i[1,], as.character)
                                                   , stringsAsFactors=FALSE))
  player_table_i <- player_table_i[2:nrow(player_table_i),c(1:3,5:15)]
  i <- i + 40
  
  player_proj <- rbind(player_proj,player_table_i)
  
}


player_proj <- unique(player_proj)
player_proj$value <- 1
player_proj <- player_proj[player_proj$PTS!='--',]

names(player_proj)[2] <- "player"

player_proj$player <- sapply(strsplit(as.character(player_proj$player), ","), `[[`, 1)
player_proj$player <- gsub("*","", player_proj$player , fixed=TRUE)
player_proj$player <- gsub(" ","", player_proj$player , fixed=TRUE)
player_proj$player <- gsub(".","", player_proj$player , fixed=TRUE)
player_proj$player <- gsub("-","", player_proj$player , fixed=TRUE)
player_proj$player <- gsub("'","", player_proj$player , fixed=TRUE)

player_proj$FGM <- as.numeric(as.character(player_proj$FGM))
player_proj$FGA <- as.numeric(as.character(player_proj$FGA))
player_proj$FTM <- as.numeric(as.character(player_proj$FTM))
player_proj$FTA <- as.numeric(as.character(player_proj$FTA))
player_proj$REB <- as.numeric(as.character(player_proj$REB))
player_proj$AST <- as.numeric(as.character(player_proj$AST))
player_proj$STL <- as.numeric(as.character(player_proj$STL))
player_proj$BLK <- as.numeric(as.character(player_proj$BLK))
player_proj$PTS <- as.numeric(as.character(player_proj$PTS))
player_proj[,"3PM"] <- as.numeric(as.character(player_proj[,"3PM"]))
player_proj$GP <- as.numeric(as.character(player_proj$GP))

### INCORPORATE SECONDARY DATA SOURCE ###

table_url_i <- paste0(sec_player_proj_tables_prefix)
table_i <- readHTMLTable(table_url_i)
sec_player_proj <- table_i$data

sec_player_proj <- unique(sec_player_proj)
#sec_player_proj$value <- 1
#sec_player_proj <- sec_player_proj[sec_player_proj$PTS!='--',]

names(sec_player_proj)[1] <- "player"

sec_player_proj$player <- gsub("(",")", sec_player_proj$player , fixed=TRUE)
sec_player_proj$player <- sapply(strsplit(as.character(sec_player_proj$player), ')'), `[[`, 1)
sec_player_proj$player <- gsub("*","", sec_player_proj$player , fixed=TRUE)
sec_player_proj$player <- gsub(" ","", sec_player_proj$player , fixed=TRUE)
sec_player_proj$player <- gsub(".","", sec_player_proj$player , fixed=TRUE)
sec_player_proj$player <- gsub("-","", sec_player_proj$player , fixed=TRUE)
sec_player_proj$player <- gsub("'","", sec_player_proj$player , fixed=TRUE)

sec_player_proj <- sec_player_proj[,c("player","PTS","REB","AST","BLK","STL","3PM","GP")]


sec_player_proj$REB <- as.numeric(as.character(sec_player_proj$REB))
sec_player_proj$AST <- as.numeric(as.character(sec_player_proj$AST))
sec_player_proj$STL <- as.numeric(as.character(sec_player_proj$STL))
sec_player_proj$BLK <- as.numeric(as.character(sec_player_proj$BLK))
sec_player_proj$PTS <- as.numeric(as.character(sec_player_proj$PTS))
sec_player_proj[,"3PM"] <- as.numeric(as.character(sec_player_proj[,"3PM"]))
sec_player_proj$GP <- as.numeric(as.character(sec_player_proj$GP))
sec_player_proj$value <- 1

temp_player_proj <- merge(player_proj,sec_player_proj,by="player",all=TRUE)
temp_player_names <- temp_player_proj$player
temp_player_proj <- remove_na(temp_player_proj[,4:ncol(temp_player_proj)])
temp_player_proj <- data.frame(player=temp_player_names,temp_player_proj)

gp_table <- data.frame(PLAYER=temp_player_proj$player,GP=(temp_player_proj$GP.x+temp_player_proj$GP.y)/(temp_player_proj$value.x+temp_player_proj$value.y))
player_proj <- player_proj[,names(player_proj)!="GP"]

player_proj <- data.frame(PLAYER=temp_player_proj$player,
                          FGM=temp_player_proj$FGM,
                          FGA=temp_player_proj$FGA,
                          FTM=temp_player_proj$FTM,
                          FTA=temp_player_proj$FTA,
                          "TPM"=(temp_player_proj[,"X3PM.x"]+temp_player_proj[,"X3PM.y"])/(temp_player_proj$value.x+temp_player_proj$value.y),
                          REB=(temp_player_proj$REB.x+temp_player_proj$REB.y)/(temp_player_proj$value.x+temp_player_proj$value.y),
                          AST=(temp_player_proj$AST.x+temp_player_proj$AST.y)/(temp_player_proj$value.x+temp_player_proj$value.y),
                          STL=(temp_player_proj$STL.x+temp_player_proj$STL.y)/(temp_player_proj$value.x+temp_player_proj$value.y),
                          BLK=(temp_player_proj$BLK.x+temp_player_proj$BLK.y)/(temp_player_proj$value.x+temp_player_proj$value.y),
                          PTS=(temp_player_proj$PTS.x+temp_player_proj$PTS.y)/(temp_player_proj$value.x+temp_player_proj$value.y))


# SEASON PLAYER PERFORMANCE #


player_stats_scoring <- NULL

for (i in 1:25){
  
  start_index_i <- (i-1)*player_interval+1
  table_url_i <- paste(scoring_base_table_prefix_name,start_index_i,sep="")
  table_i <- readHTMLTable(table_url_i)
  player_table_i <- table_i$`NULL`
  
  if (is.null(nrow(player_table_i))){
      player_stats_scoring_i <- NULL
    }else{
      player_table_i <- subset(player_table_i,PLAYER!="PLAYER")
      player_stats_scoring_i <- player_table_i[,-1]
    }
  
  player_stats_scoring <- rbind(player_stats_scoring,player_stats_scoring_i)
}


names(player_stats_scoring) <- str_replace_all(names(player_stats_scoring), "FGM-FGA", "FGA")
names(player_stats_scoring) <- str_replace_all(names(player_stats_scoring), "FTM-FTA", "FTA")
names(player_stats_scoring) <- str_replace_all(names(player_stats_scoring), "3PM-3PA", "TPM")
names(player_stats_scoring) <- str_replace_all(names(player_stats_scoring), "FG%", "FGM")
names(player_stats_scoring) <- str_replace_all(names(player_stats_scoring), "FT%", "FTM")

player_stats_scoring$PLAYER <- sapply(strsplit(as.character(player_stats_scoring$PLAYER), ","), "[[", 1)
player_stats_scoring$PLAYER <- gsub(" ", "", player_stats_scoring$PLAYER, fixed=TRUE)
player_stats_scoring$PLAYER <- gsub("*","", player_stats_scoring$PLAYER , fixed=TRUE)
player_stats_scoring$PLAYER <- gsub(".","", player_stats_scoring$PLAYER , fixed=TRUE)
player_stats_scoring$PLAYER <- gsub("-","", player_stats_scoring$PLAYER , fixed=TRUE)
player_stats_scoring$PLAYER <- gsub("'","", player_stats_scoring$PLAYER , fixed=TRUE)

player_stats_scoring$FGM <- sapply(strsplit(as.character(player_stats_scoring$FGA), "-"), "[[", 1)
player_stats_scoring$FGM <- as.numeric(player_stats_scoring$FGM)
player_stats_scoring$FGA <- sapply(strsplit(as.character(player_stats_scoring$FGA), "-"), "[[", 2)
player_stats_scoring$FGA <- as.numeric(player_stats_scoring$FGA)
player_stats_scoring$FTM <- sapply(strsplit(as.character(player_stats_scoring$FTA), "-"), "[[", 1)
player_stats_scoring$FTM <- as.numeric(player_stats_scoring$FTM)
player_stats_scoring$FTA <- sapply(strsplit(as.character(player_stats_scoring$FTA), "-"), "[[", 2)
player_stats_scoring$FTA <- as.numeric(player_stats_scoring$FTA)
player_stats_scoring$TPM <- sapply(strsplit(as.character(player_stats_scoring$TPM), "-"), "[[", 1)
player_stats_scoring$TPM <- as.numeric(player_stats_scoring$TPM)
player_stats_scoring$FGM <- as.character(player_stats_scoring$FGM)
player_stats_scoring$FGM <- as.numeric(player_stats_scoring$FGM)
player_stats_scoring$FTM <- as.character(player_stats_scoring$FTM)
player_stats_scoring$FTM <- as.numeric(player_stats_scoring$FTM)
player_stats_scoring$PTS <- as.character(player_stats_scoring$PTS)
player_stats_scoring$PTS <- as.numeric(player_stats_scoring$PTS)

player_stats_scoring <- unique(player_stats_scoring)


player_stats_assists <- NULL

for (i in 1:25){
  
  start_index_i <- (i-1)*player_interval+1
  table_url_i <- paste(assists_base_table_prefix_name,start_index_i,sep="")
  table_i <- readHTMLTable(table_url_i)
  player_table_i <- table_i$`NULL`
  
  if (is.null(nrow(player_table_i))){
    player_stats_assists_i <- NULL
  }else{
    player_table_i <- subset(player_table_i,PLAYER!="PLAYER")
    player_stats_assists_i <- player_table_i[,-1]
  }
  
  player_stats_assists <- rbind(player_stats_assists,player_stats_assists_i)
}

player_stats_assists <- player_stats_assists[,c("PLAYER","APG")]
names(player_stats_assists) <- str_replace_all(names(player_stats_assists), "APG", "AST")
player_stats_assists$AST <- as.character(player_stats_assists$AST)
player_stats_assists$AST <- as.numeric(player_stats_assists$AST)
player_stats_assists$PLAYER <- sapply(strsplit(as.character(player_stats_assists$PLAYER), ","), "[[", 1)
player_stats_assists$PLAYER <- gsub(" ", "", player_stats_assists$PLAYER, fixed=TRUE)
player_stats_assists$PLAYER <- gsub("*","", player_stats_assists$PLAYER , fixed=TRUE)
player_stats_assists$PLAYER <- gsub(".","", player_stats_assists$PLAYER , fixed=TRUE)
player_stats_assists$PLAYER <- gsub("-","", player_stats_assists$PLAYER , fixed=TRUE)
player_stats_assists$PLAYER <- gsub("'","", player_stats_assists$PLAYER , fixed=TRUE)


player_stats_steals <- NULL

for (i in 1:25){
  
  start_index_i <- (i-1)*player_interval+1
  table_url_i <- paste(steals_base_table_prefix_name,start_index_i,sep="")
  table_i <- readHTMLTable(table_url_i)
  player_table_i <- table_i$`NULL`
  
  if (is.null(nrow(player_table_i))){
    player_stats_steals_i <- NULL
  }else{
    player_table_i <- subset(player_table_i,PLAYER!="PLAYER")
    player_stats_steals_i <- player_table_i[,-1]
  }
  
  player_stats_steals <- rbind(player_stats_steals,player_stats_steals_i)
}

player_stats_steals <- player_stats_steals[,c("PLAYER","STPG")]
names(player_stats_steals) <- str_replace_all(names(player_stats_steals), "STPG", "STL")
player_stats_steals$STL <- as.character(player_stats_steals$STL)
player_stats_steals$STL <- as.numeric(player_stats_steals$STL)
player_stats_steals$PLAYER <- sapply(strsplit(as.character(player_stats_steals$PLAYER), ","), "[[", 1)
player_stats_steals$PLAYER <- gsub(" ", "", player_stats_steals$PLAYER, fixed=TRUE)
player_stats_steals$PLAYER <- gsub("*","", player_stats_steals$PLAYER , fixed=TRUE)
player_stats_steals$PLAYER <- gsub(".","", player_stats_steals$PLAYER , fixed=TRUE)
player_stats_steals$PLAYER <- gsub("-","", player_stats_steals$PLAYER , fixed=TRUE)
player_stats_steals$PLAYER <- gsub("'","", player_stats_steals$PLAYER , fixed=TRUE)


player_stats_blocks <- NULL

for (i in 1:25){
  
  start_index_i <- (i-1)*player_interval+1
  table_url_i <- paste(blocks_base_table_prefix_name,start_index_i,sep="")
  table_i <- readHTMLTable(table_url_i)
  player_table_i <- table_i$`NULL`
  
  if (is.null(nrow(player_table_i))){
    player_stats_blocks_i <- NULL
  }else{
    player_table_i <- subset(player_table_i,PLAYER!="PLAYER")
    player_stats_blocks_i <- player_table_i[,-1]
  }
  
  player_stats_blocks <- rbind(player_stats_blocks,player_stats_blocks_i)
}

player_stats_blocks <- player_stats_blocks[,c("PLAYER","BLKPG")]
names(player_stats_blocks) <- str_replace_all(names(player_stats_blocks), "BLKPG", "BLK")
player_stats_blocks$BLK <- as.character(player_stats_blocks$BLK)
player_stats_blocks$BLK <- as.numeric(player_stats_blocks$BLK)
player_stats_blocks$PLAYER <- sapply(strsplit(as.character(player_stats_blocks$PLAYER), ","), "[[", 1)
player_stats_blocks$PLAYER <- gsub(" ", "", player_stats_blocks$PLAYER, fixed=TRUE)
player_stats_blocks$PLAYER <- gsub("*","", player_stats_blocks$PLAYER , fixed=TRUE)
player_stats_blocks$PLAYER <- gsub(".","", player_stats_blocks$PLAYER , fixed=TRUE)
player_stats_blocks$PLAYER <- gsub("-","", player_stats_blocks$PLAYER , fixed=TRUE)
player_stats_blocks$PLAYER <- gsub("'","", player_stats_blocks$PLAYER , fixed=TRUE)

player_stats_rebs <- NULL

for (i in 1:25){
  
  start_index_i <- (i-1)*player_interval+1
  table_url_i <- paste(rebs_base_table_prefix_name,start_index_i,sep="")
  table_i <- readHTMLTable(table_url_i)
  player_table_i <- table_i$`NULL`
  
  if (is.null(nrow(player_table_i))){
    player_stats_rebs_i <- NULL
  }else{
    player_table_i <- subset(player_table_i,PLAYER!="PLAYER")
    player_stats_rebs_i <- player_table_i[,-1]
  }
  
  player_stats_rebs <- rbind(player_stats_rebs,player_stats_rebs_i)
}

player_stats_rebs <- player_stats_rebs[,c("PLAYER","RPG")]
names(player_stats_rebs) <- str_replace_all(names(player_stats_rebs), "RPG", "REB")
player_stats_rebs$REB <- as.character(player_stats_rebs$REB)
player_stats_rebs$REB <- as.numeric(player_stats_rebs$REB)
player_stats_rebs$PLAYER <- sapply(strsplit(as.character(player_stats_rebs$PLAYER), ","), "[[", 1)
player_stats_rebs$PLAYER <- gsub(" ", "", player_stats_rebs$PLAYER, fixed=TRUE)
player_stats_rebs$PLAYER <- gsub("*","", player_stats_rebs$PLAYER , fixed=TRUE)
player_stats_rebs$PLAYER <- gsub(".","", player_stats_rebs$PLAYER , fixed=TRUE)
player_stats_rebs$PLAYER <- gsub("-","", player_stats_rebs$PLAYER , fixed=TRUE)
player_stats_rebs$PLAYER <- gsub("'","", player_stats_rebs$PLAYER , fixed=TRUE)


player_stats <- merge(player_stats_scoring,player_stats_assists,by="PLAYER",all.x=TRUE)
player_stats <- merge(player_stats,player_stats_steals,by="PLAYER",all.x=TRUE)
player_stats <- merge(player_stats,player_stats_blocks,by="PLAYER",all.x=TRUE)
player_stats <- merge(player_stats,player_stats_rebs,by="PLAYER",all.x=TRUE)



# BLENDED PROJECTED PERFORMANCE #

player_proj <- merge(player_proj,gp_table,by="PLAYER")
blend_data <- merge(player_proj,player_stats,by="PLAYER",all=TRUE)
blend_data_adj <- remove_na(blend_data)
blend_data_adj$PLAYER <- blend_data$PLAYER
blend_data_adj <- data.frame(blend_data_adj,wt_proj=ifelse(blend_data_adj$GP.x==0,0,ifelse((1-(blend_data_adj$GP.y/blend_data_adj$GP.x))<0,0,1-(blend_data_adj$GP.y/blend_data_adj$GP.x))))
#blend_data_adj$wt_proj <- 0
#plot(blend_data_adj$wt_proj)

blend_PTS <- blend_data_adj$wt_proj*blend_data_adj$PTS.x+(1-blend_data_adj$wt_proj)*blend_data_adj$PTS.y
blend_FGA <- blend_data_adj$wt_proj*blend_data_adj$FGA.x+(1-blend_data_adj$wt_proj)*blend_data_adj$FGA.y
blend_FTA <- blend_data_adj$wt_proj*blend_data_adj$FTA.x+(1-blend_data_adj$wt_proj)*blend_data_adj$FTA.y
blend_REB <- blend_data_adj$wt_proj*blend_data_adj$REB.x+(1-blend_data_adj$wt_proj)*blend_data_adj$REB.y
blend_BLK <- blend_data_adj$wt_proj*blend_data_adj$BLK.x+(1-blend_data_adj$wt_proj)*blend_data_adj$BLK.y
blend_STL <- blend_data_adj$wt_proj*blend_data_adj$STL.x+(1-blend_data_adj$wt_proj)*blend_data_adj$STL.y
blend_TPM <- blend_data_adj$wt_proj*blend_data_adj$TPM.x+(1-blend_data_adj$wt_proj)*blend_data_adj$TPM.y
blend_AST <- blend_data_adj$wt_proj*blend_data_adj$AST.x+(1-blend_data_adj$wt_proj)*blend_data_adj$AST.y
blend_FGM <- blend_data_adj$wt_proj*blend_data_adj$FGM.x+(1-blend_data_adj$wt_proj)*blend_data_adj$FGM.y
blend_FTM <- blend_data_adj$wt_proj*blend_data_adj$FTM.x+(1-blend_data_adj$wt_proj)*blend_data_adj$FTM.y

ttl_blend_data <- data.frame(PLAYER=blend_data_adj$PLAYER,PTS=blend_PTS,FGA=blend_FGA,FTA=blend_FTA,REB=blend_REB,BLK=blend_BLK,STL=blend_STL,TPM=blend_TPM,FGM=blend_FGM,FTM=blend_FTM,AST=blend_AST)
ttl_blend_data$PLAYER <- as.character(ttl_blend_data$PLAYER)
ttl_blend_data$PLAYER <- gsub("\\s", "",ttl_blend_data$PLAYER, fixed=TRUE)
ttl_blend_data$PLAYER <- str_trim(ttl_blend_data$PLAYER, side="both")


# ASSIGN PLAYERS TO CURRENT TEAMS #


table_url_i <- paste(roster_table_prefix_name,league_id,sep="")
table_i <- readHTMLTable(table_url_i)
n_teams <- (length(names(table_i))-2)
names(table_i) <- c("NULL","NULL",paste("playertable_",1:n_teams,sep=""))


roster_table <- NULL
for (i in 1:n_teams){
  roster_i <- get(paste("playertable_",i,sep=""),table_i)
  roster_table_i <- data.frame(roster_i,team_id=i)
  roster_table <- rbind(roster_table,roster_table_i)
}


roster_table <- subset(roster_table[,c("V2","V3","team_id")],V3!="")
roster_table <- subset(roster_table[,c("V2","V3","team_id")],V3!="ACQ")
roster_table <- roster_table[,c("V2","team_id")]
names(roster_table)[1] <- "PLAYER"


roster_table$PLAYER <- sapply(strsplit(as.character(roster_table$PLAYER), ","), "[[", 1)
roster_table$PLAYER <- str_replace_all(roster_table$PLAYER, '\\*', "")
roster_table$PLAYER <- gsub(" ", "",roster_table$PLAYER, fixed=TRUE)
roster_table$PLAYER <- gsub("*","", roster_table$PLAYER , fixed=TRUE)
roster_table$PLAYER <- gsub(".","", roster_table$PLAYER , fixed=TRUE)
roster_table$PLAYER <- gsub("-","", roster_table$PLAYER , fixed=TRUE)
roster_table$PLAYER <- gsub("'","", roster_table$PLAYER , fixed=TRUE)
roster_table$PLAYER <- str_trim(roster_table$PLAYER, side="both")



league_player_data <- merge(roster_table,ttl_blend_data,by="PLAYER")

agg_PTS <- aggregate(PTS~team_id,sum,data=league_player_data)
agg_REB <- aggregate(REB~team_id,sum,data=league_player_data)
agg_BLK <- aggregate(BLK~team_id,sum,data=league_player_data)
agg_STL <- aggregate(STL~team_id,sum,data=league_player_data)
agg_TPM <- aggregate(TPM~team_id,sum,data=league_player_data)
agg_AST <- aggregate(AST~team_id,sum,data=league_player_data)
agg_FTM <- aggregate(FTM~team_id,sum,data=league_player_data)
agg_FGM <- aggregate(FGM~team_id,sum,data=league_player_data)
agg_FTA <- aggregate(FTA~team_id,sum,data=league_player_data)
agg_FGA <- aggregate(FGA~team_id,sum,data=league_player_data)
agg_FTpct <- data.frame(team_id=agg_FTM$team_id,FTpct=agg_FTM$FTM/agg_FTA$FTA)
agg_FGpct <- data.frame(team_id=agg_FGM$team_id,FGpct=agg_FGM$FGM/agg_FGA$FGA)


PTS_matrix <- matrix(0,n_teams,n_teams)
REB_matrix <- matrix(0,n_teams,n_teams)
BLK_matrix <- matrix(0,n_teams,n_teams)
STL_matrix <- matrix(0,n_teams,n_teams)
AST_matrix <- matrix(0,n_teams,n_teams)
TPM_matrix <- matrix(0,n_teams,n_teams)
FGpct_matrix <- matrix(0,n_teams,n_teams)
FTpct_matrix <- matrix(0,n_teams,n_teams)

for (i in 1:n_teams){
  
  PTS_i <- agg_PTS[agg_PTS$team_id==i,"PTS"]
  REB_i <- agg_REB[agg_REB$team_id==i,"REB"]
  BLK_i <- agg_BLK[agg_BLK$team_id==i,"BLK"]
  STL_i <- agg_STL[agg_STL$team_id==i,"STL"]
  AST_i <- agg_AST[agg_AST$team_id==i,"AST"]
  TPM_i <- agg_TPM[agg_TPM$team_id==i,"TPM"]
  FGpct_i <- agg_FGpct[agg_FGpct$team_id==i,"FGpct"]
  FTpct_i <- agg_FTpct[agg_FTpct$team_id==i,"FTpct"]
  
  for(j in 1:n_teams){

    PTS_j <- agg_PTS[agg_PTS$team_id==j,"PTS"]
    REB_j <- agg_REB[agg_REB$team_id==j,"REB"]
    BLK_j <- agg_BLK[agg_BLK$team_id==j,"BLK"]
    STL_j <- agg_STL[agg_STL$team_id==j,"STL"]
    AST_j <- agg_AST[agg_AST$team_id==j,"AST"]
    TPM_j <- agg_TPM[agg_TPM$team_id==j,"TPM"]
    FGpct_j <- agg_FGpct[agg_FGpct$team_id==j,"FGpct"]
    FTpct_j <- agg_FTpct[agg_FTpct$team_id==j,"FTpct"]
    
    if (i!=j){
      PTS_matrix[i,j] <- ifelse(PTS_i>=PTS_j,1,0)
      REB_matrix[i,j] <- ifelse(REB_i>=REB_j,1,0)
      BLK_matrix[i,j] <- ifelse(BLK_i>=BLK_j,1,0)
      STL_matrix[i,j] <- ifelse(STL_i>=STL_j,1,0)
      AST_matrix[i,j] <- ifelse(AST_i>=AST_j,1,0)
      TPM_matrix[i,j] <- ifelse(TPM_i>=TPM_j,1,0)
      FGpct_matrix[i,j] <- ifelse(FGpct_i>=FGpct_j,1,0)
      FTpct_matrix[i,j] <- ifelse(FTpct_i>=FTpct_j,1,0)
    }
  }
}



TOTAL_matrix <- PTS_matrix+REB_matrix+BLK_matrix+STL_matrix+AST_matrix+TPM_matrix+FGpct_matrix+FTpct_matrix
proj_results_matrix <- data.frame(TOTAL_matrix,TTL_wins=rowSums(TOTAL_matrix))


subset(league_player_data,PLAYER=="GordonHayward")
subset(league_player_data,team_id==6)

# TRADE ENGINE #

player_trade_1 <- "GordonHayward"
player_trade_2 <- "JoshSmith"
player_trade_3 <- NULL
player_trade_4 <- NULL
player_trade_5 <- NULL
player_trade_6 <- NULL
player_trade_7 <- NULL
player_trade_8 <- NULL
player_trade_9 <- NULL
player_trade_10 <- NULL

dest_trade_1 <- 12
dest_trade_2 <- 9
dest_trade_3 <- NULL
dest_trade_4 <- NULL
dest_trade_5 <- NULL
dest_trade_6 <- NULL
dest_trade_7 <- NULL
dest_trade_8 <- NULL
dest_trade_9 <- NULL
dest_trade_10 <- NULL


players_moving <- c(player_trade_1,player_trade_2,player_trade_3,player_trade_4,player_trade_5,player_trade_6,player_trade_7,player_trade_8,player_trade_9,player_trade_10)
player_destinations <- c(dest_trade_1,dest_trade_2,dest_trade_3,dest_trade_4,dest_trade_5,dest_trade_6,dest_trade_7,dest_trade_8,dest_trade_9,dest_trade_10)

pt_league_player_data <- league_player_data
player_movement_table <- subset(pt_league_player_data,PLAYER%in%players_moving)

#subset(pt_league_player_data,team_id==6)


for (i in 1:length(players_moving)){
  player_i <- players_moving[i]
  dest_i <- player_destinations[i]
  pt_league_player_data[pt_league_player_data$PLAYER==player_i,"team_id"] <- dest_i
}

pt_agg_PTS <- aggregate(PTS~team_id,sum,data=pt_league_player_data)
pt_agg_REB <- aggregate(REB~team_id,sum,data=pt_league_player_data)
pt_agg_BLK <- aggregate(BLK~team_id,sum,data=pt_league_player_data)
pt_agg_STL <- aggregate(STL~team_id,sum,data=pt_league_player_data)
pt_agg_TPM <- aggregate(TPM~team_id,sum,data=pt_league_player_data)
pt_agg_AST <- aggregate(AST~team_id,sum,data=pt_league_player_data)
pt_agg_FTM <- aggregate(FTM~team_id,sum,data=pt_league_player_data)
pt_agg_FGM <- aggregate(FGM~team_id,sum,data=pt_league_player_data)
pt_agg_FTA <- aggregate(FTA~team_id,sum,data=pt_league_player_data)
pt_agg_FGA <- aggregate(FGA~team_id,sum,data=pt_league_player_data)
pt_agg_FTpct <- data.frame(team_id=pt_agg_FTM$team_id,FTpct=pt_agg_FTM$FTM/pt_agg_FTA$FTA)
pt_agg_FGpct <- data.frame(team_id=pt_agg_FGM$team_id,FGpct=pt_agg_FGM$FGM/pt_agg_FGA$FGA)


PTS_matrix_pt <- matrix(0,n_teams,n_teams)
REB_matrix_pt <- matrix(0,n_teams,n_teams)
BLK_matrix_pt <- matrix(0,n_teams,n_teams)
STL_matrix_pt <- matrix(0,n_teams,n_teams)
AST_matrix_pt <- matrix(0,n_teams,n_teams)
TPM_matrix_pt <- matrix(0,n_teams,n_teams)
FGpct_matrix_pt <- matrix(0,n_teams,n_teams)
FTpct_matrix_pt <- matrix(0,n_teams,n_teams)

for (i in 1:n_teams){
  
  PTS_i <- pt_agg_PTS[pt_agg_PTS$team_id==i,"PTS"]
  REB_i <- pt_agg_REB[pt_agg_REB$team_id==i,"REB"]
  BLK_i <- pt_agg_BLK[pt_agg_BLK$team_id==i,"BLK"]
  STL_i <- pt_agg_STL[pt_agg_STL$team_id==i,"STL"]
  AST_i <- pt_agg_AST[pt_agg_AST$team_id==i,"AST"]
  TPM_i <- pt_agg_TPM[pt_agg_TPM$team_id==i,"TPM"]
  FGpct_i <- pt_agg_FGpct[pt_agg_FGpct$team_id==i,"FGpct"]
  FTpct_i <- pt_agg_FTpct[pt_agg_FTpct$team_id==i,"FTpct"]
  
  for(j in 1:n_teams){
    
    PTS_j <- pt_agg_PTS[pt_agg_PTS$team_id==j,"PTS"]
    REB_j <- pt_agg_REB[pt_agg_REB$team_id==j,"REB"]
    BLK_j <- pt_agg_BLK[pt_agg_BLK$team_id==j,"BLK"]
    STL_j <- pt_agg_STL[pt_agg_STL$team_id==j,"STL"]
    AST_j <- pt_agg_AST[pt_agg_AST$team_id==j,"AST"]
    TPM_j <- pt_agg_TPM[pt_agg_TPM$team_id==j,"TPM"]
    FGpct_j <- pt_agg_FGpct[pt_agg_FGpct$team_id==j,"FGpct"]
    FTpct_j <- pt_agg_FTpct[pt_agg_FTpct$team_id==j,"FTpct"]
    
    if (i!=j){
      PTS_matrix_pt[i,j] <- ifelse(PTS_i>=PTS_j,1,0)
      REB_matrix_pt[i,j] <- ifelse(REB_i>=REB_j,1,0)
      BLK_matrix_pt[i,j] <- ifelse(BLK_i>=BLK_j,1,0)
      STL_matrix_pt[i,j] <- ifelse(STL_i>=STL_j,1,0)
      AST_matrix_pt[i,j] <- ifelse(AST_i>=AST_j,1,0)
      TPM_matrix_pt[i,j] <- ifelse(TPM_i>=TPM_j,1,0)
      FGpct_matrix_pt[i,j] <- ifelse(FGpct_i>=FGpct_j,1,0)
      FTpct_matrix_pt[i,j] <- ifelse(FTpct_i>=FTpct_j,1,0)
    }
  }
}



TOTAL_matrix_pt <- PTS_matrix_pt+REB_matrix_pt+BLK_matrix_pt+STL_matrix_pt+AST_matrix_pt+TPM_matrix_pt+FGpct_matrix_pt+FTpct_matrix_pt
proj_results_matrix_pt <- data.frame(TOTAL_matrix_pt,TTL_wins=rowSums(TOTAL_matrix_pt))

change_matrix <- proj_results_matrix_pt-proj_results_matrix
team_change_table <- data.frame(team_id=1:n_teams,delta=change_matrix$TTL_wins)

movement_summary_table <- merge(player_movement_table,team_change_table,by="team_id")
movement_summary_table <- data.frame(movement_summary_table,FTpct=movement_summary_table$FTM/movement_summary_table$FTA,FGpct=movement_summary_table$FGM/movement_summary_table$FGA)
movement_summary_table

  #### PLAYER VALUATION ####


##### WAIVER WIRE ######



drop_player_1 <- "MikeDunleavy"
drop_player_2 <- NULL
drop_player_3 <- NULL
drop_player_4 <- NULL
drop_player_5 <- NULL

top_limit <- 20

waiver_player_data <- ttl_blend_data[ttl_blend_data$PLAYER%in%roster_table$PLAYER==FALSE,]
players_dropped <- c(drop_player_1,drop_player_2,drop_player_3,drop_player_4,drop_player_5)
team_id <- league_player_data[league_player_data$PLAYER==players_dropped[length(players_dropped)],"team_id"]
wpt_league_player_data <- league_player_data

n_waivers <- length(waiver_player_data[,1])
waiver_matrix <- matrix(NA,n_waivers,2)

for (i in 1:length(players_dropped)){
  player_i <- players_dropped[i]
  dest_i <- 0
  wpt_league_player_data[wpt_league_player_data$PLAYER==player_i,"team_id"] <- dest_i
}

for (w in 1:n_waivers){
  pickup_player_i <- data.frame(PLAYER=waiver_player_data[w,"PLAYER"],team_id=team_id,waiver_player_data[w,-1])
  wpt_i_league_player_data <- rbind(wpt_league_player_data,pickup_player_i)

  wpt_i_agg_PTS <- aggregate(PTS~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_REB <- aggregate(REB~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_BLK <- aggregate(BLK~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_STL <- aggregate(STL~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_TPM <- aggregate(TPM~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_AST <- aggregate(AST~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_FTM <- aggregate(FTM~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_FGM <- aggregate(FGM~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_FTA <- aggregate(FTA~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_FGA <- aggregate(FGA~team_id,sum,data=wpt_i_league_player_data)
  wpt_i_agg_FTpct <- data.frame(team_id=wpt_i_agg_FTM$team_id,FTpct=wpt_i_agg_FTM$FTM/wpt_i_agg_FTA$FTA)
  wpt_i_agg_FGpct <- data.frame(team_id=wpt_i_agg_FGM$team_id,FGpct=wpt_i_agg_FGM$FGM/wpt_i_agg_FGA$FGA)
  
  
  PTS_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  REB_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  BLK_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  STL_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  AST_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  TPM_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  FGpct_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  FTpct_matrix_wpt_i <- matrix(0,n_teams,n_teams)
  
  for (i in 1:n_teams){
    
    PTS_i <- wpt_i_agg_PTS[wpt_i_agg_PTS$team_id==i,"PTS"]
    REB_i <- wpt_i_agg_REB[wpt_i_agg_REB$team_id==i,"REB"]
    BLK_i <- wpt_i_agg_BLK[wpt_i_agg_BLK$team_id==i,"BLK"]
    STL_i <- wpt_i_agg_STL[wpt_i_agg_STL$team_id==i,"STL"]
    AST_i <- wpt_i_agg_AST[wpt_i_agg_AST$team_id==i,"AST"]
    TPM_i <- wpt_i_agg_TPM[wpt_i_agg_TPM$team_id==i,"TPM"]
    FGpct_i <- wpt_i_agg_FGpct[wpt_i_agg_FGpct$team_id==i,"FGpct"]
    FTpct_i <- wpt_i_agg_FTpct[wpt_i_agg_FTpct$team_id==i,"FTpct"]
    
    for(j in 1:n_teams){
      
      PTS_j <- wpt_i_agg_PTS[wpt_i_agg_PTS$team_id==j,"PTS"]
      REB_j <- wpt_i_agg_REB[wpt_i_agg_REB$team_id==j,"REB"]
      BLK_j <- wpt_i_agg_BLK[wpt_i_agg_BLK$team_id==j,"BLK"]
      STL_j <- wpt_i_agg_STL[wpt_i_agg_STL$team_id==j,"STL"]
      AST_j <- wpt_i_agg_AST[wpt_i_agg_AST$team_id==j,"AST"]
      TPM_j <- wpt_i_agg_TPM[wpt_i_agg_TPM$team_id==j,"TPM"]
      FGpct_j <- wpt_i_agg_FGpct[wpt_i_agg_FGpct$team_id==j,"FGpct"]
      FTpct_j <- wpt_i_agg_FTpct[wpt_i_agg_FTpct$team_id==j,"FTpct"]
      
      if (i!=j){
        PTS_matrix_wpt_i[i,j] <- ifelse(PTS_i>=PTS_j,1,0)
        REB_matrix_wpt_i[i,j] <- ifelse(REB_i>=REB_j,1,0)
        BLK_matrix_wpt_i[i,j] <- ifelse(BLK_i>=BLK_j,1,0)
        STL_matrix_wpt_i[i,j] <- ifelse(STL_i>=STL_j,1,0)
        AST_matrix_wpt_i[i,j] <- ifelse(AST_i>=AST_j,1,0)
        TPM_matrix_wpt_i[i,j] <- ifelse(TPM_i>=TPM_j,1,0)
        FGpct_matrix_wpt_i[i,j] <- ifelse(FGpct_i>=FGpct_j,1,0)
        FTpct_matrix_wpt_i[i,j] <- ifelse(FTpct_i>=FTpct_j,1,0)
      }
    }
  }
  

  TOTAL_matrix_wpt_i <- PTS_matrix_wpt_i+REB_matrix_wpt_i+BLK_matrix_wpt_i+STL_matrix_wpt_i+AST_matrix_wpt_i+TPM_matrix_wpt_i+FGpct_matrix_wpt_i+FTpct_matrix_wpt_i
  proj_results_matrix_wpt_i <- data.frame(TOTAL_matrix_wpt_i,TTL_wins=rowSums(TOTAL_matrix_wpt_i))
  
  change_matrix_i <- proj_results_matrix_wpt_i-proj_results_matrix
  team_change_table_i <- data.frame(team_id=1:n_teams,delta=change_matrix_i$TTL_wins)
  inc_i <- team_change_table_i[team_change_table_i$team_id==team_id,"delta"]
  waiver_matrix[w,] <- c(as.character(pickup_player_i$PLAYER),inc_i)
  print(w)
}

waiver_results <- data.frame(PLAYER=waiver_matrix[,1],uplift=as.numeric(waiver_matrix[,2]))
waiver_results <- waiver_results[with(waiver_results, order(-uplift)), ]

top_X_waivers <- waiver_results$PLAYER[1:top_limit]
top_X_waiver_table <- merge(waiver_player_data[waiver_player_data$PLAYER%in%top_X_waivers,],waiver_results,by="PLAYER")
