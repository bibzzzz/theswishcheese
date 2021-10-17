library(XML)
library(plyr)
library(stringr)

budget <- 200
<<<<<<< HEAD
#squad_size <- 10
#n_teams <- 14
n_iters <- 10
n_sec_iters <- 10
high_perc <- 0.2
low_perc <- 0.8
n_samples <- 10
player_proj_tables_prefix <- "http://games.espn.go.com/fba/freeagency?leagueId=102953&startIndex="
player_proj_tables_suffix <- "&context=freeagency&version=projections&avail=-1"
sec_player_proj_tables_prefix <- "http://www1.fantasypros.com/nba/projections/avg-overall.php"


for (n_teams in 11:16){
  for (squad_size in 9:12){ 
    
    player_proj <- NULL
    i <- 0
    while (i<=600){
      
      table_url_i <- paste0(player_proj_tables_prefix,i,player_proj_tables_suffix)
      table_i <- readHTMLTable(table_url_i)
      player_table_i <- table_i$playertable_0
      names(player_table_i) <- as.character(data.frame(lapply(player_table_i[1,], as.character)
                                                       , stringsAsFactors=FALSE))
      player_table_i <- player_table_i[2:nrow(player_table_i),c(1,6:18)]
      i <- i + nrow(player_table_i)
      
      player_proj <- rbind(player_proj,player_table_i)
      
    }
    
    
    
    player_proj <- unique(player_proj)
    player_proj$value <- 1
    player_proj <- player_proj[player_proj$PTS!='--',]
    
    player_proj <- player_proj[,c(1,5:ncol(player_proj))]
    names(player_proj)[1] <- "player"
    names(player_proj)[2] <- "FGMA"
    names(player_proj)[4] <- "FTMA"
    
    player_proj$player <- sapply(strsplit(as.character(player_proj$player), ","), `[[`, 1)
    player_proj$player <- gsub("*","", player_proj$player , fixed=TRUE)
    player_proj$player <- gsub(" ","", player_proj$player , fixed=TRUE)
    player_proj$player <- gsub(".","", player_proj$player , fixed=TRUE)
    player_proj$player <- gsub("-","", player_proj$player , fixed=TRUE)
    player_proj$player <- gsub("'","", player_proj$player , fixed=TRUE)
    
    
    player_proj$FGM <- as.numeric(sapply(strsplit(as.character(player_proj$FGMA), "/"), `[[`, 1))
    player_proj$FGA <- as.numeric(sapply(strsplit(as.character(player_proj$FGMA), "/"), `[[`, 2))
    player_proj$FTM <- as.numeric(sapply(strsplit(as.character(player_proj$FTMA), "/"), `[[`, 1))
    player_proj$FTA <- as.numeric(sapply(strsplit(as.character(player_proj$FTMA), "/"), `[[`, 2))
    player_proj$REB <- as.numeric(as.character(player_proj$REB))
    player_proj$AST <- as.numeric(as.character(player_proj$AST))
    player_proj$STL <- as.numeric(as.character(player_proj$STL))
    player_proj$BLK <- as.numeric(as.character(player_proj$BLK))
    player_proj$PTS <- as.numeric(as.character(player_proj$PTS))
    player_proj[,"3PM"] <- as.numeric(as.character(player_proj[,"3PM"]))
    player_proj$GP <- 82
    
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
    sec_player_proj$GP <- ifelse(sec_player_proj$GP>82,82,sec_player_proj$GP)
    
    temp_player_proj <- merge(player_proj,sec_player_proj,by="player",all.x=TRUE)
    
    # gp_table <- data.frame(player=temp_player_proj$player,GP=(temp_player_proj$GP.x+temp_player_proj$GP.y)/2)
    gp_table <- data.frame(player=temp_player_proj$player[!is.na(temp_player_proj$GP.y)],GP=temp_player_proj$GP.y[!is.na(temp_player_proj$GP.y)])
    player_proj <- player_proj[,names(player_proj)!="GP"]
    
    player_proj <- data.frame(player=temp_player_proj$player,
                              FGM=temp_player_proj$FGM,
                              FGA=temp_player_proj$FGA,
                              FTM=temp_player_proj$FTM,
                              FTA=temp_player_proj$FTA,
                              "3PM"=ifelse(is.na(temp_player_proj[,"3PM.y"]),temp_player_proj[,"3PM.x"],(temp_player_proj[,"3PM.x"]+temp_player_proj[,"3PM.y"])/2),
                              REB=ifelse(is.na(temp_player_proj[,"REB.y"]),temp_player_proj[,"REB.x"],(temp_player_proj[,"REB.x"]+temp_player_proj[,"REB.y"])/2),
                              AST=ifelse(is.na(temp_player_proj[,"AST.y"]),temp_player_proj[,"AST.x"],(temp_player_proj[,"AST.x"]+temp_player_proj[,"AST.y"])/2),
                              STL=ifelse(is.na(temp_player_proj[,"STL.y"]),temp_player_proj[,"STL.x"],(temp_player_proj[,"STL.x"]+temp_player_proj[,"STL.y"])/2),
                              BLK=ifelse(is.na(temp_player_proj[,"BLK.y"]),temp_player_proj[,"BLK.x"],(temp_player_proj[,"BLK.x"]+temp_player_proj[,"BLK.y"])/2),
                              PTS=ifelse(is.na(temp_player_proj[,"PTS.y"]),temp_player_proj[,"PTS.x"],(temp_player_proj[,"PTS.x"]+temp_player_proj[,"PTS.y"])/2))
    
    
    
    ### COMMENCE VALUATION PROCESS ###
    
    
    player_proj$value <- 1
    
    valuation_table <- NULL
    for (iter in 1:n_iters){  
      playerlist <- NULL
      agg_team_proj <- NULL
      i <- 1
      while (i <= n_samples){
        sample_i <- sample(1:nrow(player_proj),squad_size,replace=FALSE)
        team_proj_i <- player_proj[sample_i,]
        team_perf_i <- colSums(team_proj_i[,2:12])
        if (team_perf_i[11]<=budget){
          playerlist[i] <- list(team_proj_i$player)
          agg_team_proj <- rbind(agg_team_proj,team_perf_i)
          print(paste0(iter,"/",n_iters," - ",i,"/",n_samples))
          i <- i + 1}
      }
      
      agg_team_proj <- data.frame(agg_team_proj)
      
      agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
      agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
      
      agg_team_proj <- agg_team_proj[,c("X3PM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
      
      mean_matrix <- matrix(apply(agg_team_proj, 2, mean),n_samples,8,byrow=TRUE)
      sd_matrix <- matrix(apply(agg_team_proj, 2, sd),n_samples,8,byrow=TRUE)
      std_matrix <- (agg_team_proj-mean_matrix)/sd_matrix
      scored_projections <- data.frame(id=seq(1,n_samples,1),std_matrix,score=rowSums(std_matrix))
      scored_projections$rank <- rank(-scored_projections$score)
      top_squads <- scored_projections$id[scored_projections$rank<=n_samples*high_perc]
      inc_players <- data.frame(player=unlist(playerlist[top_squads]))
      increase_table <- count(inc_players)
      names(increase_table)[2] <- "increase_points"
      bottom_squads <- scored_projections$id[scored_projections$rank>=n_samples*low_perc]
      dec_players <- data.frame(player=unlist(playerlist[bottom_squads]))
      decrease_table <- count(dec_players)
      names(decrease_table)[2] <- "decrease_points"
      
      new_valuation_table <- merge(player_proj[,c("player","value")],increase_table,by=c("player"),all=TRUE)
      new_valuation_table <- merge(new_valuation_table,decrease_table,by=c("player"),all=TRUE)
      new_valuation_table$increase_points <- ifelse(is.na(new_valuation_table$increase_points),0,new_valuation_table$increase_points)
      new_valuation_table$decrease_points <- ifelse(is.na(new_valuation_table$decrease_points),0,new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,adjustment=new_valuation_table$increase_points-new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,
                                        new_value=ifelse(
                                          new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)<=0
                                          ,0
                                          ,new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)))
      player_proj <- merge(player_proj,new_valuation_table[,c("player","new_value")])
      player_proj$value <- player_proj$new_value
      valuation_table <- rbind(valuation_table,data.frame(player=player_proj$player,value=player_proj$new_value,iteration=iter))
      player_proj <- player_proj[,names(player_proj)!="new_value"]
      #print(paste0(iter,"/",n_iters))
    }
    
    
    ##### ONCE TOP 120 PLAYERS ISOLATED RERUN VALUATIONS #####
    player_proj <- player_proj[rank(-player_proj$value, ties.method = "random")<=n_teams*squad_size,]
    player_proj$value <- rep(1,length(player_proj$value))
    
    
    valuation_table <- NULL
    for (iter in 1:n_sec_iters){  
      playerlist <- NULL
      agg_team_proj <- NULL
      i <- 1
      while (i <= n_samples){
        sample_i <- sample(1:nrow(player_proj),squad_size,replace=FALSE)
        team_proj_i <- player_proj[sample_i,]
        team_perf_i <- colSums(team_proj_i[,2:12])
        if (team_perf_i[11]<=budget){
          playerlist[i] <- list(team_proj_i$player)
          agg_team_proj <- rbind(agg_team_proj,team_perf_i)
          print(paste0(iter,"/",n_sec_iters," - ",i,"/",n_samples))
          i <- i + 1}
      }
      
      agg_team_proj <- data.frame(agg_team_proj)
      
      agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
      agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
      
      agg_team_proj <- agg_team_proj[,c("X3PM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
      
      mean_matrix <- matrix(apply(agg_team_proj, 2, mean),n_samples,8,byrow=TRUE)
      sd_matrix <- matrix(apply(agg_team_proj, 2, sd),n_samples,8,byrow=TRUE)
      std_matrix <- (agg_team_proj-mean_matrix)/sd_matrix
      scored_projections <- data.frame(id=seq(1,n_samples,1),std_matrix,score=rowSums(std_matrix))
      scored_projections$rank <- rank(-scored_projections$score)
      top_squads <- scored_projections$id[scored_projections$rank<=n_samples*high_perc]
      inc_players <- data.frame(player=unlist(playerlist[top_squads]))
      increase_table <- count(inc_players)
      names(increase_table)[2] <- "increase_points"
      bottom_squads <- scored_projections$id[scored_projections$rank>=n_samples*low_perc]
      dec_players <- data.frame(player=unlist(playerlist[bottom_squads]))
      decrease_table <- count(dec_players)
      names(decrease_table)[2] <- "decrease_points"
      
      new_valuation_table <- merge(player_proj[,c("player","value")],increase_table,by=c("player"),all=TRUE)
      new_valuation_table <- merge(new_valuation_table,decrease_table,by=c("player"),all=TRUE)
      new_valuation_table$increase_points <- ifelse(is.na(new_valuation_table$increase_points),0,new_valuation_table$increase_points)
      new_valuation_table$decrease_points <- ifelse(is.na(new_valuation_table$decrease_points),0,new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,adjustment=new_valuation_table$increase_points-new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,
                                        new_value=ifelse(
                                          new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)<=0
                                          ,0
                                          ,new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)))
      player_proj <- merge(player_proj,new_valuation_table[,c("player","new_value")])
      player_proj$value <- player_proj$new_value
      player_proj$value <- (budget*n_teams)*(player_proj$value/sum(player_proj$value))
      valuation_table <- rbind(valuation_table,data.frame(player=player_proj$player,value=player_proj$new_value,iteration=iter))
      player_proj <- player_proj[,names(player_proj)!="new_value"]
      #print(paste0(iter,"/",n_sec_iters))
    }
    
    full_valuation_table <- merge(player_proj,gp_table,all.x=TRUE)
    full_valuation_table$GP <- ifelse(is.na(full_valuation_table$GP),mean(gp_table$GP),full_valuation_table$GP)
    full_valuation_table$final_value <- (full_valuation_table$GP/sum(full_valuation_table$GP))*full_valuation_table$value
    full_valuation_table$final_value <- (budget*n_teams)*(full_valuation_table$final_value/sum(full_valuation_table$final_value))
    
    write.csv(full_valuation_table,paste0("/Users/amydonaldson/Documents/Habib/swish cheese/2016_valuations_",n_teams,"team_",squad_size,"players.csv"),row.names = FALSE)
    
    library(ggplot2)
    #library(reshape2)
    plot_data <- valuation_table[valuation_table$player%in%c("JamesHarden","KyleLowry", "TimDuncan","StephenCurry", "LeBronJames", "ChrisPaul", "AndreDrummond","MikeConley","KevinDurant","AnthonyDavis"),]
    plot_data2 <- full_valuation_table
    #plot_data <- reshape(plot_data)
    plot <- ggplot(plot_data,aes(iteration,value,colour=player)) +
      geom_line()
    
    plot_data2 <- data.frame(plot_data2,rank=rank(-plot_data2$final_value,ties.method="first"))
    plot_data2 <- data.frame(plot_data2,group=ceiling(plot_data2$rank/10))
    plot_data2$group <- paste(plot_data2$group*10-9,"-",plot_data2$group*10)
    plot_data2 <- plot_data2[with(plot_data2, order(rank)), ]
    plot_data2$group <- factor(plot_data2$group, levels = unique(plot_data2$group[sort(plot_data2$rank)]))
    plot_data2$player <- factor(plot_data2$player, levels = plot_data2$player[rank(-plot_data2$value,ties.method = "first")])
     
    plot2 <- ggplot(plot_data2,aes(player,final_value)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_wrap(~group, ncol=5, scales="free_x")
    
    ggsave(paste0("/Users/amydonaldson/Documents/Habib/swish cheese/2016_valuations_",n_teams,"team_",squad_size,"players_iterations.png"),plot)
    
    ggsave(paste0("/Users/amydonaldson/Documents/Habib/swish cheese/2016_valuations_",n_teams,"team_",squad_size,"players.png"),plot2)
    
    print(paste0("teams:",n_teams," - squad:",squad_size," run complete"))
  }
}

rm(data)
for (n_teams in 6:16){
  for (squad_size in 9:12){ 
    data_i <- read.csv(full_valuation_table,paste0("/Users/amydonaldson/Documents/Habib/swish cheese/2016_valuations_",n_teams,"team_",squad_size,"players.csv"),row.names = FALSE)
    data_i <- data_i[,c("player","final_value")]
    names(data_i)[2] <- paste0(final_value,"_",n_teams,"teams_",squad_size,"players")
    if exists(data){
      data <- merge(data,data_i,by="player",all=TRUE)
    }else{data <- data_i}
  }
}
=======
squad_size <- 10
n_teams <- 10
n_iters <- 100
n_sec_iters <- 300
high_perc <- 0.2
low_perc <- 0.8
n_samples <- 1000
output_dir <- "/Users/habibadam/Documents/swish cheese/"
player_proj_tables_prefix <- "http://games.espn.go.com/fba/tools/projections?leagueId=154961&startIndex="
sec_player_proj_tables_prefix <- "http://www1.fantasypros.com/nba/projections/avg-overall.php"

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

temp_player_proj <- merge(player_proj,sec_player_proj,by="player")

gp_table <- data.frame(player=temp_player_proj$player,GP=(temp_player_proj$GP.x+temp_player_proj$GP.y)/2)
player_proj <- player_proj[,names(player_proj)!="GP"]

player_proj <- data.frame(player=temp_player_proj$player,
                          FGM=temp_player_proj$FGM,
                          FGA=temp_player_proj$FGA,
                          FTM=temp_player_proj$FTM,
                          FTA=temp_player_proj$FTA,
                          "3PM"=(temp_player_proj[,"3PM.x"]+temp_player_proj[,"3PM.y"])/2,
                          REB=(temp_player_proj$REB.x+temp_player_proj$REB.y)/2,
                          AST=(temp_player_proj$AST.x+temp_player_proj$AST.y)/2,
                          STL=(temp_player_proj$STL.x+temp_player_proj$STL.y)/2,
                          BLK=(temp_player_proj$BLK.x+temp_player_proj$BLK.y)/2,
                          PTS=(temp_player_proj$PTS.x+temp_player_proj$PTS.y)/2)

player_proj$value <- 1

### COMMENCE VALUATION PROCESS ###


valuation_table <- NULL
for (iter in 1:n_iters){  
  playerlist <- NULL
  agg_team_proj <- NULL
  i <- 1
  while (i <= n_samples){
    sample_i <- sample(1:nrow(player_proj),squad_size,replace=FALSE)
    team_proj_i <- player_proj[sample_i,]
    team_perf_i <- colSums(team_proj_i[,2:12])
    if (team_perf_i[11]<=budget){
      playerlist[i] <- list(team_proj_i$player)
      agg_team_proj <- rbind(agg_team_proj,team_perf_i)
      print(paste0(iter,"/",n_iters," - ",i,"/",n_samples))
      i <- i + 1}
  }
  
  agg_team_proj <- data.frame(agg_team_proj)
  
  agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
  agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
  
  agg_team_proj <- agg_team_proj[,c("X3PM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
  
  mean_matrix <- matrix(apply(agg_team_proj, 2, mean),n_samples,8,byrow=TRUE)
  sd_matrix <- matrix(apply(agg_team_proj, 2, sd),n_samples,8,byrow=TRUE)
  std_matrix <- (agg_team_proj-mean_matrix)/sd_matrix
  scored_projections <- data.frame(id=seq(1,n_samples,1),std_matrix,score=rowSums(std_matrix))
  scored_projections$rank <- rank(-scored_projections$score)
  top_squads <- scored_projections$id[scored_projections$rank<=n_samples*high_perc]
  inc_players <- data.frame(player=unlist(playerlist[top_squads]))
  increase_table <- count(inc_players)
  names(increase_table)[2] <- "increase_points"
  bottom_squads <- scored_projections$id[scored_projections$rank>=n_samples*low_perc]
  dec_players <- data.frame(player=unlist(playerlist[bottom_squads]))
  decrease_table <- count(dec_players)
  names(decrease_table)[2] <- "decrease_points"
  
  new_valuation_table <- merge(player_proj[,c("player","value")],increase_table,by=c("player"),all=TRUE)
  new_valuation_table <- merge(new_valuation_table,decrease_table,by=c("player"),all=TRUE)
  new_valuation_table$increase_points <- ifelse(is.na(new_valuation_table$increase_points),0,new_valuation_table$increase_points)
  new_valuation_table$decrease_points <- ifelse(is.na(new_valuation_table$decrease_points),0,new_valuation_table$decrease_points)
  new_valuation_table <- data.frame(new_valuation_table,adjustment=new_valuation_table$increase_points-new_valuation_table$decrease_points)
  new_valuation_table <- data.frame(new_valuation_table,
                                    new_value=ifelse(
                                      new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)<=0
                                      ,0
                                      ,new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)))
  player_proj <- merge(player_proj,new_valuation_table[,c("player","new_value")])
  player_proj$value <- player_proj$new_value
  valuation_table <- rbind(valuation_table,data.frame(player=player_proj$player,value=player_proj$new_value,iteration=iter))
  player_proj <- player_proj[,names(player_proj)!="new_value"]
  #print(paste0(iter,"/",n_iters))
}


##### ONCE TOP 120 PLAYERS ISOLATED RERUN VALUATIONS #####
player_proj <- player_proj[rank(-player_proj$value)<=n_teams*squad_size,]
player_proj$value <- rep(1,length(player_proj$value))


valuation_table <- NULL
for (iter in 1:n_sec_iters){  
  playerlist <- NULL
  agg_team_proj <- NULL
  i <- 1
  while (i <= n_samples){
    sample_i <- sample(1:nrow(player_proj),squad_size,replace=FALSE)
    team_proj_i <- player_proj[sample_i,]
    team_perf_i <- colSums(team_proj_i[,2:12])
    if (team_perf_i[11]<=budget){
      playerlist[i] <- list(team_proj_i$player)
      agg_team_proj <- rbind(agg_team_proj,team_perf_i)
      print(paste0(iter,"/",n_sec_iters," - ",i,"/",n_samples))
      i <- i + 1}
  }
  
  agg_team_proj <- data.frame(agg_team_proj)
  
  agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
  agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
  
  agg_team_proj <- agg_team_proj[,c("X3PM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
  
  mean_matrix <- matrix(apply(agg_team_proj, 2, mean),n_samples,8,byrow=TRUE)
  sd_matrix <- matrix(apply(agg_team_proj, 2, sd),n_samples,8,byrow=TRUE)
  std_matrix <- (agg_team_proj-mean_matrix)/sd_matrix
  scored_projections <- data.frame(id=seq(1,n_samples,1),std_matrix,score=rowSums(std_matrix))
  scored_projections$rank <- rank(-scored_projections$score)
  top_squads <- scored_projections$id[scored_projections$rank<=n_samples*high_perc]
  inc_players <- data.frame(player=unlist(playerlist[top_squads]))
  increase_table <- count(inc_players)
  names(increase_table)[2] <- "increase_points"
  bottom_squads <- scored_projections$id[scored_projections$rank>=n_samples*low_perc]
  dec_players <- data.frame(player=unlist(playerlist[bottom_squads]))
  decrease_table <- count(dec_players)
  names(decrease_table)[2] <- "decrease_points"
  
  new_valuation_table <- merge(player_proj[,c("player","value")],increase_table,by=c("player"),all=TRUE)
  new_valuation_table <- merge(new_valuation_table,decrease_table,by=c("player"),all=TRUE)
  new_valuation_table$increase_points <- ifelse(is.na(new_valuation_table$increase_points),0,new_valuation_table$increase_points)
  new_valuation_table$decrease_points <- ifelse(is.na(new_valuation_table$decrease_points),0,new_valuation_table$decrease_points)
  new_valuation_table <- data.frame(new_valuation_table,adjustment=new_valuation_table$increase_points-new_valuation_table$decrease_points)
  new_valuation_table <- data.frame(new_valuation_table,
                                    new_value=ifelse(
                                      new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)<=0
                                      ,0
                                      ,new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)))
  player_proj <- merge(player_proj,new_valuation_table[,c("player","new_value")])
  player_proj$value <- player_proj$new_value
  player_proj$value <- (budget*n_teams)*(player_proj$value/sum(player_proj$value))
  valuation_table <- rbind(valuation_table,data.frame(player=player_proj$player,value=player_proj$new_value,iteration=iter))
  player_proj <- player_proj[,names(player_proj)!="new_value"]
  #print(paste0(iter,"/",n_sec_iters))
}

full_valuation_table <- merge(player_proj,gp_table)
full_valuation_table$final_value <- (full_valuation_table$GP/sum(full_valuation_table$GP))*full_valuation_table$value
full_valuation_table$final_value <- (budget*n_teams)*(full_valuation_table$final_value/sum(full_valuation_table$final_value))

#output to output directory (output_dir)
write.csv(full_valuation_table,paste0(output_dir,"2015_valuations_",n_teams,"team_",squad_size,"players.csv"))

#visualising results

library(ggplot2)
#library(reshape2)
plot_data <- valuation_table[valuation_table$player%in%c("JamesHarden","KyleLowry", "TimDuncan","StephenCurry", "LeBronJames", "ChrisPaul", "AndreDrummond","MikeConley","KevinDurant","AnthonyDavis"),]
plot_data2 <- full_valuation_table
#plot_data <- reshape(plot_data)
plot <- ggplot(plot_data,aes(iteration,value,colour=player)) +
        geom_line()

plot_data2 <- data.frame(plot_data2,rank=rank(-plot_data2$final_value,ties.method="first"))
plot_data2 <- data.frame(plot_data2,group=ceiling(plot_data2$rank/10))
plot_data2$group <- paste(plot_data2$group*10-9,"-",plot_data2$group*10)
plot_data2 <- plot_data2[with(plot_data2, order(rank)), ]

plot2 <- ggplot(plot_data2,aes(player,final_value)) +
        geom_bar() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        facet_wrap(~group, ncol=5, scales="free_x")

>>>>>>> b13d03a20144c276af82c08663f1af57fa968f24
