NFLmerge = function(season){
  load("~/Year 3 - St. Andrews/Monroe Project/Data Work/Datasets/pbp.RData")
  load(paste('~/Year 4 - William and Mary/Semester 1/Indep Study/',season,'xml.Rdata',sep=''))
  player.part = c()
  for(game in mixedsort(grep(paste('game',season,'_',sep=''), names(.GlobalEnv), value = TRUE))){
    playdf = c()
    botxml = get(game)
    # separate data set into home and away
    num_players_home = length(botxml[['players']][['home']])
    num_players_away = length(botxml[['players']][['away']])
    
    # get the individual plays for home team
    for(i in 1:num_players_home){
      data = botxml[["players"]][["home"]][[i]]
      label = paste('home_player',i,sep='_')
      play = unlist(data)[seq(10,length(unlist(data)),by=6)]
      playseq = unlist(data)[seq(12,length(unlist(data)),by=6)]
      playerid = unlist(data)[2]
      playername = unlist(data)[3]
      playernum = unlist(data)[4]
      playerpos = unlist(data)[6]
      playerteam = unlist(botxml[['players']][['home']])[4]
      gameid = unlist(botxml)[2]
      gameref = unlist(botxml)[4]
      gamedate= unlist(botxml)[6]
      df = data.frame(gameid,gameref,gamedate,play,playseq,playerid,playername,playernum,playerpos,playerteam)
      assign(label,df)
    }
    
    # combine all home players and convert into correct types
    home_players = do.call('rbind', mget(paste0("home_player_", 1:num_players_home)))
    home_players$playseq = as.numeric(as.character(home_players$playseq))
    for(i in c(1,4,6,7,9,10)){
      home_players[,i] = as.character(home_players[,i])
    }
    for(i in c(2,5,8)){
      home_players[,i] = as.numeric(as.character(home_players[,i]))
    }
    home_players[,3] = as.Date(home_players[,3])
    
    # repeat for away players
    for(i in 1:num_players_away){
      data = botxml[["players"]][["away"]][[i]]
      label = paste('away_player',i,sep='_')
      play = unlist(data)[seq(10,length(unlist(data)),by=6)]
      playseq = unlist(data)[seq(12,length(unlist(data)),by=6)]
      playerid = unlist(data)[2]
      playername = unlist(data)[3]
      playernum = unlist(data)[4]
      playerpos = unlist(data)[6]
      playerteam = unlist(botxml[['players']][['away']])[4]
      gameid = unlist(botxml)[2]
      gameref = unlist(botxml)[4]
      gamedate= unlist(botxml)[6]
      df = data.frame(gameid,gameref,gamedate,play,playseq,playerid,playername,playernum,playerpos,playerteam)
      assign(label,df)
    }
    
    # combine all home players and convert into correct types
    away_players = do.call('rbind', mget(paste0("away_player_", 1:num_players_away)))
    away_players$playseq = as.numeric(as.character(away_players$playseq))
    for(i in c(1,4,6,7,9,10)){
      away_players[,i] = as.character(away_players[,i])
    }
    for(i in c(2,5,8)){
      away_players[,i] = as.numeric(as.character(away_players[,i]))
    }
    away_players[,3] = as.Date(away_players[,3])
    
    # combine home and away to have a complete set of 
    # player participation for a single game
    game_df = rbind(away_players,home_players)
    names(game_df)[5] = 'play_id'
    game_df$play_id = as.character(game_df$play_id)
    
    # import play by play
    pbp = get(grep(paste('pbp_',season,sep=''),names(.GlobalEnv),value = TRUE))
    pbp = pbp[(pbp$HomeTeam==ifelse(unlist(botxml)[grep('home',unlist(botxml))[1]+4]=="JAC","JAX",unlist(botxml)[grep('home',unlist(botxml))[1]+4])) & pbp$AwayTeam==ifelse(unlist(botxml)[grep('away',unlist(botxml))[1]+4]=="JAC","JAX",unlist(botxml)[grep('away',unlist(botxml))[1]+4]),]
    SBdf <- game_df %>% 
      left_join(pbp, by = "play_id")
    SBdf = merge(pbp,game_df,by=intersect(names(game_df), names(pbp)))
    
    # get into dummy form
    play_id = unique(SBdf$play_id)
    playdf = unique(SBdf$play_id)
    for(j in unique(SBdf$playerid)){
      dummy = c()
      dummy_play = c()
      for(i in play_id){
        dummy_play = ifelse(i %in% SBdf[which(SBdf$playerid==j),'play_id'], 1, 0)
        dummy = rbind(dummy,dummy_play)
      }
      label = j
      playdf = data.frame(playdf,assign(label,dummy))
    }
    
    # make defensive players have -1
    offdef = c()
    for(j in unique(SBdf$playerid)){
      offdef_player = ifelse(unique(SBdf$playerpos[which(SBdf$playerid==j)]) %in% c("TE","RB","G","T","WR","QB","C","C/G","P","LS","K","OL","FB"),1,-1)
      offdef = rbind(offdef,offdef_player)
    }
    
    
    posneg = playdf[,2:length(playdf)]
    for(i in 1:(length(playdf)-1)){
      posneg[i] = offdef[i]*posneg[,i]
    }
    playdf[,2:length(playdf)] = posneg
    playdf$playdf = as.numeric(as.character(playdf$playdf))
    playdf= playdf[order(playdf$playdf),]
    SBdf$play_id = as.numeric(as.character(SBdf$play_id))
    
    # introduce wpa
    wpa = c()
    wpa_play = c()
    for(i in play_id){
      wpa_play = SBdf$WPA[which(SBdf$play_id==i)[1]]
      wpa = rbind(wpa,wpa_play)
    }
    playdf = cbind(playdf,wpa)
    gameid = c()
    id_play = c()
    for(i in play_id){
      id_play = SBdf$GameID[which(SBdf$play_id==i)[1]]
      gameid = rbind(gameid,id_play)
    }
    playdf = cbind(playdf,gameid)
    playtype = c()
    play_play = c()
    for(i in play_id){
      play_play = SBdf$PlayType[which(SBdf$play_id==i)[1]]
      playtype = rbind(playtype,play_play)
    }
    playdf = cbind(playdf,playtype)
    colnames(playdf)[2:(length(playdf)-3)] = unique(SBdf$playername)
    DefStrength = c()
    def_team = c()
    pbp = pbp[-c(which(pbp$play_id %!in% playdf$playdf)),]
    for(i in pbp$play_id){
      def_team = ifelse(pbp$PlayType[which(pbp$play_id==i)]=='Run',as.numeric(summarise(get(grep(paste('pbp_',season,sep=''),names(.GlobalEnv),value = TRUE))[pbp$DefensiveTeam==pbp$DefensiveTeam[which(pbp$play_id==i)[1]] &  pbp$PlayType=='Run',], Average = mean(EPA, na.rm = T))),as.numeric(summarise(get(grep(paste('pbp_',season,sep=''),names(.GlobalEnv),value = TRUE))[pbp$DefensiveTeam==pbp$DefensiveTeam[which(pbp$play_id==i)[1]] &  get(grep(paste('pbp_',season,sep=''),names(.GlobalEnv),value = TRUE))$PlayType=='Pass' & !is.na(pbp$EPA),], Average = mean(EPA, na.rm = T))))
      DefStrength = append(DefStrength,def_team)
    }
    playdf = cbind(playdf,DefStrength)
    
    
    
    # get rid of plays I do not need
    playdf = playdf[-which(playdf$playdf %in% unique(SBdf$play_id[SBdf$PlayType %in% c("Kickoff","Field Goal","Extra Point","Punt","No Play","QB Spike","QB Kneel")])),]
    
    colnums = c()
    for(i in 2:(ncol(playdf)-4)){
      if(abs(sum(playdf[,i]))>5){
        colnums = append(colnums,i)
      }
    }
    
    playdf = playdf[,c(1,colnums,(ncol(playdf)-4):ncol(playdf))]
    player.part = bind_rows(player.part,playdf)
  }
  player.part[is.na(player.part)] = 0
  return(player.part)
}