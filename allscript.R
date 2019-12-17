# bring in nflwar and nflscrapr
library(nflscrapR)
library(nflWAR)
library(plyr)
library(dplyr)
library(tm)

# get rid of kickoffs
kickoffplays_all = playerpart_all[playerpart_all$playtype %in% c("Kickoff","Field Goal","Extra Point","QB Kneel","Spike","Timeout"),]
playerpart_all = playerpart_all[-as.numeric(row.names(kickoffplays_all)),]

# get coefficients, after going through process outlined in other script on including important players
# NFLreg_all = glm(wpa~.-playdf-gameid-playtype,data=playerpart_2018)

# step 2: find the people that are missing and get how many snaps they played
missing_all = colSums(playerpart_all[,which(colnames(playerpart_all) %!in% gsub("`","",rownames(summary(NFLreg_all)$coefficients)) & colnames(playerpart_all) %!in% c('playdf','wpa','gameid','DefStrength','playtype'))])

# any player that has played over 3 snaps should be included
missing_all = data.frame(missing_all[missing_all>2])

# step 3: make new dataset that has edited so that the two players that have identical snaps played differ by only one
playerpart_all_edit = playerpart_all
for(player in row.names(missing_all)){
  playerpart_all_edit[which(playerpart_all_edit[,player]!=0)[1],player] = 0
}
NFLreg_all_edit = glm(wpa~.-playdf-gameid-playtype,data=playerpart_all_edit)
# only missing people are now Cardale Price (14 snaps) and Givens Price (5 snaps)

pos_vec = data.frame()
for(i in gsub("`","",rownames(summary(NFLreg_all_edit)$coefficients))){
  pos = ifelse(i %in% roster_2016$Player,roster_2016$Pos[which(roster_2016$Player==i)],NA)
  team = ifelse(i %in% roster_2016$Player,roster_2016$Team[which(roster_2016$Player==i)],NA)
  pos_vec = rbind(pos_vec,cbind(pos,team))
}
pos_vec2 = data.frame()
for(i in gsub("`","",rownames(summary(NFLreg_all_edit)$coefficients))){
  pos = ifelse(i %in% roster_all$Player,roster_all$Pos[which(roster_all$Player==i)],NA)
  team = ifelse(i %in% roster_all$Player,roster_all$Team[which(roster_all$Player==i)],NA)
  pos_vec2 = rbind(pos_vec,cbind(pos,team))
}
pos_vec3 = data.frame()
for(i in gsub("`","",rownames(summary(NFLreg_all_edit)$coefficients))){
  pos = ifelse(i %in% roster_2018$Player,roster_2018$Pos[which(roster_2018$Player==i)],NA)
  team = ifelse(i %in% roster_2018$Player,roster_2018$Team[which(roster_2018$Player==i)],NA)
  pos_vec3 = rbind(pos_vec,cbind(pos,team))
}
ipapos_all = cbind(summary(NFLreg_all_edit)$coefficients,pos_vec3)

for(i in nrow(ipapos_all)){
  if(is.na(ipapos_all[i,5])){
    ipapos_all[i,5] = ifelse(!is.na(pos_vec2[i,1]),pos_vec2[i,1],ifelse(!is.na(pos_vec[i,1]),pos_vec[i,1],NA))
  }
  if(is.na(ipapos_all[i,6])){
    ipapos_all[i,6] = ifelse(!is.na(pos_vec2[i,2]),pos_vec2[i,2],ifelse(!is.na(pos_vec[i,2]),pos_vec[i,2],NA))
  }
}

# only three players did not make it into the full dataset (Kevin Zeitler, Alex Mack, Tashawn Bower [the rest were kickers])

for(year in 2016:2018){
  roster = get(paste("roster_",year,sep=""))
  for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)]){
    ipapos_all$pos[which(rownames(ipapos_all)==i)] = ifelse(gsub(" Jr.","",gsub("`","",i)) %in% roster$Player,roster$Pos[which(roster$Player==gsub(" Jr.","",gsub("`","",i)))],NA)
    ipapos_all$team[which(rownames(ipapos_all)==i)] = ifelse(gsub(" Jr.","",gsub("`","",i)) %in% roster$Player,roster$Team[which(roster$Player==gsub(" Jr.","",gsub("`","",i)))],NA) 
  }
  for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)]){
    ipapos_all$pos[which(rownames(ipapos_all)==i)] = ifelse(gsub(" III","",gsub("`","",i)) %in% roster$Player,roster$Pos[which(roster$Player==gsub(" III","",gsub("`","",i)))],NA)
    ipapos_all$team[which(rownames(ipapos_all)==i)] = ifelse(gsub(" III","",gsub("`","",i)) %in% roster$Player,roster$Team[which(roster$Player==gsub(" III","",gsub("`","",i)))],NA) 
  }
  for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)]){
    ipapos_all$pos[which(rownames(ipapos_all)==i)] = ifelse(gsub(".1","",gsub("`","",i)) %in% roster$Player,roster$Pos[which(roster$Player==gsub(".1","",gsub("`","",i)))],NA)
    ipapos_all$team[which(rownames(ipapos_all)==i)] = ifelse(gsub(".1","",gsub("`","",i)) %in% roster$Player,roster$Team[which(roster$Player==gsub(".1","",gsub("`","",i)))],NA) 
  }
  for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)]){
    ipapos_all$pos[which(rownames(ipapos_all)==i)] = ifelse(gsub(" II","",gsub("`","",i)) %in% roster$Player,roster$Pos[which(roster$Player==gsub(" II","",gsub("`","",i)))],NA)
    ipapos_all$team[which(rownames(ipapos_all)==i)] = ifelse(gsub(" II","",gsub("`","",i)) %in% roster$Player,roster$Team[which(roster$Player==gsub(" II","",gsub("`","",i)))],NA) 
  }
  for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)]){
    ipapos_all$pos[which(rownames(ipapos_all)==i)] = ifelse(gsub(" III.1","",gsub("`","",i)) %in% roster$Player,roster$Pos[which(roster$Player==gsub(" III.1","",gsub("`","",i)))],NA)
    ipapos_all$team[which(rownames(ipapos_all)==i)] = ifelse(gsub(" III.1","",gsub("`","",i)) %in% roster$Player,roster$Team[which(roster$Player==gsub(" III.1","",gsub("`","",i)))],NA) 
  }
  for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)]){
    ipapos_all$pos[which(rownames(ipapos_all)==i)] = ifelse(gsub(" Sr.","",gsub("`","",i)) %in% roster$Player,roster$Pos[which(roster$Player==gsub(" Sr.","",gsub("`","",i)))],NA)
    ipapos_all$team[which(rownames(ipapos_all)==i)] = ifelse(gsub(" Sr.","",gsub("`","",i)) %in% roster$Player,roster$Team[which(roster$Player==gsub(" Sr.","",gsub("`","",i)))],NA) 
  }
}

for(i in row.names(ipapos_all)[is.na(ipapos_all$pos)][2:length(row.names(ipapos_all)[is.na(ipapos_all$pos)])]){
  first = strsplit(gsub("`","",i)," ")[[1]][1]
  last = strsplit(gsub("`","",i)," ")[[1]][2]
  thepage = readLines(paste("https://www.pro-football-reference.com/search/search.fcgi?hint=",first,"+",last,"&search=",first,"+",last,"&pid=&idx="))
  if(length(grep("snap_counts.201",thepage))==0){
    next
  }
  ipapos_all$pos[which(row.names(ipapos_all)==i)] = toupper(strsplit(strsplit(thepage[61],": ")[[1]][3],",")[[1]][1])
  ipapos_all$team[which(row.names(ipapos_all)==i)] = strsplit(strsplit(thepage[grep("snap_counts.201",thepage)][length(thepage[grep("snap_counts.201",thepage)])],'">')[[1]][3],"</")[[1]][1]
}
# ipapos_all = edit(ipapos_all)

# get snaps for every player
snaps_all = data.frame(colSums(playerpart_all_edit[,-which(colnames(playerpart_all_edit) %in% c('playdf','wpa','gameid','DefStrength','playtype'))]))
snap_vec = c()
for(i in gsub("`","",rownames(ipapos_all))){
  snap = ifelse(i %in% rownames(snaps_all),snaps_all[which(rownames(snaps_all)==i),],NA)
  snap_vec = append(snap_vec,snap)
}
ipapossnaps_all = cbind(ipapos_all,snap_vec)
ipapossnaps_all$snap_vec = abs(ipapossnaps_all$snap_vec)

ipapossnaps_all$team = ifelse(ipapossnaps_all$team %in% c("NWE"),"NE",ipapossnaps_all$team)
# have to do the other teams

ipapossnapsdef_all = join(ipapossnaps_all,schemes_2018,by='team')
row.names(ipapossnapsdef_all) = row.names(ipapossnaps_all)

# fix player positions
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("FS","SS","SAF","DB","LS"),"S",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("OLB","ILB","MLB"),"LB",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("NT","DT/NT","DT/LB","LDT","DL"),"DT",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("LG","RG","G/LG","G/LG/RG","LG/T/TE","LG/T/TB","LG/RG","LG/RG/T","RG/T","LG/T","G/LG/RF/T","RG/TE","G/LG/RG/T","OG","OL"),"G",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("FB","FB/TE","FB/RB","FB/RB/TE"),"RB",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("C/T","C/RG","C/LG","C/LG/TE","C/LG/RG"),"C",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("T/TE","T/TB","OT"),"T",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("TE/WR"),"TE",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("CB/RCB","LCB"),"CB",ipapossnapsdef_all$pos)
ipapossnapsdef_all$pos = ifelse(ipapossnapsdef_all$pos %in% c("DE/DT/NT"),"DE",ipapossnapsdef_all$pos)
ipapossnapsdef_all = ipapossnapsdef_all[-which(ipapossnapsdef_all$pos %in% c("P","K")),]
ipapossnapsdef_all$Scheme = ifelse(ipapossnapsdef_all$Scheme == "\"MUL\"","4-3",ipapossnapsdef_all$Scheme)
ipapossnapsdef_all$Scheme = ifelse(ipapossnapsdef_all$Scheme == "\"4-3\"","4-3",ipapossnapsdef_all$Scheme)
ipapossnapsdef_all$Scheme = ifelse(ipapossnapsdef_all$Scheme == "\"3-4\"","3-4",ipapossnapsdef_all$Scheme)

# get ipaa column
ipapossnapsdef_all$ipaa = ipapossnapsdef_all$Estimate*ipapossnapsdef_all$snap_vec

# merge with rankings information, load in file merged.RData
row.names(ipapossnapsdef_all) = gsub("`","",row.names(ipapossnapsdef_all))
Rankings = merged
test = merge(ipapossnapsdef_all,Rankings,by.x='row.names',by.y='Player',all=TRUE)
unmerged = test[is.na(test$ipaa),]
for(name in setdiff(unmerged$Row.names,c("(Intercept)","DefStrength"))){
  rank_row = which(test$Row.names==name)
  rows = setdiff(agrep(name,test$Row.names),rank_row)
  if(length(rows)==0){
    rows = setdiff(grep(paste(strsplit(name," ")[[1]][2],"$",sep=""),test$Row.names),rank_row)
    if(length(rows)!=1){
      next
    }
    if(!is.na(test$ipaa[rows])){
      next
    }
  }
  if(length(rows)==2 & ".1" %in% test$Row.names[rows]){
    next
  }
  ipa_row = ifelse(length(rows)>1,rows[1],rows)
  test = rbind(test,coalesce(test[rank_row,],test[ipa_row,]))
  test = test[-c(rank_row,ipa_row),]
}
unmerged = test[is.na(test$ipaa),]
for(name in setdiff(unmerged$Row.names,c("(Intercept)","DefStrength"))){
  rank_row = which(test$Row.names==name)
  rows = setdiff(agrep(name,test$Row.names),rank_row)
  if(length(rows)==0){
    rows = setdiff(grep(paste(strsplit(name," ")[[1]][2],"$",sep=""),test$Row.names),rank_row)
    if(length(rows)>1){
      rows = rows[which(is.na(test$G[rows]))]
    }
    if(length(rows)==0){
      next
    }
    if(!is.na(test$ipaa[rows])){
      next
    }
  }
  ipa_row = ifelse(length(rows)>1,rows[1],rows)
  test = rbind(test,coalesce(test[rank_row,],test[ipa_row,]))
  test = test[-c(rank_row,ipa_row),]
}
test1 = test[-which(is.na(test$Estimate)),]
test1 = test1[-which(is.na(test1$AP1) & test1$Row.names %!in% c("(Intercept)","DefStrength")),]
ipapossnapsdef_all1 = test1

ipapossnapsdef_all1$snap_vec = as.numeric(ipapossnapsdef_all1$snap_vec)
ipapossnapsdef_all1$Estimate = as.numeric(ipapossnapsdef_all1$Estimate)
ipapossnapsdef_all1$ipaa = as.numeric(ipapossnapsdef_all1$ipaa)

# sort by snaps and position
postable = function(position){
  table = ipapossnapsdef_all1[which(ipapossnapsdef_all1$pos==pos),]
  return(table)
}

for(pos in unique(ipapossnapsdef_all1$pos[!is.na(ipapossnapsdef_all1$pos)])){
  table1 = postable(pos)
  label = paste(pos,"table_all",sep="_")
  assign(label,table1)
}

DE_43_table_all = DE_table_all[which(DE_table_all$Scheme=="4-3"),]
DE_34_table_all = DE_table_all[which(DE_table_all$Scheme=="3-4"),]
DT_43_table_all = DT_table_all[which(DT_table_all$Scheme=="4-3"),]
DT_34_table_all = DT_table_all[which(DT_table_all$Scheme=="3-4"),]
LB_43_table_all = LB_table_all[which(LB_table_all$Scheme=="4-3"),]
LB_34_table_all = LB_table_all[which(LB_table_all$Scheme=="3-4"),]

# get replacement level snaps
replsnaps_DE43 = DE_43_table_all$snap_vec[order(DE_43_table_all$snap_vec,decreasing=TRUE)[84]]
replsnaps_DE34 = DE_34_table_all$snap_vec[order(DE_34_table_all$snap_vec,decreasing=TRUE)[44]]
replsnaps_DT43 = DT_43_table_all$snap_vec[order(DT_43_table_all$snap_vec,decreasing=TRUE)[84]]
replsnaps_DT34 = DT_34_table_all$snap_vec[order(DT_34_table_all$snap_vec,decreasing=TRUE)[33]]
replsnaps_LB43 = LB_43_table_all$snap_vec[order(LB_43_table_all$snap_vec,decreasing=TRUE)[126]]
replsnaps_LB34 = LB_34_table_all$snap_vec[order(LB_34_table_all$snap_vec,decreasing=TRUE)[77]]
replsnaps_T = T_table_all$snap_vec[order(T_table_all$snap_vec,decreasing=TRUE)[96]]
replsnaps_CB = CB_table_all$snap_vec[order(CB_table_all$snap_vec,decreasing=TRUE)[128]]
replsnaps_S = S_table_all$snap_vec[order(S_table_all$snap_vec,decreasing=TRUE)[160]]
replsnaps_G = G_table_all$snap_vec[order(G_table_all$snap_vec,decreasing=TRUE)[96]]
replsnaps_TE = TE_table_all$snap_vec[order(TE_table_all$snap_vec,decreasing=TRUE)[64]]
replsnaps_RB = RB_table_all$snap_vec[order(RB_table_all$snap_vec,decreasing=TRUE)[96]]
replsnaps_WR = WR_table_all$snap_vec[order(WR_table_all$snap_vec,decreasing=TRUE)[128]]
replsnaps_C = C_table_all$snap_vec[order(C_table_all$snap_vec,decreasing=TRUE)[32]]
replsnaps_QB = 449

# get replacement level player
for(i in grep("_table_all",names(.GlobalEnv),value=TRUE)){
  pos = strsplit(i,"_t")[[1]][1]
  table = get(grep(paste(pos,"_table",sep=""),names(.GlobalEnv),value=TRUE))
  table$snap_vec = as.numeric(table$snap_vec)
  repframe = as.numeric(get(grep(paste("replsnaps_",ifelse(pos %in% c("DT_34","DT_43","DE_43","DE_34","LB_34","LB_43"),gsub("_","",pos),pos),sep=""),names(.GlobalEnv),value=TRUE)))
  replplayer = table[table$snap_vec==repframe,]
  label = paste(pos,"_table_alla",sep="")
  table = table[-which(table$snap_vec<=replplayer$snap_vec[1]),]
  table = rbind(table,replplayer)
  row.names(table)[nrow(table)] = "replacement"
  table$ipar = as.numeric(table$ipaa)-as.numeric(table$ipaa[nrow(table)])
  table$ipar_sc = scale(table$ipar)
  diff = 0-table$ipar_sc[nrow(table)]
  table$ipar_sc = table$ipar_sc+diff
  assign(label,table)
}

