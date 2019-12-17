# Creating the WAR file
# goal: create script that can take the xml from link given 
#       by sportradar and put into the three years combined, then one full dataset
# John Kearns
# 10/24/19

# input: player part data sets for each season
# output: data sets with replacement level

# bring in nflwar and nflscrapr
library(nflscrapR)
library(nflWAR)
library(plyr)
library(dplyr)
library(tm)
'%!in%' <- function(x,y)!('%in%'(x,y))

# get rid of kickoffs
kickoffplays_2016 = playerpart_2016[playerpart_2016$playtype %in% c("Kickoff","Field Goal","Extra Point","QB Kneel","Spike","Timeout"),]
playerpart_2016 = playerpart_2016[-as.numeric(row.names(kickoffplays_2016)),]

kickoffplays_2017 = playerpart_2017[playerpart_2017$playtype %in% c("Kickoff","Field Goal","Extra Point","QB Kneel","Spike","Timeout"),]
playerpart_2017 = playerpart_2017[-as.numeric(row.names(kickoffplays_2017)),]

kickoffplays_2018 = playerpart_2018[playerpart_2018$playtype %in% c("Kickoff","Field Goal","Extra Point","QB Kneel","Spike","Timeout"),]
playerpart_2018 = playerpart_2018[-as.numeric(row.names(kickoffplays_2018)),]

kickoffplays_all = playerpart_all[playerpart_all$playtype %in% c("Kickoff","Field Goal","Extra Point","QB Kneel","Spike","Timeout"),]
playerpart_all = playerpart_all[-as.numeric(row.names(kickoffplays_all)),]

# get coefficients, after going through process outlined in other script on including important players
NFLreg_2016 = glm(wpa~.-playdf-gameid-playtype,data=playerpart_2016)
NFLreg_2017 = glm(wpa~.-playdf-gameid-playtype,data=playerpart_2017)
NFLreg_2018 = glm(wpa~.-playdf-gameid-playtype,data=playerpart_2018)
NFLreg_all = glm(wpa~.-playdf-gameid-playtype,data=playerpart_2018)


# get ipa and ipaa
ipa_2016 = summary(NFLreg_2016_edit)$coefficients[,1]
ipaa_2016 = ipa*colSums(playerpart_2016[,-which(colnames(playerpart_2016) %in% c('playdf','wpa','gameid','DefStrength','playtype')))])

ipa_2017 = summary(NFLreg_2017_edit)$coefficients[,1]

ipa_2018 = summary(NFLreg_2018_edit)$coefficients[,1]
ipaa_2018 = ipa*colSums(playerpart_2018[,-which(colnames(playerpart_2018 %in% c('playdf','wpa','gameid','DefStrength','playtype')))])

ipa_all = summary(NFLreg_all)$coefficients[,1]
ipaa_all = ipa*colSums(playerpart_all[,-which(colnames(playerpart_2016 %in% c('playdf','wpa','gameid','DefStrength','playtype')))])



# get positions for every player
roster_2016 = season_rosters(2016,positions=c("QUARTERBACK","RUNNING_BACK","WIDE_RECEIVER","TIGHT_END","DEFENSIVE_LINEMAN","LINEBACKER","DEFENSIVE_BACK"))
roster_2017 = season_rosters(2017,positions=c("QUARTERBACK","RUNNING_BACK","WIDE_RECEIVER","TIGHT_END","DEFENSIVE_LINEMAN","LINEBACKER","DEFENSIVE_BACK"))
roster_2018 = season_rosters(2018,positions=c("QUARTERBACK","RUNNING_BACK","WIDE_RECEIVER","TIGHT_END","DEFENSIVE_LINEMAN","LINEBACKER","DEFENSIVE_BACK"))

pos_vec = data.frame()
for(i in gsub("`","",rownames(summary(NFLreg_2016)$coefficients))){
  pos = ifelse(i %in% roster_2016$Player,roster_2016$Pos[which(roster_2016$Player==i)],NA)
  team = ifelse(i %in% roster_2016$Player,roster_2016$Team[which(roster_2016$Player==i)],NA)
  pos_vec = rbind(pos_vec,cbind(pos,team))
}
ipapos_2016 = cbind(summary(NFLreg_2016)$coefficients,pos_vec)




pos_vec = data.frame()
for(i in gsub("`","",rownames(summary(NFLreg_2017_edit)$coefficients))){
  pos = ifelse(i %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==i)],NA)
  team = ifelse(i %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==i)],NA)
  pos_vec = rbind(pos_vec,cbind(pos,team))
}
ipapos_2017 = cbind(summary(NFLreg_2017_edit)$coefficients,pos_vec)
# only three players did not make it into the full dataset (Kevin Zeitler, Alex Mack, Tashawn Bower [the rest were kickers])
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)]){
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" Jr.","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==gsub(" Jr.","",gsub("`","",i)))],NA)
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" Jr.","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==gsub(" Jr.","",gsub("`","",i)))],NA) 
}
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)]){
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" III","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==gsub(" III","",gsub("`","",i)))],NA)
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" III","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==gsub(" III","",gsub("`","",i)))],NA) 
}
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)]){
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = ifelse(gsub(".1","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==gsub(".1","",gsub("`","",i)))],NA)
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = ifelse(gsub(".1","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==gsub(".1","",gsub("`","",i)))],NA) 
}
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)]){
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" II","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==gsub(" II","",gsub("`","",i)))],NA)
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" II","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==gsub(" II","",gsub("`","",i)))],NA) 
}
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)]){
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" III.1","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==gsub(" III.1","",gsub("`","",i)))],NA)
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" III.1","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==gsub(" III.1","",gsub("`","",i)))],NA) 
}
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)]){
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" Sr.","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Pos[which(roster_2017$Player==gsub(" Sr.","",gsub("`","",i)))],NA)
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = ifelse(gsub(" Sr.","",gsub("`","",i)) %in% roster_2017$Player,roster_2017$Team[which(roster_2017$Player==gsub(" Sr.","",gsub("`","",i)))],NA) 
}
for(i in row.names(ipapos_2017)[is.na(ipapos_2017$pos)][2:length(row.names(ipapos_2017)[is.na(ipapos_2017$pos)])]){
  first = strsplit(gsub("`","",i)," ")[[1]][1]
  last = strsplit(gsub("`","",i)," ")[[1]][2]
  thepage = readLines(paste("https://www.pro-football-reference.com/search/search.fcgi?hint=",first,"+",last,"&search=",first,"+",last,"&pid=&idx="))
  if(length(grep("snap_counts.2017",thepage))==0){
    next
  }
  ipapos_2017$pos[which(rownames(ipapos_2017)==i)] = toupper(strsplit(strsplit(thepage[grep("snap_counts.2017",thepage)],"pos\" >")[[1]][2],"<")[[1]][1])
  ipapos_2017$team[which(rownames(ipapos_2017)==i)] = strsplit(strsplit(thepage[grep("snap_counts.2017",thepage)],'">')[[1]][3],"</")[[1]][1]
}
# ipapos_2017 = edit(ipapos_2017)


pos_vec = data.frame()
for(i in gsub("`","",rownames(summary(NFLreg_2018)$coefficients))){
  pos = ifelse(i %in% roster_2018$Player,roster_2018$Pos[which(roster_2018$Player==i)],NA)
  team = ifelse(i %in% roster_2018$Player,roster_2018$Team[which(roster_2018$Player==i)],NA)
  pos_vec = rbind(pos_vec,cbind(pos,team))
}
ipapos_2018 = cbind(summary(NFLreg_2018)$coefficients,pos_vec)


# get snaps for every player
snaps_2017 = data.frame(colSums(playerpart_2017[,-which(colnames(playerpart_2017) %in% c('playdf','wpa','gameid','DefStrength','playtype'))]))
snap_vec = c()
for(i in gsub("`","",rownames(ipapos_2017))){
  snap = ifelse(i %in% rownames(snaps_2017),snaps_2017[which(rownames(snaps_2017)==i),],NA)
  snap_vec = append(snap_vec,snap)
}
ipapossnaps_2017 = cbind(ipapos_2017,snap_vec)
ipapossnaps_2017$snap_vec = abs(ipapossnaps_2017$snap_vec)

# apply information on defensive scheme
schemes_2016 = read_excel("H:/DefSchemes.xlsx", sheet = "2016", col_names = TRUE)
schemes_2017 = read_excel("H:/DefSchemes.xlsx", sheet = "2017", col_names = TRUE)
schemes_2018 = read_excel("H:/DefSchemes.xlsx", sheet = "2018", col_names = TRUE)

ipapossnapsdef_2016 = merge(ipapossnaps_2016,schemes_2016,by='team')


ipapossnaps_2017$team = ifelse(ipapossnaps_2017$team %in% c("SFO"),"SF",ipapossnaps_2017$team)
# have to do the other teams
ipapossnapsdef_2017 = join(ipapossnaps_2017,schemes_2017)
rownames(ipapossnapsdef_2017) = rownames(ipapossnaps_2017)

ipapossnapsdef_2018 = merge(ipapossnaps_2018,schemes_2018,by='team')


# fix player positions
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("FS","SS","SAF","DB"),"S",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("OLB","ILB","MLB"),"LB",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("NT","DT/NT","DT/LB","LDT"),"DT",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("LG","RG","G/LG","G/LG/RG","LG/T/TE","LG/T/TB","LG/RG","LG/RG/T","RG/T","LG/T","G/LG/RF/T","RG/TE","G/LG/RG/T"),"G",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("FB","FB/TE","FB/RB","FB/RB/TE"),"RB",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("C/T","C/RG","C/LG","C/LG/TE","C/LG/RG"),"C",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("T/TE","T/TB"),"T",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("TE/WR"),"TE",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("CB/RCB","LCB"),"CB",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017$pos = ifelse(ipapossnapsdef_2017$pos %in% c("DE/DT/NT"),"DE",ipapossnapsdef_2017$pos)
ipapossnapsdef_2017 = ipapossnapsdef_2017[-which(ipapossnapsdef_2017$pos %in% c("P","K")),]
ipapossnapsdef_2017$Scheme = ifelse(ipapossnapsdef_2017$Scheme == "\"MUL\"","4-3",ipapossnapsdef_2017$Scheme)
ipapossnapsdef_2017$Scheme = ifelse(ipapossnapsdef_2017$Scheme == "\"4-3\"","4-3",ipapossnapsdef_2017$Scheme)
ipapossnapsdef_2017$Scheme = ifelse(ipapossnapsdef_2017$Scheme == "\"3-4\"","3-4",ipapossnapsdef_2017$Scheme)

# get ipaa column
ipapossnapsdef_2017$ipaa = ipapossnapsdef_2017$Estimate*ipapossnapsdef_2017$snap_vec

# sort by snaps and position
postable = function(position){
  table = ipapossnapsdef_2017[which(ipapossnapsdef_2017$pos==pos),]
  return(table)
}

for(pos in unique(ipapossnapsdef_2017$pos[!is.na(ipapossnapsdef_2017$pos)])){
  table1 = postable(pos)
  label = paste(pos,"table_2017",sep="_")
  assign(label,table1)
}

DE_43_table_2017 = DE_table_2017[which(DE_table_2017$Scheme=="4-3"),]
DE_34_table_2017 = DE_table_2017[which(DE_table_2017$Scheme=="3-4"),]
DT_43_table_2017 = DT_table_2017[which(DT_table_2017$Scheme=="4-3"),]
DT_34_table_2017 = DT_table_2017[which(DT_table_2017$Scheme=="3-4"),]
LB_43_table_2017 = LB_table_2017[which(LB_table_2017$Scheme=="4-3"),]
LB_34_table_2017 = LB_table_2017[which(LB_table_2017$Scheme=="3-4"),]

# get replacement level snaps
replsnaps_DE43 = DE_43_table_2017$snap_vec[order(DE_43_table_2017$snap_vec,decreasing=TRUE)[84]]
replsnaps_DE34 = DE_34_table_2017$snap_vec[order(DE_34_table_2017$snap_vec,decreasing=TRUE)[44]]
replsnaps_DT43 = DT_43_table_2017$snap_vec[order(DT_43_table_2017$snap_vec,decreasing=TRUE)[84]]
replsnaps_DT34 = DT_34_table_2017$snap_vec[order(DT_34_table_2017$snap_vec,decreasing=TRUE)[33]]
replsnaps_LB43 = LB_43_table_2017$snap_vec[order(LB_43_table_2017$snap_vec,decreasing=TRUE)[126]]
replsnaps_LB34 = LB_34_table_2017$snap_vec[order(LB_34_table_2017$snap_vec,decreasing=TRUE)[77]]
replsnaps_T = T_table_2017$snap_vec[order(T_table_2017$snap_vec,decreasing=TRUE)[96]]
replsnaps_CB = CB_table_2017$snap_vec[order(CB_table_2017$snap_vec,decreasing=TRUE)[128]]
replsnaps_S = S_table_2017$snap_vec[order(S_table_2017$snap_vec,decreasing=TRUE)[160]]
replsnaps_G = G_table_2017$snap_vec[order(G_table_2017$snap_vec,decreasing=TRUE)[96]]
replsnaps_TE = TE_table_2017$snap_vec[order(TE_table_2017$snap_vec,decreasing=TRUE)[64]]
replsnaps_RB = RB_table_2017$snap_vec[order(RB_table_2017$snap_vec,decreasing=TRUE)[96]]
replsnaps_WR = WR_table_2017$snap_vec[order(WR_table_2017$snap_vec,decreasing=TRUE)[128]]
replsnaps_C = C_table_2017$snap_vec[order(C_table_2017$snap_vec,decreasing=TRUE)[32]]
replsnaps_QB = 122

# get replacement level player
for(i in grep("_table_2017",names(.GlobalEnv),value=TRUE)){
  pos = strsplit(i,"_t")[[1]][1]
  table = get(grep(paste(pos,"_table",sep=""),names(.GlobalEnv),value=TRUE))
  repframe = get(grep(paste("replsnaps_",ifelse(pos %in% c("DT_34","DT_43","DE_43","DE_34","LB_34","LB_43"),gsub("_","",pos),pos),sep=""),names(.GlobalEnv),value=TRUE))
  replplayer = table[table$snap_vec==repframe,]
  label = paste(pos,"_table_2017a",sep="")
  table = table[-which(table$snap_vec<=replplayer$snap_vec),]
  table = rbind(table,replplayer)
  row.names(table)[nrow(table)] = "replacement"
  table$ipar = table$ipaa-table$ipaa[nrow(table)]
  assign(label,table)
}
