# script to make sure important players are represented in the regressions, to account for players who have played every snap together


# step 1: run regressions on raw player participation files

# step 2: find the people that are missing and get how many snaps they played
missing_2016 = colSums(playerpart_2016[,which(colnames(playerpart_2016) %!in% gsub("`","",rownames(summary(NFLreg_2016)$coefficients)) & colnames(playerpart_2016) %!in% c('playdf','wpa','gameid','DefStrength','playtype'))])
missing_2017 = colSums(playerpart_2017[,which(colnames(playerpart_2017) %!in% gsub("`","",rownames(summary(NFLreg_2017)$coefficients)) & colnames(playerpart_2017) %!in% c('playdf','wpa','gameid','DefStrength','playtype'))])
missing_2018 = colSums(playerpart_2018[,which(colnames(playerpart_2018) %!in% gsub("`","",rownames(summary(NFLreg_2018)$coefficients)) & colnames(playerpart_2018) %!in% c('playdf','wpa','gameid','DefStrength','playtype'))])

# any player that has played over 3 snaps should be included
missing_2016 = data.frame(missing_2016[missing_2016>2])
missing_2017 = data.frame(missing_2017[missing_2017>2])
missing_2018 = data.frame(missing_2017[missing_2018>2])

# step 3: make new dataset that has edited so that the two players that have identical snaps played differ by only one
playerpart_2017_edit = playerpart_2017
for(player in row.names(missing_2017)){
  playerpart_2017_edit[which(playerpart_2017_edit[,player]!=0)[1],player] = 0
}
NFLreg_2017_edit = glm(wpa~.-playdf-gameid-playtype,data=playerpart_2017_edit)

