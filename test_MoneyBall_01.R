test_df <- read.csv("data/test_MoneyBall_01.csv", header=TRUE)
test_df[, "RS.predict"] <- predict(glb_sel_mdl, newdata=test_df, type="response")
print(orderBy(~RS.predict-Salary, test_df))

choose(nrow(test_df), 2)
print(player_set_df <- as.data.frame(t(combn(test_df$Player_Name, 2))))
player_set_df$V1_Salary <- sapply(1:nrow(player_set_df),
    function (row_ix) test_df[test_df$Player_Name == player_set_df[row_ix, "V1"], "Salary"])
player_set_df$V2_Salary <- sapply(1:nrow(player_set_df),
    function (row_ix) test_df[test_df$Player_Name == player_set_df[row_ix, "V2"], "Salary"])
player_set_df$Tot_Salary <- sapply(1:nrow(player_set_df),
    function (row_ix) player_set_df[row_ix, "V1_Salary"] +
                      player_set_df[row_ix, "V2_Salary"])
player_set_df$V1_RS.predict <- sapply(1:nrow(player_set_df),
    function (row_ix) test_df[test_df$Player_Name == player_set_df[row_ix, "V1"],
                              "RS.predict"])
player_set_df$V2_RS.predict <- sapply(1:nrow(player_set_df),
    function (row_ix) test_df[test_df$Player_Name == player_set_df[row_ix, "V2"],
                                                                "RS.predict"])
player_set_df$Avg_RS.predict <- sapply(1:nrow(player_set_df),
    function (row_ix) mean(player_set_df[row_ix, "V1_RS.predict"],
                            player_set_df[row_ix, "V2_RS.predict"]))
print(orderBy(~ -Avg_RS.predict -Tot_Salary, player_set_df))

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
