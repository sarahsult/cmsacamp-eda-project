library(tidyverse)
# Load all regular season passes from the 2020 regular season: ---------------------------
library(nflfastR)
nfl_passing_plays<-as_tibble(nfl_passing_plays)
head(nfl_passing_plays)


# 2D: Downs and yards --------------------------------------------------------------------------
nfl_passing_plays %>%
  ggplot(aes(x=down, y=yards_gained)) +
  geom_point() +
  theme_bw()


#1D: KEEPING! QB hit+sack ---------------------------------------------------------------------
#Hypothesis: figuring out what teams have worse offensive lines
nfl_passing_plays %>%
  group_by(posteam) %>%
  summarize(total_hit = sum(qb_hit)+sum(sack)) %>%
  ungroup() %>%
  mutate(posteam = fct_reorder(posteam, total_hit))%>%
  ggplot(aes(x=posteam, y=total_hit)) +
  geom_col()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle = 90))+
  labs(x= "Possessing Team",
       y= "QB hits + sacks",
       title = "Comparing NFL Team's Offensive Line Strength",
       caption = "Data courtesy of nflfastR")
#conclusion: ranking



#1D: distribution of total yards ---------------------------------------------------------------
#Hypothesis: ??
nfl_passing_plays %>%
  ggplot(aes(x=yards_gained))+
  stat_ecdf()+
  theme_bw()





#2D: KEEPING!! if a QB is hit are they more likely to throw an interception -------------------
#Hypothesis: if QB is hit --> more likely to throw interception
nfl_passing_plays %>%                         
  group_by(qb_hit, interception) %>%
  summarize(count = n(),
            joint_prob = count / nrow(nfl_passing_plays)) %>%
  ungroup() %>%
  mutate(qb_hit_name = case_when(
    qb_hit == 0 ~ "No hit",
    TRUE ~ "Hit"
  ), interception_name = case_when(
    interception == 0 ~ "No interception",
    TRUE ~ "Interception"
  )) %>%
  ggplot(aes(x=qb_hit_name, y=interception_name)) +
  geom_tile(aes(fill=count), color="white")+
  geom_text(aes(label = round(joint_prob, digits=4)), color = "white")+
  scale_fill_viridis_b()+
  theme_bw()+
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm")) +
  labs(
    x = "QB Hit",
    y = "Interception",
    title = "Interception Not More Likely When Hit",
    caption = "Data courtesy of nflfastR"
  )
#conclusion: NOT actually true!


#2D: when each qb scores most of their touchdowns in the game ----------------------------------- 
#Hypothesis: ??
nfl_passing_plays %>%
  filter(passer_player_name %in% c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock"),
         touchdown=="1") %>%
  group_by(qtr, passer_player_name) %>%
  summarize(total_TD = sum(touchdown)) %>%
  ggplot(aes(x = passer_player_name, y = total_TD, fill = factor(qtr))) +
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  labs(
    title = "Touchdowns by QB in 2020",
    x = element_blank(),
    y = "Total Touchdowns",
    fill = "Quarter"
  )





#2D: no huddle passes vs. non no huddle passes ------------------------------------------------
#Hypothesis: no huddle plays are more successful b/c you can exploit defensive line
nfl_passing_plays %>%
  ggplot(aes(x=no_huddle, fill = complete_pass)) +  #kind of expected this to have 4 bars
  geom_bar(position = "dodge") +
  theme_bw()
  




#Clustering?: air yards and expected points added ---------------------------------------------
nfl_passing_plays %>%
  ggplot(aes(x=air_yards, y=epa, color = complete_pass))+
  geom_point()+
  theme_bw()



#2D: yards gained and pass location ----------------------------------------------------------
#Hypothesis: one area of the field might be better to pass to than another??
nfl_passing_plays %>%
  filter(!is.na(pass_location)) %>%
  ggplot(aes(x=yards_gained, color = pass_location)) +
  geom_freqpoly()+
  theme_bw()+
  theme(legend.position = "bottom")


#win probability added and completed passes ---------------------------------------------------
# nfl_passing_plays %>%
#   filter(posteam == "KC") %>%
#   group_by(posteam, game_id) %>%
#   mutate(total_comp_passes = sum(complete_pass)) %>%
#   ggplot(aes(x=total_comp_passes, y=wpa, color = posteam)) +
#   geom_point()+
#   theme_bw()







#2D: Yards gained based on pass length ------------------------------------------------------
#Hypothesis: ?
nfl_passing_plays %>%
  ggplot(aes (x = play_id, y = yards_gained)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ pass_length)

