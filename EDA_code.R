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


# 1D: QB hit+sack -------------------------------------------------------------------------------
#Hypothesis: figuring out what teams have worse offensive lines???
nfl_passing_plays %>%
  filter(passer_player_name==c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock")) %>%
  group_by(passer_player_name) %>%
  summarize(total_hit = sum(qb_hit)+sum(sack)) %>%
  ungroup() %>%
  ggplot(aes(x=passer_player_name, y=total_hit)) +
  geom_col()+
  theme_bw()

#1D: distribution of total yards ---------------------------------------------------------------
#Hypothesis: ??
nfl_passing_plays %>%
  ggplot(aes(x=yards_gained))+
  stat_ecdf()+
  theme_bw()


#2D: if a QB is hit are they more likely to throw an interception ------------------------------
#hypothesis: if QB is hit --> more likely to throw interception
nfl_passing_plays %>%                         #should we change the 0's and 1's to names?
  group_by(qb_hit, interception) %>%
  summarize(count = n(),
            joint_prob = count / nrow(nfl_passing_plays)) %>%
  ungroup() %>%
  ggplot(aes(x=qb_hit, y=interception)) +
  geom_tile(aes(fill=count), color="white")+
  geom_text(aes(label = round(joint_prob, digits=2)), color = "white")+
  scale_fill_viridis_b()+
  theme_bw()+
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm")) 
  #no hit with interception?? not what I expected
 


#2D: when each qb scores most of their touchdowns in the game ----------------------------------- 
#hypothesis: ??
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
  

#air yards and expected points added ----------------------------------------------------------




