library(tidyverse)
# Load all regular season passes from the 2020 regular season: ---------------------------
library(nflfastR)
nfl_passing_plays<-as_tibble(nfl_passing_plays)
head(nfl_passing_plays)

# Downs and yards --------------------------------------------------------------------------
nfl_passing_plays %>%
  ggplot(aes(x=down, y=yards_gained)) +
  geom_point() +
  theme_bw()

# QB hit ----------------------------------------------------------------------------------
nfl_passing_plays %>%
  filter(passer_player_name==c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock")) %>%
  group_by(passer_player_name) %>%
  summarize(total_hit = sum(qb_hit)) %>%
  ungroup() %>%
  ggplot(aes(x=passer_player_name, y=total_hit)) +
  geom_col()+
  #theme(axis.text.x=element_text(angle=90))+
  theme_bw()
 
#when each qb scores most of their touchdowns in the game ----------------------------------- 
nfl_passing_plays %>%
  filter(passer_player_name==c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock")) %>%
  group_by(passer_player_name) %>%
  #summarize(td_brady = sum(touchdown)) %>%
  ungroup() %>%
  ggplot(aes(x = passer_player_name, y = touchdown, fill = qtr)) +
  geom_col()


#no huddle passes vs. non no huddle passes (at what point in the game they are not huddling)----
nfl_passing_plays %>%
  ggplot(aes(x=no_huddle, fill = qtr)) +  #fill not working?
  geom_bar() +
  theme_bw()
  

#air yards and expected points added




