library(tidyverse)
library(StatsBombR)
library(DescTools)


## Import SB Data ## -------------

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches = Matches %>% filter(competition.competition_name=="UEFA Euro")
data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)


events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

events = events %>% left_join(data360, by = c("id" = "id"))

events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)


## SPADL - xT ## ---------------------------

dribble_fix <- events %>%
  group_by(match_id, period, team.name) %>%
  arrange(index) %>%
  mutate(
    dribble.end_location.x = lead(location.x),
    dribble.end_location.y = lead(location.y)
    ) %>%
  filter(type.name=="Dribble") %>%
  ungroup() %>%
  select(id, type.name, dribble.end_location.x, dribble.end_location.y)

spadl_xt <- events %>%
#  filter(match_id=="3788741") %>% # Placeholder for testing
  filter(
    !(type.name %in% c("Starting XI",
                       "Half Start"
                       ))
  ) %>%
  left_join(dribble_fix, by=c('id', 'type.name')) %>%
  group_by(match_id, period) %>%
  arrange(index) %>%
  mutate(
    end_x = case_when(
      type.name=="Pass" ~ pass.end_location.x,
      type.name=="Carry" ~ carry.end_location.x,
      type.name=="Dribble" ~ dribble.end_location.x,
      type.name=="Shot" ~ shot.end_location.x,
      TRUE ~ location.x
    ),
    end_y = case_when(
      type.name=="Pass" ~ pass.end_location.y,
      type.name=="Carry" ~ carry.end_location.y,
      type.name=="Dribble" ~ dribble.end_location.y,
      type.name=="Shot" ~ shot.end_location.y,
      TRUE ~ location.x
    ),
    type.name = case_when(
#      type.name=="Goal Keeper" & lag(type.name=="Shot") ~ "Save",
#      type.name=="Goal Keeper" & goalkeeper.outcome.name=="Claim" ~ "Claim",
      TRUE ~ type.name
    ),
    result_name = case_when(
      pass.outcome.name=="Pass Offside" ~ "offside",
      type.name %in% c("Pass",
                       "Ball Receipt*",
                       "Carry",
                       "Interception",
                       "Duel",
                       "Dribble"
                       ) & team.id == lead(possession_team.id) ~ "success",
      type.name %in% c("Pass",
                       "Ball Receipt*",
                       "Carry",
                       "Interception",
                       "Duel",
                       "Dribble"
      ) & team.id != lead(possession_team.id) ~ "fail",
      type.name=="Shot" & shot.outcome.name=="Goal" ~ "success",
      type.name=="Shot" & shot.outcome.name!="Goal" ~ "fail",
      type.name=="Miscontrol" ~ "fail",
      type.name %in% c("Ball Recovery", "Block", "Clearance", "Save", "Claim") ~ "success",
      TRUE ~ "unknown"
    ),
    type.name=case_when(
      pass.type.name=="Throw-in" ~ "Throw-in",
      pass.type.name=="Goal Kick" ~ "Goal kick",
      pass.type.name=="Free Kick" ~ "Free kick",
      pass.type.name=="Corner" ~ "Corner",
      TRUE ~ type.name
    )
  ) %>% #filter(result_name=="unknown") %>%
  mutate(time_seconds = (minute*60) + second) %>%
  select(game_id = match_id, 
         index,
         original_event_id = id, 
         period_id = period,
         time_seconds = ElapsedTime,
         team_id = team.id,
         team_name = team.name,
         player_id = player.id,
         player_name = player.name,
         type_name = type.name,
         type_id = type.id,
         start_x = location.x,
         start_y = location.y,
         end_x,
         end_y,
         result_name
         )

#	result_id	bodypart_id	action_id		result_name	bodypart_name	


## SPADL - xT Fx## ---------------------------
	
# Passes, Dribbles, Crosses
	
xt_df <- spadl_xt %>%
  filter(type_name %in% c("Pass", "Carry", "Dribble", "Shot"))

w = 12
l = 16
	
get_matrix <- function(input, l, w){
  
  input <- input %>%
    mutate(x_i = round((start_x/120)*(l-1),0)+1,
           y_i = round((start_y/80)*(w-1),0)+1) %>%
    group_by(x_i, y_i) %>%
    summarize(
      count = n()
    ) 
  
  out <- matrix(0, nrow=w, ncol=l)
  out[cbind(input$y_i, input$x_i)] <- input$count
  out
  
}
	

shot_matrix <- get_matrix(xt_df %>% filter(type_name=="Shot"), l, w)
goal_matrix <- get_matrix(xt_df %>% filter(type_name=="Shot", result_name=="success"), l, w)
move_matrix <- get_matrix(xt_df %>% filter(type_name %in% c("Pass", "Carry", "Dribble")), l, w)

total_matrix <- move_matrix + shot_matrix
move_matrix <- move_matrix / total_matrix

score_matrix <- goal_matrix / shot_matrix
score_matrix[is.nan(score_matrix)] = 0

shot_matrix <- shot_matrix / total_matrix


get_flat_idx <- function(xs, ys, l){
  (((l*ys) + xs) -l)
}

get_transition_matrix <- function(input, l, w){
  
  input <- input %>%
    mutate(x_i = round((start_x/120)*(l-1),0)+1,
           y_i = round((start_y/80)*(w-1),0)+1,
           x_j = round((end_x/120)*(l-1),0)+1,
           y_j = round((end_y/80)*(w-1),0)+1) %>%
    filter(!is.na(x_j))
  
  trans_matrix <- matrix(0, nrow=w*l, ncol=l*w)
  
  start_cell <- get_flat_idx(input$x_i, input$y_i, l)
  end_cell <- get_flat_idx(input$x_j, input$y_j, l)
  X <- tibble(start_cell, end_cell, input$result_name)
  
  totals <- X %>%
    group_by(start_cell) %>%
    summarize(total=n())
  
  for(i in 1:w*l){
    vc2 = X %>%
      filter(start_cell==i) %>%
      group_by(end_cell) %>%
      summarize(value = n())
    
    for(j in vc2$end_cell){
      trans_matrix[i, j] = (vc2 %>% filter(j==end_cell) %>% pull(value))/(totals %>% filter(i==start_cell) %>% pull(total))
    }
  }
  
  trans_matrix
    
}
  
trans_matrix <- get_transition_matrix(xt_df %>%
                                        filter(type_name %in% c("Pass", "Carry", "Dribble")) %>%
                                        filter(result_name=="success"), l, w)

expected_threat_fx <- function(p_scoring, p_shot, p_move, transition_matrix){
  
  xt <- matrix(0, nrow=w, ncol=l)
  eps = 1e-7
  gs = p_scoring * p_shot
  it=0
  diff=1
  
  while(any(diff>eps)){
    total_payoff = matrix(0, nrow=w, ncol=l)
    
    for(y in (0:(w-1))){
      for(x in (0:(l-1))){
        for(q in (0:(w-1))){
          for(z in (0:(l-1))){
            
            total_payoff[y+1, x+1] <- total_payoff[y+1, x+1] + (transition_matrix[(l*y + x)+1, (l*q + z)+1] * xt[q+1, z+1])
            
          }}}}
    
    new_xt <- gs + (p_move * total_payoff)
    diff <- new_xt - xt
    xt <- new_xt
    it <- it + 1
  }
  print(paste0("Total iterations to convergence: ", it))
    
  xt
    
}

xt <- expected_threat_fx(score_matrix, shot_matrix, move_matrix, trans_matrix);


pheatmap::pheatmap(xt, cluster_rows = FALSE, cluster_cols = FALSE)

## xG ## -------------------

ffs = events %>% 
  select(original_event_id= id, game_id = match_id, freeze_frame) %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))
  
ffs <- ffs %>% filter(!is.na(ff_location.x), !is.na(ff_location.y))

ffs <- spadl_xt %>%
  left_join(ffs,
            by=c("original_event_id", "game_id")) %>%
  ungroup()


#shots = events %>% 
#  filter(type.name=="Shot") %>%
#  select(type.name, shot.statsbomb_xg, under_pressure, shot.type.name, shot.first_time, shot.body_part.name, shot.technique.name, DistToGoal, DistToKeeper, AngleToGoal, AngleToKeeper, AngleDeviation, DistSGK, density, density.incone,distance.ToD1, distance.ToD2, DefendersBehindBall, DefendersInCone, InCone.GK, DefArea, distance.ToD1.360, distance.ToD2.360)

#all_events = events %>%
#  filter(type.name %in% c("Pass", "Dribble", "Carry", "Shot")) %>%
#  select(id, type.name, shot.statsbomb_xg, location.x.GK, location.y.GK, under_pressure, shot.type.name, shot.first_time, shot.body_part.name, shot.technique.name, DistToGoal, DistToKeeper, AngleToGoal, AngleToKeeper, AngleDeviation, DistSGK, density, density.incone,distance.ToD1, distance.ToD2, DefendersBehindBall, DefendersInCone, InCone.GK, DefArea, distance.ToD1.360, distance.ToD2.360)


goalkeeper_in <- ffs %>%
  filter(keeper==TRUE, teammate==FALSE) %>%
  ungroup() %>%
  select(game_id,original_event_id) %>%
  distinct() %>%
  mutate(gk_dummy = 1)

actor_in <- ffs %>%
  filter(actor==TRUE) %>%
  ungroup() %>%
  select(game_id,original_event_id) %>%
  distinct() %>%
  mutate(actor_dummy = 1)

player_in <- ffs %>%
  group_by(original_event_id) %>%
  mutate(max = max(ff_location.x)) %>%
  filter(ff_location.x==max) %>%
  ungroup() %>%
  mutate(player_dummy = ifelse(ff_location.x>=80, 1, 0)) %>%
  filter(player_dummy==1) %>%
  select(game_id,original_event_id, player_dummy) 



area_input <- ffs %>%
  full_join(goalkeeper_in, by=c("original_event_id", "game_id")) %>%
  full_join(actor_in, by=c("original_event_id", "game_id")) %>%
  full_join(player_in, by=c("original_event_id", "game_id")) %>%
  filter(gk_dummy==1, actor_dummy==1, player_dummy==1) %>%
  filter(!is.na(ff_location.x), !is.na(ff_location.y)) %>%
  filter(type_name %in% c("Pass", "Carry", "Dribble", "Shot"),
         start_x>80) #%>%
#  filter(game_id %in% c("3788765", "3788771"))


shotinfo_fx <- function(input_event){
  
  input <- ffs %>%
    filter(original_event_id == input_event)
  
  dist <- input %>% 
    filter(teammate==FALSE) %>%
    select(loc.x=ff_location.x, loc.y=ff_location.y, keeper) %>%
    mutate(actor.x = input %>% filter(actor==TRUE) %>% pull(ff_location.x), 
           actor.y = input %>% filter(actor==TRUE) %>% pull(ff_location.y),
           distance = (((actor.x-loc.x)^2) + ((actor.y-loc.y)^2))^0.5)
  
  pol_x = c(input %>% filter(actor==TRUE) %>% pull(ff_location.x), 120, 120)
  pol_y = c(input %>% filter(actor==TRUE) %>% pull(ff_location.y), 36, 44)
  
  dist <- cbind(dist, 
                sp::point.in.polygon(dist$loc.x, dist$loc.y, pol_x, pol_y, mode.checked=F))
  
  names(dist)[7] <- "in_cone"
  
  dist <- dist %>%
    mutate(in_cone = ifelse(in_cone!=0, 1, 0))
  
  tibble(
    id = input_event,
    location.x=input %>% filter(actor==TRUE) %>% pull(ff_location.x),
    location.y=input %>% filter(actor==TRUE) %>% pull(ff_location.y),
    location.x.GK=input %>% filter(keeper==TRUE) %>% pull(ff_location.x),
    location.y.GK=input %>% filter(keeper==TRUE) %>% pull(ff_location.y),
    distance.ToD1=dist %>% filter(keeper==FALSE) %>% arrange(distance) %>% slice(1) %>% pull(distance),
    distance.ToD2=dist %>% filter(keeper==FALSE) %>% arrange(distance) %>% slice(2) %>% pull(distance),
    DefendersInCone=dist %>% filter(keeper==FALSE) %>% summarize(sum=sum(in_cone)) %>% pull(sum),
    InCone.GK=dist %>% filter(keeper==TRUE) %>% pull(in_cone)
  )
    
    
}

#shot_train <- events %>%
#  filter(type.name=="Shot") %>%
#  select(id, shot.outcome.name, under_pressure, shot.type.name, shot.first_time, shot.body_part.name, shot.technique.name)

#shot_info <- map_df(unique(shot_train$id), shotinfo_fx)

all_shot_info <- map_df(unique(area_input$original_event_id), shotinfo_fx)

all_train <- events %>%
  filter(type.name %in% c("Pass", "Dribble", "Carry", "Shot")) %>%
  select(id, type.name, shot.outcome.name, under_pressure, shot.type.name, shot.first_time, shot.body_part.name, shot.technique.name)

all_train <- all_train %>%
  inner_join(all_shot_info, by="id") %>%
  filter(!(shot.type.name %in% c("Penalty", "Free Kick"))) %>%
  mutate(location.x = ifelse(location.x == 120 & location.y == 40, 119.66666, location.x)) %>%
  mutate(location.x.GK = ifelse(location.x.GK == 120 & location.y.GK == 40, 119.88888, location.x.GK)) %>%
  mutate(DistToGoal = sqrt((location.x - 120)^2 + (location.y - 40)^2),
         DistToKeeper = sqrt((location.x.GK - 120)^2 + (location.y.GK - 40)^2)) %>%
  mutate(AngleToGoal = ifelse(location.y <= 40, asin((120-location.x)/DistToGoal), (pi/2) + acos((120-location.x)/DistToGoal))) %>%
  mutate(AngleToKeeper = ifelse(location.y.GK <= 40, asin((120-location.x.GK)/DistToKeeper), (pi/2) + acos((120-location.x.GK)/DistToKeeper))) %>%
  mutate(AngleToGoal = AngleToGoal*180/pi) %>%
  mutate(AngleToKeeper = AngleToKeeper*180/pi) %>%
  mutate(AngleDeviation = abs(AngleToGoal-AngleToKeeper)) %>%
  mutate(DistSGK = sqrt((location.x - location.x.GK)^2 + (location.y - location.y.GK)^2)) %>%
  mutate(
    under_pressure = ifelse(is.na(under_pressure), FALSE, under_pressure),
    shot.type.name = "Open Play",
    shot.first_time = ifelse(is.na(shot.first_time), FALSE, shot.first_time),
    shot.body_part.name = ifelse(shot.body_part.name=="Head", "Head", "Feet"),
    shot.body_part.name = ifelse(is.na(shot.body_part.name), "Feet", shot.body_part.name),
    shot.technique.name = ifelse(is.na(shot.technique.name), "Normal", shot.technique.name),
    shot.outcome.name = ifelse(shot.outcome.name=="Goal", 1, 0)
  )

## xG Model ## -------------------
library(tidymodels)

all_train <- all_train %>%
  mutate(shot.outcome.name = as.factor(shot.outcome.name),
         under_pressure=as.factor(under_pressure),
         shot.first_time=as.factor(shot.first_time)) %>%
  select(-shot.type.name)

shot_train <- all_train %>%
  filter(type.name=="Shot") %>%
  select(-type.name)

preprocessing_recipe <- 
  recipes::recipe(shot.outcome.name ~ ., data = shot_train) %>%
  recipes::update_role(id, new_role="id") %>%
  recipes::step_dummy(c(under_pressure, shot.first_time, shot.body_part.name, shot.technique.name)) %>%
  prep()

train_cv_folds <- 
  recipes::bake(
    preprocessing_recipe, 
    new_data = shot_train
  ) %>%  
  rsample::vfold_cv(v = 3)


xgboost_model <- 
  parsnip::boost_tree(
    trees = 200,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgboost_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )

xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 20
  )

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(shot.outcome.name ~ .)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = train_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(roc_auc),
  control = tune::control_grid(verbose = TRUE)
)


xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("roc_auc")


xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)


train_processed <- bake(preprocessing_recipe,  new_data = shot_train)

all_data_processed <- bake(preprocessing_recipe, new_data = all_train %>% select(-type.name))

all_data_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = shot.outcome.name ~ ., 
    data    = all_data_processed
  ) %>%
  predict(new_data = all_data_processed, type="prob") %>%
  bind_cols(all_train) %>%
  select(id, xg = .pred_1)


#xgboost_model_final %>%
#  fit(
#    formula = shot.outcome.name ~ ., 
#    data    = train_processed
#  ) %>%
#  vip::vip(geom = "point")



## Def Positioning ## -------------------


ffs <- events %>% 
  select(original_event_id= id, game_id = match_id, freeze_frame) %>%
  unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))

ffs <- spadl_xt %>%
  left_join(ffs,
            by=c("original_event_id", "game_id")) 



goalkeeper_in <- ffs %>%
  filter(keeper==TRUE, teammate==FALSE) %>%
  ungroup() %>%
  select(game_id,original_event_id) %>%
  distinct() %>%
  mutate(gk_dummy = 1)

actor_in <- ffs %>%
  filter(actor==TRUE) %>%
  ungroup() %>%
  select(game_id,original_event_id) %>%
  distinct() %>%
  mutate(actor_dummy = 1)

player_in <- ffs %>%
  group_by(original_event_id) %>%
  mutate(max = max(ff_location.x)) %>%
  filter(ff_location.x==max) %>%
  ungroup() %>%
  mutate(player_dummy = ifelse(ff_location.x>=80, 1, 0)) %>%
  filter(player_dummy==1) %>%
  select(game_id,original_event_id, player_dummy) 



area_input <- ffs %>%
  full_join(goalkeeper_in, by=c("original_event_id", "game_id")) %>%
  full_join(actor_in, by=c("original_event_id", "game_id")) %>%
  full_join(player_in, by=c("original_event_id", "game_id")) %>%
  filter(gk_dummy==1, actor_dummy==1, player_dummy==1) %>%
  filter(!is.na(ff_location.x), !is.na(ff_location.y)) %>%
  filter(type_name %in% c("Pass", "Carry", "Dribble", "Shot"),
         start_x>80) #%>%
#  filter(game_id %in% c("3788765", "3788771"))




library(sf)

r <- raster::raster(xt)
raster::extent(r) <- c(0, 120, 0, 80)

r_st <- stars::st_as_stars(r)
r_sf <- st_as_sf(r_st)


area_fx <- function(i){
  
  input <- area_input %>% filter(original_event_id==i) 
  
  last_line <- input %>%
    filter(teammate==FALSE, keeper==FALSE) %>%
    filter(ff_location.x == max(ff_location.x)) %>%
    pull(ff_location.x)
  
  input <- input %>%
    mutate(dummy = ifelse(teammate==TRUE & ff_location.x>last_line & actor==FALSE, 1, 0)) %>%
    filter(dummy!=1) %>%
    select(-dummy)
  
  #%>%
#    mutate(loc.x = ff_location.x,
#           loc.y = ff_location.y,
#           ff_location.x = ifelse(ff_location.x<102, 102, ff_location.x),
#           ff_location.y = ifelse(ff_location.y>62, 62, ff_location.y),
#           ff_location.y = ifelse(ff_location.y<18, 18, ff_location.y))
  
  input_sf <- st_as_sf(input,
                   coords = c("ff_location.x", "ff_location.y"))
  

  input_voro <-  st_coordinates(input_sf$geometry) %>%
      st_multipoint() %>%
      st_voronoi() %>%
      st_collection_extract() %>%
      st_sfc(crs = st_crs(input_sf)) %>%
      st_crop(xmin = 0, ymin = 0, xmax = 120, ymax = 80) %>%
      st_as_sf()
    
  spat_weighted_mean <- function(voro) {

    voro <- st_sfc(voro, crs = st_crs(input_sf))
    areas <- st_intersection(r_sf, voro)
    
    areas$area <- as.numeric(st_area(areas))
    areas$xt <- areas$area * areas$layer
    
    xt <- sum(areas$xt)
#    xt <- weighted.mean(areas$layer, as.numeric(st_area(areas)))
    
    inter <- lengths(st_intersects(input_sf$geometry, voro))>0
    
    point <- input_sf[inter,"geometry"]
    
    tibble(point, voro, xt)
  }
  
  weighted_mean <- map_df(input_voro$x, spat_weighted_mean)
  
#  completion = (match(i, unique(area_input$original_event_id))/length(unique(area_input$original_event_id)))*100
#  print(paste0(completion, "%"))
  
  input_sf %>%
    left_join(weighted_mean, by="geometry")
  
}


xt_applied <- map_df(unique(area_input$original_event_id), area_fx)


## Defensive Effectiveness ## -------------------

match_teams <- events %>%
  select(game_id = match_id, team_1 = possession_team.name, team_2 = OpposingTeam) %>%
  distinct() %>%
  group_by(game_id) %>%
  slice_head(n=1) %>%
  ungroup()

goals <- spadl_xt %>%
  filter(period_id<5) %>%
  mutate(goal = ifelse(type_name=="Shot" & result_name=="success", 1, 0),
         shot = ifelse(type_name=="Shot", 1, 0)) %>%
  filter(type_name=="Shot") %>%
  inner_join(all_data_prediction %>%
               rename(original_event_id = id, shot_xg = xg), by="original_event_id") %>%
  group_by(game_id, team_name) %>%
  summarize(
    goals = sum(goal),
    shots = sum(shot),
    shot_xg = sum(shot_xg)
  )

goals_event <- spadl_xt %>%
  filter(period_id<5) %>%
  mutate(goal = ifelse(type_name=="Shot" & result_name=="success", 1, 0),
         shot = ifelse(type_name=="Shot", 1, 0)) %>%
  filter(type_name=="Shot") %>%
  inner_join(all_data_prediction %>%
               rename(original_event_id = id, shot_xg = xg), by="original_event_id") %>%
  ungroup() %>%
  select(original_event_id, team_name,shot_xg) 
  



output <- xt_applied %>%
  left_join(all_data_prediction %>% rename(original_event_id=id), by="original_event_id")


def_area_event <- xt_applied %>%
  left_join(match_teams, by="game_id") %>%
  mutate(
    attacking_team = team_name,
    defending_team = ifelse(attacking_team==team_1, team_2, team_1),
    side = ifelse(teammate==TRUE, "attacking", "defending")
  ) %>%
  group_by(original_event_id, game_id, side, teammate, attacking_team, defending_team) %>%
  summarize(
    xt = sum(xt, na.rm=T),
#    actor_xt = sum(xt[actor==TRUE], na.rm=T),
    players = n()
  ) %>%
  left_join(all_data_prediction %>% rename(original_event_id=id), by="original_event_id") %>%
  st_set_geometry(NULL) %>%
  left_join(xt_applied %>%
              filter(actor==TRUE) %>%
              select(original_event_id, actor_xt = xt) %>%
              mutate(actor_xt = ifelse(is.na(actor_xt), 0, actor_xt)) %>%
              st_set_geometry(NULL),
            by="original_event_id")

def_area_match <- def_area_event %>%
  group_by(game_id, attacking_team, defending_team, side, teammate) %>%
  summarize(
    xt = sum(xt),
    events = n(),
    xt_per = xt/events,
    actor_xt = sum(actor_xt),
    actor_xt_per = actor_xt/events,
    xg = sum(xg, na.rm=T),
    xg_per = xg/events
  ) 

def_area_comp <- def_area_match %>%
  group_by(defending_team, side, teammate) %>%
  summarize(
    xt = sum(xt),
    events = sum(events),
    xt_per = xt/events
  )

## CORR ##

corr_att <- def_area_match %>%
  filter(side=="attacking") %>%
  left_join(goals %>% rename(attacking_team=team_name), by=c("game_id", "attacking_team")) %>%
  mutate(goals_per = goals/events,
         shots_per = shots/events,
         shot_xg_per = shot_xg/events) %>%
  ungroup() %>%
  left_join(
    def_area_match %>%
      ungroup() %>%
      filter(side=="defending") %>%
      select(game_id, attacking_team, defending_team, def_xt = xt, def_events = events, def_xt_per = xt_per),
    by=c("game_id", "attacking_team", "defending_team")
  )

corr_def <- def_area_match %>%
  filter(side=="defending") %>%
  left_join(goals %>% rename(defending_team=team_name), by=c("game_id", "defending_team")) %>%
  mutate(goals_per = goals/events,
         shots_per = shots/events,
         shot_xg_per = shot_xg/events) %>%
  left_join(
    def_area_match %>%
      ungroup() %>%
      filter(side=="attacking") %>%
      select(game_id, attacking_team, defending_team, att_xt = xt, att_events = events, att_xt_per = xt_per),
    by=c("game_id", "attacking_team", "defending_team")
  )

corr_att_event <- def_area_event %>%
  filter(side=="attacking") %>%
  left_join(goals_event %>% rename(attacking_team=team_name), by=c("original_event_id")) 


summary(lm(shot_xg_per ~ xt_per + xg_per, corr_att))
reg_mod <- lm(shot_xg_per ~ xt_per + xg_per + def_xt_per, corr_att)
summary(reg_mod)

reg_predict <- def_area_event %>%
  ungroup() %>%
  filter(side=="attacking") %>%
  select(original_event_id, xt_per = xt, xg_per = xg) %>%
  left_join(
    def_area_event %>%
      ungroup() %>%
      filter(side=="defending") %>%
      select(original_event_id, def_xt_per = xt),
    by="original_event_id"
  ) %>% distinct()

reg_predict$pred <- predict.lm(reg_mod, reg_predict)

## Bad and Good Examples ## ----------------------

# Bad #

bad_def <- reg_predict %>%
  filter(xg_per<0.06) %>%
  arrange(desc(xt_per)) %>%
  ungroup() %>%
  slice_head(n=50) %>%
  slice_sample(n=1) %>%
  mutate(att_pct = round(100*(xt_per/(xt_per + def_xt_per)), 0))


bad_def_plot <- 
  area_input %>%
  #xt_applied %>%
#  filter(original_event_id == bad_def %>% pull(original_event_id)) %>%
  inner_join(bad_def %>% select(original_event_id), by='original_event_id') %>%
  mutate(`Player Type` = case_when(
    actor==TRUE ~ "Attacker w/ Ball",
    keeper==TRUE ~ "Keeper",
    teammate==TRUE ~ "Attacker",
    teammate==FALSE ~ "Defender"
  ))


voronoi <- deldir::deldir(bad_def_plot$ff_location.x, bad_def_plot$ff_location.y, rw=c(80,120,0,80))

bad_def_plot <- bad_def_plot %>%
  ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=`Player Type`),
             size = 6, alpha = 0.8, shape=21) +
  labs(title = "Bad Defending", subtitle = "Based on Expected Threat Controlled by Defenders and Pre-Shot Attacker w/Ball xG", caption = "Note:\n- Data from StatsBomb (See: https://github.com/statsbomb/open-data)\n- Expected Threat (xT) calculated using StatsBomb Euro 2020 open data, using Karun Singh's methodology (See: https://karun.in/blog/expected-threat.html).\n- Attacker w/ Ball expected goals is calculated as the hypothetical pre-shot xG based off of the attacker w/ ball location, regardless of whether or not the player shot the ball.") +
  annotate(geom="label",x=90, y=40, label=paste0("xT Controlled by Attackers = ", bad_def %>% pull(att_pct),"%"),fontface='bold', size = 6, family="Roboto",
           label.padding = unit(0.4, "lines")) +
  geom_segment(
    aes(x=x1, y=y1, xend=x2, yend=y2),
    size=0.75, alpha=0.3,
    data=voronoi$dirsgs,
    linetype=2,
    color= "black") +
    coord_flip(xlim = c(85, 125)) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=10,family="Roboto", hjust=0, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Roboto", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=14,family="Roboto"),
        legend.margin = margin(c(20, 10, -65, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 24, family="Roboto", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Roboto"))


bad_def_plot


base_size <- 8.5
asp <- 1.5
ggsave(
  filename = 'bad_def.png', 
  plot = bad_def_plot,
  height = base_size, 
  width = base_size * asp
)

# Good #

good_def <- reg_predict %>%
  filter(xg_per<0.06) %>%
  arrange(xt_per) %>%
  ungroup() %>%
  slice_head(n=50) %>%
  slice_sample(n=1) %>%
  mutate(att_pct = round(100*(xt_per/(xt_per + def_xt_per)), 0))


good_def_plot <- 
  area_input %>%
  #xt_applied %>%
  #  filter(original_event_id == bad_def %>% pull(original_event_id)) %>%
  inner_join(good_def %>% select(original_event_id), by='original_event_id') %>%
  mutate(`Player Type` = case_when(
    actor==TRUE ~ "Attacker w/ Ball",
    keeper==TRUE ~ "Keeper",
    teammate==TRUE ~ "Attacker",
    teammate==FALSE ~ "Defender"
  ))


voronoi <- deldir::deldir(good_def_plot$ff_location.x, good_def_plot$ff_location.y, rw=c(80,120,0,80))

good_def_plot <- good_def_plot %>%
  ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=`Player Type`),
             size = 6, alpha = 0.8, shape=21) +
  labs(title = "Good Defending", subtitle = "Based on Expected Threat Controlled by Defenders and Pre-Shot Attacker w/Ball xG", caption = "Note:\n- Data from StatsBomb (See: https://github.com/statsbomb/open-data)\n- Expected Threat (xT) calculated using StatsBomb Euro 2020 open data, using Karun Singh's methodology (See: https://karun.in/blog/expected-threat.html).\n- Attacker w/ Ball expected goals is calculated as the hypothetical pre-shot xG based off of the attacker w/ ball location, regardless of whether or not the player shot the ball.") +
  annotate(geom="label",x=90, y=40, label=paste0("xT Controlled by Attackers = ", good_def %>% pull(att_pct),"%"),fontface='bold', size = 6, family="Roboto",
           label.padding = unit(0.4, "lines")) +
  geom_segment(
    aes(x=x1, y=y1, xend=x2, yend=y2),
    size=0.75, alpha=0.3,
    data=voronoi$dirsgs,
    linetype=2,
    color= "black") +
  coord_flip(xlim = c(85, 125)) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=10,family="Roboto", hjust=0, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Roboto", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=14,family="Roboto"),
        legend.margin = margin(c(20, 10, -65, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 24, family="Roboto", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Roboto"))


good_def_plot

base_size <- 8.5
asp <- 1.5
ggsave(
  filename = 'good_def.png', 
  plot = good_def_plot,
  height = base_size, 
  width = base_size * asp
)

## Aggregate Plot ## ------------

plot_input <- def_area_event %>%
  left_join(reg_predict %>% select(original_event_id, pred), by='original_event_id') %>%
  group_by(game_id, attacking_team, defending_team, side, teammate) %>%
  summarize(
    xt = sum(xt),
    events = n(),
    xt_per = xt/events,
    actor_xt = sum(actor_xt),
    actor_xt_per = actor_xt/events,
    xg = sum(xg, na.rm=T),
    xg_per = xg/events,
    pred = sum(pred, na.rm=T),
    pred_per = pred/events
  ) 

plot_input <- plot_input %>%
  filter(side=="defending") %>%
  left_join(plot_input %>% filter(side=="attacking") %>%
              select(game_id, attacking_team, defending_team, att_xt_per= xt_per),
            by=c("game_id", "attacking_team", "defending_team")) %>%
  mutate(def_xt_share = 100*(xt_per/(att_xt_per + xt_per))) %>%
  group_by(defending_team) %>%
  summarize(
    events = sum(events),
    matches = n(),
    events_per = events/matches,
    def_xt_share = mean(def_xt_share),
    pred_per = mean(pred_per),
    pred_total = (events*pred_per)/matches
  ) %>%
  mutate(
    rel_xt_share = def_xt_share - mean(def_xt_share)
  )

p <- plot_input %>%
  ggplot(aes(x=reorder(defending_team, -rel_xt_share), y = rel_xt_share, fill = events_per)) +
  geom_bar(stat="identity", color="black") +
  scale_fill_gradient(low=("white"), high=("red"), name= "Opposition Final 1/3\nActions per Match") +
  geom_text(data=subset(plot_input, rel_xt_share>0), aes(label=defending_team, x=reorder(defending_team, -rel_xt_share), y = rel_xt_share+0.1), hjust=0) +
  geom_text(data=subset(plot_input, rel_xt_share<0), aes(label=defending_team, x=reorder(defending_team, -rel_xt_share), y = rel_xt_share-0.1), hjust=1) +
  coord_flip(ylim=c(-6,4)) +
  ggpubr::theme_pubr() +
  theme(text=element_text(family="Roboto")) +
   labs(title = "Euro 2020: Which defenses effectively controlled opposition expected threat?", 
       subtitle = "Expected Threat Controlled by Defenders per Opposition Open Play Actions in the Final 1/3", 
       caption = "Note:\n- Average Defensive Expected Threat Share is calculated by determining, for each opposition open play action in the final 1/3, the share of expected threat controlled by the defensive team relative \nto the attacking team.\n- Data from StatsBomb (See: https://github.com/statsbomb/open-data)\n- Expected Threat (xT) calculated using StatsBomb Euro 2020 open data, using Karun Singh's methodology (See: https://karun.in/blog/expected-threat.html).",
       y = "Average Defensive Expected Threat Share\n(Relative to Average)") +
  theme(legend.position = "right",
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=10,family="Roboto", hjust=0, vjust=0.5),
        aspect.ratio = c(65/100)) 
  

base_size <- 8.5
asp <- 1.5
ggsave(
  filename = 'bar.png', 
  plot = p,
  height = base_size, 
  width = base_size * asp
)




scatter_input <- corr_att %>%
  mutate(def_xt_per_share = 100*(def_xt_per/(def_xt_per+xt_per)))

summary(lm(shot_xg_per ~ def_xt_per_share, data=scatter_input))

p <- scatter_input %>%
  ggplot(aes(x=def_xt_per_share, y=shot_xg_per)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black") +
  ggpubr::theme_pubr() +
  theme(text=element_text(family="Roboto")) +
  labs(title = "Euro 2020: Defensive control over expected threat correlated with a decrease in opposition shot xG per final 1/3 action", 
       subtitle = "Expected Threat Controlled by Defenders per Opposition Open Play Actions in the Final 1/3 by Match", 
       caption = "Note:\n- Average Defensive Expected Threat Share is calculated by determining, for each opposition open play action in the final 1/3, the share of expected threat controlled by the\n defensive team relative to the attacking team.\n- Opposition Shot xG per Final 1/3 Action is calculated as the opposition's total shot xG in a given match divided by the opposition team's total number of open play actions in\n the final 1/3.\n- Data from StatsBomb (See: https://github.com/statsbomb/open-data).\n- Expected Threat (xT) calculated using StatsBomb Euro 2020 open data, using Karun Singh's methodology (See: https://karun.in/blog/expected-threat.html).",
       x = "Average Defensive Expected Threat Share",
       y = "Opposition Shot xG per Final 1/3 Action") +
  theme(legend.position = "right",
        plot.caption=element_text(size=10,family="Roboto", hjust=0, vjust=0.5),
        aspect.ratio = c(65/100)) 

base_size <- 8.5
asp <- 1.5
ggsave(
  filename = 'scatter.png', 
  plot = p,
  height = base_size, 
  width = base_size * asp
)


