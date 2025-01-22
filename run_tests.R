#tests

#run sorkrank 5 times and see if it converge to the same value
df1 = sorkrank(file.path("data", "SORKIN_MATRIX.csv"))
df2 = sorkrank(file.path("data", "SORKIN_MATRIX.csv"))
df3 = sorkrank(file.path("data", "SORKIN_MATRIX.csv"))
df4 = sorkrank(file.path("data", "SORKIN_MATRIX.csv"))
df5 = sorkrank(file.path("data", "SORKIN_MATRIX.csv"))
#these numbers should all be close to 0
cor(df1$expV, df2$expV) - 1
cor(df2$expV, df3$expV) - 1
cor(df3$expV, df4$expV) - 1
cor(df4$expV, df5$expV) - 1

#is expV positively correlated with inflows?
df = read_csv(file.path("data", "SORKIN_MATRIX.csv"),
              col_names = c("firmid", "lagfirm", "N_workers")) 

df_test1 = df %>% 
  group_by(firmid) %>% 
  mutate(firmid = as.character(firmid)) %>% 
  inner_join(df1) %>% 
  summarise(inflows = sum(N_workers)) %>% 
            #in_value = weighted.mean(expV, N_workers)) %>% 
  mutate(firmid = as.character(firmid)) %>% 
  inner_join(df1) 

df_test = df %>% 
  group_by(lagfirm) %>% 
  summarise(outflows = sum(N_workers)) %>% 
            #out_value = weighted.mean(expV, N_workers)) %>% 
  rename(firmid = lagfirm) %>% 
  mutate(firmid = as.character(firmid)) %>% 
  inner_join(df_test1) %>% 
  mutate(weight = inflows + outflows)

min(df_test$inflows) #should equal 1
min(df_test$outflows) #should equal 1
min(df_test$weight) #should equal 2

lm(expV ~ outflows + inflows, 
   data = df_test) %>% summary()

# sanity test: simple network with a clear "ranking"

#make a graph with 3 vertices with a clear ranking 1 > 2 > 3 
#and verify that sorkrank returns the ranking
df = tibble(
  firmid    = c(1, 2, 1, 3, 2, 3), 
  lagfirm   = c(2, 1, 3, 1, 3, 2),
  N_workers = c(5, 1, 7, 1, 3, 1))
write_csv(df, file.path("data", "test.csv"), col_names = FALSE)
sorkrank(file.path("data", "test.csv")) %>% 
  mutate(expV_norm = expV/sum(expV))
g = graph_from_data_frame(df %>% select(lagfirm, firmid, N_workers))
plot(g,
     edge.width = E(g)$N_workers,
     edge.curved = -0.5)

# test for Raffa: string IDs are OK
#make a graph with 3 vertices with a clear ranking 1 > 2 > 3 
#and verify that sorkrank returns the ranking
df = tibble(
  firmid    = c("Apple", "Carrot", "Apple", "Bread", "Carrot", "Bread"), 
  lagfirm   = c("Carrot", "Apple", "Bread", "Apple", "Bread", "Carrot"),
  N_workers = c(5, 1, 7, 1, 3, 1))
write_csv(df, file.path("data", "test.csv"), col_names = FALSE)
sorkrank(file.path("data", "test.csv")) %>% 
  mutate(expV_norm = expV/sum(expV)) %>% 
  write_csv(file.path("data", "test_output.csv"))
g = graph_from_data_frame(df %>% select(lagfirm, firmid, N_workers))
plot(g,
     edge.width = E(g)$N_workers,
     edge.curved = -0.5)

# test for Raffa: random re-shuffling rows gives same answer
#make a graph with 3 vertices with a clear ranking 1 > 2 > 3 
#and verify that sorkrank returns the ranking
df = tibble(
  firmid    = c(5, 2, 5, 3, 2, 3), 
  lagfirm   = c(2, 5, 3, 5, 3, 2),
  N_workers = c(5, 1, 7, 1, 3, 1))
df = df %>% 
  mutate(rand = runif()) %>% 
  sort(rand) %>% 
  select(-rand)
write_csv(df, file.path("data", "test.csv"), col_names = FALSE)
g = make_graph_from_data(file.path("data", "test.csv"))
sorkrank(file.path("data", "test.csv")) %>% 
  mutate(expV_norm = expV/sum(expV))
