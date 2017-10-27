EIGEN CODE 

#This code can be used to analyse co occurence, or in human language co publishing in the same journals of a specific set of authors. Its kind of difficult to 
#understand but idea is that the grouping table should be Authors.ID, as you then group journals to authors. Adding names is still beta yo. 

Authors_and_journals <- read_excel("~/Authors and journals.xlsx")
> View(Authors_and_journals)
> author <- Authors_and_journals %>%
  +     select(AuthorID.ID,Journals.ID) %>%
  +     distinct() %>%
  +     as.data.frame()

author<-transform(author, AuthorID.ID = as.numeric(AuthorID.ID))

Count_author<-author %>%
  group_by(AuthorID.ID) %>%
  tally() %>%
  rename(Count_authors = n) %>%
  mutate(source = row_number()) %>%
  as.data.frame()

author1 <- rename(author, AuthorID.ID1 = AuthorID.ID)

Count_author1 <- Count_author %>%
 rename(AuthorID.ID1 = AuthorID.ID) %>%
rename(Count_authors1 = Count_authors) %>%
 rename(target = source) %>%
  as.data.frame()

Count_coAUJ <- author %>%
  inner_join(author1) %>%
  group_by(AuthorID.ID, AuthorID.ID1)  %>%
  tally() %>%
  ungroup() %>%
  inner_join(Count_author) %>%
  inner_join(Count_author1) %>%
  filter(AuthorID.ID < AuthorID.ID1) %>%
  mutate(weight = n/(Count_authors + Count_authors1 - n)) %>%
  as.data.frame()

#6 Edge list
Edge <- Count_coAUJ %>%
  select(source, target, weight) %>%
  mutate(type = "Undirected") %>%
  as.data.frame()

write.csv(Edge, file = "Edge.csv", row.names = F)
#node list

Node_global <- Count_author %>%
  rename(id = source) %>%
  rename(label = AuthorID.ID) %>%
  as.data.frame()

write.csv(Node_global, file = "Node.csv", row.names = F)

Authornode <- author %>%
  group_by(AuthorID.ID) %>%
  as.data.frame()
