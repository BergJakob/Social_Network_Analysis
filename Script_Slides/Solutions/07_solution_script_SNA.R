# 1. Find the duplicated movie. How could you go across this?
# 2. Which director has made the longest movie?
# 3. What's the highest ranked movie?
# 4. Which movie got the most votes?
# 5. Which movie had the biggest revenue in 2016?
# 6. How much revenue did the movies in the dataset make every year in total?

library(tidyverse)

imdb <- read_csv("scripts/data/imdb2006-2016.csv")  %>% 
  select(title = Title, director = Director, year = Year, runtime = `Runtime (Minutes)`, rating = Rating, votes = Votes, revenue_million = `Revenue (Millions)`, rank = Rank) 

# 1
imdb %>% count(title) %>% 
  arrange(desc(n))

# 2
imdb %>% arrange(desc(runtime))

# 3
imdb %>% arrange(rank)

# 4
imdb %>% arrange(desc(votes))

# 5
imdb %>% filter(year == 2016) %>% arrange(desc(revenue_million))

# 6
imdb %>% group_by(year) %>% 
  summarize(rev_sum = sum(revenue_million, na.rm = TRUE)*1000000)

# 7. Filter movies following some conditions:
    # a. More runtime than the average runtime (hint: you could also use `mutate()` before).
imdb %>% filter(runtime > mean(runtime))

#    b. Movies directed by J. J. Abrams.
imdb %>% filter(director == "J.J. Abrams")

#    c. More votes than the median of all of the votes.
imdb %>% filter(votes > median(votes))

#    d. The movies which have the most common value in terms of rating (`mode()` does not exist -- run the script below and use the `my_mode` function).
imdb %>%  filter(rating == my_mode(rating))

# function
my_mode <- function(x){ 
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
         mod = NA
    else
         if(is.numeric(x))
    mod = as.numeric(names(ta)[ta == tam])
    else
         mod = names(ta)[ta == tam]
    return(mod)
} 