# rtweet bookmarks

library(rtweet)
auth_setup_default()

friends <- get_friends("riverpeek")
friends
friend_names <- lookup_users(friends$to_id)

# get followers
followers <- get_followers("riverpeek")
followers_names <- lookup_users(followers$to_id)


# get favs
favs <- get_favorites("riverpeek", n=1000)
favs
head(favs)

# save out
save(friend_names, followers_names, favs, file = "data_raw/twitter_stuff_2022_nov.rda")
