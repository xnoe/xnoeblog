# The Xnoe Blog

This is a highly work-in-progress blog frontend and backend written in Haskell and Elm-lang, using PostgreSQL for the database, it is being developed with a Docker-first mindset to make development a breeze.

Currently the only functionality that exists is providing a really bad card view of the posts that exist.

You will need to create a table called posts manually if you want to use this, the following is necessary

```sql
CREATE TABLE posts(title varchar(128), subtext varchar(128), category varchar(128));
```

And you will need to add any entries manually.

Exposed on port 80