# The Xnoe Blog

This is a highly work-in-progress blog frontend and backend written in Haskell and Elm-lang, using PostgreSQL for the database, it is being developed with a Docker-first mindset to make development a breeze.

The blog has the following functionality

- Creating Posts
- Editing Posts
- Deleting Posts

Additionally, it tracks the category that posts are in, allowing filtering posts by category.

Xnoeblog is intended to be used with Docker, an example compose file is provided within this repository. The backend container can be configured using the following environment variables

- DB_HOST: Hostname of the database
- DB_USER: Database user
- DB_PASS: Database user password
- DB_NAME: Name of database to use
- XNOEBLOG_USER: Default user 
- XNOEBLOG_PASS: Default password

All of these values are optional. Xnoeblog backend defaults to creating no user and using the following values for database if no environment variables are provided

- DB_HOST=db
- DB_USER=root
- DB_PASS=password
- DB_NAME=xnoeblog