FROM haskell:8 as build
WORKDIR /opt/backend
RUN apt-get update
RUN yes | apt-get install postgresql libpq-dev
RUN cabal update
COPY ./backend.cabal /opt/backend
RUN cabal build --only-dependencies -j4
COPY . /opt/backend
RUN cabal install
CMD ["backend"]