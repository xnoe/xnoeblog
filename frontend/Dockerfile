FROM alpine as alpine
RUN apk --no-cache add curl
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin

WORKDIR /opt/frontend
COPY . /opt/frontend
RUN elm make src/Main.elm

FROM nginx:alpine

COPY --from=alpine /opt/frontend/index.html /usr/share/nginx/html/index.html