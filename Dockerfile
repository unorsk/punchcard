FROM haskell:9.4.4

WORKDIR /opt/punchcard
COPY . /opt/punchcard

RUN apt-get update -y && apt-get install -y ca-certificates
RUN cabal update && cabal install

EXPOSE 3000:3000
CMD ["server"]