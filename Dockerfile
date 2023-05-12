FROM haskell:9.4.4

WORKDIR /opt/punchcard
COPY . /opt/punchcard

RUN cabal update && cabal install

EXPOSE 3000:3000
CMD ["server"]