
all: build run

build:
	@docker compose build haskell

run:
	@docker compose run --rm haskell runhaskell -isrc src/main.hs

