DOCKER_COMPOSE := $(shell (docker compose version > /dev/null 2>&1 && echo "docker compose") || echo "docker-compose")

all: build run

build:
	@$(DOCKER_COMPOSE) build haskell

run:
	@$(DOCKER_COMPOSE) run --rm haskell runhaskell -isrc src/main.hs
