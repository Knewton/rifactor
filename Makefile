# +--------------------+
# |     BUILD IMAGE    |  -------+  EXE
# +--------------------+         |
# |                    |         v
# |      DEV IMAGE     +-------------------+
# |                    |   RUNTIME IMAGE   |
# +--------------------+-------------------+
# |                LIB IMAGE               |
# +----------------------------------------+
# |              UBUNTU IMAGE              |
# +----------------------------------------+

default: docker-run

# LIB DOCKER IMAGE: RUNTIME FOR THIS PROJECT ADDED ON TOP "UBUNTU"
docker-lib:
	@ln -sf ./etc/docker/lib/Dockerfile .
	@docker build \
		--tag=rifactor:lib \
		$(PWD)

# DEV DOCKER IMAGE: DEV TOOLS FOR THIS PROJECT ADDED ON TOP "LIB"
docker-dev:
	@ln -sf ./etc/docker/dev/Dockerfile .
	@docker build \
		--rm=false \
		--tag=rifactor:dev \
		$(PWD)

# BLD DOCKER IMAGE: BUILD/TEST THIS PROJECT ON TOP OUR "DEV" IMAGE
docker-bld:
	@ln -sf ./etc/docker/bld/Dockerfile .
	@docker build \
		--rm=false \
		--tag=rifactor:bld \
		$(PWD)

# EXTRACT BINARY: COPY THE BINARY EXE FROM OUR PROJECT "BLD" IMAGE
rifactor: docker-bld
	@docker run \
		--volume=$(TMP):/host \
		rifactor:bld \
		cp /usr/local/bin/rifactor /host/
	@cp $(TMP)/rifactor .

# RUN DOCKER IMAGE: ADD THE PROJECT'S BINARY ON TOP OUR "LIB" IMAGE
docker-run: rifactor
	@ln -sf ./etc/docker/run/Dockerfile .
	@docker build \
		--tag=rifactor \
		$(PWD)

clean:
	@rm -f Dockerfile rifactor

.PHONY: \
	clean \
	default \
	docker \
	docker-bld \
	docker-dev \
	docker-lib \
	docker-run

TMP := $(shell mktemp -d)
