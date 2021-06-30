BASEIMAGE := docker.dragonfly.co.nz/fish-ibi-calculator-18.04
IMAGE := $(BASEIMAGE):2021-07-01

UID ?= $(shell id -u)
GID ?= $(shell id -g)

RUN ?= docker run --rm \
			-v $$(pwd):/work -w /work \
			-e HOME=/work \
			-u $(UID):$(GID) \
			$(IMAGE)

docker: Dockerfile
	docker build --tag $(BASEIMAGE) . && \
	docker tag $(BASEIMAGE) $(IMAGE) && \
	docker push $(IMAGE) && \
	touch .push

local:
	docker run -it --rm -w /work -v $$(pwd):/work \
		--net=host \
		-e RUN= \
		-e DISPLAY=$DISPLAY \
		-v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0 \
		-u $$(id -u):$$(id -g) $(IMAGE) bash

