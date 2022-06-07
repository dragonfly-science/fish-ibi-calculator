baseimage := docker.dragonfly.co.nz/fish-ibi-shiny-20.04
image := $(baseimage):2022-06-07

docker: Dockerfile
	docker build --tag $(baseimage) . && \
	docker tag $(baseimage) $(image) #&& \
#	docker push $(image) && \
#	touch .push

local:
	docker run -it --rm -w /work -v $$(pwd):/work \
		--net=host \
		-e display=$display \
		-v /tmp/.x11-unix/x0:/tmp/.x11-unix/x0 \
		-u $$(id -u):$$(id -g) $(image) bash
