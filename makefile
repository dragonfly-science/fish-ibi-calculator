baseimage := dragonflyscience/fish-ibi-shiny-20.04
image := $(baseimage):2022-06-07

PORT ?= 3090

RUN ?= docker run -it --rm --net=host --user=$$(id -u):$$(id -g) -v$$(pwd)/:/work -v /tmp/.x11-unix/x0:/tmp/.x11-unix/x0 -w /work $(image)

.PHONY: app

all: app

app:
	 $(RUN) bash -c "cd app && Rscript -e \"shiny::runApp('.', port=$(PORT), host = '127.0.0.1')\""

deploylive:
	$(RUN) bash -c "cd app && Rscript -e \"rsconnect::deployApp('.', appName='Fish-IBI-Calculator')\""

deploytest:
	$(RUN) bash -c "cd app && Rscript -e \"rsconnect::deployApp('.', appName='Fish-IBI-Calculator-TESTING')\""

docker: Dockerfile
	docker build --tag $(baseimage) . && \
	docker tag $(baseimage) $(image) #&& \
	docker push $(image) && \
	touch .push

local:
	docker run -it --rm -w /work -v $$(pwd):/work \
		--net=host \
		-e display=$display \
		-v /tmp/.x11-unix/x0:/tmp/.x11-unix/x0 \
		-u $$(id -u):$$(id -g) $(image) bash
