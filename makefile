baseimage := dragonflyscience/fish-ibi-shiny-20.04
image := $(baseimage):2022-06-15	
HOST := `hostname -I | awk '{print $$1}'`
PORT ?= 3090

RUN ?= docker run -it --rm --net=host --user=$$(id -u):$$(id -g) -v$$(pwd)/:/work -v /tmp/.x11-unix/x0:/tmp/.x11-unix/x0 -w /work $(image)

.PHONY: app

all: app

app: data/app-data.rdata
	 $(RUN) bash -c "cd app && Rscript -e \"shiny::runApp('.', port=$(PORT), host = '$(HOST)')\""
applocal: data/app-data.rdata
	 $(RUN) bash -c "cd app && Rscript -e \"shiny::runApp('.', port=$(PORT), host = '127.0.0.1')\""

deploylive: data/app-data.rdata
	$(RUN) bash -c "cd app && Rscript -e \"rsconnect::deployApp('.', appName='Fish-IBI-Calculator')\""

deploytest: data/app-data.rdata
	$(RUN) bash -c "cd app && Rscript -e \"rsconnect::deployApp('.', appName='Fish-IBI-Calculator-TESTING')\""

docker: Dockerfile
	docker build --tag $(baseimage) . && \
	docker tag $(baseimage) $(image) #&& \
	docker push $(image) && \
	touch .push

data/app-data.rdata: app/prepare-data.r \
		app/data/regional-council-2022-generalised.gpkg \
		app/data/joy-calibration.csv \
		app/data/regional_ibi_thresholds.csv \
		app/data/fish_code_lookup.csv
	$(RUN) bash -c "cd app && Rscript $(<F)"

local:
	docker run -it --rm -w /work -v $$(pwd):/work \
		--net=host \
		-e display=$display \
		-v /tmp/.x11-unix/x0:/tmp/.x11-unix/x0 \
		-u $$(id -u):$$(id -g) $(image) bash

test:
	echo $(HOST)
