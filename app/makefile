HOST ?= 192.168.0.138
PORT ?= 3090

all:

.PHONY: run local

run: 
	 Rscript -e "shiny::runApp('.', port=$(PORT), host = '$(HOST)')"

local:
	 Rscript -e "shiny::runApp('.', port=$(PORT), host = '127.0.0.1')"

deploylive:
	Rscript -e "rsconnect::deployApp('.', appName='Fish-IBI-Calculator')"

deploytest:
	Rscript -e "rsconnect::deployApp('.', appName='Fish-IBI-Calculator-TESTING')"
