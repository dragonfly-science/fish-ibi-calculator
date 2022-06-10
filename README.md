# fish-ibi-calculator

To run locally, run `make app` in the root of the repository.
This will serve a local version of the shiny app to localhost:3090

`makefile` also includes commands for:

- building and pushing the docker image (`make docker`)
- deploying the app to testing and production sites (`make deploylive` `make deploytest`)
- entering the docker container in an interactive shell (`make local`)

The dockerfile is hosted on the publicly accessible Dragonfly dockerhub registry.

If you make edits to the dockerfile, please update the docker tag in the makefile.
