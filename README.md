# mu-project

Bootstrap a mu.semte.ch microservices environment in three easy steps.

## How to

Setting up your environment is done in three easy steps:  first you configure the running microservices and their names in `docker-compose.yml`, then you configure how requests are dispatched in `config/dispatcher.ex`, and lastly you start everything.

### Hooking things up with docker-compose

Alter the `docker-compose.yml` file so it contains all microservices you need.  The example content should be clear, but you can find more information in the [Docker Compose documentation](https://docs.docker.com/compose/).  Don't remove the `identifier` and `db` container, they are respectively the entry-point and the database of your application.  Don't forget to link the necessary microservices to the dispatcher and the database to the microservices.

### Configure the dispatcher

Next, alter the file `config/dispatcher.ex` based on the example that is there by default.  Dispatch requests to the necessary microservices based on the names you used for the microservice.

### Boot up the system

Boot your microservices-enabled system using docker-compose.

    cd /path/to/mu-project
    docker-compose up

You can shut down using `docker-compose stop` and remove everything using `docker-compose rm`.


## Dataset info

Some of the information in the dataset is not explicited.

### Icons

Currently the following URIs exist as information icons to be attached:

| Name       | URI                                                            |
|------------|----------------------------------------------------------------|
| Puntafish  | http://veeakekr.be/labels/48844b54-2080-4190-b115-e5b34e742ad7 |
| Natuurpunt | http://veeakker.be/labels/1a3c72ae-8c6b-4c5c-b249-f2ef852ba827 |
| Frozen     | http://veeakker.be/labels/3a134729-ba29-4612-885a-15bc6bd33b66 |

These URIs may be hardcoded in frontends, extending the list means searching through the code.
