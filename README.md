# Run locally
SBT - run `sbt ~reStart`  
IntelliJ - run `edu.knuca.resmat.Api`

# Build for Prod
1. run `sbt assembly`
2. run `./release_env.sh aws [true]` - true if resources need to be reloaded

# Run DB migrations
1. Update `src/main/resources/flyway.conf`
2. Run `edu.knuca.resmat.Migrator` app
 
