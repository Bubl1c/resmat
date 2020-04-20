# Run locally
SBT - run `sbt ~reStart`  
IntelliJ - run `edu.knuca.resmat.Api`

# Build for Prod
1. run `sbt assembly`
2. run `./release_env.sh aws [true]` - true if resources need to be reloaded

# Run DB migrations
1. Update `src/main/resources/flyway.conf`
2. Run `edu.knuca.resmat.Migrator` app
 
# AWS
```
ssh -i ~/.ssh/aws_key_pair.pem ubuntu@ec2-52-57-195-49.eu-central-1.compute.amazonaws.com
ssh -i ~/.ssh/aws_key_pair.pem ubuntu@ec2-3-120-209-125.eu-central-1.compute.amazonaws.com

sudo systemctl status resmat-api
sudo systemctl restart resmat-api
sudo systemctl stop resmat-api
sudo systemctl start resmat-api
```

# Bash
## Disk space usage
```
//BEST TOOL EVER
sudo apt install ncdu
ncdu /

//Quick look
df -h
//show only MB and GB
du -cha --max-depth=1 / | grep -E "M|G"
```
## Cleanup
```
sudo apt-get autoremove

// all to delete packages you have already downloaded. SAFE
sudo apt clean
//purge log files older than a week or two - will not delete newer/current logs. MOSTLY SAFE
sudo rm -f /var/log/*gz
//list all open files, but filter down to the ones which have been deleted from disk. FAIRLY SAFE
sudo lsof | grep deleted
//delete some temp files - if something's using them you could upset a process. NOT REALLY THAT SAFE
sudo rm /tmp/*
```