created: 2016-02-27T13:04:38+08:00
tags: [git]


## 安装 docker

```
pacman -S docker
```


## docker-compose.yaml

```yaml
postgresql:
  restart: always
  image: sameersbn/postgresql:9.4-8
  environment:
    - DB_USER=gitlab
    - DB_PASS=password
    - DB_NAME=gitlabhq_production
  volumes:
    - /home/fu/gitlab/postgresql:/var/lib/postgresql
gitlab:
  restart: always
  image: sameersbn/gitlab:8.2.1-1
  links:
    - redis:redisio
    - postgresql:postgresql
  ports:
    - "10080:80"
    - "10022:22"
  environment:
    - DEBUG=false
    - TZ=Asia/Shanghai

    - GITLAB_SECRETS_DB_KEY_BASE=long-and-random-alphanumeric-string

    - GITLAB_HOST=192.168.1.100
    - GITLAB_PORT=10080
    - GITLAB_SSH_PORT=10022
    - GITLAB_RELATIVE_URL_ROOT=

    - GITLAB_NOTIFY_ON_BROKEN_BUILDS=true
    - GITLAB_NOTIFY_PUSHER=false

    - GITLAB_BACKUPS=daily
    - GITLAB_BACKUP_TIME=01:00

    - SMTP_ENABLED=true
    - SMTP_DOMAIN=smtp.qq.com
    - SMTP_HOST=smtp.exmail.qq.com
    - SMTP_PORT=25
    - SMTP_USER=admin@xxxxx.com
    - SMTP_PASS=xxxxx
    - SMTP_AUTHENTICATION=login
    - SMTP_STARTTLS=true
    - GITLAB_EMAIL=admin@xxxxx.com
  volumes:
    - /home/fu/gitlab/git:/home/git/data
redis:
  restart: always
  image: sameersbn/redis:latest
  volumes:
    - /home/fu/gitlab/redis:/var/lib/redis
```


## run

```bash
mkdir gitlab
cd gitlab
mkdir git postgresql redis
docker-compose up
```
