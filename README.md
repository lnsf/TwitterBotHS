# TwitterBot with DB

Twitter Bot with PostgreSQL, MeCab, and Haskell

## Usage

- Get tweets and insert database it.
```
tbot add
```

- Clean database.
```
tbot clean
```

- Post tweet.
```
tbot tweet
```

- Show help.
```
tbot help
```

All argments can be shortened  
like a, ad, tw...

## Configuration

Write the configuration in config.yaml. Then put it in the same directory as the executable file.

```config.yaml
name: screen name without @
keys:
  ck: consumer key
  cs: consumer key secret
  at: access token
  as: access token secret
```

## Database Setup

Install PostgreSQL.

```bash
$ sudo apt update
$ sudo apt install postgresql -y
$ sudo apt install postgresql-server-dev-(version) -y
```

Disable peer authentication.  

/etc/postgresql/(version)/main/pg_hba.conf
```
#local all all peer
local all all md5
```

Then start service

```bash
sudo systemctl enable postgresql
sudo systemctl start postgresql
```

Create user "bot" with password "postgres".

```bash
$ sudo passwd postgres
$ su - postgres

$ createuser -P bot
Enter password for new role: postgres
Enter it again: postgres
$ psql
postgres=# CREATE DATABASE bot;
postgres=# \q
```

Create table.
```bash
$ psql -U bot
bot=> CREATE TABLE words
bot-> (first TEXT,
bot(> second TEXT,
bot(> third TEXT,
bot(> id BIGINT
bot(> );
CREATE TABLE
bot=> \q
```
