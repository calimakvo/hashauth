## Initialize

```
stack new hashauth yesodweb/postgres
```

## Database Setup

```
createuser -U postgres hashauth --pwprompt
createdb -U postgres hashauth -E UTF-8
createdb -U postgres hashauth_test -E UTF-8
psql -U hashauth hashauth -f sql/01_init.sql
CREATE TABLE
psql -U hashauth hashauth -f sql/02_insuser.sql
INSERT 0 1
```

```
$ stack exec -- yesod devel
```

## Login Account

* LoginId

  * adminuser
  
* password

  * eeee1234
