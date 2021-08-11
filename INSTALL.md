Vervis is still in early development and the build process gets updates once in
a while, but this file tries to keep up and list the latest instructions for
running a Vervis instance.

At the time of writing, you can get a running Vervis instance if you follow the
steps below.

UPDATE: There is a binary build you can use instead of building from source. It
can be found [here](https://box.angeley.es/nextcloud/s/oWHmQDtWTAfPR4Z). If you
use it, verify the download using `sha256sum` and `gpg`, make sure the settings
in your `settings.yml` (see below) match the downloaded file paths, and run the
web app using `./bin/vervis` and not `stack run`. This may be confusing;
hopefully I'll make a nicer binary release soon :)

# (1) System libraries

Install dependency library development packages. It's very likely you already
have them all installed, and if you're missing some, the build process will
inform you. But it's still nice to have a list here. The list below isn't a
complete list, it's just libraries that people have found missing while
building, and let me know.

- PostgreSQL client library
- ZLib
- libssl
- libpcre

On Debian based distros, installation can be done like this:

    $ sudo apt install libpq-dev zlib1g-dev libssl-dev libpcre3-dev

# (2) The Stack build tool

Install stack. To install stack, go to its [website](https://haskellstack.org)
and follow the instructions.

# (3) Version control systems Darcs and Git

Install Darcs. You can grab it from your distro, e.g.:

    $ sudo apt install darcs

If you're going to create a Git repository on Vervis, you'll need Git too, you
can install it from a distro package too, e.g.:

    $ sudo apt install git

# (4) The Vervis source code

Clone the Vervis repo:

    $ darcs clone https://dev.angeley.es/s/fr33domlover/r/vervis
    $ cd vervis

Clone dependency libraries:

    $ ./update-deps.sh

# (5) Configuration and database

Generate a new SSH key with a blank password:

    $ ssh-keygen -t rsa -f config/ssh-host-key

Install PostgreSQL. You'll need the server and the client library development
files. Note that PostgreSQL needs to be at least version 9.5.

    $ sudo apt install postgresql libpq-dev

Switch to `postgres` system user:

    $ sudo su - postgres

Create a PostgreSQL user.

With password:

    $ createuser --no-createdb --no-createrole --no-superuser --encrypted --pwprompt vervis

No password (if you run Vervis as a user by the same name as the DB user):

    $ createuser --no-createdb --no-createrole --no-superuser vervis

Create a PostgreSQL database:

    $ createdb --encoding=UTF8 --owner=vervis vervis

Update the settings to specify correct database connection details and other
settings.

    $ cp config/settings-default.yaml config/settings.yml
    $ vim config/settings.yml

# (6) GHC Haskell compiler

`stack` can automatically install the correct GHC version for you, in an
isolated location that doesn't conflict with any system packages. Unless you
have a specific reason to get GHC in some other way, getting it through `stack`
is recommended.

If you'd like to install GHC manually (from a distro package, from a PPA,
etc.), this is the time to do so. And I trust you to arrange things such that
`stack` uses your manually downloaded GHC. Otherwise, simply proceed to the
next step.

# (7) Build Vervis

Build. This will also automatically install GHC.

    $ stack build

# (8) Development and deployment

To update your local clone of Vervis, run:

    $ darcs pull
    $ ./update-deps.sh
    $ stack build

For convenience, at least on actual deployments, you may wish to run the Vervis
SSH server on port 22, so that people don't have to specify a custom port. For
that to work, the user that runs the Vervis server needs to get permission to
bind to ports below 1024. There are several ways to do that. One of them is to
use file capabilities to give the Vervis executable the permission to bind to
such ports (if you prefer not to trust the code, try one of the other methods,
such as sudo):

    $ sudo setcap CAP_NET_BIND_SERVICE=+ep `stack exec which vervis`

Vervis uses various key files for cryptography and other data generation, and
once these key files are created, they shouldn't change. For some of them, it's
*critical* they don't change, because some usage or interpretation of data in
the PostgreSQL database depends on them. For this reason, by default, key file
loading happens as follows: When Vervis runs for the first time (this is
checked by detecting that the database is empty, no tables), it generates and
writes key files, and it's an error if any of them already exist. Otherwise, on
the next time(s) Vervis runs, it requires all key files to exist, and an error
is raised if any are missing.

If you're running Vervis for the first time, i.e. the database is still empty,
and for some reason you'd like Vervis to load some existing key files, while
generating the rest, run this:

    $ touch _keyfile_import_existing

Run.

    $ stack run

By default, Vervis is configured with User Registration disabled.  This is to
prevent any automatic spambot registration for bots that may be monitoring the
Federated Network.  In order to enable user registration on your instance,
change "registration: false" to "registration: "true" in the config/
settings.yml file.  Also, the maximum # of accounts is limited to 3.  You
can increase/decrease this setting to change the maximum.  If you do not wish
to have a limit, you can comment out this line entirely.

When you update Vervis to a newer version, it's possible the software now uses
some new key files, and Vervis will raise an error about those key files
missing in the filesystem. You can ask Vervis to generate missing files, and
load the rest as usual:

    $ touch _keyfile_write_missing
    $ stack run

Browse to `http://localhost:3000` and have fun!

`yesod devel` is another way to run the application, useful for rapid
development, but I haven't been using it and I'm not sure it works, possibly I
broke something along the way. But feel free to try!

I have a little script for deploying Vervis on my server. I just haven't
published it yet and haven't explained how it works. If you're interested, ask
me about it, and it will motivate me to write about it sooner :)
