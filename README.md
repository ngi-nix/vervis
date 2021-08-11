# Vervis

**Vervis** is a project hosting and management application, with a focus on
software projects and decentralization. There is still a lot to do, but if you'd
like to try it, a few things more or less work. See below for details.

Vervis is free software, and is committed to software freedom. Most of its code
is in the public domain using the CC0 Public Domain Dedication. The application
as a whole however is released under AGPL version 3. For the legal details, see
the file `COPYING`. Please use and reuse Vervis for a Good purpose, and share
your work too in the same spirit.

## Try It

You can either use the existing instance mentioned below, or install your own
instance (see `INSTALL.md`).

My focus has been on writing the backend side of features and exploring various
experimental features. As a result, the user interface and visual design and
page styles (i.e. the whole UX and UI) are in a bad shape right now, so I want
to write a few little guidelines to help you find your way until this situation
improves.

There is a Vervis instance running at <https://dev.angeley.es>, and I
store my code on it. Feel free to use it and try it, however do please treat it
as a demo, as it's not a reliable stable instance for general use yet. If for
any reason you do with to keep some code and tickets etc. on it for actual use,
please contact me to let me know.

Here are the steps to make the most out of it at this point (at least at the
time of writing, I'll try to keep this little tutorial up to date)!

1. Create an SSH key if you don't have one. Most likely you do, for pushing to
   your existing repos hosted on your favorite project hosting platform.
2. Browse to the instance I linked above, and click *Sign up*. Choose a
   username and a password. Email address is currently optional on this
   instance, but it's going to be required soon and used for things like
   password reset, so please do provide a real email address.
3. Log in. Once you log in, the front page will change, and among other things,
   it will link to your SSH keys. Go there to add your SSH key. In the
   algorithm box you paste the first part e.g. `ssh-rsa`, and in the second box
   you paste the long scary part that follows it.
4. The front page also links you to repos and projects, you can create a
   project and open tickets. You can also create a Git or Darcs repo, and
   optionally associate it with a project. A project can have any number of
   repos asociated with it, sharing the same ticket tracker. Creating a project
   or repo may ask you to create a project or repo role; the front page has
   links for creating these
5. You can browse all the projects and repos hosted on the instance.
6. Add the following to your `~/.ssh/config` file, or create it if doesn't
   exist. To be honest I'm not sure it's still required, and haven't tested
   yet. If you find that this step is unnecessary and everything works without
   it, let me know :)

        Host dev.angeley.es
            ControlMaster no
            ForwardAgent no
            ForwardX11 no

Once you create a repository, it is possible that initially, trying to look at
it will give you 404. It should be okay though once you push some commit into
it.

The examples below use Darcs, but it's exactly the same for Git. They are
equally supported.

Cloning repositories works over (unauthenticated) HTTPS and (authenticated)
SSH.

**HTTPS clone:**

    $ darcs clone https://dev.angeley.es/s/fr33domlover/r/vervis

**SSH clone and push:**

If your system username and Vervis username are identical:

    $ darcs clone dev.angeley.es:wistera

If the repo is under another user:

    $ darcs clone dev.angeley.es:aviva/poems

If usernames are different:

    $ darcs clone luke@dev.angeley.es:wistera

If the repo is under another user:

    $ darcs clone luke@dev.angeley.es:aviva/poems

A few more little notes:

- No password reset, it's already implemented but not deployed yet at the time
  of writing these words
- A few more features are available, such as ticket workflows and ticket claim
  requests, but they are less important so I'm not going into them at this
  point
- Deletion of users, projects and repos may not work
- Login session should last for 2 hours after last access
- The instance I'm running serves only HTTPS, no plain HTTP
- There's no HTTPS push
- Merge requests not implemented yet
- I started working on federation using ActivityPub, I'll update here when it's
  ready for trying against ActivityPub clients and servers
- Feedback is very very very very very welcome and needed!!! :)

## Features

This is a list of initial features I plan to support in Vervis. It helps me get
an overview of more-or-less what's left to do before the first release. It
hasn't been updated in a long time, actually, but soon I'll get to it.

    [ ] - To do
    [~] - Work in progress
    [/] - Initial coding done, needs tests and polishing
    [x] - Complete

    [ ] User management
      [~] View your personal overview
      [~] Other users' pages
      [/] Register
      [ ] Delete account
      [/] Log in
      [/] Log out
      [ ] Reset password
      [ ] TLS client cert
        [ ] Upload it
        [ ] View it
        [ ] Log in using it
      [ ] Change profile details
      [/] Key management
        [/] Add key
        [/] Delete key
        [/] View keys
    [ ] Project management
      [/] Tickets
      [ ] Wiki
      [ ] Kanban
      [ ] Merge requests
    [ ] Repo management
      [ ] Delete repo
      [/] Add repo
      [ ] Rename repo
      [ ] Edit repo settings
      [ ] View repo content
        [/] File tree
        [ ] File content
          [/] Plain text
          [/] Syntax highlighting
          [/] Document rendering
          [ ] Images
          [ ] Audio
          [ ] Video
          [ ] Raw file download
    [ ] Darcs
      [ ] Create new repo
        [/] Web
        [ ] SSH
      [ ] Delete repo
        [/] Web
        [ ] SSH
      [/] Clone
        [/] HTTP
        [/] SSH
      [/] Pull
        [/] HTTP
        [/] SSH
      [ ] Push
        [ ] HTTP
        [/] SSH
      [/] View
        [/] History
        [/] Files
    [ ] Git
      [ ] Create new repo
        [/] Web
        [ ] SSH
      [ ] Delete repo
        [/] Web
        [ ] SSH
      [ ] Clone
        [/] HTTP
        [/] SSH
        [ ] Git
      [ ] Pull
        [/] HTTP
        [/] SSH
        [ ] Git
      [ ] Push
        [ ] HTTP
        [/] SSH
      [/] View
        [/] History
        [/] Files
    [~] Federation

## Reliability requirements

These features are considered critical for a Vervis instance to be a reasonably
reliable place where people can host their projects and repos, and not worry
about some bug or missing feature causing loss of data or access.

    [ ] Password reset, at least over plain email, either automatically or a
        temporary manual hack which will work at least for personal instances
    [x] Git pull and push over SSH fully working
    [x] Darcs pull and push over SSH fully working
    [x] SSH server implementation really secure as far as I know
    [x] TLS support, especially critical when sending passwords. For now, done
        externally through Lighttpd, not Vervis itself
    [x] Darcs pull over HTTP and HTTPS
    [x] Git pull over HTTP and HTTPS
    [ ] Clear policy and guidelines for DB schema changes
    [ ] Running instance has data backups

## Federation

See `FEDERATION.md`.

## Installation

See `INSTALL.md`.

## Using

See the .cabal file for more info and link to project website and version
control.

The official download location is the Darcs repository:

<https://dev.angeley.es/s/fr33domlover/r/vervis>

There is a backup repo, not always up to date though, at:

<https://hub.darcs.net/fr33domlover/vervis>

See the file `INSTALL.md` for a detailed usage and deployment guide. The file
`ChangeLog` explains how to see the history log of the changes done in the
code. `NEWS.md` provides a friendly overview of the changes for each release.

## Reporting Bugs and Suggesting Features

If you found a bug, or you have an idea, a feature request or a wishlist item,
open a ticket for it! Even if you're going to implement something or try to
solve it.

If you're going to implement some feature or fix some bug you found, **start by
opening a ticket** [here](https://dev.angeley.es/s/fr33domlover/p/vervis/t) so
that other people will know which features are being developed and who does
what.
