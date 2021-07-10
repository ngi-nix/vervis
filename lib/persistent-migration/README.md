# persistent-migration

**persistent-migration** is a Haskell library that lets you describe your DB
migrations in terms of `persistent` entities, and it handles the details of the
`persistent` backend for you. Whenever you change your `persistent` model, all
you need to do is to list a typed description of the change in the list of
migrations, and the library has everything else covered.

Currently only the SQL backend is supported, and only PostgreSQL (because
that's what I've been using), but it's perfectly possible to add typeclass
instances for other backends and database management systems.

# Motivation

There is a variety of database migration tools out there, including in the
Haskell ecosystem and Hackage. However these migration tools work with
migration instructions written directly in SQL. This makes sense of course, but
it also makes it more difficult to write and read them. If your code has a
high-level persistent model, translating it in your head into SQL and figuring
out the types and small details is cumbersome.

In the Haskell `persistent` library, these details are handled automatically
for you. The migration process tries to guess which changes you made, and
adapts the database for you. When it doesn't, or when its guess involves
changes it considers unsafe, it gives you the details so that you can manually
run the necessary migration SQL queries.

I'm not in the software industry and I don't write single instance
software-as-a-service or centralized web app code, but I'm guessing that this
way of migration your database works nicely when you, the developer, run the
instance(s) of the web app and you can manually tweak the migrations and DB
schema when needed. And it's a one-time operation for each change made to the
DB.

What I *have* been writing, however, is web app code that is free-as-in-freedom
software and is meant to be run by everyone and have an unlimited number of
instances out there. For this reason, I can't rely on any manual tricks and
tweaks - the migration process has to be automatic and run without user
intervention when someone builds my web app and runs it and later upgrades it
to newer versions with changes in the DB schema.

To summarize, the situation is like this:

1. I write the persistent model in terms of `persistent` entities, not in terms
   of SQL, and that's how I think of them in my mind and I don't need to
   remember the SQL details behind them.
2. I write free software meant to be used by any interested person, and DB
   schema changes should therefore be handled automatically for the users

So when, for example, I change some entity field's type from A to B in my model
file, I just want to tell my web app's migration tool "change this field's type
from A to B and use the following function to convert the values" and not worry
about the SQL details, and just move on to my next coding task. `persistent`
knows all the SQL behind my nice elegant model, and I'd like it to handle these
details for me. I *can* figure out the SQL names of my entities and fields, I
*can* figure out the constraints and the SQL types behind my Haskell types,
it's just that I don't want to have to think about it every single time I
change something in my persistent model.

An additional point is that a web app with many instances may support more than
one DBMS, and each DBMS has its own SQL queries for schema changes, and in
general all kinds of small and big differences from other DBMSs. By describing
changes in terms of `persistent` models, the migrations are able to cover the
various `persistent` backends and DBMSs your web app may support.

# How It Works

There is more than one way to provide these requirements. This library doesn't
necessarily do it the best way, and I do want to put more thought into that
eventually, but right now here's how it works.

In your Haskell code, you define a list of DB schema changes and DB
transactions. You can use the provided typed ones, or write custom SQL. You
then pass this list of changes to a function `[Change] -> IO ()` which connects
to the database and runs the necessary migrations. And you can use this
function in the initialization code of your app.

Perhaps the biggest issue with this approach is that you may use some detail
from your code in the description of a change, for example some type that you
defined, and later, weeks or months later, change that type and break the
migrations. For now the solution is to be careful: Whenever you change a type
that is used by the persistent model, check in the model and in the list of
changes, and update where necessary to avoid changing old migrations.

A better approach for the future of this library may be for the developer to
specify the change using the same terms, except it is instantly converted to
SQL and stored. And the high-level change description is stored along with it,
at least as a comment, for the convenience of the developer and users running
the app.
