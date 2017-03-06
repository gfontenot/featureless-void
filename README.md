# Featureless Void

A thing into which you can scream

## What the?

This is the code that powers [my personal
microblog](https://micro.gordonfontenot.com). It's inspired heavily by [Edward
Loveall][edward-micro]'s microblog, as well as [Manton Reece][manton]'s ideas
around indie microblogging. Once it launches, this will be hooked up to
[micro.blog] as well.

[micro.gordonfontenot.com]: http://micro.gordonfontenot.com
[edward-micro]: https://edwardloveall.com/microblog
[manton]: http://www.manton.org/
[micro.blog]: http://micro.blog

## Setup

This project is written in Haskell, using [Yesod]. It requires a working
[`stack`](https://docs.haskellstack.org/en/stable/README/) installation, as
well as a working (and running) installation of Postgres.

For initial project setup:

```
$ bin/setup
```

This will set up the postgres database, stack environment, install
dependencies, and build the app.

## Development

You can run the server using the `yesod` command:

```
$ yesod devel
```

You should now be able to navigate to `localhost:3000` and see the site
running.
