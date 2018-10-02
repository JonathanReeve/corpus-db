# Corpus-DB

Corpus-DB is a textual corpus database for the digital humanities. This project aggregates public domain texts, enhances their metadata from sources like Wikipedia, and makes those texts available according to that metadata. This will make it easy to download subcorpora like: 

 - Dickens novels
 - Bildungsromane
 - Poetry published in the 1880s
 - Novels set in London

Corpus-DB has several components: 

 - Scripts for aggregating metadata, written in Python
 - The database, currently a few SQLite databases
 - A REST API for querying the database, written in Haskell (currently in progress)
 - Analytic experiments, mostly in Python
 
Read more about the database [at this introductory blog post](http://jonreeve.com/2017/06/project-gutenberg-the-database/). Scripts used to generate the database are in the [gitenberg-experiments repo](https://github.com/JonathanReeve/gitenberg-experiments). 

# Contributing

I could use some help with this, especially if you know Python or Haskell, have library or bibliography experience, or simply like books. Get in touch in the chat room, or [contact me via email](mailto:jon.reeve@gmail.com). 

[![Join the chat at https://gitter.im/corpus-db/Lobby](https://badges.gitter.im/corpus-db/Lobby.svg)](https://gitter.im/corpus-db/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Hacking

If you want to build the website and API, you can use the Haskell tool `stack`. 

```sh
stack build
cd src
export ENV=dev
stack runhaskell Main.hs
```

If you use ENV=dev, this will set the database path to `/data/dev.db`, which is a 30-row subset of the main database, since the main database is too big (16GB at the moment) to put on GitHub. You can use this dev database for hacking around on. If you need the full database for some reason, let me know.

You can also use Nix: 

```sh
nix-shell
cd src
ENV=dev runhaskell Main.hs
```

Or build the project with: 

```sh
nix-build shell.nix
ENV=dev result/bin/corpus-db
```

## Upcoming Changes

I'm rewriting corpus-db from scratch on the `next` branch (see [issues labeled `2.0`](https://github.com/JonathanReeve/corpus-db/issues?q=is%3Aissue+is%3Aopen+label%3A2.0)). This is to make the whole toolchain in Corpus-DB repeatable, in case of data loss, and future-proof, so that it can ingest new texts from Project Gutenberg and other sources as they arrive. Feel free to help out with this! Here's what the next version will do:

1. Parse Project Gutenberg RDF/XML metadata, directly from the source, and put it into a database. 
2. Mirror PG texts, using an rsync script.
3. Clean PG texts, and add them to that database. Also add HTML files. 
4. Write an ORM-level database layer, using Persistent, for more native DB interactions and typesafe queries.

From there, I'll start adding more text sources (British Library documents from Git-Lit, etc.). 
